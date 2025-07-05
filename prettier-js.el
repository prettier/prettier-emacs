;;; prettier-js.el --- Minor mode to format code on file save  -*- lexical-binding: t; -*-

;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Author: James Long and contributors
;; Created: 10 January 2017
;; URL: https://github.com/prettier/prettier-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience wp edit js

;; This file is not part of GNU Emacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;; * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.)

;;; Commentary:
;; Formats your code using 'prettier' on file save.

;;; Code:

(eval-when-compile
  (require 'org-element))

(declare-function org-element-type "org-element-ast")

(defgroup prettier-js nil
  "Minor mode to format code on file save"
  :group 'languages
  :prefix "prettier-js"
  :link '(url-link :tag "Repository" "https://github.com/prettier/prettier"))

(defcustom prettier-js-command "prettier"
  "The `prettier' command."
  :type 'string
  :group 'prettier-js)

(defcustom prettier-js-diff-command "diff"
  "The `diff' command for generating RCS diff."
  :type 'string
  :group 'prettier-js)

(defcustom prettier-js-args '()
  "List of args to send to prettier command."
  :type '(repeat string)
  :group 'prettier-js)

(defcustom prettier-js-use-modules-bin nil
  "When non-nil, search for prettier in node_modules/.bin/ directory.
Starts searching from the current directory and moves up until found.
If not found, an error is reported."
  :type 'boolean
  :group 'prettier-js)

(defcustom prettier-js-show-errors 'buffer
    "Where to display prettier error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite prettier's echo output if used from inside
a `before-save-hook'."
    :type '(choice
            (const :tag "Own buffer" buffer)
            (const :tag "Echo area" echo)
            (const :tag "None" nil))
      :group 'prettier-js)

(defcustom prettier-js-width-mode nil
  "Specify width when formatting buffer contents."
  :type '(choice
          (const :tag "Window width" window)
          (const :tag "Fill column" fill)
          (const :tag "None" nil))
  :group 'prettier-js)

(defvar prettier-js-daemon-commands '("prettier_d" "prettierd")
  "List of prettier daemon commands that put errors in stdout rather than stderr.")

(defvar-local prettier-js-error-state nil
  "Indicates if there's an error with the prettier executable.
When non-nil, contains the error message to display.")

(defun prettier-js--goto-line (line)
  "Move cursor to line LINE."
  (goto-char (point-min))
    (forward-line (1- line)))

(defun prettier-js--apply-rcs-patch (patch-buffer &optional start)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset (if start (* (- (line-number-at-pos start) 1) -1) 0)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in prettier-js--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (prettier-js--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (let ((beg (point)))
                  (forward-line len)
                  (delete-region (point) beg))))
             (t
              (error "Invalid rcs patch or internal error in prettier-js--apply-rcs-patch")))))))))

(defconst prettier-js--error-explanations
  '(("env: node: No such file or directory" . ("Node executable not found" "Could not find node executable; is it on Emacs' exec-path?")))
  "Alist mapping error patterns to user-friendly error messages.")

(defun prettier-js--get-error-explanation (error-content)
  "Check ERROR-CONTENT for known error patterns and return explanation if found."
  (cl-some (lambda (explanation)
             (when (string-match-p (car explanation) error-content)
               (cdr explanation)))
           prettier-js--error-explanations))

(defun prettier-js--show-error-buffer (filename errorfile errbuf)
  "Show error buffer for FILENAME using ERRORFILE content in ERRBUF."
  (with-current-buffer errbuf
    (if (eq prettier-js-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (prettier-js--kill-error-buffer errbuf))
      (insert-file-contents errorfile nil nil nil)
      ;; Convert the prettier stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "prettier errors:\n")
      (while (search-forward-regexp "^stdin" nil t)
        (replace-match (file-name-nondirectory filename)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun prettier-js--process-errors (filename errorfile errbuf)
  "Process errors for FILENAME, using ERRORFILE, displaying the output in ERRBUF."
  (let* ((error-content (with-temp-buffer
                          (insert-file-contents errorfile)
                          (buffer-string)))
         (explanation (prettier-js--get-error-explanation error-content)))
    (if explanation
        (progn
          (setq prettier-js-error-state (car explanation))
          (user-error (cadr explanation)))
      (message "Could not apply prettier")
      (when errbuf
        (prettier-js--show-error-buffer filename errorfile errbuf)))))

(defun prettier-js--kill-error-buffer (errbuf)
  "Kill buffer ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (with-current-buffer errbuf
        (erase-buffer))
      (kill-buffer errbuf))))

(defun prettier-js--width-args ()
  "Return prettier width arguments based on `prettier-js-width-mode'.
If mode is `window', use the window width.
If mode is `fill', use the fill-column value.
Otherwise, return nil."
  (pcase prettier-js-width-mode
    ('window (list "--print-width" (number-to-string (window-body-width))))
    ('fill (list "--print-width" (number-to-string fill-column)))))

(defun prettier-js--find-node-modules-bin ()
  "Find the node_modules/.bin/prettier executable.
Search starts from the current directory and moves up until found.
Returns the path to the executable if found, nil otherwise."
  (let ((dir (expand-file-name default-directory))
        (prettier-path nil)
        (executable-name (if (eq system-type 'windows-nt)
                             "prettier.cmd"
                           "prettier")))
    (while (and dir (not prettier-path))
      (let ((candidate (expand-file-name (concat "node_modules/.bin/" executable-name) dir)))
        (if (file-executable-p candidate)
            (setq prettier-path candidate)
          (let ((parent (file-name-directory (directory-file-name dir))))
            (if (or (not parent) (string= parent dir))
                (setq dir nil)  ; reached root directory
              (setq dir parent))))))
    prettier-path))

(defun prettier-js--get-command ()
  "Get the prettier command to use.
If `prettier-js-use-modules-bin' is non-nil, search for the local
prettier executable in node_modules/.bin.
Otherwise search for `prettier-js-command' in the system path.
Signals an error if the executable cannot be found."
  (if prettier-js-use-modules-bin
      (or (prettier-js--find-node-modules-bin)
          (progn
            (setq prettier-js-error-state "node_modules/.bin/prettier not found")
            (user-error "Could not find node_modules/.bin/prettier executable")))
    (if (executable-find prettier-js-command)
        (progn
          (setq prettier-js-error-state nil)
          prettier-js-command)
      (setq prettier-js-error-state "Prettier executable not found")
      (user-error "Could not find prettier executable; is it installed and on Emacs' exec-path?"))))

(defun prettier-js--get-diff-command ()
  "Get the diff command to use.
Searches for `prettier-js-diff-command' in the system path.
Signals an error if the executable cannot be found."
  (if (executable-find prettier-js-diff-command)
      (progn
        (setq prettier-js-error-state nil)
        prettier-js-diff-command)
    (setq prettier-js-error-state "Diff executable not found")
    (user-error "Could not find diff executable; is it installed and on Emacs' exec-path?")))

(defvar prettier-js-file-path nil
  "Override file path for prettier.
When non-nil, this path is used instead of the buffer's file path.")

(defun prettier-js--file-path ()
  "Safely get the current buffer's file path, like function `buffer-file-name'."
  (if prettier-js-file-path
      prettier-js-file-path
    ;; Support indirect buffers (created by `clone-indirect-buffer'):
    (let ((file-path (buffer-file-name (buffer-base-buffer))))
      ;; Return nil when buffer isn't visiting a file:
      (when file-path
        ;; Try to support editing remote files via TRAMP:
        (or (file-remote-p file-path 'localname) file-path)))))

(defun prettier-js--call-prettier (bufferfile outputfile errorfile)
  "Call prettier on BUFFERFILE, writing the result to OUTPUTFILE.
Any errors are written to ERRORFILE.
Returns the exit code from prettier."
  (let ((file-path (prettier-js--file-path))
        (width-args (prettier-js--width-args))
        (prettier-cmd (prettier-js--get-command)))
    (apply 'call-process
           prettier-cmd bufferfile (list (list :file outputfile) errorfile)
           nil (append prettier-js-args width-args (list "--stdin-filepath" file-path)))))

(defun prettier-js--call-diff (outputfile patchbuf &optional start end)
  "Call diff command to generate patch between current buffer and OUTPUTFILE.
PATCHBUF is the buffer where the diff output will be written."
  (let ((start-point (or start (point-min)))
        (end-point (or end (point-max)))
        (diff-cmd (prettier-js--get-diff-command)))
    (call-process-region start-point end-point diff-cmd nil patchbuf nil
                         "-n" "--strip-trailing-cr" "-" outputfile)))

(defun prettier-js--prettify (&optional start end)
   "Format content in the current buffer from START to END."
   (let* ((file-path
           (or (prettier-js--file-path)
               (user-error "Buffer `%s' is not visiting a file" (buffer-name))))
          (ext (file-name-extension file-path t))
          (bufferfile (make-temp-file "prettier" nil ext))
          (outputfile (make-temp-file "prettier" nil ext))
          (errorfile (make-temp-file "prettier" nil ext))
          ;; Some prettier daemons put errors in stdout rather than stderr
          (program-errorfile (if (member prettier-js-command prettier-js-daemon-commands)
                                 outputfile
                               errorfile))
          (errbuf (if prettier-js-show-errors (get-buffer-create "*prettier errors*")))
          (patchbuf (get-buffer-create "*prettier patch*"))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
     ;; Clear error state when attempting to run prettier
     (setq prettier-js-error-state nil)
     (unwind-protect
         (save-restriction
           (widen)
           (write-region start end bufferfile)
           (if errbuf
               (with-current-buffer errbuf
                 (setq buffer-read-only nil)
                 (erase-buffer)))
           (with-current-buffer patchbuf
             (erase-buffer))
           (if (zerop (prettier-js--call-prettier bufferfile outputfile errorfile))
               (progn
                 ;; GNU diff exit codes:
                 ;; 0 - The files are identical (no differences).
                 ;; 1 - The files are different (differences found).
                 ;; 2 - Trouble occurred (e.g. invalid options).
                 ;; 0/1 = success, 2 = problem
                 (if (<= (prettier-js--call-diff outputfile patchbuf start end) 1)
                     (progn
                       (prettier-js--apply-rcs-patch patchbuf start)
                       (message "Applied prettier with args `%s'" prettier-js-args)
                       (if errbuf (prettier-js--kill-error-buffer errbuf)))
                   (setq prettier-js-error-state "Diff command had an issue")
                   (user-error "Error calling diff; is GNU diff on your path?")))
             (prettier-js--process-errors file-path program-errorfile errbuf)))
       (kill-buffer patchbuf)
       (delete-file errorfile)
       (delete-file bufferfile)
       (delete-file outputfile))))

;;;###autoload
(defalias 'prettier-js 'prettier-js-prettify)

;;;###autoload
(defun prettier-js-prettify ()
  "Format the current buffer according to the prettier tool."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (prettier-js-prettify-code-blocks)
    (prettier-js--prettify)))

;;;###autoload
(defun prettier-js-prettify-region ()
  "Format the current region according to the prettier tool."
  (interactive)
  (when (region-active-p)
    (if (derived-mode-p 'org-mode)
        (prettier-js--prettify-org-region (region-beginning) (region-end))
      (prettier-js--prettify (region-beginning) (region-end)))))

(defvar prettier-js-language-to-extension
  '(("js" . ".js")
    ("javascript" . ".js")
    ("ts" . ".ts")
    ("typescript" . ".ts")
    ("jsx" . ".jsx")
    ("react" . ".jsx")
    ("tsx" . ".tsx")
    ("css" . ".css")
    ("scss" . ".scss")
    ("less" . ".less")
    ("json" . ".json")
    ("html" . ".html")
    ("vue" . ".vue")
    ("markdown" . ".md")
    ("md" . ".md")
    ("yaml" . ".yml")
    ("yml" . ".yml")
    ("graphql" . ".graphql"))
  "Alist mapping org-mode language names to file extensions for prettier.")

(defun prettier-js--prettify-org-region (start end)
  "Format a region from START to END in an org-mode buffer.
Validates that the region is within a single code block's contents."
  (save-excursion
    ;; Find the element at point to determine if we're in a src block
    (goto-char start)
    (let* ((element-at-start (org-element-at-point))
           (element-type (org-element-type element-at-start)))

      ;; Check if we're in a src block and the region is wholly inside it
      (if (not (eq element-type 'src-block))
          (user-error "Region is not wholly inside a source code block")

        ;; Get the boundaries of the code block contents
        (let* ((block-begin (org-element-property :begin element-at-start))
               (block-end (org-element-property :end element-at-start))
               (post-blank (org-element-property :post-blank element-at-start))
               (contents-begin (save-excursion
                                 (goto-char block-begin)
                                 (forward-line 1)
                                 (point)))
               (contents-end (save-excursion
                               (goto-char block-end)
                               (forward-line (- post-blank))
                               (forward-line -1)
                               (point)))
               (lang (org-element-property :language element-at-start))
               (ext (cdr (assoc lang prettier-js-language-to-extension))))

          ;; Check if end point is within the same code block
          (when (or (< end contents-begin) (> end contents-end))
            (user-error "Region is not wholly inside a source code block"))

          ;; Check if we have a recognized language
          (when ext
            ;; Format the region with the appropriate file extension
            (let ((prettier-js-file-path (concat "temp" ext)))
              (prettier-js--prettify start end))))))))

(defun prettier-js-prettify-code-blocks ()
  "Format all code blocks in the current org-mode buffer.
Signal an error if not in org-mode."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^[ \t]*#\\+begin_src\\s-+\\([a-zA-Z0-9]+\\)" nil t)
        (let* ((lang (match-string-no-properties 1))
               (block-start (line-beginning-position)))
          (when (and (assoc lang prettier-js-language-to-extension)
                     (re-search-forward "^[ \t]*#\\+end_src" nil t))
            (let* ((block-end (line-end-position))
                   (element (save-restriction
                              (narrow-to-region block-start block-end)
                              (org-element-at-point))))
              (setq count (1+ count))
              (prettier-js--format-code-block element)))))
      (message "Formatted %d code block%s" count (if (= count 1) "" "s")))))

(defun prettier-js-prettify-code-block ()
  "Format the current org-mode code block according to the prettier tool.
Signal an error if not within a code block."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (let ((element (org-element-at-point)))
    (unless (eq (org-element-type element) 'src-block)
      (user-error "No source code block at point"))
    (prettier-js--format-code-block element)))

(defun prettier-js--format-code-block (element)
  "Format a single org-mode code block specified by ELEMENT."
  (let* ((begin (org-element-property :begin element))
         (end (org-element-property :end element))
         (post-blank (org-element-property :post-blank element))
         (contents-begin (save-excursion
                           (goto-char begin)
                           (forward-line 1)
                           (point)))
         (contents-end (save-excursion
                         (goto-char end)
                         (forward-line (- post-blank))
                         (forward-line -1)
                         (point)))
         (lang (org-element-property :language element))
         (ext (cdr (assoc lang prettier-js-language-to-extension))))
    (when ext
      (let ((prettier-js-file-path (concat "temp" ext)))
        (prettier-js--prettify contents-begin contents-end)))))

(defvar prettier-js-mode-menu-map
  (let ((map (make-sparse-keymap "Prettier")))
    map)
  "Menu for the Prettier minor mode.")

(defun prettier-js--mode-line-indicator ()
  "Return the indicator string for the mode line.
Shows error state if any errors occurred during formatting."
  (let ((indicator (if prettier-js-error-state
                       (propertize " Prettier[!]" 'face 'error)
                     " Prettier")))
    (propertize indicator
                'local-map prettier-js-mode-menu-map
                'help-echo "Prettier menu"
                'mouse-face 'mode-line-highlight)))

(defun prettier-js--error-menu-item ()
  "Return a menu item showing the current error state.
Returns nil if there is no error state."
  (when prettier-js-error-state
    (vector (concat "Error: " prettier-js-error-state) nil nil)))

(defun prettier-js--menu-filter (menu-items)
  "Filter function for the prettier menu to dynamically add error state.
MENU-ITEMS are the static menu items.
Adds an error item at the top of the menu if there is an error state."
  (if prettier-js-error-state
      (append (list (prettier-js--error-menu-item))
              '("---")
              menu-items)
    menu-items))

(easy-menu-define prettier-js-mode-menu prettier-js-mode-menu-map
  "Menu for Prettier mode"
  '("Prettier" :filter prettier-js--menu-filter
    ["Format buffer" prettier-js-prettify t]
    ["Format region" prettier-js-prettify-region (region-active-p)]
    "---"
    ["Turn off minor mode" prettier-js-mode :visible prettier-js-mode]
    ["Help for minor mode" (describe-function 'prettier-js-mode) t]))

;;;###autoload
(define-minor-mode prettier-js-mode
  "Runs prettier on file save when this mode is turned on"
  :lighter (:eval (prettier-js--mode-line-indicator))
  :global nil
  :keymap prettier-js-mode-menu-map
  (if prettier-js-mode
      (add-hook 'before-save-hook 'prettier-js-prettify nil 'local)
    (remove-hook 'before-save-hook 'prettier-js-prettify 'local)))

(provide 'prettier-js)
;;; prettier-js.el ends here
