;;; prettier-js.el --- Minor mode to format JS code on file save  -*- lexical-binding: t; -*-

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
;; Formats your JavaScript code using 'prettier' on file save.

;;; Code:

(defgroup prettier-js nil
  "Minor mode to format JS code on file save"
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

(defvar-local prettier-js-error-state nil
  "Indicates if there's an error with the prettier executable.
When non-nil, contains the error message to display.")

(defun prettier-js--goto-line (line)
  "Move cursor to line LINE."
  (goto-char (point-min))
    (forward-line (1- line)))

(defun prettier-js--apply-rcs-patch (patch-buffer)
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
        (line-offset 0))
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

(defun prettier-js--process-errors (filename errorfile errbuf)
  "Process errors for FILENAME, using ERRORFILE, displaying the output in ERRBUF."
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
      (user-error "Could not find prettier executable"))))

(defun prettier-js--call-prettier (bufferfile outputfile errorfile)
  "Call prettier on BUFFERFILE, writing the result to OUTPUTFILE.
Any errors are written to ERRORFILE.
Returns the exit code from prettier."
  (let ((localname (or (file-remote-p buffer-file-name 'localname) buffer-file-name))
        (width-args (prettier-js--width-args))
        (prettier-cmd (prettier-js--get-command)))
    (apply 'call-process
           prettier-cmd bufferfile (list (list :file outputfile) errorfile)
           nil (append prettier-js-args width-args (list "--stdin-filepath" localname)))))

;;;###autoload
(defun prettier-js ()
   "Format the current buffer according to the prettier tool."
   (interactive)
   (let* ((file-path
           (or (buffer-file-name (buffer-base-buffer))
               (error "Buffer '%s' is not visiting a file" (buffer-name))))
          (ext (file-name-extension file-path t))
          (bufferfile (make-temp-file "prettier" nil ext))
          (outputfile (make-temp-file "prettier" nil ext))
          (errorfile (make-temp-file "prettier" nil ext))
          (errbuf (if prettier-js-show-errors (get-buffer-create "*prettier errors*")))
          (patchbuf (get-buffer-create "*prettier patch*"))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))
     ;; Clear error state when attempting to run prettier
     (setq prettier-js-error-state nil)
     (unwind-protect
         (save-restriction
           (widen)
           (write-region nil nil bufferfile)
           (if errbuf
               (with-current-buffer errbuf
                 (setq buffer-read-only nil)
                 (erase-buffer)))
           (with-current-buffer patchbuf
             (erase-buffer))
           (if (zerop (prettier-js--call-prettier bufferfile outputfile errorfile))
               (progn
                 (call-process-region (point-min) (point-max) prettier-js-diff-command nil patchbuf nil "-n" "--strip-trailing-cr" "-"
                                      outputfile)
                 (prettier-js--apply-rcs-patch patchbuf)
                 (message "Applied prettier with args `%s'" prettier-js-args)
                 (if errbuf (prettier-js--kill-error-buffer errbuf)))
             (message "Could not apply prettier")
             (if errbuf
                 (prettier-js--process-errors file-path errorfile errbuf))
             ))
       (kill-buffer patchbuf)
       (delete-file errorfile)
       (delete-file bufferfile)
       (delete-file outputfile))))

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
    ["Format buffer" prettier-js t]
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
      (add-hook 'before-save-hook 'prettier-js nil 'local)
    (remove-hook 'before-save-hook 'prettier-js 'local)))

(provide 'prettier-js)
;;; prettier-js.el ends here
