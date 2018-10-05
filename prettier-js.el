;;; prettier-js.el --- Minor mode to format JS code on file save

;; Version: 0.1.0

;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

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

;; Author: James Long and contributors
;; Created: 10 January 2017
;; Url: https://github.com/prettier/prettier-emacs
;; Keywords: convenience wp edit js

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Formats your JavaScript code using 'prettier' on file save.

;;; Code:

(require 'json)

(defgroup prettier-js nil
  "Minor mode to format JS code on file save"
  :group 'languages
  :prefix "prettier-js"
  :link '(url-link :tag "Repository" "https://github.com/prettier/prettier"))

(defcustom prettier-js-command "prettier"
  "The 'prettier' command."
  :type 'string
  :group 'prettier-js)

(defcustom prettier-js-args '()
  "List of args to send to prettier command."
  :type '(repeat string)
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

(defcustom prettier-js-sync-config-p t
  "Whether to attempt to sync prettier configuration to Emacs."
  :type 'boolean
  :group 'prettier-js)

(defvar prettier-home (file-name-directory
                       (or load-file-name buffer-file-name))
  "Directory with `prettier-js.el' and the JS helper plugin.")

(defvar prettier-js-sync-settings
  '(((fill-column                   ; built-in
      js3-max-columns)              ; js3-mode
     :printWidth)

    ((enh-ruby-indent-tabs-mode
      indent-tabs-mode              ; built-in
      js3-indent-tabs-mode          ; js3-mode
      ruby-indent-tabs-mode)        ; ruby-mode
     :useTabs)

    ((c-basic-offset                ; cc-mode
      css-indent-offset             ; css-mode, scss-mode etc
      enh-ruby-indent-level         ; enh-ruby-mode
      graphql-indent-level          ; graphql-mode
      js-indent-level               ; js-mode
      js2-basic-offset              ; js2-mode
      js3-indent-level              ; js3-mode
      lua-indent-level              ; lua-mode
      python-indent                 ; python-mode
      ruby-indent-level             ; ruby-mode
      sgml-basic-offset             ; js2-mode (used for JSX)
      smie-indent-basic             ; smie.el (generic)
      standard-indent               ; indent.el (generic)
      tab-width                     ; built-in
      typescript-indent-level       ; typescript-mode
      web-mode-code-indent-offset   ; web-mode
      web-mode-css-indent-offset    ; web-mode
      yaml-indent-offset)           ; yaml-mode
     :tabWidth)

    ((js-indent-first-init)
     nil)

    ;; Unless prettier has trailing commas disabled, don't warn
    ;; about their presence
    ((js2-strict-trailing-comma-warning
      js3-strict-trailing-comma-warning)
     :trailingComma
     (lambda (trailing-comma)
       (case trailing-comma
         ("es5" nil)
         ("all" nil)
         (otherwise 'unchanged))))

    ;; When prettier has semicolons disabled, don't warn
    ;; about their absence
    ((js2-strict-missing-semi-warning
      js3-strict-missing-semi-warning)
     :semi
     (lambda (semi)
       (case semi
         (nil nil)
         (otherwise 'unchanged))))

    ((web-mode-auto-quote-style)
     :singleQuote
     (lambda (single-quote)
       (if single-quote 2 1))))
  "Settings to sync from Prettier to Emacs configuration.

A list with element a list of two or three elements:

  `(VAR-LIST SOURCE-CONFIG [TRANSFORM-FN])'

VAR-LIST is a list of Emacs variables to set.

SOURCE-CONFIG is a keyword specifying which Prettier
configuration option to use for setting the Emacs variables.

TRANSFORM-FN is an optional function; when present, it is called
with the value of the Prettier option and the result is used for
setting the Emacs variables, unless it is the symbol `unchanged'.
If that symbol is returned, Emacs variables won't be changed.")

(defvar-local prettier-js-previous-local-settings nil
  "Used to backup settings so they can be restored later.")

(defvar-local prettier-js-config-cache nil)

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
  "Process errors for FILENAME, using an ERRORFILE and display the output in ERRBUF."
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

(defun prettier-js--base-args ()
  "Return common CLI arguments."
  (append
   prettier-js-args
   (cond
    ((equal prettier-js-width-mode 'window)
     (list "--print-width" (number-to-string (window-body-width))))
    ((equal prettier-js-width-mode 'fill)
     (list "--print-width" (number-to-string fill-column)))
    (t
     '()))))

(defun prettier-js ()
   "Format the current buffer according to the prettier tool."
   (interactive)
   (let* ((ext (file-name-extension buffer-file-name t))
          (bufferfile (make-temp-file "prettier" nil ext))
          (outputfile (make-temp-file "prettier" nil ext))
          (errorfile (make-temp-file "prettier" nil ext))
          (errbuf (if prettier-js-show-errors (get-buffer-create "*prettier errors*")))
          (patchbuf (get-buffer-create "*prettier patch*"))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8)
          (coding-system-for-write 'utf-8))
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
           (if (zerop (apply 'call-process
                             prettier-js-command bufferfile (list (list :file outputfile) errorfile)
                             nil (append (prettier-js--base-args) (list "--stdin" "--stdin-filepath" buffer-file-name))))
               (progn
                 (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "--strip-trailing-cr" "-"
                                      outputfile)
                 (prettier-js--apply-rcs-patch patchbuf)
                 (message "Applied prettier with args `%s'" prettier-js-args)
                 (if errbuf (prettier-js--kill-error-buffer errbuf)))
             (message "Could not apply prettier")
             (if errbuf
                 (prettier-js--process-errors (buffer-file-name) errorfile errbuf))
             ))
       (kill-buffer patchbuf)
       (delete-file errorfile)
       (delete-file bufferfile)
       (delete-file outputfile))))

(defun prettier-js--load-config ()
  "Load prettier config for current buffer.

Prettier configuration is serialized using a special plug-in and
then loaded as JSON with objects as plists and false as nil."
  (let ((file-name (buffer-file-name))
        (json-object-type 'plist)
        (json-false nil))
    (with-temp-buffer
      ;; Note: Prettier outputs blank for blank input, therefore we
      ;; need to feed it something. Any text will do since it's
      ;; ignored by the plugin parser.
      (insert ".")
      (if (zerop (apply 'call-process-region
                        (point-min) (point-max)
                        prettier-js-command
                        t '(t nil) nil
                        (append
                         (prettier-js--base-args)
                         `("--plugin" ,(concat prettier-home
                                               "OptionsPlugin.js")
                           "--parser" "optionsPrinter"
                           "--stdin"
                           "--stdin-filepath" ,file-name
                           ))))
          (progn
            (goto-char (point-min))
            (json-read))
        (error "Cannot load prettier config")))))

(defun prettier-js--sync-config ()
  "Try to sync prettier config for current buffer.

Tries loading the configuration, ignoring failure with a message.

If loaded successfully, uses it to set a variety of buffer-local
variables in an effort to make pre-formatting indentation etc as
close to post-formatting as possible."
  (condition-case-unless-debug nil
      (setq
       prettier-js-config-cache
       (prettier-js--load-config)

       prettier-js-previous-local-settings
       (seq-filter
        'identity
        (apply
         'append
         (mapcar
          (lambda (setting)
            (let* ((vars (nth 0 setting))
                   (source (nth 1 setting))
                   (value (funcall
                           (or (nth 2 setting) 'identity)
                           (if (keywordp source)
                               (plist-get prettier-js-config-cache
                                          source)
                             source))))
              (unless (eq value 'unchanged)
                (mapcar
                 (lambda (var)
                   (when (boundp var)
                     (let ((result
                            (list var (local-variable-p var)
                                  (eval var)
                                  value)))
                       (set (make-local-variable var) value)
                       result)))
                 vars))))
          prettier-js-sync-settings))))
    ;; Ignore any errors but print a warning
    ((debug error)
     (message "Could not sync Prettier config, consider setting \
`prettier-js-sync-config-p' to nil"))))

;;;###autoload
(define-minor-mode prettier-js-mode
  "Runs prettier on file save when this mode is turned on"
  :lighter " Prettier"
  :global nil
  (if prettier-js-mode
      (progn
        (add-hook 'before-save-hook 'prettier-js nil 'local)
        (when prettier-js-sync-config-p
          (prettier-js--maybe-sync-config)
          (add-hook 'after-change-major-mode-hook
                    'prettier-js--maybe-sync-config
                    t
                    'local)))

    (remove-hook 'before-save-hook 'prettier-js 'local)
    (remove-hook 'after-change-major-mode-hook
                 'prettier-js--maybe-sync-config
                 'local)

    ;; Restore modified settings
    (ignore-errors
      (mapc
       (lambda (backup-setting)
         (let ((var (car backup-setting))
               (new-value (nth 3 backup-setting)))
           ;; Leave the variable alone if the user has changed it
           ;; since loading `prettier-js-mode'
           (when (equal new-value (eval var))
             ;; Was it a local variable before we set it?
             (if (nth 1 backup-setting)
                 ;; Yes, set it to the old value
                 (set var (nth 2 backup-setting))
               ;; No, remove the local variable
               (kill-local-variable var)))))
       prettier-js-previous-local-settings))))

(defun prettier-js--maybe-sync-config ()
  "Sync Prettier config in current buffer when appropriate.

Config is synced when `prettier-js-sync-config-p' is non-nil and
`prettier-js-mode' is enabled in current buffer."
  (when (and prettier-js-mode
             prettier-js-sync-config-p
             (null prettier-js-config-cache))
    (prettier-js--sync-config)))

(provide 'prettier-js)
;;; prettier-js.el ends here
