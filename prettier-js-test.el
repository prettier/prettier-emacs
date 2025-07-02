;;; prettier-js-test.el --- Tests for prettier-js  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 The prettier-js Authors. All rights reserved.

;;; Commentary:
;; Tests for prettier-js.el

;;; Code:

(require 'ert)
(require 'prettier-js)
(require 'files)
(require 'subr-x)

(defun prettier-js-test--copy-directory (source destination)
  "Copy SOURCE directory to DESTINATION recursively."
  (if (file-directory-p source)
      (progn
        (make-directory destination t)
        (let ((files (directory-files source t)))
          (dolist (file files)
            (let ((name (file-name-nondirectory file)))
              (unless (or (string= name ".") (string= name ".."))
                (let ((dest-file (expand-file-name name destination)))
                  (if (file-directory-p file)
                      (prettier-js-test--copy-directory file dest-file)
                    (copy-file file dest-file t))))))))
    (copy-file source destination t)))

(defun prettier-js-test--run-process (program &rest args)
  "Run PROGRAM with ARGS in the current directory."
  (let ((exit-code (apply #'call-process program nil nil nil args)))
    (unless (= exit-code 0)
      (error "Process %s exited with code %d" program exit-code))))

(ert-deftest prettier-js-test-formatting ()
  "Test that prettier-js correctly formats JavaScript code."
  (let* ((dirty-file (expand-file-name "fixtures/dirty.js"))
         (clean-file (expand-file-name "fixtures/clean.js"))
         (temp-file (make-temp-file "prettier-test-" nil ".js")))
    (unwind-protect
        (progn
          ;; Copy dirty content to temp file
          (copy-file dirty-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            ;; Format the buffer
            (prettier-js)

            ;; Get the expected clean content
            (let ((expected-content
                   (with-temp-buffer
                     (insert-file-contents clean-file)
                     (buffer-string)))
                  (actual-content (buffer-string)))
              ;; Compare the formatted content with the expected clean content
              (should (string= actual-content expected-content))
              (kill-buffer))))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-node-modules ()
  "Test that prettier-js can use the local node_modules/.bin/prettier."
  (let* ((project-dir (expand-file-name "fixtures/project"))
         (temp-dir (make-temp-file "prettier-project-" t))
         (temp-src-dir (expand-file-name "src" temp-dir))
         (temp-src-file (expand-file-name "messy.js" temp-src-dir))
         (clean-file (expand-file-name "fixtures/clean.js"))
         (default-directory temp-dir)
         (prettier-js-use-modules-bin t))
    (unwind-protect
        (progn
          ;; Copy project to temp directory
          (prettier-js-test--copy-directory project-dir temp-dir)

          ;; Install dependencies
          (message "Installing npm dependencies in %s..." temp-dir)
          (prettier-js-test--run-process "npm" "install")

          ;; Visit the messy file
          (with-current-buffer (find-file-noselect temp-src-file)
            ;; Format the buffer using node_modules/.bin/prettier
            (prettier-js)

            ;; Get the expected clean content
            (let ((expected-content
                   (with-temp-buffer
                     (insert-file-contents clean-file)
                     (buffer-string)))
                  (actual-content (buffer-string)))
              ;; Compare the formatted content with the expected clean content
              (should (string= actual-content expected-content))
              (kill-buffer))))

      ;; Clean up temp directory
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest prettier-js-test-indirect-buffer ()
  "Test that prettier-js works correctly with indirect buffers."
  (let* ((dirty-file (expand-file-name "fixtures/dirty.js"))
         (clean-file (expand-file-name "fixtures/clean.js"))
         (temp-file (make-temp-file "prettier-test-" nil ".js")))
    (unwind-protect
        (progn
          ;; Copy dirty content to temp file
          (copy-file dirty-file temp-file t)

          (let (direct-buffer indirect-buffer)
            ;; Visit the temp file and clone its buffer
            (setq direct-buffer (find-file-noselect temp-file))
            (with-current-buffer direct-buffer
              (setq indirect-buffer (clone-indirect-buffer nil nil)))

            ;; Switch to the indirect buffer and format it
            (with-current-buffer indirect-buffer
              (prettier-js)

              ;; Get the expected clean content
              (let ((expected-content
                     (with-temp-buffer
                       (insert-file-contents clean-file)
                       (buffer-string)))
                    (actual-content (buffer-string)))
                ;; Compare the formatted content with the expected clean content
                (should (string= actual-content expected-content))
                (kill-buffer)))

            ;; Kill the direct buffer too
            (kill-buffer direct-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-no-file-buffer ()
  "Test that prettier-js signals an error when buffer is not visiting a file."
  (with-temp-buffer
    ;; Add some JavaScript content to the buffer
    (insert "function test(a,b) { return a + b; }")
    (js-mode)

    ;; Verify that the appropriate error is signaled with the correct message
    (let ((err (should-error (prettier-js) :type 'user-error)))
      (should (string-match-p "Buffer ‘.*’ is not visiting a file" (cadr err))))))

(provide 'prettier-js-test)
;;; prettier-js-test.el ends here
