;;; prettier-js-test.el --- Tests for prettier-js  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 The prettier-js Authors. All rights reserved.

;;; Commentary:
;; Tests for prettier-js.el

;; These tests will fail unless node/npm are installed, as well as prettier and
;; prettierd.  Run the following command to install those latter dependencies:

;; npm install -g prettier @fsouza/prettierd

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
            (prettier-js-prettify)

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
            (prettier-js-prettify)

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
              (prettier-js-prettify)

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
    (let ((err (should-error (prettier-js-prettify) :type 'user-error)))
      (should (string-match-p "Buffer .* is not visiting a file" (cadr err))))))

(ert-deftest prettier-js-test-executable-not-found ()
  "Test that prettier-js signals an error when prettier executable is not found."
  (let* ((dirty-file (expand-file-name "fixtures/dirty.js"))
         (temp-file (make-temp-file "prettier-test-" nil ".js"))
         (prettier-js-command "non-existent-prettier-command")
         (prettier-js-use-modules-bin nil))
    (unwind-protect
        (progn
          ;; Copy dirty content to temp file
          (copy-file dirty-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            ;; Verify that the appropriate error is signaled with the correct message
            (let ((err (should-error (prettier-js-prettify) :type 'user-error)))
              (should (string-match-p "Could not find prettier executable" (cadr err))))
            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-node-modules-not-found ()
  "Test that prettier-js signals an error when node_modules/.bin/prettier is not found."
  (let* ((dirty-file (expand-file-name "fixtures/dirty.js"))
         (temp-dir (make-temp-file "prettier-test-dir-" t))
         (temp-file (expand-file-name "test.js" temp-dir))
         (default-directory temp-dir)
         (prettier-js-use-modules-bin t))
    (unwind-protect
        (progn
          ;; Copy dirty content to temp file
          (copy-file dirty-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            ;; Verify that the appropriate error is signaled with the correct message
            (let ((err (should-error (prettier-js-prettify) :type 'user-error)))
              (should (string= "Could not find node_modules/.bin/prettier executable" (cadr err))))
            (kill-buffer)))

      ;; Clean up temp directory
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest prettier-js-test-diff-not-found ()
  "Test that prettier-js signals an error when diff executable is not found."
  (let* ((dirty-file (expand-file-name "fixtures/dirty.js"))
         (temp-file (make-temp-file "prettier-test-" nil ".js"))
         (prettier-js-diff-command "non-existent-diff-command"))
    (unwind-protect
        (progn
          ;; Copy dirty content to temp file
          (copy-file dirty-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            ;; Verify that the appropriate error is signaled with the correct message
            (let ((err (should-error (prettier-js-prettify) :type 'user-error)))
              (should (string-match-p "Could not find diff executable" (cadr err))))
            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-diff-error ()
  "Test that prettier-js signals an error when diff command returns an error exit code."
  (let* ((dirty-file (expand-file-name "fixtures/dirty.js"))
         (temp-file (make-temp-file "prettier-test-" nil ".js"))
         (orig-call-process-region (symbol-function 'call-process-region)))
    (unwind-protect
        (progn
          ;; Copy dirty content to temp file
          (copy-file dirty-file temp-file t)

          ;; Mock call-process-region to return error code 2 for diff
          (cl-letf (((symbol-function 'call-process-region)
                     (lambda (start end program &rest args)
                       (if (string-match-p "diff$" program)
                           2  ;; Return error code 2 for diff
                         (apply orig-call-process-region start end program args)))))

            ;; Visit the temp file
            (with-current-buffer (find-file-noselect temp-file)
              ;; Verify that the appropriate error is signaled with the correct message
              (let ((err (should-error (prettier-js-prettify) :type 'user-error)))
                (should (string= "Error calling diff; is GNU diff on your path?" (cadr err))))
              (kill-buffer))))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-node-not-found-error ()
  "Test that prettier-js handles 'env: node: No such file or directory' error correctly."
  (let* ((dirty-file (expand-file-name "fixtures/dirty.js"))
         (temp-file (make-temp-file "prettier-test-" nil ".js"))
         (orig-call-process (symbol-function 'call-process)))
    (unwind-protect
        (progn
          ;; Copy dirty content to temp file
          (copy-file dirty-file temp-file t)

          ;; Mock call-process to simulate the node not found error
          (cl-letf (((symbol-function 'call-process)
                     (lambda (program &rest args)
                       (if (string= program prettier-js-command)
                           (progn
                             ;; Write the error message to the error file
                             (let ((error-file (nth 1 args)))
                               (when (and (listp error-file) (eq (caar error-file) :file))
                                 (with-temp-file (cadr error-file)
                                   (insert "env: node: No such file or directory"))))
                             1) ; Return error code
                         (apply orig-call-process program args)))))

            ;; Visit the temp file
            (with-current-buffer (find-file-noselect temp-file)
              ;; Verify that the appropriate error is signaled with the correct message
              (let ((err (should-error (prettier-js-prettify) :type 'user-error)))
                (should (string-match-p "Could not find node executable" (cadr err))))
              (kill-buffer))))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-error-display ()
  "Test that errors from prettier are displayed in the error buffer."
  (let* ((syntax-error-file (expand-file-name "fixtures/syntax-error.js"))
         (temp-dir (make-temp-file "prettier-test-dir-" t))
         (temp-file (expand-file-name "syntax-error.js" temp-dir))
         (prettier-js-command "prettier")
         (prettier-js-show-errors 'buffer)
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Copy syntax error file to temp directory
          (copy-file syntax-error-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            ;; Run prettier-js which should display the error
            (prettier-js-prettify)

            ;; Check that the error buffer exists and contains the error message
            (let ((error-buffer (get-buffer "*prettier errors*")))
              (should error-buffer)
              (with-current-buffer error-buffer
                (should (string-match-p "SyntaxError: Unexpected token" (buffer-string)))))

            (kill-buffer)
            ;; Clean up error buffer
            (when-let ((buf (get-buffer "*prettier errors*")))
              (kill-buffer buf))))

      ;; Clean up temp directory
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest prettier-js-test-prettierd-error-display ()
  "Test that errors from prettierd are displayed in the error buffer."
  (let* ((syntax-error-file (expand-file-name "fixtures/syntax-error.js"))
         (temp-dir (make-temp-file "prettier-test-dir-" t))
         (temp-file (expand-file-name "syntax-error.js" temp-dir))
         (prettier-js-command "prettierd")
         (prettier-js-show-errors 'buffer)
         (default-directory temp-dir))
    (unwind-protect
        (progn
          ;; Copy syntax error file to temp directory
          (copy-file syntax-error-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            ;; Run prettier-js which should display the error
            (prettier-js-prettify)

            ;; Check that the error buffer exists and contains the error message
            (let ((error-buffer (get-buffer "*prettier errors*")))
              (should error-buffer)
              (with-current-buffer error-buffer
                (should (string-match-p "SyntaxError: Unexpected token" (buffer-string)))))

            (kill-buffer)
            ;; Clean up error buffer
            (when-let ((buf (get-buffer "*prettier errors*")))
              (kill-buffer buf))))

      ;; Stop prettierd daemon to ensure the temp directory can be deleted on
      ;; Windows; I think prettierd "locks" the directory where it's started
      (when (executable-find "prettierd")
        (message "Stopping prettierd daemon...")
        (call-process "prettierd" nil nil nil "stop"))

      ;; Clean up temp directory
      (when (file-exists-p temp-dir)
        (delete-directory temp-dir t)))))

(ert-deftest prettier-js-test-prettify-region ()
  "Test that prettier-js-prettify-region correctly formats a region of JavaScript code."
  (let* ((dirty-file (expand-file-name "fixtures/dirty.js"))
         (partial-clean-file (expand-file-name "fixtures/partial-clean.js"))
         (temp-file (make-temp-file "prettier-test-" nil ".js")))
    (unwind-protect
        (progn
          ;; Copy dirty content to temp file
          (copy-file dirty-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            ;; Find the object declaration and format just that region
            (goto-char (point-min))
            (search-forward "const obj = {")
            (beginning-of-line)
            (let ((start (point))
                  (end (progn
                         (search-forward "};")
                         (point))))

              ;; Set the region and format it
              (push-mark start)
              (goto-char end)
              (activate-mark)
              (prettier-js-prettify-region)

              ;; Get the expected partial clean content
              (let ((expected-content
                     (with-temp-buffer
                       (insert-file-contents partial-clean-file)
                       (buffer-string)))
                    (actual-content (buffer-string)))

                ;; Compare the formatted content with the expected partial clean content
                (should (string= actual-content expected-content))))

            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-prettify-code-block ()
  "Test that prettier-js-prettify-code-block correctly formats code blocks in org-mode."
  (let* ((messy-file (expand-file-name "fixtures/messy.org"))
         (clean-file (expand-file-name "fixtures/clean.org"))
         (temp-file (make-temp-file "prettier-test-" nil ".org")))
    (unwind-protect
        (progn
          ;; Copy messy content to temp file
          (copy-file messy-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            (org-mode)

            ;; Format the JavaScript code block
            (goto-char (point-min))
            (search-forward "#+BEGIN_SRC js")
            (forward-line 1)
            (prettier-js-prettify-code-block)

            ;; Format the TypeScript code block
            (goto-char (point-min))
            (search-forward "#+BEGIN_SRC typescript")
            (forward-line 1)
            (prettier-js-prettify-code-block)

            ;; Get the expected clean content
            (let ((expected-content
                   (with-temp-buffer
                     (insert-file-contents clean-file)
                     (buffer-string)))
                  (actual-content (buffer-string)))

              ;; Compare the formatted content with the expected clean content
              (should (string= actual-content expected-content)))

            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-prettify-code-block-not-in-org-mode ()
  "Test that prettier-js-prettify-code-block signals an error when not in org-mode."
  (with-temp-buffer
    (insert "function test() { return 42; }")
    (js-mode)

    ;; Verify that the appropriate error is signaled with the correct message
    (let ((err (should-error (prettier-js-prettify-code-block) :type 'user-error)))
      (should (string= "Not in org-mode" (cadr err))))))

(ert-deftest prettier-js-test-prettify-code-block-not-in-src-block ()
  "Test that prettier-js-prettify-code-block signals an error when not in a source block."
  (with-temp-buffer
    (insert "#+TITLE: Test Org File\n\n* Heading\nSome regular text, not in a code block.")
    (org-mode)

    ;; Verify that the appropriate error is signaled with the correct message
    (let ((err (should-error (prettier-js-prettify-code-block) :type 'user-error)))
      (should (string= "No source code block at point" (cadr err))))))

(ert-deftest prettier-js-test-prettify-code-blocks ()
  "Test that prettier-js-prettify-code-blocks formats all code blocks in an org file."
  (let* ((messy-file (expand-file-name "fixtures/messy.org"))
         (clean-file (expand-file-name "fixtures/clean.org"))
         (temp-file (make-temp-file "prettier-test-" nil ".org")))
    (unwind-protect
        (progn
          ;; Copy messy content to temp file
          (copy-file messy-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            (org-mode)

            ;; Format all code blocks at once
            (prettier-js-prettify-code-blocks)

            ;; Get the expected clean content
            (let ((expected-content
                   (with-temp-buffer
                     (insert-file-contents clean-file)
                     (buffer-string)))
                  (actual-content (buffer-string)))

              ;; Compare the formatted content with the expected clean content
              (should (string= actual-content expected-content)))

            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-prettify-code-blocks-not-in-org-mode ()
  "Test that prettier-js-prettify-code-blocks signals an error when not in org-mode."
  (with-temp-buffer
    (insert "function test() { return 42; }")
    (js-mode)

    ;; Verify that the appropriate error is signaled with the correct message
    (let ((err (should-error (prettier-js-prettify-code-blocks) :type 'user-error)))
      (should (string= "Not in org-mode" (cadr err))))))

(ert-deftest prettier-js-test-unrecognizable-language-blocks ()
  "Test that prettier-js-prettify-code-blocks skips unrecognizable language blocks."
  (let* ((unrecognizable-file (expand-file-name "fixtures/unrecognizable.org"))
         (temp-file (make-temp-file "prettier-test-" nil ".org")))
    (unwind-protect
        (progn
          ;; Copy unrecognizable content to temp file
          (copy-file unrecognizable-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            (org-mode)

            ;; Remember the original content
            (let ((original-content (buffer-string)))
              ;; Format all code blocks
              (prettier-js-prettify-code-blocks)

              ;; Verify the content hasn't changed after formatting all blocks
              (should (string= (buffer-string) original-content)))

            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-unrecognizable-language-block ()
  "Test that prettier-js-prettify-code-block skips unrecognizable language blocks."
  (let* ((unrecognizable-file (expand-file-name "fixtures/unrecognizable.org"))
         (temp-file (make-temp-file "prettier-test-" nil ".org")))
    (unwind-protect
        (progn
          ;; Copy unrecognizable content to temp file
          (copy-file unrecognizable-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            (org-mode)

            ;; Remember the original content
            (let ((original-content (buffer-string)))
              ;; Try to format the specific unrecognizable block
              (goto-char (point-min))
              (search-forward "#+BEGIN_SRC unrecognizable-language")
              (forward-line 1)
              (prettier-js-prettify-code-block)

              ;; Verify the content hasn't changed after trying to format the specific block
              (should (string= (buffer-string) original-content)))

            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-prettify-org-region ()
  "Test that prettier-js-prettify-region correctly formats regions in org-mode files."
  (let* ((messy-file (expand-file-name "fixtures/messy.org"))
         (partial-clean-file (expand-file-name "fixtures/partial-clean.org"))
         (temp-file (make-temp-file "prettier-test-" nil ".org")))
    (unwind-protect
        (progn
          ;; Copy messy content to temp file
          (copy-file messy-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            (org-mode)

            ;; Format the JavaScript object region
            (goto-char (point-min))
            (search-forward "const obj = {")
            (beginning-of-line)
            (let ((start (point))
                  (end (progn
                         (search-forward "};")
                         (point))))
              ;; Set the region and format it
              (push-mark start)
              (goto-char end)
              (activate-mark)
              (prettier-js-prettify-region))

            ;; Format the TypeScript interface region
            (goto-char (point-min))
            (search-forward "interface Person")
            (beginning-of-line)
            (let ((start (point))
                  (end (progn
                         (search-forward "}")
                         (point))))
              ;; Set the region and format it
              (push-mark start)
              (goto-char end)
              (activate-mark)
              (prettier-js-prettify-region))

            ;; Get the expected partial clean content
            (let ((expected-content
                   (with-temp-buffer
                     (insert-file-contents partial-clean-file)
                     (buffer-string)))
                  (actual-content (buffer-string)))

              ;; Compare the formatted content with the expected partial clean content
              (should (string= actual-content expected-content)))

            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest prettier-js-test-prettify-org-region-validation ()
  "Test that prettier-js-prettify-region validates regions in org-mode files."
  (let* ((messy-file (expand-file-name "fixtures/messy.org"))
         (temp-file (make-temp-file "prettier-test-" nil ".org")))
    (unwind-protect
        (progn
          ;; Copy messy content to temp file
          (copy-file messy-file temp-file t)

          ;; Visit the temp file
          (with-current-buffer (find-file-noselect temp-file)
            (org-mode)

            ;; Test case 1: Region start outside a code block
            (goto-char (point-min))
            (search-forward "* JavaScript Code Block")
            (let ((start (point))
                  (end (progn
                         (search-forward "#+BEGIN_SRC js")
                         (forward-line 2)
                         (point))))
              ;; Set the region and try to format it
              (push-mark start)
              (goto-char end)
              (activate-mark)
              (let ((err (should-error (prettier-js-prettify-region) :type 'user-error)))
                (should (string= "Region is not wholly inside a source code block" (cadr err)))))

            ;; Test case 2: Region end outside the code block
            (goto-char (point-min))
            (search-forward "#+BEGIN_SRC js")
            (forward-line 1)
            (let ((start (point))
                  (end (progn
                         (search-forward "#+END_SRC")
                         (forward-line 1)
                         (point))))
              ;; Set the region and try to format it
              (push-mark start)
              (goto-char end)
              (activate-mark)
              (let ((err (should-error (prettier-js-prettify-region) :type 'user-error)))
                (should (string= "Region is not wholly inside a source code block" (cadr err)))))

            ;; Test case 3: Region spans multiple code blocks
            (goto-char (point-min))
            (search-forward "#+BEGIN_SRC js")
            (forward-line 1)
            (let ((start (point))
                  (end (progn
                         (search-forward "#+BEGIN_SRC typescript")
                         (forward-line 2)
                         (point))))
              ;; Set the region and try to format it
              (push-mark start)
              (goto-char end)
              (activate-mark)
              (let ((err (should-error (prettier-js-prettify-region) :type 'user-error)))
                (should (string= "Region is not wholly inside a source code block" (cadr err)))))

            (kill-buffer)))

      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'prettier-js-test)
;;; prettier-js-test.el ends here
