;;; prettier-js-test.el --- Tests for prettier-js  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 The prettier-js Authors. All rights reserved.

;;; Commentary:
;; Tests for prettier-js.el

;;; Code:

(require 'ert)
(require 'prettier-js)

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

(provide 'prettier-js-test)
;;; prettier-js-test.el ends here
