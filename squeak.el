;;; squeak.el --- Tools for Go programming

;; Copyright (c) 2016 Antoine Kalmbach

;; Author: Antoine Kalmbach <ane@iki.fi>
;; Created: 2016-03-05
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((s "1.10.0") (go-mode "1.3.1") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Squeak contains tools for running tests in Go programs, e.g.,
;; letting you run unit tests from within Emacs itself.
(require 's)
(require 'go-mode)
(require 'cl-lib)

;;; Code:
(defconst squeak-go-executable-name "go")

(defgroup squeak nil
  "Options for Squeak."
  :group 'convenience)

(defface squeak-tests-successful
  '((default :foreground "black" :background "green"))
  "The face used for reporting successful tests in the echo area.")

(defface squeak-tests-failed
  '((default :foreground "white" :background "red"))
  "The face used for reporting failed tests in the echo area.")

(defface squeak-tests-mixed
  '((default :foreground "black" :background "orange"))
  "The face used for reporting mixed successful and failed tests in the echo area.")

(defconst squeak--pass-regexp-no-capture   "^--- PASS:")
(defconst squeak--fail-regexp-no-capture   "^--- FAIL:")
(defconst squeak--pass-regexp-named-with-capture "^--- PASS: %s \\(([^\\)]+?)\\)")
(defconst squeak--fail-regexp-named-with-capture "^--- FAIL: %s \\(([^\\)]+?)\\)")

(defun squeak--inside-test-file-p ()
  "Check whether we are inside a test file."
  (string-match "_test\\.go" buffer-file-truename))

(defun squeak--get-test-name ()
  "Get the name of test under point."
  (when (go--in-function-p (point))
    (save-excursion
      (go-goto-function-name)
      (let ((name (symbol-name (symbol-at-point))))
        (when (s-prefix-p "Test" name)
          name)))))

;; I stole this from Stack Overflow
;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(defun squeak--how-many-str (regexp str)
  "Count how many times REGEXP matched in STR."
  (cl-loop with start = 0
           for count from 0
           while (string-match regexp str start)
           do (setq start (match-end 0))
           finally return count))

(defun squeak--parse-results (result)
  "Parse the test runner results in RESULT."
  (let ((total (cond ((string-match "^ok\s+\t+.+?\t\\(.+\\)$" result)
                      `((success . ,(s-trim (match-string 1)))))
                     ((string-match "^FAIL\t+.+?\t\\(.+\\)$" result)
                      `((failure . ,(s-trim (match-string 1))))))))
    (push `(successes . ,(squeak--how-many-str squeak--pass-regexp-no-capture result)) total)
    (push `(failures . ,(squeak--how-many-str squeak--fail-regexp-no-capture result)) total)))



(defun squeak--get-test-results (result &optional test)
  "Check what happened with the RESULT, or if TEST is provided, use that."
  (let* ((test-name   (or test ""))
         (pass-regexp (format squeak--pass-regexp-named-with-capture test-name))
         (fail-regexp (format squeak--fail-regexp-named-with-capture test-name)))
    (cond ((string-match pass-regexp result)
           `((success . ,(s-chop-prefix " (" (match-string 1)))))
          ((string-match fail-regexp result)
           `((failure . ,(s-chop-prefix " (" (match-string 1))))))))

(defun squeak--failed-p (result-string)
  "Returns t if running the tests failed for some reason."
  (or (string-match-p "^can't load package" result-string)
      (string-match-p "\\(setup\\|build\\) failed" result-string)
      (string-match-p "cannot find package" result-string)))

(defun squeak--format-failure (result-string)
  (s-concat
   "Could not run tests: "
   (cond ((string-match-p "^can't load package" result-string)
          "no Go packages found in the current directory.")
         ((string-match-p "\\(setup\\|build\\) failed" result-string)
          "building the package failed.")
         ((string-match-p "cannot find package" result-string)
          "dependencies are missing. Try running `go get'.")
         (t "unknown error. See the buffer *squeak-test* for what happened."))))

(defun squeak--run-go-test (&optional test)
  "Run `go test', or if TEST is provided, run only that test."
  (let ((go-command (executable-find squeak-go-executable-name)))
    (if go-command
        (let* ((output-buffer-name "*squeak-test*") 
               (arguments '("test" "-v"))
               (full-args (append arguments )))
          (when (get-buffer output-buffer-name)
            (with-current-buffer (get-buffer output-buffer-name)
              (erase-buffer)))
          (let ((output-buffer (get-buffer-create output-buffer-name)))
            (call-process "go" nil output-buffer nil "test" "-v"
                          (if (s-present? test) "-run" "")
                          (if (s-present? test) test ""))
            (with-current-buffer output-buffer
              (if (squeak--failed-p (buffer-string))
                  `((err . ,(squeak--format-failure (buffer-string))))
                (if (s-present? test)
                    (or (squeak--get-test-results (buffer-string) test)
                        `((err . ,(format "The test %s was not run." test))))  
                  (squeak--parse-results (buffer-string)))))))
      `((err . ,(format "`%s' command not found in PATH!" squeak-go-executable-name))))))


(defun squeak--report-result (result &optional name)
  "Report the RESULT of a test run.  If NAME is given, then report its results."
  (cond ((assq 'success result)
         (let ((time (cdr (assq 'success result))))
           (message (if (s-present? name)
                        (squeak--colorize (format "Test %s passed in %s." name time) 'squeak-tests-successful)
                      (squeak--report-multiple result)))))
        ((assq 'failure result)
         (let ((time (cdr (assq 'failure result))))
           (message (if (s-present? name)
                        (squeak--colorize (format "Test %s failed in %s." name time) 'squeak-tests-failed)
                      (squeak--report-multiple result)))))
        ((assq 'err result)
         (message (cdr (assq 'err result))))
        (t
         (message "An error occurred when running the tests."))))

(defun squeak--report-multiple (result)
  "Parse the results of a rich test run in RESULT."
  (let* ((successes (cdr (assq 'successes result)))
         (failures  (cdr (assq 'failures result)))
         (time      (cdr (or (assq 'success result) (assq 'failure result))))
         (per-test  (lambda (tests)
                      (let ((tiem (string-to-number time)))
                        (if (< 0.0 tiem)
                            (/ tiem tests)
                          tiem)))))
    (cond ((and (< 0 successes) (= 0 failures))
           (squeak--colorize
            (format "All tests passed in %ss (%.3fs per test)." time (funcall per-test successes))
            'squeak-tests-successful))
          ((and (= 0 successes) (< 0 failures))
           (squeak--colorize
            (format "All tests failed in %ss (%.3fs per test)." time (funcall per-test failures))
            'squeak-tests-failed))
          (t
           (squeak--colorize
            (format
             "%s tests passed, %s tests failed in %ss (%.3fs per test)."
             successes failures time (funcall per-test (+ successes failures)))
            'squeak-tests-mixed)))))

(defun squeak--colorize (string string-face)
  "Set the face of STRING to STRING-FACE."
  (propertize string 'face string-face))

(defun squeak-run-test-under-point ()
  "Run the test under point."
  (interactive)
  (if (squeak--inside-test-file-p)
      (let ((test-name (squeak--get-test-name)))
        (if test-name
            (let ((result (squeak--run-go-test test-name)))
              (message nil)
              (squeak--report-result result test-name))
          (message "Not inside a test.")))
    (message "Not inside a test file.")))

(defun squeak-run-tests-for-package ()
  "Run all the tests in the directory of the current buffer."
  (interactive)
  (when (buffer-file-name)
    (if (directory-files (file-name-directory (buffer-file-name)) "_test\\.go$")
        (progn
          (let ((result (squeak--run-go-test)))
            (message nil)
            (squeak--report-result result)))
      (message "No test files found in the current directory."))))

(defun squeak--find-tests-in-buffer (&optional buffer)
  "Find all the tests in BUFFER, or use the current buffer."
  (let ((reg "^func.*\\(Test\\w+\\)(.*{$")
        (results '()))
    (save-excursion
      (with-current-buffer (or buffer (current-buffer))
        (progn
          (goto-char (point-min))
          (while (re-search-forward reg nil t)
            (push (match-string-no-properties 1) results))
          results)))))

(defun squeak--choose-and-run-test (tests sorted)
  "Pick a test from a TESTS and run it.  
Present choices sorted alphabetically if SORTED is non-nil."
  (if (and (listp tests) (< 0 (length tests)))
      (let ((test-list (if sorted
                           (sort tests #'s-less-p)
                         (nreverse tests))))
        (let ((test (completing-read "Run test: " test-list nil t "Test" nil t)))
          (when test
            (squeak--report-result (squeak--run-go-test test) test))))
    (message "No tests found.")))

(defun squeak-run-test-in-current-buffer (arg)
  "Run a test from the current buffer, with completion support. 
With a prefix argument ARG, it sorts the completion list."
  (interactive "P")
  (if (squeak--inside-test-file-p)
      (let ((tests (squeak--find-tests-in-buffer)))
        (cond ((assq 'err tests) (message (cdr (assq 'err tests))))
              (t                 (squeak--choose-and-run-test tests arg))))
    (message "Not inside a Go test file.")))

(defun squeak-run-test-from-package (arg)
  "Run a test from the current package, with completion support.
With a prefix argument ARG, sort the completion list alphabetically."
  (interactive "P")
  (let ((files (find-lisp-find-files (file-name-directory buffer-file-name) "_test\\.go"))
        (tests '()))
    (if (< 0 (length files))
        (progn
          (mapc (lambda (file)
                 (with-temp-buffer
                   (insert-file-contents file nil nil nil t)
                   (setq tests (append tests (squeak--find-tests-in-buffer)))))
                files)
          (if (< 0 (length tests))
              (squeak--choose-and-run-test tests arg)
            (message "No tests found in test files.")))
      (message "No test files found in current directory."))))

(defun squeak-show-test-buffer ()
  "Show the test buffer, if one exists."
  (interactive)
  (let ((test-buffer (get-buffer "*squeak-test*")))
    (if test-buffer
        (switch-to-buffer test-buffer)
      (message "No test buffer."))))

(defvar squeak-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-t t") #'squeak-run-test-under-point)
    (define-key m (kbd "C-c C-t a") #'squeak-run-tests-for-package)
    (define-key m (kbd "C-c C-t c") #'squeak-run-test-in-current-buffer)
    (define-key m (kbd "C-c C-t p") #'squeak-run-test-from-package)
    (define-key m (kbd "C-c C-t b") #'squeak-show-test-buffer)
    m)
  "Bindings for Squeak minor mode.")

(easy-menu-define squeak-mode-menu squeak-mode-map
  "Menu for Squeak."
  '("Squeak"
    ["Run test at point"                                 squeak-run-test-under-point  t]
    ["Run all tests for current package"                 squeak-run-tests-for-package t]
    ["Choose and run test from this buffer..."           squeak-run-test-in-current-buffer t]
    ["Choose and run test from package..."               squeak-run-test-from-package t]
    ["Show test report"                                  squeak-show-test-buffer t]
    "---"
    ["Customize squeak-mode"             (customize-group 'squeak) t]))

;;;###autoload
(define-minor-mode squeak-mode
  "Squeak is a minor mode for running Go tests."
  :init-value nil
  :lighter " Sqk"
  :group 'squeak
  :keymap squeak-mode-map)

(provide 'squeak-mode)
;;; squeak.el ends here
