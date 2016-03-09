;;; rats.el --- Rapid testing suite for Go

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

;; rats contains tools for running tests in Go programs, e.g.,
;; letting you run unit tests from within Emacs itself.
(require 's)
(require 'go-mode)
(require 'cl-lib)

;;; Code:
(defconst rats-go-executable-name "go")

(defgroup rats nil
  "Options for rats."
  :group 'convenience)

(defface rats-tests-successful
  '((default :foreground "black" :background "green"))
  "The face used for reporting successful tests in the echo area.")

(defface rats-tests-failed
  '((default :foreground "white" :background "red"))
  "The face used for reporting failed tests in the echo area.")

(defface rats-tests-mixed
  '((default :foreground "black" :background "orange"))
  "The face used for reporting mixed successful and failed tests in the echo area.")

(defconst rats--pass-regexp-no-capture   "^--- PASS:")
(defconst rats--fail-regexp-no-capture   "^--- FAIL:")
(defconst rats--pass-regexp-named-with-capture "^--- PASS: %s \\(([^\\)]+?)\\)")
(defconst rats--fail-regexp-named-with-capture "^--- FAIL: %s \\(([^\\)]+?)\\)")

(defun rats--inside-test-file-p ()
  "Check whether we are inside a test file."
  (string-match "_test\\.go" buffer-file-truename))

(defun rats--get-test-name ()
  "Get the name of test under point."
  (when (go--in-function-p (point))
    (save-excursion
      (go-goto-function-name)
      (let ((name (symbol-name (symbol-at-point))))
        (when (s-prefix-p "Test" name)
          name)))))

;; I stole this from Stack Overflow
;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(defun rats--how-many-str (regexp str)
  "Count how many times REGEXP matched in STR."
  (cl-loop with start = 0
           for count from 0
           while (string-match regexp str start)
           do (setq start (match-end 0))
           finally return count))

(defun rats--parse-results (result)
  "Parse the test runner results in RESULT."
  (let ((total (cond ((string-match "^ok\s+\t+.+?\t\\(.+\\)$" result)
                      `((success . ,(s-trim (match-string 1)))))
                     ((string-match "^FAIL\t+.+?\t\\(.+\\)$" result)
                      `((failure . ,(s-trim (match-string 1))))))))
    (push `(successes . ,(rats--how-many-str rats--pass-regexp-no-capture result)) total)
    (push `(failures . ,(rats--how-many-str rats--fail-regexp-no-capture result)) total)))



(defun rats--get-test-results (result &optional test)
  "Check what happened with the RESULT, or if TEST is provided, use that."
  (let* ((test-name   (or test ""))
         (pass-regexp (format rats--pass-regexp-named-with-capture test-name))
         (fail-regexp (format rats--fail-regexp-named-with-capture test-name)))
    (cond ((string-match pass-regexp result)
           `((success . ,(s-chop-prefix " (" (match-string 1)))))
          ((string-match fail-regexp result)
           `((failure . ,(s-chop-prefix " (" (match-string 1))))))))

(defun rats--failed-p (result-string)
  "Returns t if running the tests failed for some reason."
  (or (string-match-p "^can't load package" result-string)
      (string-match-p "\\(setup\\|build\\) failed" result-string)
      (string-match-p "cannot find package" result-string)))

(defun rats--format-failure (result-string)
  (s-concat
   "Could not run tests: "
   (cond ((string-match-p "^can't load package" result-string)
          "no Go packages found in the current directory.")
         ((string-match-p "\\(setup\\|build\\) failed" result-string)
          "building the package failed.")
         ((string-match-p "cannot find package" result-string)
          "dependencies are missing. Try running `go get'.")
         (t "unknown error. See the buffer *rats-test* for what happened."))))

(defun rats--run-go-test (&optional test)
  "Run `go test', or if TEST is provided, run only that test."
  (let ((go-command (executable-find rats-go-executable-name)))
    (if go-command
        (let* ((output-buffer-name "*rats-test*") 
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
              (if (rats--failed-p (buffer-string))
                  `((err . ,(rats--format-failure (buffer-string))))
                (if (s-present? test)
                    (rats--get-test-results (buffer-string) test)  
                  (rats--parse-results (buffer-string)))))))
      '(err . (format "`%s' command not found in PATH!" rats-go-executable-name)))))


(defun rats--report-result (result &optional name)
  "Report the RESULT of a test run.  If NAME is given, then report its results."
  (cond ((assq 'success result)
         (let ((time (cdr (assq 'success result))))
           (message (if (s-present? name)
                        (rats--colorize (format "Test %s passed in %s." name time) 'rats-tests-successful)
                      (rats--report-multiple result)))))
        ((assq 'failure result)
         (let ((time (cdr (assq 'failure result))))
           (message (if (s-present? name)
                        (rats--colorize (format "Test %s failed in %s." name time) 'rats-tests-failed)
                      (rats--report-multiple result)))))
        ((assq 'err result)
         (message (cdr (assq 'err result))))))

(defun rats--report-multiple (result)
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
           (rats--colorize
            (format "All tests passed in %ss (%.3fs per test)." time (funcall per-test successes))
            'rats-tests-successful))
          ((and (= 0 successes) (< 0 failures))
           (rats--colorize
            (format "All tests failed in %ss (%.3fs per test)." time (funcall per-test failures))
            'rats-tests-failed))
          (t
           (rats--colorize
            (format
             "%s tests passed, %s tests failed in %ss (%.3fs per test)."
             successes failures time (funcall per-test (+ successes failures)))
            'rats-tests-mixed)))))

(defun rats--colorize (string string-face)
  "Set the face of STRING to STRING-FACE."
  (propertize string 'face string-face))

(defun rats-run-test-under-point ()
  "Run the test under point."
  (interactive)
  (if (rats--inside-test-file-p)
      (let ((test-name (rats--get-test-name)))
        (if test-name
            (let ((result (rats--run-go-test test-name)))
              (message nil)
              (rats--report-result result test-name))
          (message "Not inside a test.")))
    (message "Not inside a test file.")))

(defun rats-run-tests-for-package ()
  "Run all the tests in the directory of the current buffer."
  (interactive)
  (when (buffer-file-name)
    (if (directory-files (file-name-directory (buffer-file-name)) "_test\\.go$")
        (progn
          (let ((result (rats--run-go-test)))
            (message nil)
            (rats--report-result result)))
      (message "No test files found in the current directory."))))


(defvar rats-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-t t") #'rats-run-test-under-point)
    (define-key m (kbd "C-c C-t p") #'rats-run-tests-for-package)
    m)
  "Bindings for rats minor mode.")

(defvar rats-mode-menu-map
  (easy-menu-create-menu
   "rats"
   '(["Run test at point"                 rats-run-test-under-point  t]
     ["Run all tests for current package" rats-run-tests-for-package t]))
   "Menu for rats minor mode.")

(easy-menu-add-item nil '("Go") rats-mode-menu-map)

;;;###autoload
(define-minor-mode rats-mode
  "rats is a minor mode for running Go tests."
  :init-value nil
  :lighter " Rats"
  :group 'rats
  :keymap rats-mode-map)

(provide 'rats-mode)
;;; rats.el ends here
