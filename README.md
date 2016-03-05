# squeak.el

Manage Go tests inside Emacs.

## Installation

Install Squeak from MELPA using the package manager, then add

``` emacs-lisp
(add-hook go-mode-hook #'squeak-mode)
```

## Usage

If you're in a test file and inside a test, you can use `squeak-run-test-under-point` to run that
test. If you're editing a file inside a directory that has tests, `squeak-run-tests-for-package` and
it will run all the tests in the directory.




