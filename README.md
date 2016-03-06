# squeak.el

Squeak lets you run Go tests inside emacs, so that you don't need to keep a shell open and run `go
test` yourself. You can either choose to run a single test or multiple, and the results will be
displayed in the echo area.

## Installation

Install Squeak from MELPA using the package manager, then add

``` emacs-lisp
(add-hook go-mode-hook #'squeak-mode)
```

Once `go-mode` activates again, you can now access Squeak from the menu.

## Usage

If you're in a test file and inside a test, you can use `squeak-run-test-under-point` to run that
test. If you're editing a file inside a directory that has tests, `squeak-run-tests-for-package` and
it will run all the tests in the directory.

| Binding              | Description                           |
|----------------------|---------------------------------------|
| <kbd>C-c C-t t</kbd> | Run test under point.                 |
| <kbd>C-c C-t p</kbd> | Run all tests in the current package. |
