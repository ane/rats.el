# rats.el

Rats lets you run Go tests inside emacs, so that you don't need to keep a shell open and run `go
test` yourself. You can either choose to run a single test or multiple, and the results will be
displayed in the echo area.

## Installation

Using the Emacs package manager, make sure that you have [MELPA](http://melpa.org) or MELPA stable
in the package sources. For information on how to get MELPA in your package sources, please see
[here](http://melpa.org/#/getting-started).

Once you have installed `rats`, add this to your initialization script:

``` emacs-lisp
(add-hook go-mode-hook #'rats-mode)
```

Once `go-mode` activates again, you can now access Rats from the menu, or run commands interactively
(see [Usage](#usage)).

## Usage

If you're in a test file and inside a test, you can use `rats-run-test-under-point` to run that
test. If you're editing a file inside a directory that has tests, `rats-run-tests-for-package` and
it will run all the tests in the directory.

If you want to run a specific test, use `rats-run-test-in-current-buffer` to run a test from the
current buffer. This will list all the tests in the current buffer and let you pick one. The same
can be done for the whole package with `rats-run-test-from-package`.

To view the test report buffer, you can run `rats-show-test-buffer`.

## Bindings

| Binding              | Description                           |
|----------------------|---------------------------------------|
| <kbd>C-c C-t t</kbd> | Run test under point.                 |
| <kbd>C-c C-t a</kbd> | Run all tests in the current package. |
| <kbd>C-c C-t c</kbd> | Choose and run a test from the current buffer, with completion support. |
| <kbd>C-c C-t p</kbd> | Choose and run a test from the current package, with completion support. |
| <kbd>C-c C-t b</kbd> | Show the test report buffer. |
