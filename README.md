# Prettier-js for Emacs

[![MELPA](http://melpa.org/packages/prettier-js-badge.svg)](http://melpa.org/#/prettier-js) [![CI](https://github.com/prettier/prettier-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/prettier/prettier-emacs/actions/workflows/test.yml)

prettier-js is a function that formats the current buffer using [prettier](https://github.com/prettier/prettier). The
package also exports a minor mode that applies `(prettier-js)` on save.

## Configuration

### Requirements

Ensure that the prettier program is installed:

```bash
which prettier
```

If prettier is not installed already, you can install prettier using `npm install -g prettier` or via your package manager.


### Basic configuration

First require the package:

```elisp
(require 'prettier-js)
```

Or use `use-package` (available in Emacs 29.1 and above):

```elisp
(use-package prettier-js)
```

Then you can hook to your favorite javascript mode:

```elisp
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
...
```

### Prettier arguments

To adjust the CLI args used for the prettier command, you can customize the `prettier-js-args` variable:

```elisp
(setq prettier-js-args '(
  "--trailing-comma" "all"
  "--bracket-spacing" "false"
))
```

### Usage with web-mode

Web-mode is a popular mode for editing .js and .jsx files, but it is used to edit other template files too. If you want to hook prettier-js to web-mode for .js and .jsx files only, you can define a helper function like this:

```elisp
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))
```

And then hook to web-mode like this:

```elisp
(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))
```
## Installing on Windows

This package requires the `diff` tool which is already included on Unix platforms. The simplest way to install `diff` on Windows is to use [Chocolatey](https://chocolatey.org/). The steps are as follows:

1. Follow the Chocolatey install instructions: https://chocolatey.org/install
2. Open an Admin Powershell session
3. Install the `diff` program: `choco install diffutils`

You should now be able to open Emacs and successfully use this package.

## BSD Users

On BSD systems (like OpenBSD, FreeBSD), the default `diff` program may not support some GNU diff features that prettier-js requires, such as `--strip-trailing-cr`. To resolve this:

1. Install GNU diff (often called `gdiff` or `gnudiff`):
   - On OpenBSD: `pkg_add gdiff`
   - On FreeBSD: `pkg install diffutils`

2. Configure prettier-js to use the GNU diff implementation:
   ```elisp
   (setq prettier-js-diff-command "gdiff")
   ```
   (Use the appropriate command name for your system, which might be `gdiff`, `gnudiff`, or `gd`)

## Customization

This package is customizable via custom.el:

```
M-x customize-group prettier-js
```

* `prettier-js-command` is the prettier command
* `prettier-js-args` are the args passed to the prettier command
* `prettier-js-use-modules-bin` enables use of `node_modules/.bin/prettier` (your project's prettier version)
* `prettier-js-show-errors` customizes where to display the error output (buffer, echo or nil)
* `prettier-js-width-mode` customizes the width when formatting buffer contents (window, fill or nil)
