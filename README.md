# prettier-js for Emacs

[![MELPA](http://melpa.org/packages/prettier-js-badge.svg)](http://melpa.org/#/prettier-js) [![CI](https://github.com/prettier/prettier-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/prettier/prettier-emacs/actions/workflows/test.yml)

This Emacs package provides a function, `prettier-js`, which formats the current buffer using [Prettier](https://github.com/prettier/prettier). It also exports a minor mode, `prettier-js-mode`, which calls `prettier-js` on save.

## Configuration

### Requirements

#### Have prettier installed

Ensure that the `prettier` program is installed:

```bash
which prettier
```

If `prettier` is not installed already, you can install it using `npm install -g prettier` or via your package manager.

Alternatively, if your project uses Prettier (i.e. it's a `devDependency` in your `package.json`), then you'll be able to use the executable installed via `npm install`, instead.

#### Have diff installed

This package uses the `diff` program under the hood to reliably identify formatting changes made by Prettier.  On macOS and GNU/Linux, `diff` should already be installed.  On other platforms, you should install/configure GNU's diff implementation.

<details><summary><b>Windows</b></summary>

  On Windows, install via [Chocolatey](https://chocolatey.org/):

  1. Follow the Chocolatey install instructions: https://chocolatey.org/install
  2. Open an Admin PowerShell session
  3. Install the `diff` program: `choco install diffutils`

  Also, make sure that the correct version of `diff` is accessible from your system path; sometimes, multiple conflicting `diff` executables may be installed.
</details>

<details><summary><b>BSD systems (OpenBSD, FreeBSD)</b></summary>

  On BSD systems (like OpenBSD, FreeBSD), the default `diff` program may not support some GNU diff features that this package requires, such as `--strip-trailing-cr`. To resolve this:

  1. Install GNU diff (often called `gdiff` or `gnudiff`):
     - On OpenBSD: `pkg_add gdiff`
     - On FreeBSD: `pkg install diffutils`
  2. Configure this package to use the GNU diff implementation:
     ```elisp
     (setq prettier-js-diff-command "gdiff")
     ```
     (Use the appropriate command name for your system, which might be `gdiff`, `gnudiff`, or `gd`)
</details>

### Basic configuration

First require the package:

```elisp
(require 'prettier-js)
```

Or use `use-package` (available in Emacs 29.1 and above):

```elisp
(use-package prettier-js)
```

Then you can hook into your favorite JavaScript mode:

```elisp
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
```

You can also enable use of your project's Prettier (instead of the system one):

```elisp
(setq prettier-js-use-modules-bin t)
```

This uses `node_modules/.bin/prettier`. Make sure to run `npm install` from your project directory so that the command is available there.

### Prettier arguments

To adjust the CLI args used for the `prettier` command, you can customize the `prettier-js-args` variable:

```elisp
(setq prettier-js-args '(
  "--trailing-comma" "all"
  "--bracket-spacing" "false"
))
```

### Usage with web-mode

`web-mode` is a popular mode for editing `.js` and `.jsx` files, but it is used to edit other template files too. If you want to hook `prettier-js-mode` into `web-mode` for `.js` and `.jsx` files only, you can define a helper function like this:

```elisp
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.
MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))
```

And then hook into `web-mode` like this:

```elisp
(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))
```

## Customization

This package is customizable via Emacs' easy customization interface:

```
M-x customize-group prettier-js
```

* `prettier-js-command` is the `prettier` executable to use
* `prettier-js-diff-command` is the `diff` executable to use
* `prettier-js-args` are the args passed to the prettier command
* `prettier-js-use-modules-bin` enables use of `node_modules/.bin/prettier` (your project's Prettier version)
* `prettier-js-show-errors` customizes where to display the error output (`buffer`, `echo` or `nil`)
* `prettier-js-width-mode` customizes the width when formatting buffer contents (`window`, `fill` or `nil`)
