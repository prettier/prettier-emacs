# prettier-js for Emacs

[![MELPA](http://melpa.org/packages/prettier-js-badge.svg)](http://melpa.org/#/prettier-js) [![CI](https://github.com/prettier/prettier-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/prettier/prettier-emacs/actions/workflows/test.yml)

This Emacs package provides a function, `prettier-js-prettify`, which formats the current buffer using [Prettier](https://github.com/prettier/prettier). It also exports a minor mode, `prettier-js-mode`, which calls `prettier-js-prettify` on save.

Despite the "-js" in its name, this package actually supports more than just JavaScript. It works with any language that Prettier supports! [List here.](https://prettier.io/docs/)

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

#### Ensure Emacs' path is configured correctly

Depending on how the programs are installed, `node`, `prettier`, and/or `diff` may be missing on Emacs' `exec-path`.  When attempting to format a buffer, this could result in errors such as:

- "Node executable not found"
- "Could not find prettier executable"

An easy solution to this issue is to install and use [`exec-path-from-shell`](https://github.com/purcell/exec-path-from-shell).  Add to your init file:

```elisp
(exec-path-from-shell-initialize)
```

### Basic configuration

First require the package:

```elisp
(require 'prettier-js)
```

Or use `use-package` (available in Emacs 29.1 and above):

```elisp
(use-package prettier-js)
```

Then you can hook into any modes where you want to format with Prettier:

```elisp
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
```

You can also enable use of your project's Prettier (instead of the system one):

```elisp
(setq prettier-js-use-modules-bin t)
```

This uses `node_modules/.bin/prettier`. Make sure to run `npm install` from your project directory so that the command is available there.

### Enable/disable per project

Say that you only want to use Prettier with certain projects. Instead of configuring Emacs with `(add-hook 'some-mode-hook 'prettier-js-mode)`, you could check for the presence of a config file:

```elisp
(defun maybe-use-prettier ()
  "Enable `prettier-js-mode' if an rc file is located."
  (if (locate-dominating-file default-directory ".prettierrc")
      (prettier-js-mode +1)))

(add-hook 'js-mode-hook 'maybe-use-prettier)
```

Alternatively, say that you want to use Prettier everywhere by default, except for when editing files in certain directories. Use `(add-hook 'some-mode-hook 'prettier-js-mode)`, and in directories where you want Prettier disabled, add a `.dir-locals.el` file with the following contents:

```elisp
((nil . ((eval . (prettier-js-mode 0)))))
```

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

### Usage with prettierd

If you want to mostly eliminate the overhead of running the `prettier` command on every file save (improving performance slightly), you could try using [`prettierd`](https://github.com/fsouza/prettierd) to run Prettier as a daemon.

```elisp
(setq prettier-js-command "prettierd")
```

Note that this may come at the expense of a bit more complexity in terms of configuring/managing the daemon.

## Available commands

* `M-x prettier-js-prettify` formats the current buffer.  In `org-mode`, it formats all the code blocks in the buffer.
* `M-x prettier-js-prettify-region` formats the current region
* `M-x prettier-js-prettify-code-block` formats the code block at point (in `org-mode`)

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
