# Prettier-js for Emacs
[![MELPA](http://melpa.org/packages/prettier-js-badge.svg)](http://melpa.org/#/prettier-js)

prettier-js is a function that formats the current buffer using [prettier](https://github.com/prettier/prettier). The
package also exports a minor mode that applies `(prettier-js)` on save.

## Configuration

### Requirements

Ensure that the prettier program is installed:

```bash
which prettier
```

If prettier is not installed already, you can install prettier using `npm -g prettier` or via your package manager.


### Basic configuration

First require the package:

```elisp
(require 'prettier-js)
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

## Customization

This package is customizable via custom.el:

```
M-x customize-group prettier-js
```

* `prettier-js-command` is the prettier command
* `prettier-js-args` are the args passed to the prettier command
* `prettier-js-show-errors` customizes where to display the error output (buffer, echo or nil)
* `prettier-js-width-mode` customizes the width when formatting buffer contents (window, fill or nil)
