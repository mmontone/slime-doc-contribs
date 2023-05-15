# SLIME-DOC-CONTRIBS

This repository contains two different SLIME (The Superior Lisp Interaction Mode for Emacs) contribs that extend Common Lisp documentation from Emacs. SLIME-HELP nd SLIME-INFO.

SLIME-HELP is inspired by helpful-mode, but for browsing Common Lisp documentation instead.

SLIME-INFO uses Emacs info-mode for displaying Common Lisp documentation.

![slime-help](slime-help.png "slime-help screenshot")

## Install

ℹ️ Please consider using [SLIME :star:](https://github.com/mmontone/slime-star), that comes with this extension preinstalled.

Clone this repository using git `--recursive` option, as this repository contains submodules.

Load `swank` and add this repository path to `swank::*load-path*`, in your Lisp compiler init file (~/.sbclrc if using SBCL):

```lisp
(require :swank)
(push #p"/home/marian/src/lisp/slime-doc-contribs/" swank::*load-path*)
```

In Emacs, add this repository path to `load-path` and ddd `slime-help` and `slime-info` to `slime-contribs` in `~/.emacs` init file, like:

```
(push "/home/marian/src/lisp/slime-doc-contribs" load-path)

(setq slime-contribs '(slime-fancy slime-help slime-info))

(slime-setup)
```

## Use

### SLIME-HELP

#### Commands

```
slime-help		      M-x ... RET
   (not documented)
slime-help--kill-current-buffer	M-x ... RET
   (not documented)
slime-help-apropos	      M-x ... RET
   Show all bound symbols whose names match STRING. With prefix
slime-help-apropos-all	      M-x ... RET
   Shortcut for (slime-help-apropos <string> nil nil)
slime-help-apropos-documentation M-x ... RET
   Show symbols whose documentation contains matches for PATTERN.
slime-help-apropos-package    M-x ... RET
   Show apropos listing for symbols in PACKAGE.
slime-help-class	      M-x ... RET
   Display documentation about Common Lisp class bound to SYMBOL-NAME.
slime-help-function	      M-x ... RET
   Display documentation about Common Lisp function bound to
   SYMBOL-NAME.
slime-help-generic-function   M-x ... RET
   Display documentation about Common Lisp generic function bound to
   SYMBOL-NAME.
slime-help-macro	      M-x ... RET
   Display documentation about Common Lisp macro bound to SYMBOL-NAME.
slime-help-mode		      M-x ... RET
   Quicklisp systems minor mode.
slime-help-mode-menu	      M-x ... RET
   Menu for SLIME-Help
slime-help-package	      M-x ... RET
   Display information about Common Lisp package named PACKAGE-NAME.
slime-help-packages	      M-x ... RET
   Display information about Common Lisp packages.
slime-help-quit		      M-x ... RET
   Kill all slime-help buffers at once.
slime-help-special-operator   M-x ... RET
   Display documentation about Common Lisp macro bound to SYMBOL-NAME.
slime-help-submenu	      M-x ... RET
   Menu for SLIME-Help
slime-help-symbol	      M-x ... RET
   Open a help buffer for each kind of SYMBOL-NAME.
slime-help-system	      M-x ... RET
   Display documentation about ASDF system named SYSTEM-NAME.
slime-help-systems	      M-x ... RET
   Display information about registered ASDF systems.
slime-help-variable	      M-x ... RET
   Display documentation about Common Lisp variable bound to
   SYMBOL-NAME.
```

Use `q` to kill individual buffers, and `Q` to kill all help buffers at once.

### SLIME-INFO

#### Commands

```
slime-info-apropos	      M-x ... RET
   (not documented)
slime-info-apropos-all	      M-x ... RET
   Shortcut for (slime-apropos <string> nil nil).
slime-info-apropos-package    M-x ... RET
   Show apropos listing for symbols in PACKAGE.
slime-info-package	      M-x ... RET
   Show information about Common Lisp package named PACKAGE-NAME,
   using an Info buffer.
slime-info-symbol	      M-x ... RET
   Show a buffer with description of SYMBOL-NAME in an Info buffer.
slime-info-system	      M-x ... RET
   Show information about Common Lisp ASDF system named SYSTEM-NAME,
   using an Info buffer.
```

Use `q` to kill individual buffers, and `Q` to kill all help buffers at once.
