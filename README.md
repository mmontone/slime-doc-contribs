# SLIME-DOC-CONTRIBS

This repository contains two different SLIME (The Superior Lisp Interaction Mode for Emacs) contribs that extend Common Lisp documentation from Emacs. SLIME-HELP nd SLIME-INFO.

SLIME-HELP is inspired by helpful-mode, but for browsing Common Lisp documentation instead.

SLIME-INFO uses Emacs info-mode for displaying Common Lisp documentation.

NOTE THAT THIS IS VERY MUCH WORK IN PROGRESS AT THIS MOMENT.

![slime-help](slime-help.png "slime-help screenshot")

## Install

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
slime-help-function	      M-x ... RET
   Display documentation about Common Lisp function bound to
   SYMBOL-NAME.
slime-help-package	      M-x ... RET
   Display information about Common Lisp package named PACKAGE-NAME.
slime-help-symbol	      M-x ... RET
   Open a help buffer for each kind of SYMBOL-NAME.
slime-help-system	      M-x ... RET
   Display documentation about ASDF system named SYSTEM-NAME.
slime-help-variable	      M-x ... RET
   Display documentation about Common Lisp variable bound to
   SYMBOL-NAME.
slime-help-packages
   Browse all loaded packages
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
