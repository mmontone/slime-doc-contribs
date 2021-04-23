## Install

Load `swank` and add this repository path to `swank::*load-path*`, in your Lisp compiler init file (~/.sbclrc if using SBCL):

```lisp
(require :swank)
(push #p"/home/marian/src/lisp/slime-info/" swank::*load-path*)
```

In Emacs, add this repository path to `load-path` and ddd `slime-help` and `slime-info` to `slime-contribs` in `~/.emacs` init file, like:

```
(push "/home/marian/src/lisp/slime-info" load-path)

(setq slime-contribs '(slime-fancy slime-help slime-info))

(slime-setup)
```
