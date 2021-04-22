## Install

Copy `slime-info.el` and `slime-help.el` to slime `contrib` directory.

Add to Lisp compiler init file (for example, .sbclrc):

```lisp
(require :swank)
(push #p"/home/marian/src/lisp/slime-info/" swank::*load-path*)
```

Add `slime-help` and `slime-info` to `slime-contribs` in `~/.emacs` init file, like:

```
(setq slime-contribs '(slime-fancy slime-help slime-info))

(slime-setup)
```
