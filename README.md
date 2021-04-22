## Install

Copy `slime-info.el` and `slime-help.el` to slime `contrib` directory.

Add to Lisp compiler init file (for example, .sbclrc):

```lisp
(require :swank)
(push #p"/home/marian/src/lisp/slime-info/" swank::*load-path*)
```
