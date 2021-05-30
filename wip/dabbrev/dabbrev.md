- [dabbrev](#orgc5a80ff)
- [dabbrev-expand](#orgc58b449)

<a id="orgc5a80ff"></a>

# dabbrev

[dynamic abbrev](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/dabbrev.el?h=fa65c044f2ebe666467166075c1507a8d0e1347f#n429)

<a id="orgc58b449"></a>

# dabbrev-expand

[code](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/dabbrev.el?h=fa65c044f2ebe666467166075c1507a8d0e1347f#n429)

```elisp
(defun dabbrev-expand (arg)
  "Expand previous word \"dynamically\".
...""
  (interactive "*P")
...
```

[interactive-codes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html)

1.  `*` Signal an error if the current buffer is read only
2.  `P` The raw prefix argument.
