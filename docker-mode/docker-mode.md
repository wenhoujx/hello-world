- [package `docker-mode`](#org423dcb3)
- [`auto-mode-list`](#orgd665447)
- [mode definition](#org16626f1)
- [`dockerfile-mode-syntax-table`](#org9f79324)
- [`make-local-variable`](#orgd9076c6)
- [`dockerfile-mode-abbrev-table`](#org53392bf)
- [`dockerfile-build-buffer`](#org3726456)
- [`dockerfile-build-no-cache-buffer`](#orgb2a8017)
- [`dockerfile-read-image-name`](#orgf5923fc)

<a id="org423dcb3"></a>

# package `docker-mode`

<a id="orgd665447"></a>

# `auto-mode-list`

`auto-mode-alist` is a alist `(regex . major-mode)` see [wiki](https://www.emacswiki.org/emacs/AutoModeAlist) here.

the following lines ties dockerfiles to `dockerfile-mode`

```elisp
;;;###autoload
(add-to-list 'auto-mode-alist '("/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" .
                                dockerfile-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode))
```

Note that `\'` matches the end of a string, whereas `$` matches the empty string before a newline.

```elisp
(s-match "/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" "/Dockerfile.foo")
;; => (/Docker.foo) it's a match
```

<a id="org16626f1"></a>

# mode definition

this autoload code block defines the `dockerfile-mode` derived `prog-mode`,

it defines

- a syntax table
- a few local vars
- an abbrev table

more on these latter

```elisp
;;;###autoload
(define-derived-mode dockerfile-mode prog-mode "Dockerfile"
  "A major mode to edit Dockerfiles.
\\{dockerfile-mode-map}
"
  (set-syntax-table dockerfile-mode-syntax-table)
  (set (make-local-variable 'imenu-generic-expression)
       `(("Stage" dockerfile--imenu-function 1)))
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults)
       '(dockerfile-font-lock-keywords nil t))
  (setq local-abbrev-table dockerfile-mode-abbrev-table)
  (set (make-local-variable 'indent-line-function) #'dockerfile-indent-line-function))
```

\*

<a id="org9f79324"></a>

# `dockerfile-mode-syntax-table`

<a id="orgd9076c6"></a>

# `make-local-variable`

<a id="org53392bf"></a>

# `dockerfile-mode-abbrev-table`

<a id="org3726456"></a>

# `dockerfile-build-buffer`

```elisp
;;;###autoload
(defun dockerfile-build-buffer (image-name &optional no-cache)
  "Build an image called IMAGE-NAME based upon the buffer.
...
```

<a id="orgb2a8017"></a>

# `dockerfile-build-no-cache-buffer`

It calls `dockerfile-build-buffer` with `image-name` and explicit `no-cache =` t=.

`interactive` must use `list` for custom args.

```elisp
;;;###autoload
(defun dockerfile-build-no-cache-buffer (image-name)
  "Build an image called IMAGE-NAME based upon the buffer without cache."
  (interactive (list (dockerfile-read-image-name)))
  (dockerfile-build-buffer image-name t))
```

<a id="orgf5923fc"></a>

# `dockerfile-read-image-name`

```elisp
(defvar dockerfile-image-name-history nil
  "History of image names read by `dockerfile-read-image-name'.")

(defun dockerfile-read-image-name ()
  "Read a docker image name."
  (read-string "Image name: " dockerfile-image-name 'dockerfile-image-name-history))
```

`read-string` with init-input and history.

nice thing about `read-string` or `read-from-minibuffer` is if given a symbol,it automatically records the history.

> read-from-minibuffer:
>
> Fifth arg HIST, if non-nil, specifies a history list and optionally the initial position in the list. It can be a symbol, which is the history list variable to use, or a cons cell (HISTVAR . HISTPOS). In that case, HISTVAR is the history list variable to use, and HISTPOS is the initial position for use by the minibuffer history commands. For consistency, you should also specify that element of the history as the value of INITIAL-CONTENTS. Positions are counted starting from 1 at the beginning of the list. If HIST is the symbol t, history is not recorded.
