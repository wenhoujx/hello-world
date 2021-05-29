# Table of Contents

1.  [package `docker-mode`](#org59305eb)
2.  [`auto-mode-list`](#org94dc70a)
3.  [mode definition](#org001b390)
4.  [`dockerfile-mode-syntax-table`](#org2ba1f36)
5.  [`make-local-variable`](#orgfa52101)
6.  [`dockerfile-mode-abbrev-table`](#org0df28b4)
7.  [`dockerfile-build-buffer`](#org6f1e1de)
8.  [`dockerfile-build-no-cache-buffer`](#org6a9d3fc)
9.  [`dockerfile-read-image-name`](#orgd4f6b65)

<a id="org59305eb"></a>

# package `docker-mode`

<a id="org94dc70a"></a>

# `auto-mode-list`

`auto-mode-alist` is a alist `(regex . major-mode)` see [wiki](https://www.emacswiki.org/emacs/AutoModeAlist) here.

the following lines ties dockerfiles to `dockerfile-mode`

    ;;;###autoload
    (add-to-list 'auto-mode-alist '("/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" .
                                    dockerfile-mode))

    ;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode))

Note that `\'` matches the end of a string, whereas `$` matches the empty string before a newline.

    (s-match "/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" "/Dockerfile.foo")
    ;; => (/Docker.foo) it's a match

<a id="org001b390"></a>

# mode definition

this autoload code block defines the `dockerfile-mode` derived `prog-mode`,

it defines

- a syntax table
- a few local vars
- an abbrev table

more on these latter

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

\*

<a id="org2ba1f36"></a>

# `dockerfile-mode-syntax-table`

<a id="orgfa52101"></a>

# `make-local-variable`

<a id="org0df28b4"></a>

# `dockerfile-mode-abbrev-table`

<a id="org6f1e1de"></a>

# `dockerfile-build-buffer`

    ;;;###autoload
    (defun dockerfile-build-buffer (image-name &optional no-cache)
      "Build an image called IMAGE-NAME based upon the buffer.

<a id="org6a9d3fc"></a>

# `dockerfile-build-no-cache-buffer`

It calls `dockerfile-build-buffer` with `image-name` and explicit `no-cache =` t=.

`interactive` must use `list` for custom args.

    ;;;###autoload
    (defun dockerfile-build-no-cache-buffer (image-name)
      "Build an image called IMAGE-NAME based upon the buffer without cache."
      (interactive (list (dockerfile-read-image-name)))
      (dockerfile-build-buffer image-name t))

<a id="orgd4f6b65"></a>

# `dockerfile-read-image-name`

    (defvar dockerfile-image-name-history nil
      "History of image names read by `dockerfile-read-image-name'.")

    (defun dockerfile-read-image-name ()
      "Read a docker image name."
      (read-string "Image name: " dockerfile-image-name 'dockerfile-image-name-history))

`read-string` with init-input and history.

nice thing about `read-string` or `read-from-minibuffer` is if given a symbol,it automatically records the history.

> read-from-minibuffer:
>
> Fifth arg HIST, if non-nil, specifies a history list and optionally
> the initial position in the list. It can be a symbol, which is the
> history list variable to use, or a cons cell (HISTVAR . HISTPOS).
> In that case, HISTVAR is the history list variable to use, and
> HISTPOS is the initial position for use by the minibuffer history
> commands. For consistency, you should also specify that element of
> the history as the value of INITIAL-CONTENTS. Positions are counted
> starting from 1 at the beginning of the list. If HIST is the symbol
> t, history is not recorded.
