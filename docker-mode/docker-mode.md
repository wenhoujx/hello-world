- [package `docker-mode`](#orgbcb8a85)
- [`auto-mode-list`](#org451e820)
- [mode definition](#org8acc731)
- [`dockerfile-mode-syntax-table`](#org2304a1b)
- [`make-local-variable`](#org7bc0e69)
- [`dockerfile-mode-abbrev-table`](#org2c1a963)
- [`dockerfile-build-buffer`](#org3af7fc0)
  - [`dockerfile-mode-command`](#org7745d25)
  - [`dockerfile-tag-string image-name`](#org02622a7)
  - [`dockerfile-build-arg-string`](#org74a4e2f)
  - [`dockerfile-standard-filename`](#org94f7d99)
- [`dockerfile-build-no-cache-buffer`](#org3351d12)
  - [`dockerfile-read-image-name`](#orgce357bb)

<a id="orgbcb8a85"></a>

# package `docker-mode`

I don&rsquo;t actually use this mode directly, but i use other packages rely on this one.

It&rsquo;s maintained by `spotify`, so hopefully the code quality is good and up to date.

<a id="org451e820"></a>

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

<a id="org8acc731"></a>

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

<a id="org2304a1b"></a>

# `dockerfile-mode-syntax-table`

<a id="org7bc0e69"></a>

# `make-local-variable`

<a id="org2c1a963"></a>

# `dockerfile-mode-abbrev-table`

<a id="org3af7fc0"></a>

# `dockerfile-build-buffer`

```elisp
;;;###autoload
(defun dockerfile-build-buffer (image-name &optional no-cache)
  "Build an image called IMAGE-NAME based upon the buffer.

If prefix arg NO-CACHE is set, don't cache the image.
The build string will be of the format:
`sudo docker build --no-cache --tag IMAGE-NAME --build-args arg1.. -f filename directory`"

  (interactive (list (dockerfile-read-image-name) prefix-arg))
  (save-buffer)
    (compilation-start
        (format
            "%s%s%s build %s %s %s -f %s %s"
            (if dockerfile-use-buildkit "DOCKER_BUILDKIT=1 " "")
            (if dockerfile-use-sudo "sudo " "")
            dockerfile-mode-command
            (if no-cache "--no-cache" "")
            (dockerfile-tag-string image-name)
            (dockerfile-build-arg-string)
            (shell-quote-argument (dockerfile-standard-filename
				   (or (file-remote-p (buffer-file-name) 'localname)
				       (buffer-file-name))))
            (shell-quote-argument (dockerfile-standard-filename
				   (or (file-remote-p default-directory 'localname)
				       default-directory))))
    nil
    (lambda (_) (format "*docker-build-output: %s *" image-name))))
```

1.  saves buffer
2.  `compilation-start`, afaik this is better than a plain `shell command` b/c it pops a compliation buffer in `compliation mode` with `syntax highlighting` and easier `previous-error` and `next-error` navigation and more
3.  run a docker command with `compilation-start`

<a id="org7745d25"></a>

## `dockerfile-mode-command`

```elisp
(defcustom dockerfile-mode-command "docker"
  "Which binary to use to build images."
  :group 'dockerfile
  :type 'string)
```

straightforward customizeable var to use, make sure `docker` is in your `PATH`, do

```sh
which docker
# and see if this prints out the right path for docker executable
```

<a id="org02622a7"></a>

## `dockerfile-tag-string image-name`

```elisp
(defun dockerfile-tag-string (image-name)
  "Return a --tag shell-quoted IMAGE-NAME string or an empty string if image-name is blank."
    (if (string= image-name "") "" (format "--tag %s " (shell-quote-argument image-name))))
```

this method returns a string like `"--tag <image-name>"`, tag a docker with image-name e.g. `"test-service"` automatically creates a alias `"test-service:latest"`

<a id="org74a4e2f"></a>

## `dockerfile-build-arg-string`

<a id="org94f7d99"></a>

## `dockerfile-standard-filename`

<a id="org3351d12"></a>

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

<a id="orgce357bb"></a>

## `dockerfile-read-image-name`

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
