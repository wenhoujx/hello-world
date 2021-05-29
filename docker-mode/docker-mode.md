- [package `dockerfile-mode`](#orgd02fd7d)
  - [disclaimer](#org4cb33be)
- [`auto-mode-list`](#org22bda44)
- [`define-derived-mode`](#org8fe6d27)
  - [`dockerfile-mode-syntax-table`](#org49212df)
  - [`make-local-variable`](#org4bd6fc5)
  - [`imenu-generic-expression`](#orgc446d88)
  - [`indent-line-function`](#org599c5e4)
- [More than just syntax highlighting](#org62f78e4)
- [`dockerfile-build-buffer`](#orgdf0d34f)
  - [`dockerfile-mode-command`](#orga36d3e4)
  - [`dockerfile-tag-string image-name`](#org75038a5)
  - [`dockerfile-build-arg-string`](#org26907f2)
    - [`dockerfile-build-args`](#org0d310f6)
  - [`dockerfile-standard-filename`](#orgf5a3508)
- [`dockerfile-build-no-cache-buffer`](#orgcbf314e)
  - [`dockerfile-read-image-name`](#orgd5d078c)

<a id="orgd02fd7d"></a>

# package `dockerfile-mode`

This packge provides the following features:

1.  dockerfile syntax highlighting
2.  dockerfile indentation
3.  dockerfile compilation

<a id="org4cb33be"></a>

## disclaimer

I don&rsquo;t actually use this mode directly, but i use other packages rely on this one. It&rsquo;s maintained by `spotify`, so it should be of good code quality and fun to read and learn.

<a id="org22bda44"></a>

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

<a id="org8fe6d27"></a>

# `define-derived-mode`

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

<a id="org49212df"></a>

## `dockerfile-mode-syntax-table`

```elisp
(defvar dockerfile-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?= "." table)
    table)
  "Syntax table for `dockerfile-mode'.")
```

see [Xah, Elisp: syntax table](http://ergoemacs.org/emacs/elisp_syntax_table.html) for more details.

The parent `prog-mode` has some syntax chars that need to be changed for dockerfile.

1.  `#` means `comment=` starts, it&rsquo;s punctuation in `prog-mode`
2.  `\` means `comment ends, it's a whitespace in =prog-mode`
3.  `'` means `string delimiter`, it&rsquo;s a punctuation in `prog-mode`
4.  `=` means `punctuation`, it&rsquo;s a symbol in `prog-mode`

examples:

```yaml
# this is a comment

# 'foo' is a string in docker file.

# the = sign separates the words
syntax=docker/dockerfile:1
```

<a id="org4bd6fc5"></a>

## `make-local-variable`

use `make-local-variable` to make certain vars local to the curreng opening `dockerfile` buffer, so that change their meaning/behavior doesn&rsquo;t change their meaning/behavior in other `non-dockerfile-mode` buffers.

`comment-start`, `comment-end` and `comment-start-skip` affects `comment-region` and `uncomment-region` and other comments related packages. In particular, `comment-start-skip` is at least 1 `#` and any whitespace.

<a id="orgc446d88"></a>

## `imenu-generic-expression`

provides regex for create an imenu

> List of definition matchers for creating an Imenu index.
>
> Each element of this list should have the form
>
> (MENU-TITLE REGEXP INDEX [FUNCTION] [ARGUMENTS&#x2026;])

<a id="org599c5e4"></a>

## `indent-line-function`

```elisp
(defun dockerfile-indent-line-function ()
  "Indent lines in a Dockerfile.

Lines beginning with a keyword are ignored, and any others are
indented by one `dockerfile-indent-offset'."
  (unless (member (get-text-property (point-at-bol) 'face)
                  '(font-lock-comment-delimiter-face font-lock-keyword-face))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward "[ \t]" (point-at-eol))
      (unless (equal (point) (point-at-eol)) ; Ignore empty lines.
        ;; Delete existing whitespace.
        (delete-char (- (point-at-bol) (point)))
        (indent-to dockerfile-indent-offset)))))
```

1.  ignore keyword and comments at the beginning-of-line
2.  otherwise, go to the beginning-of-line, skip whitespace.
3.  ignore empty line
4.  delete whitespace from beginning-of-line
5.  indent `dockerfile-indent-offset` which is `(or standard-indent 2)`

<a id="org62f78e4"></a>

# More than just syntax highlighting

dockerfile-mode provides more than just syntax highlighting, two more autoload functions that compile the current buffer of a docker file.

See the following sections

<a id="orgdf0d34f"></a>

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

<a id="orga36d3e4"></a>

## `dockerfile-mode-command`

```elisp
(defcustom dockerfile-mode-command "docker"
  "Which binary to use to build images."
  :group 'dockerfile
  :type 'string)
```

straightforward customizeable var to use, make sure `docker` is in your `PATH`, try

```sh
which docker
# and see if this prints out the right path for docker executable
```

<a id="org75038a5"></a>

## `dockerfile-tag-string image-name`

```elisp
(defun dockerfile-tag-string (image-name)
  "Return a --tag shell-quoted IMAGE-NAME string or an empty string if image-name is blank."
    (if (string= image-name "") "" (format "--tag %s " (shell-quote-argument image-name))))
```

this method returns a string like `--tag <image-name>`, tag a docker with image-name e.g. `test-service` automatically creates a alias. [doc](https://docs.docker.com/engine/reference/commandline/tag/) `test-service:latest`

<a id="org26907f2"></a>

## `dockerfile-build-arg-string`

```elisp
(defun dockerfile-build-arg-string ()
  "Create a --build-arg string for each element in `dockerfile-build-args'."
  (mapconcat (lambda (arg) (concat "--build-arg " (shell-quote-argument arg)))
             dockerfile-build-args " "))
```

`mapconcat` applies the `lambda` to each element in the list to set a list of strings and concat them with a joiner, which in this case is `" "`.

<a id="org0d310f6"></a>

### `dockerfile-build-args`

```elisp
(defcustom dockerfile-build-args nil
  "List of --build-arg to pass to docker build.

Each element of the list will be passed as a separate
 --build-arg to the docker build command."
  :type '(repeat string)
  :group 'dockerfile)
```

default `nil`, customizeable.

<a id="orgf5a3508"></a>

## `dockerfile-standard-filename`

`file` arg is `(buffer-file-name)` in the case when the file is local, and `buffer-file-name` returns the full path.

```elisp
(defun dockerfile-standard-filename (file)
  "Convert the FILE name to OS standard.
If in Cygwin environment, uses Cygwin specific function to convert the
file name.  Otherwise, uses Emacs' standard conversion function."
  (if (fboundp 'cygwin-convert-file-name-to-windows)
      (replace-regexp-in-string
       (rx "\\") "\\\\" (cygwin-convert-file-name-to-windows file) t t)
    (convert-standard-filename file)))
```

get OS standard filename

the reason

> (defun convert-standard-filename (filename) &ldquo;Convert a standard file&rsquo;s name to something suitable for the OS. This means to guarantee valid names and perhaps to canonicalize certain patterns.
>
> FILENAME should be an absolute file name since the conversion rules sometimes vary depending on the position in the file name. E.g. c:/foo is a valid DOS file name, but c:/bar/c:/foo is not.
>
> This function&rsquo;s standard definition is trivial; it just returns the argument. However, on Windows and DOS, replace invalid characters. On DOS, make sure to obey the 8.3 limitations. In the native Windows build, turn Cygwin names into native names.
>
> See Info node \`(elisp)Standard File Names&rsquo; for more details.&ldquo;

<a id="orgcbf314e"></a>

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

<a id="orgd5d078c"></a>

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
