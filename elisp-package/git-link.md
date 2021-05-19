# Table of Contents

1.  [git-link](#orgcc6c70d)
    1.  [github link to the package](#orgd5dae9e)
    2.  [A little background](#orga35bc2a)
    3.  [autoload - maker that exports functions and fake the behavior as if the whole file has been loaded.](#org9e566be)
    4.  [Source of truth about git](#org0bee833)
        1.  [git-link&#x2013;exec](#orgd7dd206)
    5.  [git-link-homepage](#org20fe842)
        1.  [interactive](#org08a51c3)
        2.  [git-link&#x2013;select-remote](#orgdb014d2)
        3.  [git-link&#x2013;remote-url](#orge7aaea7)
        4.  [cadr](#orgf8b80bd)
        5.  [git-link&#x2013;new](#orgcf5d34b)

<a id="orgcc6c70d"></a>

# git-link

<a id="orgd5dae9e"></a>

## [github link to the package](https://github.com/sshaw/git-link)

<a id="orga35bc2a"></a>

## A little background

I use this package daily at work to get the github url of the file + line number at point.

<a id="org9e566be"></a>

## [autoload](https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html) - maker that exports functions and fake the behavior as if the whole file has been loaded.

three are three autoload functions in git-link

    ;;;###autoload
    (defun git-link (remote start end) ;; link to file
    ;;;###autoload
    (defun git-link-commit (remote) ;; link to commit
    ;;;###autoload
    (defun git-link-homepage (remote) ;; link to homepage.

<a id="org0bee833"></a>

## Source of truth about git

This package eventually calls a set of `git` commands to get the information. Depending on where you call the commands, the `default-directory` is automatically set as the working directory.

<a id="orgd7dd206"></a>

### git-link&#x2013;exec

    (defun git-link--exec(&rest args)
      (ignore-errors
        (with-temp-buffer
          (when (zerop (apply #'process-file "git" nil (current-buffer) nil args))
            (goto-char (point-min))
            (cl-loop until (eobp)
                     collect (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))
                     do (forward-line 1))))))

1.  ignore-errors

    returns nil when encounter exception

2.  process-file function

    This function runs a git command and print output into a buffer

    - zerop to check exit code is 0 (success), which is [bash convention](https://tldp.org/LDP/abs/html/exit-status.html).
    - use apply instead of call `process-file` directly b/c the dynamic list of args, [as long as the last parameter is a list](https://stackoverflow.com/questions/3862394/when-do-you-use-apply-and-when-funcall).
    - cl-loop collects the output into a list

3.  example

        ;; this is the same output as git link and collect each line of output as element in a list.
        (git-link--exec "remote") ;; => ("origin") since i only have one remote

    try out process-file function, this `C-c C-c` should switch you to a buffer with the output of &ldquo;git remote&rdquo;, make sure this is invoked in a git project.

        (let ((buf (get-buffer-create "test-1")))
          (switch-to-buffer buf)
          (process-file "git" nil (current-buffer) nil "remote"))

<a id="org20fe842"></a>

## git-link-homepage

This method jumps to the github homepage of the current git project.

I set `(setq git-link-open-in-browser t)` so that this funtion jumps to chrome.

<a id="org08a51c3"></a>

### interactive

    (interactive (list (git-link--select-remote)))

- it&rsquo;s usually a string, but it may be a Lisp expression that is not a string; then it should be a form that is evaluated to get a list of arguments to pass to the command.
- it&rsquo;s as if calling this function in java or clang style: `git-link-homepage(...)` where `...` is the computed `list` of args.

<a id="orgdb014d2"></a>

### git-link&#x2013;select-remote

either read or compute the remote, e.g. `origin`

    (defun git-link--select-remote ()
      (if current-prefix-arg ;; if invoked with C-u
          (git-link--read-remote) ;; read remote from prompt
        (git-link--remote))) ;; compute the default remote

1.  git-link&#x2013;read-remote

        (defun git-link--read-remote ()
          (let ((remotes (git-link--remotes))
        	(current (git-link--remote)))
        ...)

    1.  git-link&#x2013;remote

            (git-link--current-branch) ;; => "master"
            (git-link--branch-remote "master") ;; => "origin"
            (git-link--get-config (format "branch.%s.remote" "master")) ;; calls git get-config

            git config --get "branch.master.remote"
            # origin

    2.  git-link&#x2013;remotes

            (git-link--exec "remote")

    3.  completing-read

            (completing-read "Pick one: "
            		     '(a b c) ;; collection
            		     nil ;; predicate
            		     t ;; must match one
            		     "" ;; initial input
            		     nil ;; hist
            		     "b")

<a id="orge7aaea7"></a>

### git-link&#x2013;remote-url

    (git-link--remote-url "origin")
    (git-link--parse-remote (git-link--remote-url "origin"))

<a id="orgf8b80bd"></a>

### cadr

    (car '(0 1 2))
    (cdr '(0 1 2))
    (cadr '(0 1 2))

<a id="orgcf5d34b"></a>

### git-link&#x2013;new

    (message "test%20") ;; error
    (message "test%%20") ;; single %
