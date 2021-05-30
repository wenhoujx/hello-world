- [Command `er/expand-region`](#org796d9fa)
- [`er/expand-region`](#orgc649f56)
  - [function walkthrough](#org3e8c7ee)
  - [all those eval-after-load](#orga042ce0)
  - [`er--expand-region-1`](#orgbb71e08)

<a id="org796d9fa"></a>

# Command `er/expand-region`

All emacers must have used this command. [github link](https://github.com/magnars/expand-region.el)

<a id="orgc649f56"></a>

# `er/expand-region`

```elisp
(require 'expand-region-core)
(require 'expand-region-custom)
(require 'er-basic-expansions)

;;;###autoload
(defun er/expand-region (arg)
  "Increase selected region by semantic units.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time."
  (interactive "p")
  (if (< arg 1)
      (er/contract-region (- arg))
    (er--prepare-expanding)
    (while (>= arg 1)
      (setq arg (- arg 1))
      (when (eq 'early-exit (er--expand-region-1))
        (setq arg 0)))
    (when (and expand-region-fast-keys-enabled
               (not (memq last-command '(er/expand-region er/contract-region))))
      (er/prepare-for-more-expansions))))

(eval-after-load 'clojure-mode   '(require 'clojure-mode-expansions))
(eval-after-load 'css-mode       '(require 'css-mode-expansions))
(eval-after-load 'erlang-mode    '(require 'erlang-mode-expansions))
(eval-after-load 'feature-mode   '(require 'feature-mode-expansions))
(eval-after-load 'sgml-mode      '(require 'html-mode-expansions)) ;; html-mode is defined in sgml-mode.el
(eval-after-load 'rhtml-mode     '(require 'html-mode-expansions))
(eval-after-load 'nxhtml-mode    '(require 'html-mode-expansions))
(eval-after-load 'web-mode       '(require 'web-mode-expansions))
(eval-after-load 'js             '(require 'js-mode-expansions))
(eval-after-load 'js2-mode       '(require 'js-mode-expansions))
(eval-after-load 'js2-mode       '(require 'js2-mode-expansions))
(eval-after-load 'js3-mode       '(require 'js-mode-expansions))
(eval-after-load 'latex          '(require 'latex-mode-expansions))
(eval-after-load 'nxml-mode      '(require 'nxml-mode-expansions))
(eval-after-load 'octave-mod     '(require 'octave-expansions))
(eval-after-load 'octave         '(require 'octave-expansions))
(eval-after-load 'python         '(progn
                                    (when expand-region-guess-python-mode
                                      (expand-region-guess-python-mode))
                                    (if (eq 'python expand-region-preferred-python-mode)
                                        (require 'python-el-expansions)
                                      (require 'python-el-fgallina-expansions))))
(eval-after-load 'python-mode    '(require 'python-mode-expansions))
(eval-after-load 'ruby-mode      '(require 'ruby-mode-expansions))
(eval-after-load 'org            '(require 'the-org-mode-expansions))
(eval-after-load 'cc-mode        '(require 'cc-mode-expansions))
(eval-after-load "text-mode"      '(require 'text-mode-expansions))
(eval-after-load 'cperl-mode     '(require 'cperl-mode-expansions))
(eval-after-load 'sml-mode       '(require 'sml-mode-expansions))
(eval-after-load 'enh-ruby-mode  '(require 'enh-ruby-mode-expansions))
(eval-after-load 'subword        '(require 'subword-mode-expansions))
```

This is pretty much the content from `expand-region.el`.

<a id="org3e8c7ee"></a>

## function walkthrough

pretty straightforward

- `interactive "p"` the prefix arg is a number
- if prefix arg is less than 1, call `er/contract-region`
- when prefix arg is n and n `gte` 1, `er--expand-region-1` is invoked n times, unless `'early-exit` is returned when `er--expand-region-1` expanded to the whole buffer.
- if `expand-region-fast-keys-enabled` is truthy, and the previous command is not `expand-region`, some preprocess is done to ensure faster later expansions.

reference: [interactive codes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html#Interactive-Codes)

<a id="orga042ce0"></a>

## all those eval-after-load

whenever a major mode is loaded, loads te corresponding `*-expansions` sub-package.

different language has different syntax-table and semantic meanings for the same char, that&rsquo;s why there is one `*-expansions` per language.

<a id="orgbb71e08"></a>

## `er--expand-region-1`

```elisp
(defun er--expand-region-1 ()
  "Increase selected region by semantic units.
Basically it runs all the mark-functions in `er/try-expand-list'
and chooses the one that increases the size of the region while
moving point or mark as little as possible."
  (let* ((p1 (point))
         (p2 (if (use-region-p) (mark) (point)))
         (start (min p1 p2))
         (end (max p1 p2))
         (try-list er/try-expand-list)
         (best-start (point-min))
         (best-end (point-max))
         (set-mark-default-inactive nil))

    ;; add hook to clear history on buffer changes
    (unless er/history
      (add-hook 'after-change-functions 'er/clear-history t t))

    ;; remember the start and end points so we can contract later
    ;; unless we're already at maximum size
    (unless (and (= start best-start)
                 (= end best-end))
      (push (cons p1 p2) er/history))

    (when (and expand-region-skip-whitespace
               (er--point-is-surrounded-by-white-space)
               (= start end))
      (skip-chars-forward er--space-str)
      (setq start (point)))

    (while try-list
      (er--save-excursion
       (ignore-errors
         (funcall (car try-list))
         (when (and (region-active-p)
                    (er--this-expansion-is-better start end best-start best-end))
           (setq best-start (point))
           (setq best-end (mark))
           (when (and er--show-expansion-message (not (minibufferp)))
             (message "%S" (car try-list))))))
      (setq try-list (cdr try-list)))

    (setq deactivate-mark nil)
    ;; if smart cursor enabled, decide to put it at start or end of region:
    (if (and expand-region-smart-cursor
             (not (= start best-start)))
        (progn (goto-char best-end)
               (set-mark best-start))
      (goto-char best-start)
      (set-mark best-end))

    (er--copy-region-to-register)

    (when (and (= best-start (point-min))
               (= best-end (point-max))) ;; We didn't find anything new, so exit early
      'early-exit)))
```

As documented, the key part is:

```elisp
  (let* (...
         (try-list er/try-expand-list)
         (best-start (point-min))
         (best-end (point-max))
         ...)

         ...
    (while try-list
      (er--save-excursion
       (ignore-errors
         (funcall (car try-list))
         (when (and (region-active-p)
                    (er--this-expansion-is-better start end best-start best-end))
           (setq best-start (point))
           (setq best-end (mark))
           (when (and er--show-expansion-message (not (minibufferp)))
             (message "%S" (car try-list))))))
      (setq try-list (cdr try-list)))
...
```

In above code block, it makes a copy of `er/try-expand-list` and loop through one by one, update `best-start` and `best-end`. By `er/expand-region` convention, the `point` is always at the beginning of the expanded region and a `mark` at the end. Unless `expand-region-smart-cursor` is truthy.

```elisp
;;;###autoload
(defcustom expand-region-smart-cursor nil
  "Defines whether the cursor should be placed intelligently after expansion.

If set to t, and the cursor is already at the beginning of the new region,
keep it there; otherwise, put it at the end of the region.

If set to nil, always place the cursor at the beginning of the region."
  :group 'expand-region
  :type '(choice (const :tag "Smart behaviour" t)
                 (const :tag "Standard behaviour" nil)))
```
