- [Command `er/expand-region`](#orgc6f8c83)
- [`er/expand-region`](#orge639e9b)
  - [function walkthrough](#orgb1890cb)
  - [all those `eval-after-load`](#org9637e24)
  - [`er--expand-region-1`](#orgda3e9b8)
    - [er&#x2013;this-expansion-is-better](#org4811e07)
    - [er&#x2013;show-expansion-message](#orgff4a294)
    - [expand-region-smart-cursor](#orgf9e2184)
    - [early exit](#orge9577a1)
- [expansion and contraction history](#orgecc0713)
- [text-mode-expansions.el](#orgff80d58)
- [subword-mode-expansion.el](#orgd12c37f)
- [contraction](#org28bec66)

<a id="orgc6f8c83"></a>

# Command `er/expand-region`

All emacers must have used this command. [github link](https://github.com/magnars/expand-region.el)

<a id="orge639e9b"></a>

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
... skip
(eval-after-load "text-mode"      '(require 'text-mode-expansions))
(eval-after-load 'cperl-mode     '(require 'cperl-mode-expansions))
(eval-after-load 'sml-mode       '(require 'sml-mode-expansions))
(eval-after-load 'enh-ruby-mode  '(require 'enh-ruby-mode-expansions))
(eval-after-load 'subword        '(require 'subword-mode-expansions))
```

This is pretty much the content from `expand-region.el`.

<a id="orgb1890cb"></a>

## function walkthrough

pretty straightforward

- `interactive "p"` the prefix arg is a number
- if prefix arg is less than 1, call `er/contract-region`
- when prefix arg is n and n `gte` 1, `er--expand-region-1` is invoked n times, unless `'early-exit` is returned when `er--expand-region-1` expanded to the whole buffer.
- if `expand-region-fast-keys-enabled` is truthy, and the previous command is not `expand-region`, some preprocess is done to ensure faster later expansions.

reference: [interactive codes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Interactive-Codes.html#Interactive-Codes)

<a id="org9637e24"></a>

## all those `eval-after-load`

whenever a major mode is loaded, loads te corresponding `*-expansions` sub-package.

different language has different syntax-table and semantic meanings for the same char, that&rsquo;s why there is one `*-expansions` per language.

<a id="orgda3e9b8"></a>

## `er--expand-region-1`

[link](https://github.com/magnars/expand-region.el/blob/4b8322774d9c1d8b64a0049d1dbbc1e7ce80c1a0/expand-region-core.el#L78)

As the docstring points out, the key part is:

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

In above code block, it makes a copy of `er/try-expand-list` and loop through one by one, update `best-start` and `best-end`.

<a id="org4811e07"></a>

### er&#x2013;this-expansion-is-better

simple arithemtic computation to find the smallest expand that&rsquo;s larger than the starting region. You can checkout the code [here](https://github.com/magnars/expand-region.el/blob/4b8322774d9c1d8b64a0049d1dbbc1e7ce80c1a0/expand-region-core.el#L135).

<a id="orgff4a294"></a>

### er&#x2013;show-expansion-message

[link](https://github.com/magnars/expand-region.el/blob/4b8322774d9c1d8b64a0049d1dbbc1e7ce80c1a0/expand-region-custom.el#L118) &ldquo;Whether expand-region should show usage message.&rdquo;

<a id="orgf9e2184"></a>

### expand-region-smart-cursor

By `er/expand-region` convention, the `point` is always at the beginning of the expanded region and a `mark` at the end. Unless `expand-region-smart-cursor` is truthy.

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

<a id="orge9577a1"></a>

### early exit

```elisp
    (when (and (= best-start (point-min))
               (= best-end (point-max))) ;; We didn't find anything new, so exit early
      'early-exit)
```

if the expansion is already at the max and min possible, return `'early-exit'`.

<a id="orgecc0713"></a>

# expansion and contraction history

```elisp
    ;; add hook to clear history on buffer changes
    (unless er/history
      (add-hook 'after-change-functions 'er/clear-history t t))

    ;; remember the start and end points so we can contract later
    ;; unless we're already at maximum size
    (unless (and (= start best-start)
                 (= end best-end))
      (push (cons p1 p2) er/history))

```

1.  register at the beginning of expansion session to clear history when buffer content change.
2.  register at each invocation of expansion the start and end of the current region so later we can easily and deterministically contract back to the same region.

<a id="orgff80d58"></a>

# text-mode-expansions.el

let&rsquo;s deep-dive one of the easiest expansion: [text-mode-expansions](https://github.com/magnars/expand-region.el/blob/4b8322774d9c1d8b64a0049d1dbbc1e7ce80c1a0/text-mode-expansions.el#L56).

The most important line is to add to the `er/try-expand-list`

```elisp
(defun er/add-text-mode-expansions ()
  "Adds expansions for buffers in `text-mode' except for `html-mode'.
Unfortunately `html-mode' inherits from `text-mode' and
text-mode-expansions don't work well in `html-mode'."
  (unless (member major-mode expand-region-exclude-text-mode-expansions)
    (set (make-local-variable 'er/try-expand-list)
         (append
          er/try-expand-list
          '(er/mark-text-sentence
            er/mark-text-paragraph
            mark-page)))))
```

only add `text-mode-expansions` when the current major-mode is NOT `expand-region-exclude-text-mode-expansions`

expand-region-exclude-text-mode-expansions is customizable.

```elisp
;;;###autoload
(defcustom expand-region-exclude-text-mode-expansions
  '(html-mode nxml-mode)
  "List of modes which derive from `text-mode' for which text mode expansions are not appropriate."
  :group 'expand-region
  :type '(repeat (symbol :tag "Major Mode" unknown)))
```

set `er/try-expand-list` to buffer-local so that it doesn&rsquo;t affect its list value elsewhere in other buffers.

append three new `try-expand-list` methods for text-mode into `er/try-expand-list`:

1.  `er/mark-text-sentence`
2.  `er/mark-text-paragraph`
3.  `mark-page`

```elisp
(defun er/mark-text-sentence ()
  "Marks one sentence."
  (interactive)
  ;; The obvious
  ;; (backward-sentence 1) (mark-end-of-sentence 1)
  ;; doesn't work here because it's repeated and the selection keeps
  ;; growing by sentences, which isn't what's wanted.
  (forward-sentence 1)
  (set-mark (point))
  (backward-sentence 1))
```

goto the end of the setence, mark, then go back to the beginning of the setence.

Note repeated calls of this method doesn&rsquo;t grow the region. b/c the higher semantic region is a paragraph not two setences.

```elisp
(defun er/mark-text-paragraph ()
  "Marks one paragraph."
  (interactive)
  (mark-paragraph)
  (skip-chars-forward er--space-str))

```

straightforward, `mark-paragraph` and &ldquo;trim&rdquo; the whitespace at the beginning of the paragraph.

and finally the higher level of semantic region is a page for text mode.

<a id="orgd12c37f"></a>

# subword-mode-expansion.el

this expansion extension covers the expanding subwords, e.g. camelCased words. It appends the `er/mark-subword` method into `er/try-expand-list`

```elisp
(defun er/mark-subword ()
  "Mark a subword, a part of a CamelCase identifier."
  (interactive)
  (when (and subword-mode
             expand-region-subword-enabled)
    (subword-right 1)
    (set-mark (point))
    (subword-left 1)))
```

only invoke when `subword-mode` is on and `expand-region-subword-enabled` is truthy.

1.  move one subword right
2.  mark
3.  move one subword left.

repeated calls doens&rsquo;t grow the region.

<a id="org28bec66"></a>

# contraction

`er/contract-region` uses the `er/history` built by `er/expand-region`.
