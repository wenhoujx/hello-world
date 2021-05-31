- [dabbrev](#org0b82fe7)
- [dabbrev-expand](#org939f2f1)
- [The Meat: dabbrev&#x2013;search](#org638d021)
  - [dabbrev&#x2013;abbrev-char-regexp](#orgd2fd589)
  - [dabbrev-limit](#org2c8743a)
  - [search](#org8cc3c3b)
  - [filter matches](#org8ea7b55)
- [remember the last search](#orga9ad0ae)
- [search from other buffers and friend buffers](#orgf3cc5dc)
- [find all candidates at once.](#org6435527)
  - [completion-in-region](#org5800421)
  - [table lambda:](#orgdc67c0d)
    - [when action is not &rsquo;metadata](#org3c3ae1d)
    - [dabbrev&#x2013;find-all-expansions](#orgb5003af)
    - [complete-with-action](#org986b3fe)

<a id="org0b82fe7"></a>

# dabbrev

[dynamic abbrev](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/dabbrev.el?h=fa65c044f2ebe666467166075c1507a8d0e1347f#n429)

<a id="org939f2f1"></a>

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

<a id="org638d021"></a>

# The Meat: dabbrev&#x2013;search

[link](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/dabbrev.el?h=fa65c044f2ebe666467166075c1507a8d0e1347f#n918)

```elisp
(defun dabbrev--search (abbrev reverse ignore-case)
  "Search for something that could be used to expannd ABBREV.

Second arg, REVERSE, is t for reverse search, nil for forward.
The variable `dabbrev-limit' controls the maximum search region size.
Third argument IGNORE-CASE non-nil means treat case as insignificant while
looking for a match and when comparing with previous matches.  Also if
that's non-nil and the match is found at the beginning of a sentence
and is in lower case except for the initial then it is converted to
all lower case for return.

Table of expansions already seen is examined in buffer
`dabbrev--last-table' so that only distinct possibilities are found
by dabbrev-re-expand.

Returns the expansion found, or nil if not found.
Leaves point at the location of the start of the expansion."
  (save-match-data
    (let ((pattern1 (concat (regexp-quote abbrev)
			    "\\(" dabbrev--abbrev-char-regexp "\\)"))
	  (pattern2 (concat (regexp-quote abbrev)
			   "\\(\\(" dabbrev--abbrev-char-regexp "\\)+\\)"))
	  ;; This makes it possible to find matches in minibuffer prompts
	  ;; even when they are "inviolable".
	  (inhibit-point-motion-hooks t)
	  found-string result)
      ;; Limited search.
      (save-restriction
	(and dabbrev-limit
	     (narrow-to-region
              dabbrev--last-expansion-location
              (+ (point) (if reverse (- dabbrev-limit) dabbrev-limit))))
	;;--------------------------------
	;; Look for a distinct expansion, using dabbrev--last-table.
	;;--------------------------------
	(while (and (not found-string)
		    (if reverse
			(re-search-backward pattern1 nil t)
		      (re-search-forward pattern1 nil t)))
	  (goto-char (match-beginning 0))
	  ;; In case we matched in the middle of a word,
	  ;; back up to start of word and verify we still match.
	  (dabbrev--goto-start-of-abbrev)

	  (if (not (looking-at pattern1))
	      nil
	    ;; We have a truly valid match.  Find the end.
	    (re-search-forward pattern2)
	    (setq found-string (match-string-no-properties 0))
	    (setq result found-string)
	    (and ignore-case (setq found-string (downcase found-string)))
	    ;; Ignore this match if it's already in the table.
	    (if (dabbrev-filter-elements
		 table-string dabbrev--last-table
		 (string= found-string table-string))
		(setq found-string nil)))
	  ;; Prepare to continue searching.
	  (goto-char (if reverse (match-beginning 0) (match-end 0))))
	;; If we found something, use it.
	(when found-string
	  ;; Put it into `dabbrev--last-table'
	  ;; and return it (either downcased, or as is).
	  (setq dabbrev--last-table
		(cons found-string dabbrev--last-table))
	  result)))))
```

this is bottom of the call chain, the caller methods above manages last matched buffer, last matched location, case-sensitivity, customizations and many other features.

three args:

1.  abbrev: the substring you just typed for completion.
2.  reverse: t if search backward, nil for forward.
3.  ignore-case: case-sensitivity search or not.

<a id="orgd2fd589"></a>

## dabbrev&#x2013;abbrev-char-regexp

[link](https://ftp.gnu.org/old-gnu/Manuals/emacs-20.7/html_chapter/emacs_28.html)

> The variable dabbrev-abbrev-char-regexp, if non-nil, controls which characters are considered part of a word, for dynamic expansion purposes. The regular expression must match just one character, never two or more. The same regular expression also determines which characters are part of an expansion. The value nil has a special meaning: abbreviations are made of word characters, but expansions are made of word and symbol characters.

- `pattern1` is the abbreb followed by one more allowed char.
- `pattern2` is the abbrev followed by 1 or more allowed chars.

<a id="org2c8743a"></a>

## dabbrev-limit

dabbrev-limit controled the range to search, from current `point` to `(point - dabbrev-limit)` if search backward. Here the library use `save-restriction` and `narrow-to-region` pattern to avoid search beyond the region and later restore the original narrowing.

<a id="org8cc3c3b"></a>

## search

the search is performed by `re-search-forward` or `re-search-backward` with `pattern1`.

1.  goto the beginning of matched string.
2.  back up to the start of the word if necessary.
3.  if the string starting from current point matches `pattern1`.
4.  `(re-search-forward pattern2)` to find the end of the match.
5.  get the string without text properties.

<a id="org8ea7b55"></a>

## filter matches

if the current match was offered before, we don&rsquo;t want to offer it again, b/c we know the user didn&rsquo;t want that match. That&rsquo;s why there is var `dabbrev--last-table` that keeps the previous matches in this matching session.

`dabbrev-filter-elements` is a macro that filters apply `condition` to each `element` in the `list`, and return the matched elements in a list.

the body of the defmacro is straightforward and as expected, it loops the list and keep the rest in `dabbrev-tail`, apply the condition/predicate to the first elements of the list. If truthy, puts at the beginning of `dabbrev-result`, and finally returnsreversed `dabbrev-result`.

```elisp
;; if CONDITION evaluates non-nil.
(defmacro dabbrev-filter-elements (element list condition)
  `(let (dabbrev-result dabbrev-tail ,element)
    (setq dabbrev-tail ,list)
    (while dabbrev-tail
      (setq ,element (car dabbrev-tail))
      (if ,condition
          (setq dabbrev-result (cons ,element dabbrev-result)))
      (setq dabbrev-tail (cdr dabbrev-tail)))
    (nreverse dabbrev-result)))
```

this `dabbrev-filter-elements` returns a non-empty list, i.e. truthy, set found-string to nil and continue the search, otherwise, puts `found-string` into `dabbrev--last-table` for furture filtering.

<a id="orga9ad0ae"></a>

# remember the last search

b/c `dabbrev-expand` offers only one candidate at a time, so to avoid repeated wasted work, it has a few variables that &ldquo;remembers&rdquo; the last matched buffer and location.

- `dabbrev-search-these-buffers-only` controls which buffers is allowed to provide match candidates.
- `dabbrev--last-abbreviation` records last string it tries to expand.
- `dabbrev--last-expansion` records last expansion it offered.
- `dabbrev--last-direction` records the direction last time.

and more [here](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/dabbrev.el?h=fa65c044f2ebe666467166075c1507a8d0e1347f#n292) .

<a id="orgf3cc5dc"></a>

# search from other buffers and friend buffers

```elisp
;; The list of remaining buffers with the same mode as current buffer.
(defvar dabbrev--friend-buffer-list nil)

;; The buffer we looked in last, not counting the current buffer.
(defvar dabbrev--last-buffer nil)

;; The buffer we found the expansion last time.
(defvar dabbrev--last-buffer-found nil)
```

`dabbrev-expand` has a few var to remember the last buffer it found the candidate.

if the `dabbrev--last-buffer` is truthy, continue search from that buffer.

in `dabbrev-expand`, it sets `dabbrev--last-buffer-found` for the last buffer where it found the last match.

```elisp
(defun dabbrev-expand (arg)
  "Expand previous word \"dynamically\".
...
		  (if dabbrev--last-buffer
		      (set-buffer dabbrev--last-buffer))
...
      (if (not (or (eq dabbrev--last-buffer dabbrev--last-buffer-found)
		   (minibuffer-window-active-p (selected-window))))
	  (progn
	    (message "Expansion found in `%s'"
		     (buffer-name dabbrev--last-buffer))
	    (setq dabbrev--last-buffer-found dabbrev--last-buffer))

```

in `dabbrev--find-expansion` it contines from the last buffer if `dabbrev--last-buffer` is set.

When current buffer fail to produce a candidate, it creates a `dabbrev--friend-buffer-list` and pop the first one as the `dabbrev--last-buffer`.

```elisp
(defun dabbrev--find-expansion (abbrev direction ignore-case)
...
(save-excursion
    ;; If we were scanning something other than the current buffer,
    ;; continue scanning there.
    (when dabbrev--last-buffer
      (set-buffer dabbrev--last-buffer))
...
(progn
       (setq dabbrev--last-direction -1)
       (unless dabbrev--last-buffer
	 ;; If we have just now begun to search other buffers,
	 ;; determine which other buffers we should check.
	 ;; Put that list in dabbrev--friend-buffer-list.
	 (unless dabbrev--friend-buffer-list
           (setq dabbrev--friend-buffer-list
                 (dabbrev--make-friend-buffer-list))
           (setq dabbrev--progress-reporter
                 (make-progress-reporter
                  "Scanning for dabbrevs..."
                  (- (length dabbrev--friend-buffer-list)) 0 0 1 1.5))))
       ;; Walk through the buffers till we find a match.
       (let (expansion)
	 (while (and (not expansion) dabbrev--friend-buffer-list)
	   (setq dabbrev--last-buffer (pop dabbrev--friend-buffer-list))
	   (set-buffer dabbrev--last-buffer)
           (progress-reporter-update dabbrev--progress-reporter
                                     (- (length dabbrev--friend-buffer-list)))
	   (setq dabbrev--last-expansion-location (point-min))
	   (setq expansion (dabbrev--try-find abbrev nil 1 ignore-case)))
	 (progress-reporter-done dabbrev--progress-reporter)
	 expansion)))))
```

<a id="org6435527"></a>

# find all candidates at once.

dabbrev-expand finds one candidate at a time, user has to cycle through and determine if the offered candidate is the wanted one or try the next. It&rsquo;s sometimes easier to show all the candidates in a minibuffer and continue type to narrow down until just one left. `dabbrev-completion` is for this. [link](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/dabbrev.el?h=fa65c044f2ebe666467166075c1507a8d0e1347f#n372).

```elisp
(defun dabbrev-completion (&optional arg)
  "Completion on current word.
Like \\[dabbrev-expand] but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument ARG, it searches all buffers accepted by the
function pointed out by `dabbrev-friend-buffer-function' to find the
completions.

If the prefix argument is 16 (which comes from \\[universal-argument] \\[universal-argument]),
then it searches *all* buffers."
```

dabbrev-completion command body does the following things:

1.  define a `table` var that&rsquo;s a lambda function
2.  setup `dabbrev--check-other-buffers` to t if arg is present
3.  setup `dabbrev--check-all-buffers` to t if arg is numerical and is 16.
4.  call `completion-in-region`.

<a id="org5800421"></a>

## completion-in-region

This function is defined in `minibuffer.el`.

> (completion-in-region START END COLLECTION &optional PREDICATE) Documentation Complete the text between START and END using COLLECTION.
>
> Point needs to be somewhere between START and END. PREDICATE (a function called with no arguments) says when to exit. This calls the function that completion-in-region-function specifies (passing the same four arguments that it received) to do the work, and returns whatever it does. The return value should be nil if there was no valid completion, else t.
>
> [basic completion](https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Completion.html): You can also use a function as collection. Then the function is solely responsible for performing completion; try-completion returns whatever this function returns. The function is called with three arguments: string, predicate and nil (the third argument is so that the same function can be used in all-completions and do the appropriate thing in either case). See Programmed Completion.

In this case, the table var is a function. so we know s - string, p - predicate, a - aciton.

<a id="orgdc67c0d"></a>

## table lambda:

```elisp
         (table
          (lambda (s p a)
            (if (eq a 'metadata)
                `(metadata (cycle-sort-function . ,#'identity)
                           (category . dabbrev))
              (when (eq list 'uninitialized)
                (save-excursion
                  ;;--------------------------------
                  ;; New abbreviation to expand.
                  ;;--------------------------------
                  (setq dabbrev--last-abbreviation abbrev)
                  ;; Find all expansion
                  (let ((completion-list
                         (dabbrev--find-all-expansions abbrev ignore-case-p))
                        (completion-ignore-case ignore-case-p))
                    (or (consp completion-list)
                        (user-error "No dynamic expansion for \"%s\" found%s"
                                    abbrev
                                    (if dabbrev--check-other-buffers
                                        "" " in this-buffer")))
                    (setq list
                          (cond
                           ((not (and ignore-case-p dabbrev-case-replace))
                            completion-list)
                           ((string= abbrev (upcase abbrev))
                            (mapcar #'upcase completion-list))
                           ((string= (substring abbrev 0 1)
                                     (upcase (substring abbrev 0 1)))
                            (mapcar #'capitalize completion-list))
                           (t
                            (mapcar #'downcase completion-list)))))))
              (complete-with-action a list s p))))

```

when action is `'metadata`, this is our chance to determine the behavior of the minibuffer candidate:

- `cycle-sort-function` function to sort entries when cycling. i am not 100% sure what this does, and the doc is vague. I guess when user comes to the bottom of the match minibuffer and still tries go down, this function is trigger with the current list and the returned list is used to update the minibuffer content. In our case, it simply goes back to the head of the minibuffer and start again.
- `category` a symbol describing what kind of text the completion function is trying to complete. This category is shown in the minibuffer so you know where this candidate is computed and offered.
- more controls [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Programmed-Completion.html)

<a id="org3c3ae1d"></a>

### when action is not &rsquo;metadata

we need to do real search and provide a list of candidate for minibuffer.

1.  find candidates

    `dabbrev--find-all-expansions` finds all candidates for user-typed substring `abbrev`.

2.  case handling

    the last part of the function handles case to match the user-typed substring case.

    - if `dabbrev-case-replace` is falsey or `ignore-case-p` is truthy, return the candidates list as is
    - else if user-typed substring is UPCASE, upcase the candidates list
    - else if user-typed subsrtring is capitalized, capitalized the candidates list
    - else downcase the candidates list.

<a id="orgb5003af"></a>

### dabbrev&#x2013;find-all-expansions

```elisp
(defun dabbrev--find-all-expansions (abbrev ignore-case)
  "Return a list of all possible expansions of ABBREV.
If IGNORE-CASE is non-nil, accept matches which differ in case."
  (let ((all-expansions nil)
	expansion)
    (save-excursion
      (goto-char (point-min))
      (while (setq expansion (dabbrev--find-expansion abbrev -1 ignore-case))
	(setq all-expansions (cons expansion all-expansions))))
    all-expansions))
```

go to the beginning of the buffer, continue to call `dabbrev--find-expansion` until all matches are found and inserted in `all-expansions`.

<a id="org986b3fe"></a>

### complete-with-action

complete-with-action is a helper function that handle the rest of the API so code author can focus on handling metadata, [see here](https://with-emacs.com/posts/tutorials/customize-completion-at-point/).

Defined in `minibuffer.el`

> Signature (complete-with-action ACTION COLLECTION STRING PREDICATE)
>
> Documentation Perform completion according to ACTION.
>
> STRING, COLLECTION and PREDICATE are used as in try-completion.
>
> If COLLECTION is a function, it will be called directly to perform completion, no matter what ACTION is.
>
> If ACTION is metadata or a list where the first element is boundaries, return nil. If ACTION is nil, this function works like try-completion; if it is t, this function works like all-completion; and any other value makes it work like test-completion.
