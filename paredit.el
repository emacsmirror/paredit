;;; -*- mode: emacs-lisp -*-

;;;;;; paredit: Parenthesis editing minor mode
;;;;;; Version 10

;;; This code is written by Taylor Campbell (except where explicitly
;;; noted) and placed in the Public Domain.  All warranties are
;;; disclaimed.

;;; Add this to your .emacs after adding paredit.el to /path/to/elisp/:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp/")
;;;   (autoload 'enable-paredit-mode "paredit"
;;;     "Turns on pseudo-structural editing of Lisp code."
;;;     t)
;;;   (add-hook '...-mode-hook 'enable-paredit-mode)
;;;
;;; Usually the ... will be lisp or scheme or both.  Alternatively, you
;;; can manually turn on this mode with M-x enable-paredit-mode and
;;; turn it off with M-x disable-paredit-mode.
;;;
;;; This is written for GNU Emacs.  It is known not to work in XEmacs.
;;; The author wrote it with GNU Emacs 22.0.50, but it should work in
;;; earlier versions as well.  An alternative set of keybindings is
;;; available in PAREDIT-TERMINAL-MODE that works in Emacs under Unix
;;; terminals with the -nw option (implied or otherwise).
;;;
;;; This mode changes the keybindings for (, ), and ", most notably;
;;; if you really, really want a literal one of those, use C-q.
;;;
;;; This is only lightly tested; some of it may not work as well as one
;;; might expect.  Comments, in particular, are not handled with as
;;; much grace as I'd like, but I'm not sure quite yet how to handle
;;; them as gracefully as I'd like.  (Block comments are not handled at
;;; all, only line comments.)
;;;
;;; There is one small but deeply fundamental problem in this model of
;;; pretending to be a structure editor on top of what is really a text
;;; editor, though: escapes, in character or string literals, which can
;;; throw off the parsing of balanced delimiters.  The only way I've
;;; come up to deal with this with any semblance of grace is to insert
;;; only completed escape characters, by rebinding backslash to query
;;; for the character to escape, and for the rest of the code to assume
;;; only completed escapes.  This is a kludge, but an unfortunately
;;; necessary one.
;;;
;;; Even with this kludge, it's still not perfect.  The code must
;;; assume that all backslashes are involved in completed escapes, but
;;; it's still possible to introduce an incomplete escape -- e.g., just
;;; put the point after a backslash and insert any character.  Or,
;;; rather, don't do that.  The rebound (, ), & " keys refuse to insert
;;; themselves thus, but that's a crock, too.  If you want to rewrite a
;;; character literal, first delete it and then type backslash again.
;;;
;;; Automatic reindentation is performed as locally as possible, to
;;; ensure that Emacs does not interfere with custom indentation used
;;; elsewhere in some S-expression.  It is performed only by the
;;; advanced S-expression frobnication commands, and only on the forms
;;; that were immediately operated upon (& their subforms).
;;;
;;; This code is written for clarity, not efficiency.  S-expressions
;;; are frequently walked over redundantly.  If you have problems with
;;; some of the commands taking too long to execute, tell me, but first
;;; make sure that what you're doing is reasonable: it is stylistically
;;; bad to have huge, long, hideously nested code anyway.

;;; This assumes Unix-style LF line endings.

(defconst paredit-version 10)



;;; Minor mode definition

;;; Pretend that this were organized in the natural manner, with the
;;; most important bit first -- the minor mode definitions --, and then
;;; the keymaps.  DEFINE-MINOR-MODE doesn't seem to like this, however.

(defvar paredit-mode-map
  (let ((keymap (make-sparse-keymap)))

    (define-key keymap "("          'paredit-open-list)
    (define-key keymap ")"          'paredit-close-list-and-newline)
    (define-key keymap (kbd "M-)")  'paredit-close-list)
    (define-key keymap (kbd "M-\"") 'paredit-close-string-and-newline)
    (define-key keymap "\""         'paredit-doublequote)
    (define-key keymap "\\"         'paredit-backslash)

    ;; This defies ordinary conventions, but I believe it is justified
    ;; and more convenient this way, to have RET be fancy and C-j
    ;; insert a vanilla newline.  You can always change this in your
    ;; .emacs if you want the conventional configuration, however.
    (define-key keymap (kbd "RET") 'paredit-newline)
    (define-key keymap (kbd "C-j") 'newline)
    (define-key keymap (kbd "C-d") 'paredit-forward-delete)
    (define-key keymap (kbd "<deletechar>") 'paredit-forward-delete)
    (define-key keymap (kbd "DEL") 'paredit-backward-delete)
    (define-key keymap (kbd "C-k") 'paredit-kill)

    (define-key keymap (kbd "C-M-f") 'paredit-forward)
    (define-key keymap (kbd "C-M-b") 'paredit-backward)
    (define-key keymap (kbd "C-c C-M-l") 'paredit-recentre-on-sexp)

    ;; The default keybindings in this area are:
    ;;   C-up             forward-paragraph
    ;;   C-down           backward-paragraph
    ;;   C-M-up           backward-up-list
    ;;   C-M-down         down-list
    ;;   C-right M-right  forward-word
    ;;   C-left  M-left   backward-word
    ;; This all seems rather inconsistent to me.  I'm not worried about
    ;; overriding C-up & C-down here, because paragraph commands are
    ;; not very useful in Lisp code, and C-left & C-right, because they
    ;; already have aliases with meta instead of control.
    ;;
    ;; Chosen here is for C-up to go up a list backward (because that
    ;; will usually mean going up a line, as one might logically expect
    ;; with the up key), C-down to go down a list (forward, for a
    ;; similar reason), an added meta to go in the other direction.
    (define-key keymap (kbd "<C-up>")     'backward-up-list)
    (define-key keymap (kbd "<C-down>")   'down-list)
    (define-key keymap (kbd "<C-M-up>")   'up-list)
    (define-key keymap (kbd "<C-M-down>") 'backward-down-list)
    (define-key keymap (kbd "<C-right>")  'paredit-forward)
    (define-key keymap (kbd "<C-left>")   'paredit-backward)

    (define-key keymap (kbd "M-(")  'paredit-wrap-sexp)
    (define-key keymap (kbd "M-/")  'paredit-splice-sexp)
    (define-key keymap (kbd "M-\\") 'paredit-join-sexps)

    (define-key keymap (kbd "C-)") 'paredit-forward-slurp-sexp)
    (define-key keymap (kbd "C-}") 'paredit-forward-barf-sexp)
    (define-key keymap (kbd "C-(") 'paredit-backward-slurp-sexp)
    (define-key keymap (kbd "C-{") 'paredit-backward-barf-sexp)
    
    keymap)
  "Keymap for the paredit minor mode.
Does not work in `emacs -nw' running under Unix terminals, only in
Emacs with a window system.")

(defvar paredit-terminal-mode-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap paredit-mode-map)

    ;; Bizarre terminal sequences for M-right, M-left, C-M-right, &
    ;; C-M-left, respectively.
    (define-key keymap (kbd "ESC <right>") 'paredit-forward-slurp-sexp)
    (define-key keymap (kbd "ESC <left>")  'paredit-forward-barf-sexp)
    (define-key keymap (kbd "ESC M-O d")   'paredit-backward-slurp-sexp)
    (define-key keymap (kbd "ESC M-O c")   'paredit-backward-barf-sexp)

    ;; Terminal sequences for C-up, C-down, C-M-left, & C-M-down,
    ;; respectively.  (These are the same as in the regular mode map,
    ;; except that Emacs doesn't recognize the correlation between what
    ;; the terminal sends it and what KBD gives for "<C-up>" &c.)
    (define-key keymap (kbd "ESC O a")     'backward-up-list)
    (define-key keymap (kbd "ESC O b")     'down-list)
    (define-key keymap (kbd "ESC M-O a")   'up-list)
    (define-key keymap (kbd "ESC M-O b")   'backward-down-list)

    keymap)
  "Keymap for the paredit minor mode.
Works in `emacs -nw' running under Unix terminals.")

;++ Two separate minor modes here is a bit of a kludge.  It would be
;++ nice if DEFINE-MINOR-MODE had an option for dynamically choosing a
;++ keymap when the mode is enabled.

(define-minor-mode paredit-mode
  "Minor mode for pseudo-structurally editing Lisp code.
Uses keybindings that will not work under a Unix terminal; see
`paredit-terminal-mode' for an alternative set of keybindings that will
work in `emacs -nw' running under a Unix terminal.

\\{paredit-mode-map}"
  :lighter " Paredit")

(define-minor-mode paredit-terminal-mode
  "Minor mode for pseudo-structurally editing Lisp code.
Uses alternative keybindings that work in `emacs -nw' running under
Unix terminals.

\\{paredit-terminal-mode-map}"
  :lighter " Paredit(nw)")

(defun enable-paredit-mode ()
  "Turns on pseudo-structural editing of Lisp code.
Uses `paredit-terminal-mode' if `window-system' is nil and
`paredit-mode' if not."
  (interactive)
  (if window-system
      (paredit-mode 1)
      (paredit-terminal-mode 1)))

(defun disable-paredit-mode ()
  "Turns off pseudo-structural editing of Lisp code.
Disables both `paredit-mode' and `paredit-terminal-mode'."
  (interactive)
  (paredit-mode -1)
  (paredit-terminal-mode -1))



;;; ----------------
;;; Basic editing commands

(defun paredit-open-list (&optional n)
  "Inserts a balanced parenthesis pair.
With a prefix argument N, puts the closing parentheses after N
S-expressions forward.
If in string or comment, inserts a single opening parenthesis.
If in a character literal, does nothing.  This prevents accidentally
changing what was in the character literal to a meaningful delimiter
unintentionally."
  (interactive "P")
  (cond ((or (paredit-in-string-p)
             (paredit-in-comment-p))
         (insert "("))
        ((not (paredit-in-char-p))
         (insert-parentheses (or n 0)))))

(defun paredit-close-list ()
  "Moves past one closing parenthesis and reindents.
If in a string or comment, inserts a single closing parenthesis.
If in a character literal, does nothing.  This prevents accidentally
changing what was in the character literal to a meaningful delimiter
unintentionally."
  (interactive)
  (cond ((or (paredit-in-string-p)
             (paredit-in-comment-p))
         (insert ")"))
        ((not (paredit-in-char-p))
         (paredit-move-past-close-and-reindent)
         (paredit-blink-paren-match nil))))

(defun paredit-close-list-and-newline ()
  "Moves past one closing delimiter, adds a newline, and reindents."
  (interactive)
  (cond ((or (paredit-in-string-p)
             (paredit-in-comment-p))
         (insert ")"))
        (t (if (paredit-in-char-p) (forward-char))
           (paredit-move-past-close-and-reindent)
           (insert ?\n )
           (lisp-indent-line)
           (condition-case () (indent-sexp)
             (scan-error nil))
           (paredit-blink-paren-match t))))

(defun paredit-move-past-close-and-reindent ()
  "Moves one character past the next closing parenthesis.
Deletes extraneous whitespace before the closing parenthesis.  Comments
are not deleted, however; if there is a comment between the point and
the next closing parenthesis, the closing parenthesis is moved to the
line after the comment and indented appropriately."
  (interactive)
  (let ((orig (point)))
    (up-list)
    (if (catch 'return                  ; This CATCH returns T if it
          (while t                      ; should delete leading spaces
            (save-excursion             ; and NIL if not.
              (let ((before-paren (1- (point))))
                (back-to-indentation)
                (cond ((not (eq (point) before-paren))
                       ;; Can't call PAREDIT-DELETE-LEADING-WHITESPACE
                       ;; here -- we must return from SAVE-EXCURSION
                       ;; first.
                       (throw 'return t))
                      ((save-excursion (previous-line 1)
                                       (end-of-line)
                                       (paredit-in-comment-p))
                       ;; Moving the closing parenthesis any further
                       ;; would put it into a comment, so we just
                       ;; indent the closing parenthesis where it is
                       ;; and abort the loop, telling its continuation
                       ;; that no leading whitespace should be deleted.
                       (lisp-indent-line)
                       (throw 'return nil))
                      (t (delete-indentation)))))))
        (paredit-delete-leading-whitespace))))

(defun paredit-delete-leading-whitespace ()
  ;; This assumes that we're on the closing parenthesis already.
  (save-excursion
    (backward-char)
    (while (let ((syn (char-syntax (char-before))))
             (and (or (eq syn ?\ ) (eq syn ?-))     ; whitespace syntax
                  ;; The above line is a perfect example of why the
                  ;; following test is necessary.
                  (not (paredit-in-char-p (1- (point))))))
      (backward-delete-char 1))))

(defun paredit-blink-paren-match (absolutely-p)
  (if (or absolutely-p blink-matching-paren)
      (condition-case ()
          (save-excursion
            (backward-sexp)
            (forward-sexp)
            (let ((blink-matching-paren-on-screen t))
              (blink-matching-open)))
        (scan-error nil))))

(defun paredit-close-string-and-newline ()
  "Moves to the end of the string, inserts a newline, and indents.
If not in a string, acts as `paredit-doublequote'."
  (interactive)
  (if (not (paredit-in-string-p))
      (paredit-doublequote)
    (let ((start+end (paredit-string-start+end-points)))
      (goto-char (1+ (cdr start+end)))
      (insert ?\n )
      (lisp-indent-line)
      (condition-case () (indent-sexp)
        (scan-error nil)))))

(defun paredit-doublequote ()
  "Inserts a pair of double-quotes.
Inside a comment, inserts a literal double-quote.
At the end of a string, moves past the closing double-quote.
In the middle of a string, inserts a backslash-escaped double-quote.
If in a character literal, does nothing.  This prevents accidentally
changing a what was in the character literal to a meaningful delimiter
unintentionally."
  (interactive)
  (cond ((paredit-in-string-p)
         (if (eq (cdr (paredit-string-start+end-points))
                 (point))
             (forward-char)             ; We're on the closing quote.
           (insert ?\\ ?\" )))
        ((paredit-in-comment-p)
         (insert ?\" ))
        ((not (paredit-in-char-p))
         (let ((insert-space
                (lambda (endp delim-syn)
                  (if (and (not (if endp (eobp) (bobp)))
                           (memq (char-syntax
                                  (if endp (char-after) (char-before)))
                                 (list ?w ?_
                                       (char-syntax ?\" )
                                       delim-syn)))
                      (insert " ")))))
           (funcall insert-space nil ?\) )
           (insert ?\" )
           (save-excursion
             (insert ?\" )
             (funcall insert-space t ?\( ))))))

(defun paredit-backslash ()
  "Inserts a backslash followed by a character to escape."
  (interactive)
  ;; This funny conditional is necessary because PAREDIT-IN-COMMENT-P
  ;; assumes that PAREDIT-IN-STRING-P already returned false; otherwise
  ;; it may give erroneous answers.
  (insert ?\\ )
  (if (or (paredit-in-string-p)
          (not (paredit-in-comment-p)))
      (let ((delp t))
        (unwind-protect (setq delp
                              (call-interactively #'paredit-escape))
          ;; We need this in an UNWIND-PROTECT so that the backlash is
          ;; left in there *only* if PAREDIT-ESCAPE return NIL normally
          ;; -- in any other case, such as the user hitting C-g or an
          ;; error occurring, we must delete the backslash to avoid
          ;; leaving a dangling escape.
          (if delp (backward-delete-char 1))))))

;;; This auxiliary interactive function returns true if the backslash
;;; should be deleted and false if not.

(defun paredit-escape (char)
  ;; I'm too lazy to figure out how to do this without a separate
  ;; interactive function.
  (interactive "cEscaping character...")
  (if (eq char 127)                     ; The backslash was a typo, so
      t                                 ; the luser wants to delete it.
    (insert char)                       ; (Is there a better way to
    nil))                               ; express the rubout char?
                                        ; ?\^? works, but ugh...)
(defun paredit-newline ()
  "Inserts a newline and indents it.
This is like `newline-and-indent', but it not only indents the line
that the point is on but also the S-expression following the point, if
there is one.
Moves forward one character first if on an escaped character."
  (interactive)
  (if (paredit-in-char-p)
      (forward-char))
  (newline-and-indent)
  ;; Indent the following S-expression, but don't signal an error if
  ;; there's only a closing parenthesis after the point, not a full
  ;; S-expression.
  (condition-case () (indent-sexp)
    (scan-error nil)))

(defun paredit-forward-delete (&optional arg)
  "Deletes a character forward or moves forward over a delimiter.
If on an opening S-expression delimiter, moves forward into the
S-expression.
If on a closing S-expression delimiter, refuses to delete unless the
S-expression is empty, in which case the whole S-expression is deleted.
With a prefix argument, simply deletes a character forward, without
regard for delimiter balancing."
  (interactive "P")
  (cond (arg (delete-char 1))        ; I'd pass the arg if I knew how.
        ((paredit-in-string-p)
         (paredit-forward-delete-in-string))
        ((paredit-in-comment-p)
         ;++ What to do here?  This could move a partial S-expression
         ;++ into a comment and thereby invalidate the file's form,
         ;++ or move random text out of a comment.
         (delete-char 1))
        ((paredit-in-char-p)            ; Escape -- delete both chars.
         (backward-delete-char 1)
         (delete-char 1))
        ((eq (char-after) ?\\ )         ; ditto
         (delete-char 2))
        ((or (eq (char-after) ?\( )
             (eq (char-after) ?\" ))
         (forward-char))
        ((and (eq (char-before) ?\( )
              (not (paredit-in-char-p (1- (point))))
              (eq (char-after)  ?\) ))
         (backward-delete-char 1)
         (delete-char 1))
        ;; Just delete a single character, if it's not a closing
        ;; parenthesis.  (The character literal case is already
        ;; handled by now.)
        ((not (eq (char-after) ?\) ))
         (delete-char 1))))

(defun paredit-forward-delete-in-string ()
  (let ((start+end (paredit-string-start+end-points)))
    (cond ((not (eq (point) (cdr start+end)))
           ;; If it's not the close-quote, it's safe to delete.  But
           ;; first handle the case that we're in a string escape.
           (cond ((paredit-in-string-escape-p)
                  ;; We're right after the backslash, so backward
                  ;; delete it before deleting the escaped character.
                  (backward-delete-char 1))
                 ((eq (char-after) ?\\ )
                  ;; If we're not in a string escape, but we are on a
                  ;; backslash, it must start the escape for the next
                  ;; character, so delete the backslash before deleting
                  ;; the next character.
                  (delete-char 1)))
           (delete-char 1))
          ((eq (1- (point)) (cdr start+end))
           ;; If it is the close-quote, delete only if we're also right
           ;; past the open-quote (i.e. it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (backward-delete-char 1)
           (delete-char 1)))))

(defun paredit-backward-delete (&optional arg)
  "Deletes a character backward or moves backward over a delimiter.
If on a closing S-expression delimiter, moves backward into the
S-expression.
If on an opening S-expression delimiter, refuses to delete unless the
S-expression is empty, in which case the whole S-expression is deleted.
With a prefix argument, simply deletes a character backward, without
regard for delimiter balancing."
  (interactive "P")
  (cond (arg (backward-delete-char 1))  ;++ should this untabify?
        ((paredit-in-string-p)
         (paredit-backward-delete-in-string))
        ((paredit-in-comment-p)
         (backward-delete-char 1))
        ((paredit-in-char-p)            ; Escape -- delete both chars.
         (backward-delete-char 1)
         (delete-char 1))
        ((paredit-in-char-p (1- (point)))
         (backward-delete-char 2))      ; ditto
        ((and (or (eq (char-before) ?\) )
                  (eq (char-before) ?\" ))
              (not (paredit-in-char-p (1- (point)))))
         (backward-char))
        ((and (eq (char-before) ?\( )
              (not (paredit-in-char-p (1- (point))))
              (eq (char-after)  ?\) ))
         (backward-delete-char 1)
         (delete-char 1))
        ;; Delete it, unless it's an opening parenthesis.  The case
        ;; of character literals is already handled by now.
        ((not (eq (char-before) ?\( ))
         (backward-delete-char-untabify 1))))

(defun paredit-backward-delete-in-string ()
  (let ((start+end (paredit-string-start+end-points)))
    (cond ((not (eq (1- (point)) (car start+end)))
           ;; If it's not the open-quote, it's safe to delete.
           (if (paredit-in-string-escape-p)
               ;; If we're on a string escape, since we're about to
               ;; delete the backslash, we must first delete the
               ;; escaped char.
               (delete-char 1))
           (backward-delete-char 1)
           (if (paredit-in-string-escape-p)
               ;; If, after deleting a character, we find ourselves in
               ;; a string escape, we must have deleted the escaped
               ;; character, and the backslash is behind the point, so
               ;; backward delete it.
               (backward-delete-char 1)))
          ((eq (point) (cdr start+end))
           ;; If it is the open-quote, delete only if we're also right
           ;; past the close-quote (i.e. it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (backward-delete-char 1)
           (delete-char 1)))))

(defun paredit-kill (&optional arg)
  "Kills a line as if with `kill-line', but respecting delimiters.
In a string, acts exactly as `kill-line' but will not kill past the
closing string delimiter.
On a line with no S-expressions on it starting after the point or
within a comment, acts exactly as `kill-line'.
Otherwise, kills all S-expressions that start after the point."
  (interactive "P")
  (cond (arg (kill-line))
        ((paredit-in-string-p)
         (paredit-kill-line-in-string))
        ((or (paredit-in-comment-p)
             (save-excursion
               (skip-chars-forward " \t\n" (point-at-eol))
               (or (eq (char-after) ?\; )
                   (eolp))))
         ;** Be careful about trailing backslashes.
         (kill-line))
        (t (paredit-kill-sexps-on-line))))

(defun paredit-kill-line-in-string ()
  (if (save-excursion (skip-chars-forward " \t\n" (point-at-eol))
                      (eolp))
      (kill-line)
    (save-excursion
      ;; Be careful not to split an escape sequence.
      (if (paredit-in-string-escape-p)
          (backward-char))
      (let ((beg (point)))
        (while (not (or (eolp)
                        (eq (char-after) ?\" )))
          (forward-char)
          ;; Skip past escaped characters.
          (if (eq (char-before) ?\\ )
              (forward-char)))
        (kill-region beg (point))))))

(defun paredit-kill-sexps-on-line ()
  (if (paredit-in-char-p)               ; Move past the \ and prefix.
      (backward-char 2))                ; (# in Scheme/CL, ? in elisp)
  (let ((beg (point))
        (eol (point-at-eol))
        (end-of-list-p nil))
    ;; Move to the end of the last S-expression that started on this
    ;; line, or to the closing delimiter if the last S-expression in
    ;; this list is on the line.
    (catch 'return
      (while (save-excursion
               (condition-case ()
                   (forward-sexp)
                 ;++ I wrote here:
                 ;++      ;++ THIS IS BROKEN -- FIX
                 ;++ But now I don't remember what was broken and needs
                 ;++ fixing.  This whole thing, notably END-OF-LIST-P,
                 ;++ was a crock to fix a corner case that I also don't
                 ;++ remember now...
                 (scan-error
                  (up-list)
                  (setq end-of-list-p (eq (point-at-eol) eol))
                  (throw 'return nil)))
               (and (not (eobp))
                    (progn (backward-sexp)
                           (eq (point-at-eol) eol))))
        (forward-sexp)))
    ;; If we got to the end of the list and it's on the same line,
    ;; move backward past the closing delimiter before killing.  (This
    ;; allows something like killing the whitespace in (    ).)
    (if end-of-list-p (progn (up-list) (backward-char)))
    (if (not kill-whole-line)
        (kill-region beg
                     ;; If all of the S-expressions were on one line,
                     ;; i.e. we're still on that line after moving past
                     ;; the last one, kill the whole line, including
                     ;; any comments; otherwise just kill to the end of
                     ;; the last S-expression we found.  Be sure,
                     ;; though, not to kill any closing parentheses.
                     (if (and (not end-of-list-p)
                              (eq (point-at-eol) eol))
                         eol
                         (point)))
      (kill-region beg
                   (or (save-excursion  ; Delete indentation forward...
                         (skip-chars-forward " \n\t")
                         (and (not (eq (char-after) ?\; ))
                              (point)))
                       ;; ...or just use the point past the newline, if
                       ;; we encounter a comment.
                       (point-at-eol)))
      (cond ((save-excursion (skip-chars-backward " \n\t"
                                                  (point-at-bol))
                             (bolp))
             ;; Nothing but indentation before the point, so indent it.
             (lisp-indent-line))
            ;; If there is something before the point, make sure we
            ;; don't join things that shouldn't be joined.
            ((let ((syn-before (char-syntax (char-before)))
                   (syn-after  (char-syntax (char-after))))
               (or (and (eq syn-before ?\) )        ; Separate opposing
                        (eq syn-after  ?\( ))       ;   parentheses,
                   (and (eq syn-before ?\" )        ; string delimiter
                        (eq syn-after  ?\" ))       ;   pairs,
                   (and (memq syn-before '(?_ ?w))  ; or word or symbol
                        (memq syn-after  '(?_ ?w))) ;   constituents.
                   ))
             (insert " "))))))



;;; ----------------
;;; Cursor and screen movement

(defun paredit-forward ()
  "Moves forward an S-expression.
If there are any closing delimiters impeding such movement, first moves
forward up lists until there are no more."
  (interactive)
  (catch 'return
    (while t
      (condition-case ()
          (progn (forward-sexp)
                 (throw 'return nil))
        (scan-error (up-list))))))

(defun paredit-backward ()
  "Moves backward an S-expression.
If there are any opening delimiters impeding such movement, first moves
backward up lists until there are no more."
  (interactive)
  (catch 'return
    (while t
      (condition-case ()
          (progn (backward-sexp)
                 (throw 'return nil))
        (scan-error (backward-up-list))))))

;;; Why is this not in lisp.el?

(defun backward-down-list (&optional arg)
  "Move backward and descend into one level of parentheses.
With ARG, do this that many times.
A negative argument means move forward but still descend a level."
  (interactive "p")
  (down-list (- (or arg 1))))

;;; Thanks to Marco Baringer for suggesting & writing this function.

(defun paredit-recentre-on-sexp (&optional n)
  "Recentres the screen on the S-expression following the point.
With a prefix argument N, encompasses all N S-expressions forward."
  (interactive "P")
  (save-excursion
    (forward-sexp n)
    (let ((end-point (point)))
      (backward-sexp n)
      (let* ((start-point (point))
             (start-line (count-lines (point-min) (point)))
             (lines-on-sexps (count-lines start-point end-point)))
        (goto-line (+ start-line (/ lines-on-sexps 2)))
        (recenter)))))



;;; ----------------
;;; Wrappage, splicage, & joinage

(defun paredit-wrap-sexp (&optional n)
  "Wraps the following S-expression in a list.
If a prefix argument N is given, N S-expressions are contained in the
list.
Automatically indents the newly wrapped S-expression.
As a special case, if at the end of a list, will simply insert a pair
of parentheses, rather than insert a lone opening parenthesis and then
signal an error."
  (interactive "p")
  (condition-case ()
      (insert-parentheses (or n 1))
    (scan-error (insert ?\) )
                (backward-char)))
  (save-excursion (backward-up-list) (indent-sexp)))

;;; Thanks to Marco Baringer for the suggestion of a prefix argument
;;; for PAREDIT-SPLICE-SEXP.  (I, Taylor Campbell, however, still
;;; implemented it, in case any of you lawyer-folk get confused by the
;;; remark in the top of the file about explicitly noting code written
;;; by other people.)

(defun paredit-splice-sexp (&optional arg)
  "Splices the list the point is on by removing its delimiters.
With a prefix argument as in `C-u', deletes all S-expressions backward
in the current list before splicing all S-expressions forward into the
enclosing list.
With two prefix arguments as in `C-u C-u', deletes all S-expressions
forward in the current list before splicing all S-expressions backward
into the enclosing list.
With a numerical prefix argument N, deletes N S-expressions backward in
the current list before splicing the remaining S-expressions into the
enclosing list."
  (interactive "P")
  (save-excursion
    (if (and arg (not (eq arg 0)))
        (cond ((numberp arg)
               ;; Delete ARG S-expressions before/after the point by
               ;; saving the point, moving across them, and deleting
               ;; the region.
               (let ((saved (point)))
                 (condition-case ()
                     (backward-sexp arg)
                   (scan-error nil))
                 (if (< arg 0)
                     (delete-region saved (point))
                     (delete-region (point) saved))))
              ((consp arg)
               (let ((v (car arg)))
                 (if (= v 4)
                     ;; Move backward until we hit the open paren; then
                     ;; delete that selected region.
                     (let ((end (point)))
                       (condition-case ()
                           (while (not (bobp)) (backward-sexp))
                         (scan-error nil))
                       (delete-region (point) end))
                     ;; Move forward until we hit the close paren; then
                     ;; delete that selected region.
                     (let ((beg (point)))
                       (condition-case ()
                           (while (not (eobp)) (forward-sexp))
                         (scan-error nil))
                       (delete-region beg (point))))))
              (t (error "Bizarre prefix argument: %s" arg))))
    (backward-up-list)                  ; Go up to the beginning...
    (save-excursion
      (forward-sexp)                    ; Go forward an expression, to
      (backward-delete-char 1))         ;   delete the end delimiter.
    (delete-char 1)                     ; ...to delete the open char.
    (backward-up-list)                  ; Reindent, now that the
    (indent-sexp)))                     ;   structure has changed.

(defun paredit-join-sexps ()
  "Joins two adjacent S-expressions into one S-expression."
  (interactive)
  (save-excursion
    (backward-sexp)                     ; Go to the end of the
    (forward-sexp)                      ;   preceding expression.
    (backward-delete-char 1)            ; Delete the closing delimiter.
    (forward-sexp)                      ; Go to the start of the
    (backward-sexp)                     ;   following expression.
    (delete-char 1)                     ; Delete the opening delimiter.
    (backward-up-list)                  ; Reindent the list, now that
    (indent-sexp)))                     ;   its structure has changed.



;;; ----------------
;;; Slurpage & barfage

(defun paredit-forward-slurp-sexp ()
  "Adds the S-expression following the current list into that list by
moving the closing delimiter.
Automatically reindents the newly slurped S-expressions with respect to
their new enclosing form."
  (interactive)
  (save-excursion
    (up-list)                           ; Up to the end of the list to
    (let ((close (char-before)))        ;   save and delete the closing
      (backward-delete-char 1)          ;   delimiter.
      (catch 'return                    ; Go to the end of the desired
        (while t                        ;   S-expression, going up a
          (condition-case ()            ;   list if it's not in this,
              (progn (paredit-forward-and-indent)
                     (throw 'return nil))
            (scan-error (up-list)))))
      (insert close))))                 ; to insert that delimiter.

(defun paredit-forward-barf-sexp ()
  "Removes the last S-expression in the current list from that list by
moving the closing delimiter.
Automatically reindents all of the newly barfed S-expressions with
respect to their new enclosing form."
  (interactive)
  (save-excursion
    (up-list)                           ; Up to the end of the list to
    (let ((close (char-before)))        ;   save and delete the closing
      (backward-delete-char 1)          ;   delimiter.
      (condition-case ()                ; Go back to where we want to
          (backward-sexp)               ;   insert the delimiter.
        (scan-error nil))               ; Ignore scan errors, and
      (skip-chars-backward " \t\n")     ;   skip leading whitespace.
      (cond ((bobp)
             (message
              "Barfing all subexpressions with no open-paren?"))
            ((paredit-in-comment-p)     ; Don't put the close-paren in
             (newline-and-indent)))     ;   a comment.
      (insert close))
    ;; Reindent all of the newly barfed S-expressions.
    (paredit-forward-and-indent)))

(defun paredit-backward-slurp-sexp ()
  "Adds the S-expression preceding the current list into that list by
moving the closing delimiter.
Automatically reindents the whole form into which new S-expression was
slurped."
  (interactive)
  (save-excursion
    (backward-up-list)
    (let ((open (char-after)))
      (delete-char 1)
      (catch 'return
        (while t
          (condition-case ()
              (progn (backward-sexp)
                     (throw 'return nil))
            (scan-error (backward-up-list)))))
      (insert open))
    ;; Reindent the line at the beginning of wherever we inserted the
    ;; opening parenthesis, and then indent the whole S-expression.
    (backward-up-list)
    (lisp-indent-line)
    (indent-sexp)))

(defun paredit-backward-barf-sexp ()
  "Removes the first S-expression in the current list from that list by
moving the closing delimiter.
Automatically reindents the barfed S-expression and the form from which
it was barfed."
  (interactive)
  ;; SAVE-EXCURSION here does the wrong thing, but manually saving and
  ;; restoring the point does the right thing.  Here's an example of
  ;; how SAVE-EXCURSION breaks:
  ;;   (foo|)   C-{
  ;;   foo|()
  ;; It should be:
  ;;   foo(|)
  (let ((beg (point)))
    (unwind-protect
        (progn
          (backward-up-list)
          (let ((open (char-after)))
            (delete-char 1)
            (condition-case () (paredit-forward-and-indent)
              (scan-error nil))
            (while (progn (skip-chars-forward " \t\n")
                          (eq (char-after) ?\; ))
              (goto-char (1+ (point-at-eol))))
            (if (eobp)
                (message
                 "Barfing all subexpressions with no close-paren?"))
            (insert open))
          (backward-up-list)
          (lisp-indent-line)
          (indent-sexp))
      (goto-char beg))))



;;; ----------------
;;; Several utility functions

(defun paredit-in-string-p ()
  "True if the point is within a double-quote-delimited string."
  (save-excursion
    (let ((orig (point)))
      (beginning-of-defun)
      ;; Item 3 of the list PARSE-PARTIAL-SEXP returns is true if the
      ;; point at the second argument is in a string, otherwise false.
      (nth 3 (parse-partial-sexp (point) orig)))))

(defun paredit-string-start+end-points ()
  "Returns a cons of the points of the open and quotes of this string.
This assumes that `paredit-in-string-p' has already returned true, i.e.
that the point is already within a string."
  (save-excursion
    (let ((orig (point)))
      (beginning-of-defun)
      (let* ((state (parse-partial-sexp (point) orig))
             (start (nth 8 state)))
        (goto-char start)
        (forward-sexp)
        (cons start (1- (point)))))))

(defun paredit-in-string-escape-p ()
  "True if the point is on a character escaped by a backslash.
This is true only if the character is preceded by an odd number of
backslashes.
This assumes that `paredit-in-string-p' has already returned true."
  (let ((oddp nil))
    (save-excursion
      (while (eq (char-before) ?\\ )
        (setq oddp (not oddp))
        (backward-char)))
    oddp))

(defun paredit-in-comment-p ()
  "True if the point is within a Lisp line comment.
This assumes that `paredit-in-string-p' has already returned false."
  ;++ Make this work on block comments?
  (save-excursion
    (let ((orig (point)) (res nil))
      (goto-char (point-at-bol))
      ;; The second T argument to SEARCH-FORWARD says to return NIL,
      ;; not to signal an error, if no match is found.
      (while (progn (setq res (search-forward ";" orig t))
                    (and res
                         (or (paredit-in-string-p)
                             (paredit-in-char-p (1- (point))))))
        (forward-char))
      (and res (<= res orig)))))

(defun paredit-in-char-p (&optional arg)
  "True if the point is immediately after a character literal.
A preceding backslash, not preceded by another backslash, is considered
a character literal prefix.  (This works for elisp, Common Lisp, and
Scheme.)
Assumes that `paredit-in-string-p' is false, so that it need not handle
long sequences of preceding backslashes in string escapes.  (This
assumes some other leading character token -- ? in elisp, # in Scheme
and Common Lisp.)"
  (let ((arg (or arg (point))))
    (and (eq (char-before arg) ?\\ )
         (not (eq (char-before (1- arg))
                  ?\\ )))))

(defun paredit-forward-and-indent ()
  "Move forward an S-expression, indenting it fully with both
`lisp-indent-line' and then `indent-sexp'."
  (forward-sexp)                        ; Go forward, and then find the
  (save-excursion                       ;   beginning of this next
    (backward-sexp)                     ;   S-expression.
    (lisp-indent-line)                  ; Indent its opening line, and
    (indent-sexp)))                     ;   the rest of it.



(provide 'paredit)
