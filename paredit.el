;;; -*- mode: emacs-lisp -*-

;;;;;; paredit: Parenthesis editing minor mode
;;;;;; Version 7

;;; Taylor Campbell wrote this code; he places it in the public domain.

;;; Add this to your .emacs after adding paredit.el to /path/to/elisp/:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp/")
;;;   (require 'paredit)
;;;   (add-hook '...-mode-hook (lambda () (paredit-mode 1)))
;;;
;;; Usually the ... will be lisp or scheme or both.  Alternatively, you
;;; can manually toggle this mode with M-x paredit-mode.
;;;
;;; This mode changes the keybindings for (, ), and ", most notably;
;;; if you really, really want a literal one of those, use C-q.
;;;
;;; This is only lightly tested; some of it may not work as well as one
;;; might expect.  Comments, in particular, are not handled with as
;;; much grace as I'd like, but I'm not sure quite yet how to handle
;;; them as gracefully as I'd like.
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

(defconst paredit-version 7)

(defvar paredit-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "(" 'paredit-open-list)
    (define-key keymap ")" 'paredit-close-list)
    (define-key keymap "\"" 'paredit-doublequote)
    (define-key keymap "\\" 'paredit-backslash)

    (define-key keymap (kbd "C-j") 'paredit-newline)
    (define-key keymap (kbd "C-d") 'paredit-forward-delete)
    (define-key keymap (kbd "DEL") 'paredit-backward-delete)
    
    (define-key keymap (kbd "C-k") 'paredit-kill)

    ;; C-up & C-down are by default useless paragraph commands, while
    ;; C-M-up & C-M-down are BACKWARD-UP-LIST & BACKWARD-DOWN-LIST.
    ;; C-left & C-right are by default word movement commands, but as
    ;; are M-left & M-right, so I think it's OK to override them.
    (define-key keymap (kbd "<C-up>") 'up-list)
    (define-key keymap (kbd "<C-down>") 'down-list)
    (define-key keymap (kbd "<C-right>") 'forward-sexp)
    (define-key keymap (kbd "<C-left>") 'backward-sexp)

    (define-key keymap (kbd "M-(") 'forward-wrap-sexp)
    (define-key keymap (kbd "M-)") 'backward-wrap-sexp)
    (define-key keymap (kbd "M-/") 'splice-sexp)
    (define-key keymap (kbd "M-\\") 'join-sexps)

    (define-key keymap (kbd "C-)") 'forward-slurp-sexp)
    (define-key keymap (kbd "C-}") 'forward-barf-sexp)
    (define-key keymap (kbd "C-(") 'backward-slurp-sexp)
    (define-key keymap (kbd "C-{") 'backward-barf-sexp)
    
    keymap)
  "Keymap for the paredit minor mode.")

(define-minor-mode paredit-mode
  "Minor mode for pseudo-structurally editing Lisp code."
  :lighter " Paredit")



;;; ----------------
;;; Basic editing commands

(defun paredit-open-list ()
  "Inserts a balanced parenthesis pair.
If in string or comment, inserts a single opening parenthesis.
If in a character literal, does nothing.  This prevents accidentally
changing what was in the character literal to a meaningful delimiter
unintentionally."
  (interactive)
  (cond ((or (paredit-in-string-p)
             (paredit-in-comment-p))
         (insert "("))
        ((not (paredit-in-char-p))
         (insert-parentheses 0))))

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
         (if blink-matching-paren
             (condition-case nil
                 (save-excursion
                   (backward-sexp)
                   (forward-sexp)
                   (blink-matching-open))
               (scan-error nil))))))

(defun paredit-move-past-close-and-reindent ()
  "Moves one character past the next closing parenthesis.
Deletes extraneous whitespace before the closing parenthesis.  Comments
are not deleted, however; if there is a comment between the point and
the next closing parenthesis, the closing parenthesis is moved to the
line after the comment and indented appropriately."
  (interactive)
  (let ((orig (point)))
    (up-list)
    (if (catch 'exit                    ; This CATCH returns T if it
          (while t                      ; should delete leading spaces
            (save-excursion             ; and NIL if not.
              (let ((before-paren (1- (point))))
                (back-to-indentation)
                (cond ((not (eq (point) before-paren))
                       ;; Can't call PAREDIT-DELETE-LEADING-WHITESPACE
                       ;; here -- we must return from SAVE-EXCURSION
                       ;; first.
                       (throw 'exit t))
                      ((save-excursion (previous-line)
                                       (end-of-line)
                                       (paredit-in-comment-p))
                       ;; Moving the closing parenthesis any further
                       ;; would put it into a comment, so we just
                       ;; indent the closing parenthesis where it is
                       ;; and abort the loop, telling its continuation
                       ;; that no leading whitespace should be deleted.
                       (lisp-indent-line)
                       (throw 'exit nil))
                      (t (delete-indentation)))))))
        (paredit-delete-leading-whitespace)))
  (condition-case nil (indent-sexp)
    (scan-error nil)))

(defun paredit-delete-leading-whitespace ()
  ;; This assumes that we're on the closing parenthesis already.
  (save-excursion
    (backward-char)
    (while (let ((syn (char-syntax (char-before))))
             (and (or (eq syn ?\ ) (eq syn ?-)) ; whitespace syntax
                  ;; The above line is a perfect example of why the
                  ;; following test is necessary.
                  (not (paredit-in-char-p (1- (point))))))
      (backward-delete-char 1))))

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
  (if (eq char 127)                     ; The luser made a typo and hit
      t                                 ; DEL to delete the backslash.
    (insert char)
    nil))

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
  (condition-case nil (indent-sexp)
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
  (if arg                       ; I'd pass the argument to DELETE-CHAR,
      (delete-char 1)           ; but I don't know how to do it right.
    (cond ((paredit-in-string-p)
           (paredit-forward-delete-in-string))
          ((paredit-in-comment-p)
           ;++ What to do here?  This could move a partial S-expression
           ;++ into a comment and thereby invalidate the file's form,
           ;++ or move random text out of a comment.
           (delete-char 1))
          ((paredit-in-char-p)          ; Escape -- delete both chars.
           (backward-delete-char 1)
           (delete-char 1))
          ((eq (char-after) ?\\ )       ; ditto
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
           (delete-char 1)))))

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
  (if arg
      (backward-delete-char 1)          ;++ should this untabify?
    (cond ((paredit-in-string-p)
           (paredit-backward-delete-in-string))
          ((paredit-in-comment-p)
           (backward-delete-char 1))
          ((paredit-in-char-p)          ; Escape -- delete both chars.
           (backward-delete-char 1)
           (delete-char 1))
          ((paredit-in-char-p (1- (point)))
           (backward-delete-char 2))    ; ditto
          ((and (or (eq (char-before) ?\) )
                    (eq (char-before) ?\" ))
                (not (paredit-in-char-p (1- (point)))))
           (backward-char))
          ;++ This should test more thoroughly, e.g. for (   ).
          ((and (eq (char-before) ?\( )
                (not (paredit-in-char-p (1- (point))))
                (eq (char-after)  ?\) ))
           (backward-delete-char 1)
           (delete-char 1))
          ;; Delete it, unless it's an opening parenthesis.  The case
          ;; of character literals is already handled by now.
          ((not (eq (char-before) ?\( ))
           (backward-delete-char-untabify 1)))))

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

(defun paredit-kill ()
  "Kills a line or S-expression.
If an S-expression starts on the same line as the point, kills that
S-expression; otherwise, behaves as `kill-line', except won't kill a
closing string delimiter."
  (interactive)
  (cond ((paredit-in-string-p)
         (paredit-kill-in-string))
        ((or (eq (char-after) ?\n )
             (paredit-in-comment-p)
             (save-excursion
               (skip-chars-forward " \t\n" (point-at-eol))
               (or (eq (point) (point-at-eol))
                   (eq (char-after) ?\; ))))
         (if (eq (char-before (point-at-eol))
                 ?\\ )
             ;++ This is a crock: we don't want to kill an incomplete
             ;++ escape sequence, so we include the newline.  This
             ;++ won't work on the last line of the buffer, however, if
             ;++ it is not followed by one empty line.
             (progn (kill-region (point) (1+ (point-at-eol)))
                    (insert ?\n ))
           (kill-line)))
        (t (kill-sexp))))

(defun paredit-kill-in-string ()
  (if (eq (char-after) ?\n )
      ;; Delete the newline only if we're at the end of the line.  (The
      ;; ordinary Emacs behaviour is to do this also if there's only
      ;; whitespace following, but I hate that behaviour.)
      (kill-region (point) (1+ (point)))
    ;; Skip ahead to the end of the line or the double-quote.  Kill
    ;; that region.
    (save-excursion
      ;; Make sure not to split an escaped character sequence.
      (if (paredit-in-string-escape-p)
          (backward-char))
      (let ((beg (point)))
        (while (not (memq (char-after) '(?\n ?\" )))
          (forward-char)
          ;; Skip past escaped characters.
          (if (eq (char-before) ?\\ )
              (forward-char)))
        (kill-region beg (point))))))



;;; ----------------
;;; Wrappage, splicage, & joinage

(defun forward-wrap-sexp (&optional n)
  "Wraps the following S-expression in a list.
If a prefix argument N is given, N S-expressions are contained in the
list.
Automatically indents the newly wrapped S-expression."
  (interactive "p")
  (insert-parentheses (or n 1))
  (save-excursion (backward-up-list) (indent-sexp)))

(defun backward-wrap-sexp (&optional n)
  "Wraps the preceding S-expression in a list.
If a prefix argument N is given, N S-expressions are contained in the
list.
Automatically indents the newly wrapped S-expression."
  (interactive "p")
  (insert-parentheses (- (or n 1)))
  (save-excursion (backward-up-list) (indent-sexp)))

(defun splice-sexp ()
  "Splices the list the point is on by removing its delimiters."
  (interactive)
  (save-excursion
    (backward-up-list)                  ; Go up to the beginning...
    (save-excursion
      (forward-sexp)                    ; Go forward an expression, to
      (backward-delete-char 1))         ;   delete the end delimiter.
    (delete-char 1)                     ; ...to delete the open char.
    (backward-up-list)                  ; Reindent, now that the
    (indent-sexp)))                     ;   structure has changed.

(defun join-sexps ()
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

(defun forward-slurp-sexp (&optional n)
  "Adds the S-expression following the current list into that list by
moving the closing delimiter.
If a prefix argument N is given, N S-expressions are slurped into the
current list.
Automatically reindents the newly slurped S-expressions with respect to
their new enclosing form."
  (interactive "p")
  (save-excursion
    (up-list)                           ; Up to the end of the list to
    (let ((close (char-before)))        ;   save and delete the closing
      (backward-delete-char 1)          ;   delimiter.
      (condition-case nil               ; Go to the end of the last
          (paredit-forward-and-indent n);   S-expression,
        (scan-error nil))               ;   (ignoring going too far)
      (insert close))))                 ; to insert that delimiter.

(defun forward-barf-sexp (&optional n)
  "Removes the last S-expression in the current list from that list by
moving the closing delimiter.
If a prefix argument N is given, the last N S-expressions are barfed
out of the current list.
Automatically reindents all of the newly barfed S-expressions with
respect to their new enclosing form."
  (interactive "p")
  (save-excursion
    (up-list)                           ; Up to the end of the list to
    (let ((close (char-before)))        ;   save and delete the closing
      (backward-delete-char 1)          ;   delimiter.
      (condition-case nil               ; Go back to where we want to
          (backward-sexp n)             ;   insert the delimiter.
        (scan-error nil))               ; Ignore scan errors, and
      (skip-chars-backward " \t\n")     ;   skip leading whitespace.
      (if (bobp)
          (message "Barfing all subexpressions with no open-paren?"))
      (insert close))
    ;; Reindent all of the newly barfed S-expressions.
    (paredit-forward-and-indent n)))

(defun backward-slurp-sexp (&optional n)
  "Adds the S-expression preceding the current list into that list by
moving the closing delimiter.
If a prefix argument N is given, N S-expressions are slurped into the
current list.
Automatically reindents the whole form into which new S-expressions
were slurped."
  (interactive "p")
  (save-excursion
    (backward-up-list)
    (let ((open (char-after)))
      (delete-char 1)
      (condition-case nil (backward-sexp n)
        (scan-error nil))
      (insert open))
    ;; Reindent the line at the beginning of wherever we inserted the
    ;; opening parenthesis, and then indent the whole S-expression.
    (backward-up-list)
    (lisp-indent-line)
    (indent-sexp)))

(defun backward-barf-sexp (&optional n)
  "Removes the first S-expression in the current list from that list by
moving the closing delimiter.
If a prefix argument N is given, the first N S-expressions are barfed
out of the current list.
Automatically reindents all of the barfed S-expressions and the form
from which they were barfed."
  (interactive "p")
  (save-excursion
    (backward-up-list)
    (let ((open (char-after)))
      (delete-char 1)
      (condition-case nil (paredit-forward-and-indent n)
        (scan-error nil))
      (skip-chars-forward " \t\n")      ;++ should handle comments
      (if (eobp)
          (message "Barfing all subexpressions with no close-paren?"))
      (insert open))
    (backward-up-list)
    (lisp-indent-line)
    (indent-sexp)))



;;; ----------------
;;; Several utility functions

(defun paredit-in-string-p ()
  "True if the point is within a double-quote-delimited string."
  (save-excursion
    (let ((orig (point)))
      (beginning-of-defun)
      ;; Item 3 of the list PARSE-PARTIAL-SEXP returns is the string
      ;; delimiter if the point at the second argument is in a string;
      ;; otherwise it's nil.
      (eq (nth 3 (parse-partial-sexp (point) orig))
          ?\" ))))

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

(defun paredit-forward-and-indent (n)
  "Move forward N S-expressions, indenting them all fully with
`lisp-indent-line' and then `indent-sexp'."
  (while (< 0 n)
    (forward-sexp)                      ; Find the beginning of this
    (backward-sexp)                     ;   next S-expression.
    (lisp-indent-line)                  ; Indent its opening line, and
    (indent-sexp)                       ;   the rest of it.
    (forward-sexp)                      ; Advance past it.
    (setq n (1- n))))



(provide 'paredit)
