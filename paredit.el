;;; -*- mode: emacs-lisp -*-

;;;;;; paredit: Parenthesis editing minor mode
;;;;;; Version 6

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
;;; them as gracefully as I'd like.  There is one small but deeply
;;; fundamental problem in this model of pretending to be a structure
;;; editor on top of what is really a text editor, though: escapes, in
;;; character or string literals, which can throw off the parsing of
;;; balanced delimiters.  The only way I've come up to deal with this
;;; with any semblance of grace is to insert only completed escape
;;; characters, by rebinding backslash to query for the character to
;;; escape, and for the rest of the code to assume only completed
;;; escapes.  This is a crock, but an unfortunately necessary one.
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

(defconst paredit-version 6)

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
  "Keymap for paredit minor mode.")

(define-minor-mode paredit-mode
  "Minor mode for pseudo-structurally editing Lisp code."
  :lighter " Paredit")



;;; ----------------
;;; Basic editing commands

(defun paredit-open-list ()
  "Inserts a balanced parenthesis pair.
If in string, comment, or character literal, inserts a single opening
parenthesis."
  (interactive)
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p)
          (paredit-in-char-p))
      (insert "(")
    (insert-parentheses 0)))

(defun paredit-close-list ()
  "Moves past one closing parenthesis and reindents.
If in a string, comment, or character literal, inserts a single closing
parenthesis."
  (interactive)
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p)
          (paredit-in-char-p))
      (insert ")")
    (move-past-close-and-reindent)
    ;; Reindent not only the current line but, if there is a valid
    ;; S-expression following the point, that too.
    (condition-case nil (indent-sexp)
      (scan-error nil)))
  (if blink-matching-paren
      (condition-case nil
          (save-excursion
            (backward-sexp)
            (forward-sexp)
            (blink-matching-open))
        (error nil))))

(defun paredit-doublequote ()
  "Inserts a pair of double-quotes.
Inside a comment, inserts a literal double-quote.
At the end of a string, moves past the closing double-quote.
In the middle of a string, inserts a backslash-escaped double-quote."
  (interactive)
  (cond ((paredit-in-string-p)
         (if (eq (cdr (paredit-string-start+end-points))
                 (point))
             (forward-char)
           (insert ?\\ ?\" )))
        ((paredit-in-comment-p)
         (insert ?\" ))
        ;; I'm not sure what to do about the character literal case.
        ((not (paredit-in-char-p))
         (let ((insert-space
                (lambda (endp)
                  (if (and (not (if endp (eobp) (bobp)))
                           (memq (char-syntax
                                  (if endp (char-after) (char-before)))
                                 (list ?w ?_
                                       (char-syntax ?\" )
                                       (char-syntax ?\( )
                                       (if endp    ;++ sloppy
                                           nil
                                         (char-syntax ?\) )))))
                      (insert " ")))))
           (funcall insert-space nil)
           (insert ?\" )
           (save-excursion
             (insert ?\" )
             (funcall insert-space t))))))

(defun paredit-backslash ()
  "Inserts a backslash followed by a character to escape."
  (interactive)
  ;; This funny conditional is necessary because PAREDIT-IN-COMMENT-P
  ;; assumes that PAREDIT-IN-STRING-P already returned false; otherwise
  ;; it may break.
  (insert ?\\ )
  (if (or (paredit-in-string-p)
          (not (paredit-in-comment-p)))
      (let ((delp t))
        (unwind-protect (setq delp
                              (call-interactively #'paredit-escape))
          (if delp (backward-delete-char 1))))))

;;; This auxiliary interactive function returns true if the backslash
;;; should be deleted and false if not.

(defun paredit-escape (char)
  ;; I'm too lazy to figure out how to do this without a separate
  ;; interactive function.
  (interactive "cCharacter to escape: ")
  (if (eq char 127)                     ; luser made a typo and deleted
      t
    (insert char)
    nil))

(defun paredit-newline ()
  "Inserts a newline and indents it.
This is like `newline-and-indent', but it not only indents the line
that the point is on but also the S-expression following the point, if
there is one."
  (interactive)
  (if (paredit-in-char-p)
      (forward-char))
  (newline-and-indent)
  ;; Indent the following S-expression, but don't signal an error if
  ;; there's only a closing parenthesis after the point.
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
          ((eq (char-after) ?\\ )       ; Escape -- delete both chars.
           (delete-char 2))
          ((paredit-in-char-p)          ; ditto
           (backward-delete-char 1)
           (delete-char 1))
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
  (cond ((paredit-in-string-escape-p)
         (backward-delete-char 1)
         (delete-char 1))
        ((eq (char-after) ?\\ )
         (delete-char 2))
        (t
         (let ((start+end (paredit-string-start+end-points)))
           (cond ((not (eq (point) (cdr start+end)))
                  ;; Delete the char if it's not the close-quote.
                  (delete-char 1))
                 ((eq (1- (point)) (car start+end))
                  ;; If it is the close-quote, delete only if we're also
                  ;; right past the open-quote (i.e. it's empty), and
                  ;; then delete both quotes.  Otherwise we refuse to
                  ;; delete it.
                  (backward-delete-char 1)
                  (delete-char 1)))))))

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
      (backward-delete-char 1)
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
  (cond ((paredit-in-string-escape-p)
         (backward-delete-char 1)
         (delete-char 1))
        ((and (not (eq (char-before) ?\"))
              ;; Delete a whole string escape -- but first make sure we
              ;; don't run backwards out the front end of the string.
              (save-excursion (backward-char)
                              (paredit-in-string-escape-p)))
         (backward-delete-char 2))
        (t
         (let ((start+end (paredit-string-start+end-points)))
           (cond ((not (eq (1- (point)) (car start+end)))
                  ;; Delete the char if it's not the open-quote.
                  ;; Delete twice if it's an escaped character.
                  (backward-delete-char 1)
                  (if (paredit-in-string-escape-p)
                      (backward-delete-char 1)))
                 ((eq (point) (cdr start+end))
                  ;; If it is the open-quote, delete only if we're also
                  ;; right past the close-quote (i.e. it's empty), and
                  ;; then delete both quotes.  Otherwise we refuse to
                  ;; delete it.
                  (backward-delete-char 1)
                  (delete-char 1)))))))

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
             ;++ This is a crock: we don't want to catch an incomplete
             ;++ escape sequence.
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

;;; This shouldn't be here.

(defmacro ignore-errors (&rest body)
  `(condition-case nil (progn ,@body)
     (error nil)))

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
      (ignore-errors                    ; Go to the end of the last
        (paredit-forward-and-indent n)) ;   S-expression,
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
      (ignore-errors (backward-sexp n)) ; Go back to where we want to
      (skip-chars-backward " \t\n")     ;   insert the delimiter.
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
      (ignore-errors (backward-sexp n))
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
      (ignore-errors (paredit-forward-and-indent n))
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
      ;; The third T argument to SEARCH-FORWARD says to return NIL,
      ;; not to signal an error, if no match is found.
      (setq res (search-forward ";" orig t))
      (while (and res
                  (or (paredit-in-string-p)
                      (prog2 (backward-char)
                          (paredit-in-char-p)
                        (forward-char))))
        (forward-char)
        (setq res (search-forward ";" orig t)))
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
