;;; -*- mode: emacs-lisp -*-

;;;;;; paredit: Parenthesis editing minor mode
;;;;;; Version 1

;;; Taylor Campbell wrote this code; he places it in the public domain.

;;; Add this to your .emacs after adding paredit.el to /path/to/elisp/:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp/")
;;;   (require 'paredit)
;;;   (add-hook '...-mode-hook (lambda () (paredit-mode 1)))
;;;
;;; Usually the ... will be lisp or scheme or both.

;;; This assumes Unix-style LF line endings.

(defconst paredit-version 1)

(defvar paredit-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "(" 'paredit-open-list)
    (define-key keymap ")" 'paredit-close-list)
    (define-key keymap "\"" 'paredit-doublequote)
    (define-key keymap (kbd "M-\"") 'paredit-insert-doublequote)

    (define-key keymap (kbd "RET") 'newline-and-indent)
    (define-key keymap (kbd "DEL") 'paredit-backspace)
    
    (define-key keymap (kbd "C-k") 'paredit-kill)

    ;; In case something broke and you really, really need to insert a
    ;; literal parenthesis.  Don't use these often.
    (define-key keymap (kbd "C-M-(")
      (lambda () (interactive) (insert "(")))
    (define-key keymap (kbd "C-M-)")
      (lambda () (interactive) (insert ")")))

    ;; C-up & C-down are by default useless paragraph commands, while
    ;; C-M-up & C-M-down are BACKWARD-UP-LIST & BACKWARD-DOWN-LIST.
    (define-key keymap (kbd "<C-up>") 'up-list)
    (define-key keymap (kbd "<C-down>") 'down-list)

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
  (if (or (paredit-in-comment-p)
          (paredit-in-string-p)
          (eq (char-before) ?\\ ))
      (insert "(")
    (insert-parentheses 0)))

(defun paredit-close-list ()
  "Moves past one closing parenthesis and reindents.
If in a string, comment, or character literal, inserts a single closing
parenthesis."
  (interactive)
  (if (or (paredit-in-comment-p)
          (paredit-in-string-p)
          (eq (char-before) ?\\ ))
      (insert ")")
    (move-past-close-and-reindent)))

(defun paredit-doublequote ()
  "Inserts a pair of double-quotes.
Inside a comment or character literal, inserts a literal double-quote.
Inside a string, moves to the end of the string."
  (interactive)
  (cond ((or (paredit-in-comment-p)
             (eq (char-before) ?\\ ))
         ;; Special case for when we're on character literals, just to
         ;; be convenient.
         (insert "\""))
        ((paredit-in-string-p)
         (while (not (eq (char-after) ?\" ))
           (forward-char)
           (if (eq (char-after) ?\\ )   ; Skip escaped characters.
               (forward-char)))
         (forward-char))
        (t (insert-pair 0 ?\" ?\"))))

(defun paredit-insert-doublequote (&optional n)
  "Inserts a single literal double-quote.
Inside a string, inserts an escaped double-quote: \\\"
Outside of a string, comment, or character literal, displays a message
to the user and inserts a single literal double-quote nevertheless."
  (interactive "p")
  (let ((string
         (cond ((or (paredit-in-comment-p)
                    (eq (char-before) ?\\ ))
                "\"")                   ; plain doublequote
               ((paredit-in-string-p)
                "\\\"")                 ; escaped doublequote
               (t (message "Inserting naked doublequote%s..."
                           (if (> n 1) "s" ""))
                  "\""))))              ; plain doublequote
    (while (< 0 n)
      (insert string)
      (setq n (1- n)))))

(defun paredit-backspace ()
  "Deletes a character backward or moves backward over a delimiter.
If at the start of an empty string literal, deletes the whole string,
including both delimiters.  If at the start of a non-empty string
literal, moves back outside of the string literal.  If anywhere else in
a string literal, deletes a single character.
If on a closing parenthesis, moves backward one character.
If on an opening parenthesis followed by a closing parenthesis, deletes
both parentheses.
If on any other opening parenthesis, does nothing.
Anywhere else, deletes a character backward."
  (interactive)
  (cond ((paredit-in-comment-p)
         (backward-delete-char 1))
        ((paredit-in-string-p)
         (if (and (eq (char-after) ?\" )
                  (eq (char-before) ?\" )
                  (not (eq (char-before (1- (point)))
                           ?\\ )))
             (progn (backward-char)
                    (kill-sexp))
           (backward-delete-char 1)))
        ((and (or (eq (char-before) ?\) )
                  (eq (char-before) ?\" ))
              (not (eq (char-before (1- (point)))
                       ?\\ )))
         (backward-char))
        ;++ This should test more thoroughly, e.g. for (   ).
        ((and (eq (char-before) ?\( )
              (eq (char-after)  ?\) ))
         (backward-char)
         (kill-sexp))
        ;; Delete it, unless it's an opening parenthesis not preceded
        ;; by a backslash (i.e. not a character literal).
        ((or (not (eq (char-before) ?\( ))
             (eq (char-before (1- (point)))
                 ?\\ ))
         (backward-delete-char-untabify 1))))

(defun paredit-kill ()
  "Kills a line or S-expression.
If an S-expression starts on the same line as the point, kills that
S-expression; otherwise, behaves as `kill-line'."
  (interactive)
  (cond ((or (eq (char-after) ?\n )
             (paredit-in-comment-p)
             (save-excursion
               (skip-chars-forward " \t\n" (point-at-eol))
               (or (eq (point) (point-at-eol))
                   (eq (char-after) ?\; ))))
         (kill-line))
        ((paredit-in-string-p)
         (if (eq (char-after) ?\n )
             ;; Delete the newline only if we're at the end of the
             ;; line.  (The ordinary Emacs behaviour is to do this also
             ;; if there's only whitespace following, but I hate that
             ;; behaviour.)
             (delete-char)
           (while (not (or (eq (char-after) ?\n )
                           (eq (char-after) ?\" )))
             (cond ((eq (char-after) ?\\ )
                    (delete-char)
                    ;; The one after the backslash is escaped, so eat
                    ;; it (most importantly if it's a doublequote),
                    ;; unless it's a newline.
                    (if (not (eq (char-after (1+ point))
                                 ?\n ))
                        (delete-char)))
                   (t (delete-char))))))
        (t (kill-sexp))))



;;; ----------------
;;; Wrappage, splicage, & joinage

(defun forward-wrap-sexp (&optional n)
  "Wraps the following S-expression in a list.
If a prefix argument N is given, N S-expressions are contained in the
list."
  (interactive "p")
  (insert-parentheses (or n 1))
  (save-excursion (beginning-of-defun)
                  (indent-sexp)))

(defun backward-wrap-sexp (&optional n)
  "Wraps the preceding S-expression in a list.
If a prefix argument N is given, N S-expressions are contained in the
list."
  (interactive "p")
  (insert-parentheses (- (or n 1)))
  (save-excursion (beginning-of-defun)
                  (indent-sexp)))

(defun splice-sexp ()
  "Splices the list the point is on by removing its delimiters."
  (interactive)
  (save-excursion
    (backward-up-list)                  ; Go up to the beginning...
    (save-excursion
      (forward-sexp)                    ; Go forward an expression, to
      (backward-delete-char 1))         ;   delete the end delimiter.
    (delete-char 1)                     ; ...to delete the open char.
    (beginning-of-defun)                ; Reindent, now that the
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
    (beginning-of-defun)                ; Reindent the whole defun, now
    (indent-sexp)))                     ;   that its structure changed.



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
current list."
  (interactive "p")
  (save-excursion
    (up-list)                           ; Up to the end of the list to
    (let ((close (char-before)))        ;   save and delete the closing
      (backward-delete-char 1)          ;   delimiter.
      (ignore-errors (forward-sexp n))  ; Go to the end of the last exp
      (insert close))                   ;   to insert that delimiter.
    (beginning-of-defun)                ; Reindent the form, now that
    (indent-sexp)))                     ;   the structure has changed.

(defun forward-barf-sexp (&optional n)
  "Removes the last S-expression in the current list from that list by
moving the closing delimiter.
If a prefix argument N is given, the last N S-expressions are barfed
out of the current list."
  (interactive "p")
  (save-excursion
    (up-list)                           ; Up to the end of the list to
    (let ((close (char-before)))        ;   save and delete the closing
      (backward-delete-char 1)          ;   delimiter.
      (ignore-errors (backward-sexp n)) ; Go back to where we want to
      (skip-chars-backward " \t\n")     ;   insert the delimiter.
      (if (eq (point) (point-min))
          (error "Barfing all subexpressions with no open-paren?")
        (insert close)))
    (beginning-of-defun)                ; Reindent: structure has
    (indent-sexp)))                     ;   changed.

(defun backward-slurp-sexp (&optional n)
  "Adds the S-expression preceding the current list into that list by
moving the closing delimiter.
If a prefix argument N is given, N S-expressions are slurped into the
current list."
  (interactive "p")
  (save-excursion
    (backward-up-list)
    (let ((open (char-after)))
      (delete-char 1)
      (ignore-errors (backward-sexp n))
      (insert open))
    (beginning-of-defun)
    (indent-sexp)))

(defun backward-barf-sexp (&optional n)
  "Removes the first S-expression in the current list from that list by
moving the closing delimiter.
If a prefix argument N is given, the first N S-expressions are barfed
out of the current list."
  (interactive "p")
  (save-excursion
    (backward-up-list)
    (let ((open (char-after)))
      (delete-char 1)
      (ignore-errors (forward-sexp n))
      (skip-chars-forward " \t\n")
      (if (eq (point) (point-max))
          (error "Barfing all subexpressions with no close-paren?")
        (insert open)))
    (beginning-of-defun)
    (indent-sexp)))



;;; ----------------
;;; Two utility functions

(defun paredit-in-comment-p ()
  "True if the point is within a Lisp line comment."
  ;++ Make this work on block comments?
  (save-excursion
    ;; The third T argument to SEARCH-BACKWARD says to return NIL,
    ;; not to signal an error, if no match is found.
    (and (search-backward ";" (point-at-bol) t)
         (not (eq (char-before) ?\\ ))
         t)))

;;; Taken roughly from thingatpt.el.

(defun paredit-in-string-p ()
  "True if the point is within a double-quote-delimited string."
  (let ((orig (point)))
    (save-excursion
      (beginning-of-defun)
      (eq (nth 3 (parse-partial-sexp (point) orig))
          ?\" ))))



(provide 'paredit)
