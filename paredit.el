;;; -*- mode: emacs-lisp -*-

;;;;;; paredit: Parenthesis editing minor mode
;;;;;; Version 15

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
;;; The author wrote it with GNU Emacs 22.0.50; it may work in
;;; slightly earlier versions, but not older than 21 or so.  An
;;; alternative minor mode, PAREDIT-TERMINAL-MODE, is provided that
;;; works in Emacs under Unix terminals (i.e. `emacs -nw').
;;;
;;; This mode changes the keybindings for a number of simple keys,
;;; notably (, ), ", \, and ;.  The round bracket keys are defined to
;;; insert parenthesis pairs and move past the close, respectively;
;;; the double-quote key is multiplexed to do both, and also insert an
;;; escape if within a string; backslashes prompt the user for the
;;; next character to input, because a lone backslash can break
;;; structure inadvertently; and semicolons ensure that they do not
;;; accidentally comment valid structure.  (Use M-; to comment an
;;; expression.)  These all have their ordinary behaviour when inside
;;; comments, and, outside comments, if truly necessary, you can insert
;;; them literally with C-q.
;;;
;;; It also changes several standard editing keybindings including
;;; RET, C-j, C-d, DEL, & C-k.  RET & C-j are transposed from their
;;; usual paired meaning, where RET inserts a newline and C-j fancily
;;; adds a new line with indentation &c., but I find the transposition
;;; more convenient.  (You are free to change this, of course.)  C-d,
;;; DEL, & C-k are instrumented to respect the S-expression structure.
;;; You can, however, pass a prefix argument to them to get their
;;; usual behaviour if necessary; e.g., C-u C-k will kill the whole
;;; line, regardless of what S-expression structure there is on it.
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
;;;
;;; Questions, bug reports, comments, feature suggestions, &c., can be
;;; addressed to the author via mail on the host mumble.net to campbell
;;; or via IRC on irc.freenode.net in #emacs, #scheme, or #lisp, under
;;; the nick Riastradh.

;;; This assumes Unix-style LF line endings.

(defconst paredit-version 15)



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
    (define-key keymap ";"          'paredit-semicolon)
    (define-key keymap "M-;"        'paredit-comment-dwim)

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
    ;; Chosen here is for C-{up,down} to ascend a list, where C-up goes
    ;; in a direction usually upward with respect to the window (it
    ;; will often end up in a line above the current one), i.e. it will
    ;; use BACKWARD-UP-LIST, and where the converse is true of C-down.
    ;; C-M-{up,down}, then, descends in a similar manner.
    (define-key keymap (kbd "<C-up>")     'backward-up-list)
    (define-key keymap (kbd "<C-down>")   'up-list)
    (define-key keymap (kbd "<C-M-up>")   'backward-down-list)
    (define-key keymap (kbd "<C-M-down>") 'down-list)
    (define-key keymap (kbd "<C-right>")  'paredit-forward)
    (define-key keymap (kbd "<C-left>")   'paredit-backward)

    (define-key keymap (kbd "M-(")  'paredit-wrap-sexp)
    (define-key keymap (kbd "M-s")  'paredit-splice-sexp)
    (define-key keymap (kbd "<M-up>")
      'paredit-splice-sexp-killing-backward)
    (define-key keymap (kbd "<M-down>")
      'paredit-splice-sexp-killing-forward)
    (define-key keymap (kbd "M-r") 'paredit-raise-sexp)

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

    ;; These are the same as in the regular mode map, except that Emacs
    ;; doesn't recognize the correlation between what the terminal
    ;; sends it and what KBD gives for "<C-up>" &c.)
    (define-key keymap (kbd "ESC O a")   'backward-up-list)
    (define-key keymap (kbd "ESC O b")   'down-list)
    (define-key keymap (kbd "ESC M-O a") 'up-list)
    (define-key keymap (kbd "ESC M-O b") 'backward-down-list)
    (define-key keymap (kbd "ESC M-O c") 'paredit-forward)
    (define-key keymap (kbd "ESC M-O d") 'paredit-backward)
    (define-key keymap (kbd "ESC M-O A")
      'paredit-splice-sexp-killing-backward)
    (define-key keymap (kbd "ESC M-O B")
      'paredit-splice-sexp-killing-forward)

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
Disables whichever of `paredit-mode' and `paredit-terminal-mode' is
active in the current buffer, if either."
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
  "Moves past one closing delimiter, adds a newline, and reindents.
If there was a margin comment after the closing delimiter, preserves
the margin comment on the same line."
  (interactive)
  (cond ((or (paredit-in-string-p)
             (paredit-in-comment-p))
         (insert ")"))
        (t (if (paredit-in-char-p) (forward-char))
           (paredit-move-past-close-and-reindent)
           (let ((comment.point (paredit-find-comment-on-line)))
             (newline)
             (if comment.point
                 (save-excursion
                   (forward-line -1)
                   (end-of-line)
                   (indent-to (cdr comment.point))
                   (insert (car comment.point)))))
           (lisp-indent-line)
           (condition-case () (indent-sexp)
             (scan-error nil))
           (paredit-blink-paren-match t))))

(defun paredit-find-comment-on-line ()
  "Finds a margin comment on the current line.
If a comment exists, deletes the comment (including all leading
whitespace) and returns a cons whose car is the comment as a string
and whose cdr is the point of the comment's initial semicolon,
relative to the start of the line."
  (save-excursion
    (catch 'return
      (while t
        (if (search-forward ";" (point-at-eol) t)
            (if (not (or (paredit-in-string-p)
                         (paredit-in-char-p)))
                (let* ((start (progn (backward-char)  ;before semicolon
                                     (point)))
                       (comment (buffer-substring start
                                                  (point-at-eol))))
                  (paredit-skip-whitespace nil (point-at-bol))
                  (delete-region (point) (point-at-eol))
                  (throw 'return
                         (cons comment (- start (point-at-bol))))))
            (throw 'return nil))))))

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
                      ((save-excursion (forward-line -1)
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
      (newline)
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
                                 (list ?w ?_ ?\" delim-syn)))
                      (insert " ")))))
           (funcall insert-space nil ?\) )
           (insert ?\" )
           (save-excursion
             (insert ?\" )
             (funcall insert-space t ?\( ))))))

(defun paredit-backslash ()
  "Inserts a backslash followed by a character to escape."
  (interactive)
  (insert ?\\ )
  ;; This funny conditional is necessary because PAREDIT-IN-COMMENT-P
  ;; assumes that PAREDIT-IN-STRING-P already returned false; otherwise
  ;; it may give erroneous answers.
  (if (or (paredit-in-string-p)
          (not (paredit-in-comment-p)))
      (let ((delp t))
        (unwind-protect (setq delp
                              (call-interactively #'paredit-escape))
          ;; We need this in an UNWIND-PROTECT so that the backlash is
          ;; left in there *only* if PAREDIT-ESCAPE return NIL normally
          ;; -- in any other case, such as the user hitting C-g or an
          ;; error occurring, we must delete the backslash to avoid
          ;; leaving a dangling escape.  (This control structure is a
          ;; crock.
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
(defun paredit-semicolon (&optional arg)
  "Insert a comment beginning, moving other items on the line.
If in a string, comment, or character literal, or with a prefix
argument, inserts just a literal semicolon and does not move anything
to the next line."
  (interactive "P")
  (if (not (or (paredit-in-string-p)
               (paredit-in-comment-p)
               (paredit-in-char-p)
               arg
               ;; No more code on the line after the point.
               (save-excursion
                 (paredit-skip-whitespace t (point-at-eol))
                 (eq (point) (point-at-eol)))))
      ;; Don't use NEWLINE-AND-INDENT, because that will delete all of
      ;; the horizontal whitespace first, but we just want to move the
      ;; code following the point onto the next line while preserving
      ;; the point on this line.
      (save-excursion (newline) (lisp-indent-line)))
  (insert ";"))

(defun paredit-comment-dwim (&optional arg)
  "Calls the Lisp comment command you want (Do What I Mean).
This is like `comment-dwim', but it is specialized for Lisp editing.
If transient mark mode is enabled and the mark is active, comments or
uncomments the selected region, depending on whether it was entirely
commented not not already.
If there is already a comment on the current line, with no prefix
argument, indents to that comment; with a prefix argument, kills that
comment.
Otherwise, inserts a comment appropriate for the context and ensures
that any code following the comment is moved to the next line.
At the top level, where indentation is calculated to be at column 0,
this inserts a triple-semicolon comment; within code, where the
indentation is calculated to be non-zero, and there is either no code
on the line or code after the point on the line, inserts a double-
semicolon comment; and if the point is after all code on the line,
inserts a single-semicolon margin comment at `comment-column'."
  (interactive "*P")
  (comment-normalize-vars)
  (cond ((and mark-active transient-mark-mode)
         (comment-or-uncomment-region (region-beginning)
                                      (region-end)
                                      arg))
        ((paredit-comment-on-line-p)
         (if arg
             (comment-kill (if (integerp arg) arg nil))
             (comment-indent)))
        (t (paredit-insert-comment))))

(defun paredit-comment-on-line-p ()
  (save-excursion
    (goto-char (point-at-bol))
    (let ((comment-p nil))
      ;; Search forward for a comment beginning.  If there is one, set
      ;; COMMENT-P to true; if not, it will be nil.
      (while (progn (setq comment-p
                          (search-forward ";" (point-at-eol)
                                          ;; t -> no error
                                          t))
                    (and comment-p
                         (or (paredit-in-string-p)
                             (paredit-in-char-p (1- (point))))))
        (forward-char))
      comment-p)))

(defun paredit-insert-comment ()
  (let ((code-after-p
         (save-excursion (paredit-skip-whitespace t (point-at-eol))
                         (not (eq (point) (point-at-eol)))))
        (code-before-p
         (save-excursion (paredit-skip-whitespace nil (point-at-bol))
                         (not (eq (point) (point-at-bol))))))
    ;; We have to use EQ 0 here and not ZEROP because ZEROP signals an
    ;; error if its argument is non-numeric, but CALCULATE-LISP-INDENT
    ;; may return nil.
    (if (eq (let ((indent (calculate-lisp-indent)))
              (if (consp indent)
                  (car indent)
                  indent))
            0)
        ;; Top-level comment
        (progn (if code-after-p (save-excursion (newline)))
               (insert ";;; "))
      (if code-after-p
          ;; Code comment
          (progn (if code-before-p (newline-and-indent))
                 (lisp-indent-line)
                 (insert ";; ")
                 ;; Move the following code.  (NEWLINE-AND-INDENT will
                 ;; delete whitespace after the comment, though, so use
                 ;; NEWLINE & LISP-INDENT-LINE manually here.)
                 (save-excursion (newline)
                                 (lisp-indent-line)))
          ;; Margin comment
          (progn (indent-to comment-column
                            1)          ; 1 -> force one space after
                 (insert "; "))))))

(defun paredit-newline ()
  "Inserts a newline and indents it.
This is like `newline-and-indent', but it not only indents the line
that the point is on but also the S-expression following the point, if
there is one.
Moves forward one character first if on an escaped character.
If in a string, just inserts a literal newline."
  (interactive)
  (if (paredit-in-string-p)
      (newline)
    (if (and (not (paredit-in-comment-p)) (paredit-in-char-p))
        (forward-char))
    (newline-and-indent)
    ;; Indent the following S-expression, but don't signal an error if
    ;; there's only a closing parenthesis after the point.
    (condition-case () (indent-sexp)
      (scan-error nil))))

(defun paredit-forward-delete (&optional arg)
  "Deletes a character forward or moves forward over a delimiter.
If on an opening S-expression delimiter, moves forward into the
S-expression.
If on a closing S-expression delimiter, refuses to delete unless the
S-expression is empty, in which case the whole S-expression is deleted.
With a prefix argument, simply deletes a character forward, without
regard for delimiter balancing."
  (interactive "P")
  (cond ((or arg (eobp))
         (delete-char 1))
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
        ((let ((syn (char-syntax (char-after))))
           (or (eq syn ?\( )
               (eq syn ?\" )))
         (forward-char))
        ((and (not (paredit-in-char-p (1- (point))))
              (eq (char-syntax (char-after)) ?\) )
              (eq (char-before) (matching-paren (char-after))))
         (backward-delete-char 1)       ; Empty list -- delete both
         (delete-char 1))               ;   delimiters.
        ;; Just delete a single character, if it's not a closing
        ;; parenthesis.  (The character literal case is already
        ;; handled by now.)
        ((not (eq (char-syntax (char-after)) ?\) ))
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
  (cond ((or arg (bobp))
         (backward-delete-char 1))      ;++ should this untabify?
        ((paredit-in-string-p)
         (paredit-backward-delete-in-string))
        ((paredit-in-comment-p)
         (backward-delete-char 1))
        ((paredit-in-char-p)            ; Escape -- delete both chars.
         (backward-delete-char 1)
         (delete-char 1))
        ((paredit-in-char-p (1- (point)))
         (backward-delete-char 2))      ; ditto
        ((let ((syn (char-syntax (char-before))))
           (or (eq syn ?\) )
               (eq syn ?\" )))
         (backward-char))
        ((and (eq (char-syntax (char-before)) ?\( )
              (eq (char-after) (matching-paren (char-before))))
         (backward-delete-char 1)       ; Empty list -- delete both
         (delete-char 1))               ;   delimiters.
        ;; Delete it, unless it's an opening parenthesis.  The case
        ;; of character literals is already handled by now.
        ((not (eq (char-syntax (char-before)) ?\( ))
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
               (paredit-skip-whitespace t (point-at-eol))
               (or (eq (char-after) ?\; )
                   (eolp))))
         ;** Be careful about trailing backslashes.
         (kill-line))
        (t (paredit-kill-sexps-on-line))))

(defun paredit-kill-line-in-string ()
  (if (save-excursion (paredit-skip-whitespace t (point-at-eol))
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
        (end-of-list-p (paredit-forward-sexps-to-kill)))
    ;; If we got to the end of the list and it's on the same line,
    ;; move backward past the closing delimiter before killing.  (This
    ;; allows something like killing the whitespace in (    ).)
    (if end-of-list-p (progn (up-list) (backward-char)))
    (if kill-whole-line
        (paredit-kill-sexps-on-whole-line beg)
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
                         (point))))))

(defun paredit-forward-sexps-to-kill ()
  (let ((beg (point))
        (eol (point-at-eol))
        (end-of-list-p nil))
    ;; Move to the end of the last S-expression that started on this
    ;; line, or to the closing delimiter if the last S-expression in
    ;; this list is on the line.
    (catch 'return
      (while (and (not (eobp))
                  (save-excursion
                    (condition-case ()
                        (forward-sexp)
                      (scan-error
                       (up-list)
                       (setq end-of-list-p (eq (point-at-eol) eol))
                       (throw 'return nil)))
                    ;; We have to deal with a weird special case here
                    ;; of kill
                    (and (condition-case ()
                             (progn (backward-sexp) t)
                           (scan-error nil))
                         (eq (point-at-eol) eol))))
        (forward-sexp)))
    end-of-list-p))

(defun paredit-kill-sexps-on-whole-line (beg)
  (kill-region beg
               (or (save-excursion     ; Delete trailing indentation...
                     (paredit-skip-whitespace t)
                     (and (not (eq (char-after) ?\; ))
                          (point)))
                   ;; ...or just use the point past the newline, if
                   ;; we encounter a comment.
                   (point-at-eol)))
  (cond ((save-excursion (paredit-skip-whitespace nil (point-at-bol))
                         (bolp))
         ;; Nothing but indentation before the point, so indent it.
         (lisp-indent-line))
        ((eobp) nil)       ; Protect the CHAR-SYNTAX below against NIL.
        ;; Insert a space to avoid invalid joining if necessary.
        ((let ((syn-before (char-syntax (char-before)))
               (syn-after  (char-syntax (char-after))))
           (or (and (eq syn-before ?\) )            ; Separate opposing
                    (eq syn-after  ?\( ))           ;   parentheses,
               (and (eq syn-before ?\" )            ; string delimiter
                    (eq syn-after  ?\" ))           ;   pairs,
               (and (memq syn-before '(?_ ?w))      ; or word or symbol
                    (memq syn-after  '(?_ ?w)))))   ;   constituents.
         (insert " "))))



;;; ----------------
;;; Cursor and screen movement

(defun paredit-forward ()
  "Moves forward an S-expression, or up an S-expression forward.
If there are no more S-expressions in this one before the closing
delimiter, will move past that closing delimiter; otherwise, will move
forward past the S-expression following the point."
  (interactive)
  (condition-case ()
      (forward-sexp)
    (scan-error (if (paredit-in-string-p) (forward-char) (up-list)))))

(defun paredit-backward ()
  "Moves backward an S-expression, or up an S-expression backward.
If there are no more S-expressions in this one before the opening
delimiter, will move past that opening delimiter backward; otherwise,
will move backward past the S-expression preceding the point."
  (interactive)
  (condition-case ()
      (backward-sexp)
    (scan-error (if (paredit-in-string-p)
                    (backward-char)
                    (backward-up-list)))))

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
  (forward-sexp n)
  (let ((end-point (point)))
    (backward-sexp n)
    (let* ((start-point (point))
           (start-line (count-lines (point-min) (point)))
           (lines-on-sexps (count-lines start-point end-point)))
      (goto-line (+ start-line (/ lines-on-sexps 2)))
      (recenter))))



;;; ----------------
;;; Wrappage, splicage, & raisage

(defun paredit-wrap-sexp (&optional n)
  "Wraps the following S-expression in a list.
If a prefix argument N is given, N S-expressions are wrapped.
Automatically indents the newly wrapped S-expression.
As a special case, if at the end of a list, will simply insert a pair
of parentheses, rather than insert a lone opening parenthesis and then
signal an error, in the interest of preserving structural validity."
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
  "Splices the list that the point is on by removing its delimiters.
With a prefix argument as in `C-u', kills all S-expressions backward in
the current list before splicing all S-expressions forward into the
enclosing list.
With two prefix arguments as in `C-u C-u', kills all S-expressions
forward in the current list before splicing all S-expressions backward
into the enclosing list.
With a numerical prefix argument N, kills N S-expressions backward in
the current list before splicing the remaining S-expressions into the
enclosing list.  If N is negative, kills forward."
  (interactive "P")
  (save-excursion
    (paredit-kill-surrounding-sexps-for-splice arg)
    (backward-up-list)                  ; Go up to the beginning...
    (save-excursion
      (forward-sexp)                    ; Go forward an expression, to
      (backward-delete-char 1))         ;   delete the end delimiter.
    (delete-char 1)                     ; ...to delete the open char.
    (condition-case ()
        (progn (backward-up-list)       ; Reindent, now that the
               (indent-sexp))           ;   structure has changed.
      (scan-error nil))))

(defun paredit-kill-surrounding-sexps-for-splice (arg)
  (if (and arg (not (eq arg 0)))
      (cond ((numberp arg)
             ;; Kill ARG S-expressions before/after the point by saving
             ;; the point, moving across them, and killing the region.
             (let ((saved (point)))
               (condition-case ()
                   (backward-sexp arg)
                 (scan-error nil))
               (if (< arg 0)
                   (kill-region saved (point))
                   (kill-region (point) saved))))
            ((consp arg)
             (let ((v (car arg)))
               (if (= v 4)              ; one prefix argument
                   ;; Move backward until we hit the open paren; then
                   ;; kill that selected region.
                   (let ((end (point)))
                     (condition-case ()
                         (while (not (bobp)) (backward-sexp))
                       (scan-error nil))
                     (kill-region (point) end))
                   ;; Move forward until we hit the close paren; then
                   ;; kill that selected region.
                   (let ((beg (point)))
                     (condition-case ()
                         (while (not (eobp)) (forward-sexp))
                       (scan-error nil))
                     (kill-region beg (point))))))
            (t (error "Bizarre prefix argument: %s" arg)))))

(defun paredit-splice-sexp-killing-backward (&optional n)
  "Splices the list the point is on by removing its delimiters, and
also kills all S-expressions before the point in the current list.
With a prefix argument N, kills only the preceding N S-expressions."
  (interactive "P")
  (paredit-splice-sexp (if n
                           (prefix-numeric-value n)
                           '(4))))

(defun paredit-splice-sexp-killing-forward (&optional n)
  "Splices the list the point is on by removing its delimiters, and
also kills all S-expressions after the point in the current list.  With
a prefix argument N, kills only the following N S-expressions."
  (interactive "P")
  (paredit-splice-sexp (if n
                           (- (prefix-numeric-value n))
                           '(16))))

(defun paredit-raise-sexp (&optional n)
  "Raises the following S-expression in a tree, deleting its siblings.
With a prefix argument N, raises the following N S-expressions.  If N
is negative, raises the preceding N S-expressions."
  (interactive "p")
  ;; Select the S-expressions we want to raise in a buffer substring.
  (let* ((bound (save-excursion (forward-sexp n) (point)))
         (sexps (if (and n (< n 0))
                    ;; We backward & forward over one S-expression in
                    ;; order to get to the exact beginning or exact end
                    ;; of it, not wherever the point happened to be.
                    (buffer-substring bound
                                      (save-excursion (backward-sexp)
                                                      (forward-sexp)
                                                      (point)))
                    (buffer-substring (save-excursion (forward-sexp)
                                                      (backward-sexp)
                                                      (point))
                                      bound))))
    ;; Move up to the list we're raising those S-expressions out of and
    ;; delete it.
    (backward-up-list)
    (delete-region (point) (save-excursion (forward-sexp) (point)))
    (save-excursion (insert sexps))     ; Insert & reindent the sexps.
    (save-excursion (let ((n (abs (or n 1))))
                      (while (> n 0)
                        (paredit-forward-and-indent)
                        (setq n (1- n)))))))



;;; ----------------
;;; Slurpage & barfage

(defun paredit-forward-slurp-sexp ()
  "Adds the S-expression following the current list into that list
by moving the closing delimiter.
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
  "Removes the last S-expression in the current list from that list
by moving the closing delimiter.
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
      (paredit-skip-whitespace nil)     ;   skip leading whitespace.
      (cond ((bobp)
             (error "Barfing all subexpressions with no open-paren?"))
            ((paredit-in-comment-p)     ; Don't put the close-paren in
             (newline-and-indent)))     ;   a comment.
      (insert close))
    ;; Reindent all of the newly barfed S-expressions.
    (paredit-forward-and-indent)))

(defun paredit-backward-slurp-sexp ()
  "Adds the S-expression preceding the current list into that list
by moving the closing delimiter.
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
  "Removes the first S-expression in the current list from that list
by moving the closing delimiter.
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
            (while (progn (paredit-skip-whitespace t)
                          (eq (char-after) ?\; ))
              (goto-char (1+ (point-at-eol))))
            (if (eobp)
                (error
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
  "True if the point is on a character escape of a string.
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
A preceding escape character, not preceded by another escape character,
is considered a character literal prefix.  (This works for elisp,
Common Lisp, and Scheme.)
Assumes that `paredit-in-string-p' is false, so that it need not handle
long sequences of preceding backslashes in string escapes.  (This
assumes some other leading character token -- ? in elisp, # in Scheme
and Common Lisp.)"
  (let ((arg (or arg (point))))
    (and (eq (char-before arg) ?\\ )
         (not (eq (char-before (1- arg)) ?\\ )))))

(defun paredit-forward-and-indent ()
  "Moves forward an S-expression, indenting it fully.
Indents with `lisp-indent-line' and then `indent-sexp'."
  (forward-sexp)                        ; Go forward, and then find the
  (save-excursion                       ;   beginning of this next
    (backward-sexp)                     ;   S-expression.
    (lisp-indent-line)                  ; Indent its opening line, and
    (indent-sexp)))                     ;   the rest of it.

(defun paredit-skip-whitespace (trailing-p &optional limit)
  "Skips past any whitespace, or until the point LIMIT is reached.
If TRAILING-P is nil, skips leading whitespace; otherwise, skips
trailing whitespace."
  (funcall (if trailing-p #'skip-chars-forward #'skip-chars-backward)
           " \t\n"  ; This should skip using the syntax table, but LF
           limit))    ; is a comment end, not newline, in Lisp mode.



(provide 'paredit)
