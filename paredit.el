;;; -*- Mode: Emacs-Lisp; outline-regexp: "\n;;;;+" -*-

;;;;;; Paredit: Parenthesis-Editing Minor Mode
;;;;;; Version 18

;;; This code is written by Taylor Campbell (except where explicitly
;;; noted) and placed in the Public Domain.  All warranties are
;;; disclaimed.

;;; Add this to your .emacs after adding paredit.el to /path/to/elisp/:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp/")
;;;   (autoload 'paredit-mode "paredit"
;;;     "Minor mode for pseudo-structurally editing Lisp code."
;;;     t)
;;;   (add-hook '...-mode-hook (lambda () (paredit-mode +1)))
;;;
;;; Usually the ... will be lisp or scheme or both.  Alternatively, you
;;; can manually toggle this mode with M-x paredit-mode.  Customization
;;; of paredit can be accomplished with `eval-after-load':
;;;
;;;   (eval-after-load 'paredit
;;;     '(progn ...redefine keys, &c....))
;;;
;;; The REPL of SLIME (the Superior Lisp Interaction Mode for Emacs,
;;; <http://common-lisp.net/projects/slime/>) requires a binding that
;;; paredit mode overrides, namely RET, which paredit mode defines to
;;; have fancy newline-and-indent behaviour, and which SLIME's REPL
;;; mode defines to send a REPL input.  A simple workaround is to
;;; undefine RET in paredit's keymap and to define it in all keymaps
;;; where you want to use it, but which SLIME can override; e.g.,
;;;
;;;   (define-key paredit-mode-map (kbd "RET") nil)
;;;   (define-key lisp-mode-shared-map (kbd "RET") 'paredit-newline)
;;;
;;; This is written for GNU Emacs.  It is known not to work in XEmacs
;;; in ways that the author is not willing to spend time kludging over
;;; with compatibility workarounds.  The author wrote it with GNU Emacs
;;; 22.0.50; it may work in slightly earlier versions, but not older
;;; than 21 or so.

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
;;; or via IRC on irc.freenode.net in the #paredit channel under the
;;; nickname Riastradh.

;;; This assumes Unix-style LF line endings.

(defconst paredit-version 18)

;;;; Minor Mode Definition

(defvar paredit-mode-map (make-sparse-keymap)
  "Keymap for the paredit minor mode.")

(define-minor-mode paredit-mode
  "Minor mode for pseudo-structurally editing Lisp code.
\\<paredit-mode-map>"
  :lighter " Paredit"
  ;; If we're enabling paredit-mode, the prefix to this code that
  ;; DEFINE-MINOR-MODE inserts will have already set PAREDIT-MODE to
  ;; true.  If this is the case, then first check the parentheses, and
  ;; if there are any imbalanced ones we must inhibit the activation of
  ;; paredit mode.
  (if paredit-mode
      (condition-case condition
          (check-parens)
        (error (setq paredit-mode nil)
               (signal (car condition) (cdr condition))))))

;;; Old functions from when there was a different mode for emacs -nw.

(defun enable-paredit-mode ()
  "Turn on pseudo-structural editing of Lisp code.

Deprecated: use `paredit-mode' instead."
  (interactive)
  (paredit-mode +1))

(defun disable-paredit-mode ()
  "Turn off pseudo-structural editing of Lisp code.

Deprecated: use `paredit-mode' instead."
  (interactive)
  (paredit-mode -1))

;;;; Paredit Keys

;;; Separating the definition and initialization of this variable
;;; simplifies the development of paredit, since re-evaluating DEFVAR
;;; forms doesn't actually do anything.

(defvar paredit-commands nil
  "List of paredit commands with their keys and examples.")

;;; Each specifier is of the form:
;;;   (key[s] function (example-input example-output) ...)
;;; where key[s] is either a single string suitable for passing to KBD
;;; or a list of such strings.  Entries in this list may also just be
;;; strings, in which case they are headings for the next entries.

(progn (setq paredit-commands
 `(
   "Basic Insertion Commands"
   ("("         paredit-open-list
                ("(a b |c d)"
                 "(a b (|) c d)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar (|baz\" quux)"))
   (")"         paredit-close-list-and-newline
                ("(defun f (x|  ))"
                 "(defun f (x)\n  |)"))
   ("M-)"       paredit-close-list
                ("(a b |c   )" "(a b c)|")
                ("; Hello,| world!"
                 "; Hello,)| world!"))
   ("\""        paredit-doublequote
                ("(frob grovel |full lexical)"
                 "(frob grovel \"|\" full lexical)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar \\\"|baz\" quux)"))
   ("M-\""      paredit-meta-doublequote
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar baz\"\n     |quux)")
                ("(foo |(bar #\\x \"baz \\\\ quux\") zot)"
                 ,(concat "(foo \"|(bar #\\\\x \\\"baz \\\\"
                          "\\\\ quux\\\")\" zot)")))
   ("\\"        paredit-backslash
                ("(string #|)\n  ; Escaping character... (x)"
                 "(string #\\x|)")
                ("\"foo|bar\"\n  ; Escaping character... (\")"
                 "\"foo\\\"|bar\""))
   (";"         paredit-semicolon
                ("|(frob grovel)"
                 ";|\n(frob grovel)")
                ("(frob grovel)    |"
                 "(frob grovel)    ;|"))
   ("M-;"       paredit-comment-dwim
                ("(foo |bar)   ; baz"
                 "(foo bar)                               ; |baz")
                ("(frob grovel)|"
                 "(frob grovel)                           ;|")
                ("    (foo bar)\n|\n    (baz quux)"
                 "    (foo bar)\n    ;; |\n    (baz quux)")
                ("    (foo bar) |(baz quux)"
                 "    (foo bar)\n    ;; |\n    (baz quux)")
                ("|(defun hello-world ...)"
                 ";;; |\n(defun hello-world ...)"))

   ;; Unconventional, but I prefer C-j & RET this way, and you can
   ;; change it if you want anyway.
   ("RET"       paredit-newline
                ("(let ((n (frobbotz))) |(display (+ n 1)\nport))"
                 ,(concat "(let ((n (frobbotz)))"
                          "\n  |(display (+ n 1)"
                          "\n            port))")))
   ("C-j"       newline)

   "Deleting & Killing"
   (("C-d" "<deletechar>")
                paredit-forward-delete
                ("(quu|x \"zot\")" "(quu| \"zot\")")
                ("(quux |\"zot\")"
                 "(quux \"|zot\")"
                 "(quux \"|ot\")")
                ("(foo (|) bar)" "(foo | bar)")
                ("|(foo bar)" "(|foo bar)"))
   ("DEL"       paredit-backward-delete
                ("(\"zot\" q|uux)" "(\"zot\" |uux)")
                ("(\"zot\"| quux)"
                 "(\"zot|\" quux)"
                 "(\"zo|\" quux)")
                ("(foo (|) bar)" "(foo | bar)")
                ("(foo bar)|" "(foo bar|)"))
   ("C-k"       paredit-kill
                ("(foo bar)|     ; Useless comment!"
                 "(foo bar)|")
                ("(|foo bar)     ; Useful comment!"
                 "(|)     ; Useful comment!")
                ("|(foo bar)     ; Useless line!"
                 "|")
                ("(foo \"|bar baz\"\n     quux)"
                 "(foo \"|\"\n     quux)"))
   ("M-d"       paredit-forward-kill-word
                ("|(foo bar)    ; baz"
                 "(| bar)    ; baz"
                 "(|)    ; baz"
                 "()    ;|")
                (";;;| Frobnicate\n(defun frobnicate ...)"
                 ";;;|\n(defun frobnicate ...)"
                 ";;;\n(| frobnicate ...)"))
   ("M-DEL"     paredit-backward-kill-word
                ("(foo bar)    ; baz\n(quux)|"
                 "(foo bar)    ; baz\n(|)"
                 "(foo bar)    ; |\n()"
                 "(foo |)    ; \n()"
                 "(|)    ; \n()"))

   "Movement & Navigation"
   ("C-M-f"     paredit-forward
                ("(foo |(bar baz) quux)"
                 "(foo (bar baz)| quux)")
                ("(foo (bar)|)"
                 "(foo (bar))|"))
   ("C-M-b"     paredit-backward
                ("(foo (bar baz)| quux)"
                 "(foo |(bar baz) quux)")
                ("(|(foo) bar)"
                 "|((foo) bar)"))
;;;("C-M-u"     backward-up-list)       ; These two are built-in.
;;;("C-M-d"     down-list)
   ("C-M-p"     backward-down-list)     ; Built-in, these are FORWARD-
   ("C-M-n"     up-list)                ; & BACKWARD-LIST, which have
                                        ; no need given C-M-f & C-M-b.

   "Depth-Changing Commands"
   ("M-("       paredit-wrap-sexp
                ("(foo |bar baz)"
                 "(foo (|bar) baz)"))
   ("M-s"       paredit-splice-sexp
                ("(foo (bar| baz) quux)"
                 "(foo bar| baz quux)"))
   (("<M-up>" "ESC M-O A")
                paredit-splice-sexp-killing-backward
                ("(foo (let ((x 5)) |(sqrt n)) bar)"
                 "(foo (sqrt n) bar)"))
   (("<M-down>" "ESC M-O B")
                paredit-splice-sexp-killing-forward
                ("(a (b c| d e) f)"
                 "(a b c f)"))
   ("M-r"       paredit-raise-sexp
                ("(dynamic-wind in (lambda () |body) out)"
                 "(dynamic-wind in |body out)"
                 "|body"))

   "Barfage & Slurpage"
   (("C-)" "<M-right>" "ESC <right>" "ESC M-O C")
                paredit-forward-slurp-sexp
                ("(foo (bar |baz) quux zot)"
                 "(foo (bar |baz quux) zot)")
                ("(a b ((c| d)) e f)"
                 "(a b ((c| d) e) f)"))
   (("C-}" "<M-left>" "ESC <left>" "ESC M-O D")
                paredit-forward-barf-sexp
                ("(foo (bar |baz quux) zot)"
                 "(foo (bar |baz) quux zot)"))
   (("C-(" "<C-M-left>" "ESC <C-left>" "ESC M-O d")
                paredit-backward-slurp-sexp
                ("(foo bar (baz| quux) zot)"
                 "(foo (bar baz| quux) zot)")
                ("(a b ((c| d)) e f)"
                 "(a (b (c| d)) e f)"))
   (("C-{" "<C-M-right>" "ESC <C-right>" "ESC M-O c")
                paredit-backward-barf-sexp
                ("(foo (bar baz |quux) zot)"
                 "(foo bar (baz |quux) zot)"))

   "Miscellaneous Commands"
   ("M-S"       paredit-split-sexp
                ("(hello| world)"
                 "(hello)| (world)")
                ("\"Hello, |world!\""
                 "\"Hello, \"| \"world!\""))
   ("M-J"       paredit-join-sexps
                ("(hello)| (world)"
                 "(hello| world)")
                ("\"Hello, \"| \"world!\""
                 "\"Hello, |world!\"")
                ("hello-\n|  world"
                 "hello-|world"))
   ("C-c C-M-l" paredit-recentre-on-sexp)
   ))
       nil)                             ; end of PROGN

;;;;; Command Examples

; (put 'paredit-do-commands 'lisp-indent-function 2)

(eval-when-compile
  (defmacro paredit-do-commands (vars string-case &rest body)
    (let ((spec     (nth 0 vars))
          (keys     (nth 1 vars))
          (fn       (nth 2 vars))
          (examples (nth 3 vars)))
      `(dolist (,spec paredit-commands)
         (if (stringp ,spec)
             ,string-case
           (let ((,keys (let ((k (car spec)))
                          (cond ((stringp k) (list k))
                                ((listp k) k)
                                (t (error "Invalid paredit command %s."
                                          ,spec)))))
                 (,fn (cadr spec))
                 (,examples (cddr spec)))
             ,@body))))))

(defun paredit-define-keys ()
  (paredit-do-commands (spec keys fn examples)
      nil       ; string case
    (dolist (key keys)
      (define-key paredit-mode-map (read-kbd-macro key) fn))))

(defun paredit-function-documentation (fn)
  (let ((original-doc (get fn 'paredit-original-documentation))
        (doc (documentation fn 'function-documentation)))
    (or original-doc
        (progn (put fn 'paredit-original-documentation doc)
               doc))))

(defun paredit-annotate-mode-with-examples ()
  (let ((contents
         (list (paredit-function-documentation 'paredit-mode))))
    (paredit-do-commands (spec keys fn examples)
        (push (concat "\n\n" spec "\n")
              contents)
      (let ((name (symbol-name fn)))
        (if (string-match (symbol-name 'paredit-) name)
            (push (concat "\n\n\\[" name "]\t" name
                          (if examples
                              (mapconcat (lambda (example)
                                           (concat
                                            "\n"
                                            (mapconcat 'identity
                                                       example
                                                       "\n  --->\n")
                                            "\n"))
                                         examples
                                         "")
                              "\n  (no examples)\n"))
                  contents))))
    (put 'paredit-mode 'function-documentation
         (apply #'concat (reverse contents))))
  ;; PUT returns the huge string we just constructed, which we don't
  ;; want it to return.
  nil)

(defun paredit-annotate-functions-with-examples ()
  (paredit-do-commands (spec keys fn examples)
      nil       ; string case
    (put fn 'function-documentation
         (concat (paredit-function-documentation fn)
                 "\n\n\\<paredit-mode-map>\\[" (symbol-name fn) "]\n"
                 (mapconcat (lambda (example)
                              (concat "\n"
                                      (mapconcat 'identity
                                                 example
                                                 "\n  ->\n")
                                      "\n"))
                            examples
                            "")))))

;;;;; HTML Examples

(defun paredit-insert-html-examples ()
  "Insert HTML for a paredit quick reference table."
  (interactive)
  (let ((insert-lines (lambda (&rest lines)
                        (mapc (lambda (line) (insert line) (newline))
                              lines)))
        (html-keys
         (lambda (keys)
           (mapconcat (lambda (key)
                        (if (and (eq (elt key 0) ?\<)
                                 (eq (elt key (- (length key)
                                                 1))
                                     ?\>))
                            (substring key 1 (- (length key) 1))
                            key))
                      keys
                      ", ")))
        (html-example
         (lambda (example)
           (concat "<table><td><table>"
                   "<tr><td><pre>"
                   (mapconcat 'identity
                              example
                              (concat "</pre></td></tr><tr><td>"
                                      "&nbsp;&nbsp;&nbsp;&nbsp;---&gt;"
                                      "</td></tr><tr><td><pre>"))
                   "</pre></td></tr>"
                   "</table></td></table>")))
        (firstp t))
    (paredit-do-commands (spec keys fn examples)
        (progn (if (not firstp)
                   (insert "</table>\n")
                   (setq firstp nil))
               (funcall insert-lines
                        (concat "<h3>" spec "</h3>")
                        "<table border=\"1\" cellpadding=\"1\">"
                        "  <tr>"
                        "    <th>Command</th>"
                        "    <th>Keys</th>"
                        "    <th>Examples</th>"
                        "  </tr>"))
      (let ((name (symbol-name fn)))
        (if (string-match (symbol-name 'paredit-) name)
            (funcall insert-lines
                     "  <tr>"
                     (concat "    <td><tt>" name "</tt></td>")
                     (concat "    <td align=\"center\">"
                             (funcall html-keys keys)
                             "</td>")
                     (concat "    <td>"
                             (if examples
                                 (mapconcat html-example examples
                                            "<hr>")
                                 "(no examples)")
                             "</td>")
                     "  </tr>")))))
  (insert "</table>\n"))

;;;; Basic Editing Commands

(defun paredit-open-list (&optional n)
  "Insert a balanced parenthesis pair.
With a prefix argument N, put the closing parentheses after N
  S-expressions forward.
If in string or comment, insert a single opening parenthesis.
If in a character literal, do nothing.  This prevents accidentally
  changing what was in the character literal to a meaningful delimiter
  unintentionally."
  (interactive "P")
  (cond ((or (paredit-in-string-p)
             (paredit-in-comment-p))
         (insert "("))
        ((not (paredit-in-char-p))
         (insert-parentheses (or n 0)))))

(defun paredit-close-list ()
  "Move past one closing parenthesis and reindent.
If in a string or comment, insert a single closing parenthesis.
If in a character literal, do nothing.  This prevents accidentally
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
  "Move past one closing delimiter, add a newline, and reindent.
If there was a margin comment after the closing delimiter, preserve
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
  "Find a margin comment on the current line.
If such a comment exists, delete the comment (including all leading
  whitespace) and return a cons whose car is the comment as a string
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
  "Move one character past the next closing parenthesis.
Delete extraneous whitespace before the closing parenthesis.  Do not
  delete comments, however; if there is a comment between the point and
  the next closing parenthesis, move the closing parenthesis to the
  line after the comment and indent appropriately."
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
            (let ((blink-matching-paren-on-screen t)
                  (show-paren-mode nil))
              (blink-matching-open)))
        (scan-error nil))))

(defun paredit-doublequote (&optional n)
  "Insert a pair of double-quotes.
With a prefix argument N, wrap the following N S-expressions in
  double-quotes, escaping intermediate characters if necessary.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."
  (interactive "P")
  (cond ((paredit-in-string-p)
         (if (eq (cdr (paredit-string-start+end-points))
                 (point))
             (forward-char)             ; We're on the closing quote.
             (insert ?\\ ?\" )))
        ((paredit-in-comment-p)
         (insert ?\" ))
        ((not (paredit-in-char-p))
         (if n (paredit-skip-whitespace t))
         (let* ((end (and n (save-excursion (paredit-forward-for-quote
                                             (prefix-numeric-value n))
                                            (point))))
                (spacep (paredit-space-for-quote-p nil ?\) )))
           (if spacep (insert " "))
           (insert ?\" )
           (save-excursion
             ;; Move past the S-expressions we counted, if we were to
             ;; count them.  Account for the quote and optionally the
             ;; space, which we just inserted.
             (if n (goto-char (+ end 1 (if spacep 1 0))))
             (insert ?\" )
             (if (paredit-space-for-quote-p t ?\( )
                 (insert " ")))))))

(defun paredit-meta-doublequote (&optional n)
  "Move to the end of the string, insert a newline, and indent.
If not in a string, act as `paredit-doublequote'; if no prefix argument
  is specified, the default is to wrap one S-expression, however, not
  zero."
  (interactive "p")
  (if (not (paredit-in-string-p))
      (paredit-doublequote (or n 1))
    (let ((start+end (paredit-string-start+end-points)))
      (goto-char (1+ (cdr start+end)))
      (newline)
      (lisp-indent-line)
      (condition-case () (indent-sexp)
        (scan-error nil)))))

(defun paredit-space-for-quote-p (endp delim-syn)
  ;; If at the buffer limit, don't insert a space.  If there is a word,
  ;; symbol, other quote, or non-matching parenthesis delimiter (i.e. a
  ;; close when want an open the string or an open when we want to
  ;; close the string), do insert a space.
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp
                              (char-after)
                              (char-before)))
             (list ?w ?_ ?\" delim-syn))))

(defun paredit-forward-for-quote (n)
  (let ((end (save-excursion (forward-sexp n) (point)))
        (state (paredit-current-parse-state)))
    (while (< (point) end)
      (let ((new-state (parse-partial-sexp (point) (1+ (point))
                                           nil nil state)))
        (if (not (paredit-in-string-p new-state))
            (if (not (paredit-in-string-escape-p))
                (setq state new-state)
              ;; Escape character: turn it into an escaped escape
              ;; character by appending another backslash.
              (insert ?\\ )
              ;; Now the point is after both escapes, and we want to
              ;; rescan from before the first one to after the second
              ;; one.
              (setq state
                    (parse-partial-sexp (- (point) 2) (point)
                                        nil nil state))
              ;; Advance the end point, since we just inserted a new
              ;; character.
              (setq end (1+ end)))
          ;; String: escape by inserting a backslash before the quote.
          (backward-char)
          (insert ?\\ )
          ;; The point is now between the escape and the quote, and we
          ;; want to rescan from before the escape to after the quote.
          (setq state
                (parse-partial-sexp (1- (point)) (1+ (point))
                                    nil nil state))
          ;; Advance the end point for the same reason as above.
          (setq end (1+ end)))))))

(defun paredit-backslash ()
  "Insert a backslash followed by a character to escape."
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
          ;; crock.)
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

(defun paredit-semicolon (&optional n)
  "Insert a semicolon, moving any code after the point to a new line.
If in a string, comment, or character literal, insert just a literal
  semicolon, and do not move anything to the next line.
With a prefix argument N, insert N semicolons."
  (interactive "P")
  (if (not (or (paredit-in-string-p)
               (paredit-in-comment-p)
               (paredit-in-char-p)
               ;; No more code on the line after the point.
               (save-excursion
                 (paredit-skip-whitespace t (point-at-eol))
                 (or (eolp)
                     ;; Let the user prefix semicolons to existing
                     ;; comments.
                     (eq (char-after) ?\;)))))
      ;; Don't use NEWLINE-AND-INDENT, because that will delete all of
      ;; the horizontal whitespace first, but we just want to move the
      ;; code following the point onto the next line while preserving
      ;; the point on this line.
      ;++ Why indent only the line?
      (save-excursion (newline) (lisp-indent-line)))
  (insert (make-string (if n (prefix-numeric-value n) 1)
                       ?\; )))

(defun paredit-comment-dwim (&optional arg)
  "Call the Lisp comment command you want (Do What I Mean).
This is like `comment-dwim', but it is specialized for Lisp editing.
If transient mark mode is enabled and the mark is active, comment or
  uncomment the selected region, depending on whether it was entirely
  commented not not already.
If there is already a comment on the current line, with no prefix
  argument, indent to that comment; with a prefix argument, kill that
  comment.
Otherwise, insert a comment appropriate for the context and ensure that
  any code following the comment is moved to the next line.
At the top level, where indentation is calculated to be at column 0,
  insert a triple-semicolon comment; within code, where the indentation
  is calculated to be non-zero, and on the line there is either no code
  at all or code after the point, insert a double-semicolon comment;
  and if the point is after all code on the line, insert a single-
  semicolon margin comment at `comment-column'."
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
    (beginning-of-line)
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
                         (not (eolp))))
        (code-before-p
         (save-excursion (paredit-skip-whitespace nil (point-at-bol))
                         (not (bolp)))))
    (if (and (bolp)
             ;; We have to use EQ 0 here and not ZEROP because ZEROP
             ;; signals an error if its argument is non-numeric, but
             ;; CALCULATE-LISP-INDENT may return nil.
             (eq (let ((indent (calculate-lisp-indent)))
                   (if (consp indent)
                       (car indent)
                     indent))
                 0))
        ;; Top-level comment
        (progn (if code-after-p (save-excursion (newline)))
               (insert ";;; "))
      (if code-after-p
          ;; Code comment
          (progn (if code-before-p
                     ;++ Why NEWLINE-AND-INDENT here and not just
                     ;++ NEWLINE, or PAREDIT-NEWLINE?
                     (newline-and-indent))
                 (lisp-indent-line)
                 (insert ";; ")
                 ;; Move the following code.  (NEWLINE-AND-INDENT will
                 ;; delete whitespace after the comment, though, so use
                 ;; NEWLINE & LISP-INDENT-LINE manually here.)
                 (save-excursion (newline)
                                 (lisp-indent-line)))
          ;; Margin comment
          (progn (indent-to comment-column
                            1)          ; 1 -> force one leading space
                 (insert ?\; ))))))

;;; The placement of this function in this file is totally random.

(defun paredit-newline ()
  "Insert a newline and indent it.
This is like `newline-and-indent', but it not only indents the line
  that the point is on but also the S-expression following the point,
  if there is one.
Move forward one character first if on an escaped character.
If in a string, just insert a literal newline."
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
  "Delete a character forward or move forward over a delimiter.
If on an opening S-expression delimiter, move forward into the
  S-expression.
If on a closing S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a prefix argument, simply delete a character forward, without
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
          ((eq (1- (point)) (car start+end))
           ;; If it is the close-quote, delete only if we're also right
           ;; past the open-quote (i.e. it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (backward-delete-char 1)
           (delete-char 1)))))

(defun paredit-backward-delete (&optional arg)
  "Delete a character backward or move backward over a delimiter.
If on a closing S-expression delimiter, move backward into the
  S-expression.
If on an opening S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a prefix argument, simply delete a character backward, without
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
  "Kill a line as if with `kill-line', but respecting delimiters.
In a string, act exactly as `kill-line' but do not kill past the
  closing string delimiter.
On a line with no S-expressions on it starting after the point or
  within a comment, act exactly as `kill-line'.
Otherwise, kill all S-expressions that start after the point."
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
      (let ((beginning (point)))
        (while (not (or (eolp)
                        (eq (char-after) ?\" )))
          (forward-char)
          ;; Skip past escaped characters.
          (if (eq (char-before) ?\\ )
              (forward-char)))
        (kill-region beginning (point))))))

(defun paredit-kill-sexps-on-line ()
  (if (paredit-in-char-p)               ; Move past the \ and prefix.
      (backward-char 2))                ; (# in Scheme/CL, ? in elisp)
  (let ((beginning (point))
        (eol (point-at-eol))
        (end-of-list-p (paredit-forward-sexps-to-kill)))
    ;; If we got to the end of the list and it's on the same line,
    ;; move backward past the closing delimiter before killing.  (This
    ;; allows something like killing the whitespace in (    ).)
    (if end-of-list-p (progn (up-list) (backward-char)))
    (if kill-whole-line
        (paredit-kill-sexps-on-whole-line beginning)
        (kill-region beginning
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
  (let ((beginning (point))
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

(defun paredit-kill-sexps-on-whole-line (beginning)
  (kill-region beginning
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

(defun paredit-forward-kill-word ()
  "Kill a word forward, skipping over intervening delimiters."
  (interactive)
  (let ((beginning (point)))
    (skip-syntax-forward " -")
    (if (eq (char-syntax (char-after)) ?w)
        (progn (goto-char beginning)    ; Easy case: no intervening
               (kill-word 1))           ;   delimiters.
      (let* ((parse-state (paredit-current-parse-state))
             (state (paredit-kill-word-state parse-state)))
        (catch 'exit
          (while t
            ;; Go character-by-character forward.  If we encounter a
            ;; state change -- that is, if we move into or out of a
            ;; comment or string, or encounter a bracket --, then reset
            ;; the beginning point to after wherever the state changed,
            ;; so that we don't destroy any intervening delimiters.
            (setq parse-state           ; PPS advances the point.
                  (parse-partial-sexp (point) (1+ (point))
                                      nil nil parse-state))
            (let ((old-state state)
                  (new-state (paredit-kill-word-state parse-state)))
              (setq state new-state)
              (if (not (eq old-state new-state))
                  (setq beginning
                        (paredit-kill-word-hack-comments old-state))))
            ;; Finally, if we found a word, kill up to there and exit.
            ;; BEGINNING will be the first point in this state.
            (cond ((eq (char-syntax (char-after)) ?w)
                   (goto-char beginning)
                   (kill-word 1)
                   (throw 'exit nil)))))))))

(defun paredit-backward-kill-word ()
  "Kill a word backward, skipping over any intervening delimiters."
  (interactive)
  (if (eq (char-syntax (char-before)) ?w)
      ;; We're *on* the word, so we don't need to do anything else.
      (backward-kill-word 1)
    (let ((beginning (point)))
      (backward-word 1)
      (let* ((word-start (point))
             (parse-state (paredit-current-parse-state))
             (state (paredit-kill-word-state parse-state)))
        (forward-word 1)
        (setq parse-state
              (parse-partial-sexp word-start (point)
                                  nil nil parse-state))
        (while (and (eq state (paredit-kill-word-state parse-state))
                    (< (point) beginning))
          (setq parse-state             ; PPS advances the point.
                (parse-partial-sexp (point) (1+ (point))
                                    nil nil parse-state)))
        (if (or (and (eq state 'comment) (bolp))
                (and (eq state 'string)  (eq (char-before) ?\" )))
            (backward-char 1))
        (kill-region word-start (point))))))

(defun paredit-kill-word-state (parse-state)
  (cond ((paredit-in-comment-p parse-state) 'comment)
        ((paredit-in-string-p  parse-state) 'string)
        ((memq (char-syntax (char-after))
               '(?\( ?\) ))
         'bracket-sequence)
        (t 'other)))

(defun paredit-kill-word-hack-comments (state)
  (cond ((and (eq state 'comment)
              (eq (char-after) ?\#))
         (1+ (point)))
        ((and (not (eq state 'comment))
              (eq (char-before) ?\;))
         (skip-chars-forward ";")
         (point))
        (t (point))))

;;;; Cursor and Screen Movement

(defun paredit-forward ()
  "Move forward an S-expression, or up an S-expression forward.
If there are no more S-expressions in this one before the closing
  delimiter, move past that closing delimiter; otherwise, move forward
  past the S-expression following the point."
  (interactive)
  (condition-case ()
      (forward-sexp)
    ;++ Is it necessary to use UP-LIST and not just FORWARD-CHAR?
    (scan-error (if (paredit-in-string-p) (forward-char) (up-list)))))

(defun paredit-backward ()
  "Move backward an S-expression, or up an S-expression backward.
If there are no more S-expressions in this one before the opening
  delimiter, move past that opening delimiter backward; otherwise, move
  move backward past the S-expression preceding the point."
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
  "Recentre the screen on the S-expression following the point.
With a prefix argument N, encompass all N S-expressions forward."
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

;;;; Wrappage, Splicage, & Raisage

(defun paredit-wrap-sexp (&optional n)
  "Wrap the following S-expression in a list.
If a prefix argument N is given, wrap N S-expressions.
Automatically indent the newly wrapped S-expression.
As a special case, if the point is at the end of a list, simply insert
  a pair of parentheses, rather than insert a lone opening parenthesis
  and then signal an error, in the interest of preserving structural
  validity."
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
  "Splice the list that the point is on by removing its delimiters.
With a prefix argument as in `C-u', kill all S-expressions backward in
  the current list before splicing all S-expressions forward into the
  enclosing list.
With two prefix arguments as in `C-u C-u', kill all S-expressions
  forward in the current list before splicing all S-expressions
  backward into the enclosing list.
With a numerical prefix argument N, kill N S-expressions backward in
  the current list before splicing the remaining S-expressions into the
  enclosing list.  If N is negative, kill forward."
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
                   (let ((beginning (point)))
                     (condition-case ()
                         (while (not (eobp)) (forward-sexp))
                       (scan-error nil))
                     (kill-region beginning (point))))))
            (t (error "Bizarre prefix argument: %s" arg)))))

(defun paredit-splice-sexp-killing-backward (&optional n)
  "Splice the list the point is on by removing its delimiters, and
  also kill all S-expressions before the point in the current list.
With a prefix argument N, kill only the preceding N S-expressions."
  (interactive "P")
  (paredit-splice-sexp (if n
                           (prefix-numeric-value n)
                           '(4))))

(defun paredit-splice-sexp-killing-forward (&optional n)
  "Splice the list the point is on by removing its delimiters, and
  also kill all S-expressions after the point in the current list.
With a prefix argument N, kill only the following N S-expressions."
  (interactive "P")
  (paredit-splice-sexp (if n
                           (- (prefix-numeric-value n))
                           '(16))))

(defun paredit-raise-sexp (&optional n)
  "Raise the following S-expression in a tree, deleting its siblings.
With a prefix argument N, raise the following N S-expressions.  If N
  is negative, raise the preceding N S-expressions."
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

;;;; Slurpage & Barfage

(defun paredit-forward-slurp-sexp ()
  "Add the S-expression following the current list into that list
  by moving the closing delimiter.
Automatically reindent the newly slurped S-expression with respect to
  its new enclosing form."
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
  "Remove the last S-expression in the current list from that list
  by moving the closing delimiter.
Automatically reindent the newly barfed S-expression with respect to
  its new enclosing form."
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
  "Add the S-expression preceding the current list into that list
  by moving the closing delimiter.
Automatically reindent the whole form into which new S-expression was
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
  "Remove the first S-expression in the current list from that list
  by moving the closing delimiter.
Automatically reindent the barfed S-expression and the form from which
  it was barfed."
  (interactive)
  ;; SAVE-EXCURSION here does the wrong thing, but manually saving and
  ;; restoring the point does the right thing.  Here's an example of
  ;; how SAVE-EXCURSION breaks:
  ;;   (foo|)   C-{
  ;;   foo|()
  ;; It should be:
  ;;   foo(|)
  (let ((beginning (point)))
    (unwind-protect
        (progn
          (backward-up-list)
          (let ((open (char-after)))
            (delete-char 1)
            (condition-case () (paredit-forward-and-indent)
              (scan-error nil))
            (while (progn (paredit-skip-whitespace t)
                          (eq (char-after) ?\; ))
              (forward-line 1))
            (if (eobp)
                (error
                 "Barfing all subexpressions with no close-paren?"))
            (insert open))
          (backward-up-list)
          (lisp-indent-line)
          (indent-sexp))
      (goto-char beginning))))

;;;; Splitting & Joining

(defun paredit-split-sexp ()
  "Split the list or string the point is on into two."
  (interactive)
  (cond ((paredit-in-string-p)
         (insert "\"")
         (save-excursion (insert " \"")))
        ((or (paredit-in-comment-p)
             (paredit-in-char-p))
         (error "Invalid context for `paredit-split-sexp'"))
        (t (let ((open  (save-excursion (backward-up-list)
                                        (char-after)))
                 (close (save-excursion (up-list)
                                        (char-before))))
             (delete-horizontal-space)
             (insert close)
             (save-excursion (insert ?\ )
                             (insert open)
                             (backward-char)
                             (indent-sexp))))))

(defun paredit-join-sexps ()
  "Join the S-expressions adjacent on either side of the point.
Both must be lists, strings, or atoms; error if there is a mismatch."
  (interactive)
  (save-excursion
    (if (or (paredit-in-comment-p)
            (paredit-in-string-p)
            (paredit-in-char-p))
        (error "Invalid S-expression join.")
      (let ((left-point  (save-excursion (backward-sexp)
                                         (forward-sexp)
                                         (point)))
            (right-point (save-excursion (forward-sexp)
                                         (backward-sexp)
                                         (point))))
        (let ((left-syntax (char-syntax (char-before left-point)))
              (right-syntax (char-syntax (char-after right-point))))
          (cond ((or (and (eq left-syntax  ?\) )
                          (eq right-syntax ?\( ))
                     (and (eq left-syntax  ?\" )
                          (eq right-syntax ?\" )))
                 (goto-char right-point)
                 (delete-char 1)
                 (goto-char left-point)
                 (backward-delete-char 1)
                 (if (not (paredit-in-string-p))
                     (progn (backward-up-list) (indent-sexp))))
                ((and (memq left-syntax  '(?w ?_)) ; Word or symbol
                      (memq right-syntax '(?w ?_)))
                 ;++ What about intervening comments?
                 (delete-region left-point right-point))
                (t (error "Mismatched S-expressions to join."))))))))

;;;; Utilities

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
  "Move forward an S-expression, indenting it fully.
Indent with `lisp-indent-line' and then `indent-sexp'."
  (forward-sexp)                        ; Go forward, and then find the
  (save-excursion                       ;   beginning of this next
    (backward-sexp)                     ;   S-expression.
    (lisp-indent-line)                  ; Indent its opening line, and
    (indent-sexp)))                     ;   the rest of it.

(defun paredit-skip-whitespace (trailing-p &optional limit)
  "Skip past any whitespace, or until the point LIMIT is reached.
If TRAILING-P is nil, skip leading whitespace; otherwise, skip trailing
  whitespace."
  (funcall (if trailing-p #'skip-chars-forward #'skip-chars-backward)
           " \t\n"  ; This should skip using the syntax table, but LF
           limit))    ; is a comment end, not newline, in Lisp mode.

;;;;; S-expression Parsing Utilities

;++ These routines redundantly traverse S-expressions a great deal.
;++ If performance issues arise, this whole section will probably have
;++ to be refactored to preserve the state longer, like paredit.scm
;++ does, rather than to traverse the definition N times for every key
;++ stroke as it presently does.

(defun paredit-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    ;; Calling PARSE-PARTIAL-SEXP will advance the point to its second
    ;; argument (unless parsing stops due to an error, but we assume it
    ;; won't in paredit-mode).
    (parse-partial-sexp (point) point)))

(defun paredit-in-string-p (&optional state)
  "True if the parse state is within a double-quote-delimited string.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 3. non-nil if inside a string (the terminator character, really)
  (and (nth 3 (or state (paredit-current-parse-state)))
       t))

(defun paredit-string-start+end-points (&optional state)
  "Return a cons of the points of the open and quotes of the string.
The string is determined from the parse state STATE, or the parse state
  from the beginning of the defun to the point.
This assumes that `paredit-in-string-p' has already returned true, i.e.
  that the point is already within a string."
  (save-excursion
    ;; 8. character address of start of comment or string; nil if not
    ;;    in one
    (let ((start (nth 8 (or state (paredit-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun paredit-in-comment-p (&optional state)
  "True if parse state STATE is within a comment.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 4. nil if outside a comment, t if inside a non-nestable comment,
  ;;    else an integer (the current comment nesting)
  (and (nth 4 (or state (paredit-current-parse-state)))
       t))

;;;; Initialization

(paredit-define-keys)
(paredit-annotate-mode-with-examples)
(paredit-annotate-functions-with-examples)

(provide 'paredit)
