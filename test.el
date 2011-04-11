;;; -*- Mode: Emacs-Lisp -*-

;;; Rudimentary, kludgey test suite for paredit -- work in progress!

;; Copyright (C) 2005--2011 Taylor R. Campbell

;; This file is part of paredit.
;;
;; Paredit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Paredit is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with paredit.  If not, see <http://www.gnu.org/licenses/>.

(defun paredit-test-failure-default (command before after expected)
  (error "%S failed test: after %S, got %S but expected %S."
         command before after expected))

(defvar paredit-test-failure-function 'paredit-test-failure-default
  "Function to call when `paredit-test' fails.
Four arguments: the paredit command, the text of the buffer
  before, the text of the buffer after, and the expected text of
  the buffer after.")

(defun paredit-test-failed (command before after expected)
  (funcall paredit-test-failure-function command before after expected))

(defun paredit-test (command examples)
  (dolist (example examples)
    (let ((before (car example)))
      (dolist (expected (cdr example))
        (with-temp-buffer
          (scheme-mode)
          (set (make-local-variable 'indent-tabs-mode) nil)
          (set (make-local-variable 'comment-column) 40)
          (insert before)
          (goto-char (point-min))
          (if (search-forward "_" nil t)
              (progn (backward-delete-char +1) (set-mark (point))))
          (goto-char (point-min))
          (search-forward "|")
          (backward-delete-char +1)
          (if (cond ((eq expected 'error)
                     ;++ Check that there are no more expected states.
                     (condition-case condition
                         (progn (call-interactively command) t)
                       (error nil)))
                    ((stringp expected)
                     (call-interactively command)
                     (insert ?\|)
                     (not (string= expected (buffer-string))))
                    (t (error "Bad test expectation:" expected)))
              (paredit-test-failed command before (buffer-string) expected)))
        (setq before expected)))))

(paredit-do-commands (spec keys command examples)
    nil                                 ;string case
  ;; `paredit-backslash' has a funny example.
  (if (not (eq command 'paredit-backslash))
      (paredit-test command examples)))

(defun paredit-test-bracketed (entries examples)
  (dolist (entry entries)
    (let ((command (car entry))
          (left (car (cdr entry)))
          (right (car (cdr (cdr entry)))))
      (paredit-test command
        (mapcar
         (lambda (example)
           (mapcar (lambda (step)
                     (if (stringp step)
                         (replace-regexp-in-string "(" (string left)
                           (replace-regexp-in-string ")" (string right) step))
                         step))
                   example))
         examples)))))

;++ Test `paredit-open-...' with the region active.

(paredit-test-bracketed '((paredit-open-round ?\( ?\))
                          (paredit-open-square ?\[ ?\])
                          (paredit-open-curly ?\{ ?\})
                          ;; (paredit-open-angled ?\< ?\>)
                          )
  '(("(foo|bar)" "(foo (|) bar)")
    ("(foo| bar)" "(foo (|) bar)")
    ("(foo |bar)" "(foo (|) bar)")
    ("(foo|\n bar)" "(foo (|)\n bar)")
    ("(foo |(bar) baz)" "(foo (|) (bar) baz)")
    ("(foo (bar)| baz)" "(foo (bar) (|) baz)")
    ("(foo |\"bar\" baz)" "(foo (|) \"bar\" baz)")
    ("(foo \"bar\"| baz)" "(foo \"bar\" (|) baz)")
    ("foo|" "foo (|)")
    ("|foo" "(|) foo")))

(let ((current-prefix-arg 1))
  (paredit-test-bracketed '((paredit-open-round ?\( ?\))
                            (paredit-open-square ?\[ ?\])
                            (paredit-open-curly ?\{ ?\})
                            ;; (paredit-open-angled ?\< ?\>)
                            )
    '(("(foo |bar baz)" "(foo (|bar) baz)")
      ("(x |;y\n z\n w)"
       "(x (|                                    ;y\n    z)\n w)"))))

(let ((current-prefix-arg '(4)))
  ;++ Oops -- `C-u (' is like `M-4 (', not like `C-u M-('.
  (paredit-test-bracketed '((paredit-open-round ?\( ?\))
                            (paredit-open-square ?\[ ?\])
                            (paredit-open-curly ?\{ ?\})
                            ;; (paredit-open-angled ?\< ?\>)
                            )
    '(("(foo |bar baz)" "(foo (|bar baz))")
      ("(x |;y\n z\n w)"
       "(x (|                                    ;y\n    z\n    w))")
      ("foo |bar baz" "foo (|bar baz)")
      ;++ These tests are kinda bunk.  It's not immediately clear to me
      ;++ which is right: including or excluding the trailing comment.
      ("foo\n|bar\nbaz\n;quux\n" "foo\n(|bar\n baz)\n;quux\n")
      ("foo\n|bar\nbaz\n;; quux" "foo\n(|bar\n baz\n ;; quux\n )"))))

(paredit-test-bracketed '((paredit-close-round ?\( ?\))
                          (paredit-close-square ?\[ ?\])
                          (paredit-close-curly ?\{ ?\})
                          (paredit-close-angled ?\< ?\>))
  '(("(#\\|x)" "(#\\|x)")
    ("(#\\|])" "(#\\|])")
    ("(#\\| )" "(#\\| )")
    ("(#\\|\")" "(#\\|\")")
    ("(\"|\")" "(\")|\")")
    ("(\"|\")" "(\")|\")")))

(paredit-test-bracketed '((paredit-close-round ?\( ?\))
                          (paredit-close-square ?\[ ?\])
                          (paredit-close-curly ?\{ ?\})
                          ;; (paredit-close-angled ?\< ?\>)
                          )
  '(("(|" "()|")
    ("foo|" error)
    ("(foo|  ;\n   )" "(foo  ;\n )|")
    ("(foo|  ;\n   bar)" "(foo  ;\n   bar)|")
    ("(foo|  ;\n   bar )" "(foo  ;\n   bar)|")))

(paredit-test-bracketed '((paredit-close-round-and-newline ?\( ?\))
                          (paredit-close-square-and-newline ?\[ ?\])
                          (paredit-close-curly-and-newline ?\{ ?\})
                          ;; (paredit-close-angled-and-newline ?\< ?\>)
                          )
  '(("(foo #\\|(  )" "(foo #\\()\n|")
    ("(foo|\n )   ;bar" "(foo)   ;bar\n|")
    ("((foo|\n)    (bar))" "((foo)\n |(bar))")))

(paredit-test 'paredit-newline
  '(("\"foo|bar\"" "\"foo\n|bar\"")
    ("(frob grovel ;full |(lexical)\n      mumble)"
     "(frob grovel ;full\n      |(lexical)\n      mumble)")
    ("(frob grovel ;full (|lexical)\n      mumble)"
     "(frob grovel ;full (\n             ;|lexical)\n      mumble)")
    ("#\\|(" "#\\(\n|")))

(paredit-test 'paredit-reindent-defun
  ;++ Test filling paragraphs in comments and strings.
  '(("|(define (square x)\n     (* x x))"
     "|(define (square x)\n  (* x x))")
    ("(define (square x)\n     (* x x))|"
     "(define (square x)\n  (* x x))|")
    ("(define (square x)\n     (* x x))|\n(frob\n    wotz)"
     "(define (square x)\n  (* x x))|\n(frob\n    wotz)")
    ("(define (square x)\n     (* x x))\n|(frob\n wotz)"
     "(define (square x)\n     (* x x))\n|(frob\n wotz)")
    ("(define (square x)\n |  (* x x))"
     "(define (square x)\n | (* x x))")
    ("(define (square x)\n    | (* x x))"
     "(define (square x)\n  |(* x x))")
    ("(define (square x)\n     (* |x x))"
     "(define (square x)\n  (* |x x))")))

(paredit-test 'paredit-semicolon
  '(("#\\|(" ";|#\\(")))

(paredit-test 'paredit-comment-dwim
  '(("\"foo|bar;baz\"    ;quux"
     "\"foobar;baz\"                            ;|quux")
    ;; Uh oh!  Bug in `comment-indent'...
    ;; ("\"foo\nbar|baz;quux\"         ;zot"
    ;;  "\"foo\nbarbaz;quux\"                            ;|zot")
    ;; I think the loop in `paredit-comment-on-line' is bogus.  Can you
    ;; elicit more than one iteration of it?  That is, can you cause
    ;; `comment-search-forward' to wind up inside a character or a
    ;; string?
    ))

(paredit-test 'paredit-forward-delete
  '(("f|oo" "f|o")
    (";f|(oo" ";f|oo")
    (";|;(foo)" ";|(foo)")
    ("|;;(foo)" "|;(foo)" "|(foo)")
    (";foo|\n(bar)\n(baz\n quux)" ";foo|(bar)\n(baz\n quux)")
    (";foo|\n(bar\n baz)" error)
    ("|;;foo(" "|;foo(" error)
    (";foo|\n(bar);baz\n" ";foo|(bar);baz\n")
    (";foo|\n(bar);baz" ";foo|(bar);baz")
    (";foo|\n(bar ;baz\n quux)\n" error)
    (";foo|\n(bar ;baz\n quux)" error)))

(paredit-test 'paredit-backward-delete
  '(("fo|o" "f|o")
    (";fo(|o" ";fo|o")
    (";|;(foo)" "|;(foo)")
    (";;|(foo)" ";|(foo)" "|(foo)")
    (";foo\n|(bar)\n(baz\n quux)" ";foo|(bar)\n(baz\n quux)")
    (";foo\n|(bar\n baz)" error)
    (";;|foo(" ";|foo(" error)
    (";foo\n|(bar);baz\n" ";foo|(bar);baz\n")
    (";foo\n|(bar);baz" ";foo|(bar);baz")
    (";foo\n|(bar ;baz\n quux)\n" error)
    (";foo\n|(bar ;baz\n quux)" error)))

;++ Need lots more tests for this, the hairiest paredit command...

(paredit-test 'paredit-kill
  '((";foo|\n(bar)\n" ";foo|(bar)\n")
    (";foo|\n(bar\n baz)\n" error)))

(defun paredit-canary-indent-method (state indent-point normal-indent)
  (check-parens)
  nil)

(put 'paredit-canary 'scheme-indent-function 'paredit-canary-indent-method)

;;; Check for regressions the indentation behaviour of forward slurping
;;; and barfing.

(paredit-test 'paredit-forward-slurp-sexp
  '(("(paredit-canary|)\n(lose)"
     "(paredit-canary|\n (lose))")))

(paredit-test 'paredit-forward-barf-sexp
  '(("(paredit-canary|  ;\n (lose))")
    ("(paredit-canary|  ;\n)\n(lose)")))
