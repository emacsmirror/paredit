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
      (dolist (after (cdr example))
        (with-temp-buffer
          (scheme-mode)
          (insert before)
          (goto-char (point-min))
          (search-forward "|")
          (backward-delete-char +1)
          (funcall command)
          (insert ?\|)
          (if (not (string= after (buffer-string)))
              (paredit-test-failed command before (buffer-string) after)))
        (setq before after)))))

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
                     (replace-regexp-in-string "(" (string left)
                       (replace-regexp-in-string ")" (string right) step)))
                   example))
         examples)))))

;++ Test `paredit-open-...' with the region active.
;++ Test `paredit-open-...' with prefix arguments.

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
    ;++ No way to express expected errors.
    ;; ("foo|" error)
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
    ;; (";foo|\n(bar\n baz)" error)
    ("|;;foo(" "|;foo(")
    ;; ("|;;foo(" "|;foo(" error)
    ))

(paredit-test 'paredit-backward-delete
  '(("fo|o" "f|o")
    (";fo(|o" ";fo|o")
    (";|;(foo)" "|;(foo)")
    (";;|(foo)" ";|(foo)" "|(foo)")
    (";foo\n|(bar)\n(baz\n quux)" ";foo|(bar)\n(baz\n quux)")
    ;; (";foo\n|(bar\n baz)" error)
    (";;|foo(" ";|foo(")
    ;; (";;|foo(" ";|foo(" error)
    ))

;++ Killing commands...ugh...
