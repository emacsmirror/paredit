#!/bin/sh

set -Ceu

exec emacs --batch --load paredit.el --eval '
(with-temp-buffer
  (paredit-insert-html-examples)
  (write-file "paredit.html"))
'
