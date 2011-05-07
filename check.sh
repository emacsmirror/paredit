#!/bin/sh

set -eu

exec emacs --batch --eval '(progn
  (byte-compile-file "paredit.el" t)
  (byte-compile-file "test.el" t)
)'
