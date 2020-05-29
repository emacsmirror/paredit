#!/bin/sh

set -eu

: ${EMACS:=emacs}

exec ${EMACS} -q --no-site-file --batch --eval '(progn
  (byte-compile-file "paredit.el" t)
  (byte-compile-file "test.el" t)
)'
