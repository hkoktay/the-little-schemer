#! /usr/local/bin/scheme --program

(import (chezscheme))

;; load-test: string? -> void
;; 
;; We need this procedure because we have to reset the interaction environment
;; before loading another source file to prevent unexpected test results. The
;; reason is that we revise and thus redefine some procedures in later chapters.
(define load-test
  (lambda (t)
    (begin
      (interaction-environment (copy-environment (scheme-environment) #t))
      (load t))))

;; load-and-test: listof-string? -> void
(define load-and-test
  (lambda (l)
    (for-each (lambda (f) (load-test f)) l)))


(load-and-test
 '("chapter01.scm"
   "chapter02.scm"
   "chapter03.scm"
   "chapter04.scm"
   "chapter05.scm"
   "chapter06.scm"
   "chapter07.scm"
   "chapter08.scm"
   "chapter09.scm"
   "chapter10.scm"))
