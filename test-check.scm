;; Tests if the computed result matches the expected result.
;; Example:
;; (test "+" (+ 1 2) 3)
;; ->
;; Testing +
;; #t
;;
;; (test "+" (+ 1 2) 2)
;; ->
;; Testing +
;; Failed: (+ 1 2)
;; Expected:   2
;; Computed:   3
;; 
;; test: s-exp s-exp -> void
(define-syntax test 
  (syntax-rules ()
    ((_ title tested-expression expected-result)
       (let ((expected expected-result)
             (produced tested-expression))
         (begin
           (display "Testing ")
           (display title)
           (newline))
         (or (equal? expected produced)
             (begin
               (display "Failed: ")
               (write 'tested-expression)
               (newline)
               (display "Expected:   ")
               (write expected)
               (newline)
               (display "Computed:   ")
               (write produced)
               (newline)))))))


;; Returns #t if its argument, a thunk, throws an exception.
;;
;; test-exception: procedure? -> #t | any
(define test-exception
  (lambda (thunk)
    (call/cc
     (lambda (k)
       (with-exception-handler
        (lambda (x) (k #t))
        thunk)))))
