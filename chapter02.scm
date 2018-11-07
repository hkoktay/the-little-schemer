;; Chapter 2: Do it, Do it Again, and Again, and Again...

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; lat?: [listof atoms] -> boolean
;; Returns #t if all elements in a list are atoms, otherwise returns #f.
;; Page 16
(define lat?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((atom? (car lat)) (lat? (cdr lat)))
     (else #f))))

(test "lat?" (lat? '(a b)) #t)
(test "lat?" (lat? '(1 2)) #t)
(test "lat?" (lat? '()) #t)
(test "lat?" (lat? '(a '(b))) #f)

;; member?: [atom] [list-of atoms] -> boolean
;; Returns #t if [atom] 'a' is a member of [list-of sexp].
;; Page 22
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(test "member?" (member? 'x '(a b c)) #f)
(test "member?" (member? 'x '(a x c)) #t)
(test "member?" (member? 'x '(x b c)) #t)
(test "member?" (member? 'x '(a b x)) #t)
(test "member?" (member? 'b '(a (b) c)) #f)
(test "member?" (member? 'x '()) #f)
