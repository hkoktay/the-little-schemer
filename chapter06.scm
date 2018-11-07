;; Chapter 6: Shadows

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; add1: number -> number
(define add1
  (lambda (n)
    (+ n 1)))

;; sub1: number -> number
(define sub1
  (lambda (n)
    (- n 1)))

;; o+: number number -> number
;; Returns the addition of 'n' and 'm'
;;
;; Page 60
(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(test "o+"
      (o+ 2 7) 9)

(test "o+"
      (o+ 2 0) 2)

;; o*: [number] [number] -> [number]
;; Returns the result of the multiplication of 'n' and 'm'.
;;
;; Page 65
(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

(test "o*"
      (o* 12 3) 36)

(test "o*"
      (o* 12 0) 0)

(test "o*"
      (o* 0 3) 0)

;; oexpt: number number -> number
;; Return n^m
;; 
;; Page 74
(define oexpt
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (o* n (oexpt n (sub1 m)))))))

(test "oexpt"
      (oexpt 1 1) 1)

(test "oexpt"
      (oexpt 2 3) 8)

(test "oexpt"
      (oexpt 5 3) 125)

(test "oexpt"
      (oexpt 5 0) 1)

(test "oexpt"
      (oexpt 0 2) 0)

;; numbered?: [aexp] -> boolean
;; Returns #t if the argument 'aexp' represents an [aexp].
;;
;; Page 101
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) 'o+)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'o*)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'oexpt)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     (else
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

(test "numbered?"
      (numbered? 1) #t)

(test "numbered?"
      (numbered? '(3 + (4 oexpt 5))) #t)

(test "numbered?"
      (numbered? '(2 o* sausage)) #f)

;; numbered?: [aexp] -> boolean
;; 1. revision of numbered?
;;
;; Page 101
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

(test "numbered? - 1.revision"
      (numbered? 1) #t)

(test "numbered? - 1.revision"
      (numbered? '(3 + (4 oexpt 5))) #t)

(test "numbered? - 1.revision"
      (numbered? '(2 o* sausage)) #f)

;; value: [aexp] -> [number]
;; Returns the natural value of an arithmetic expression.
;;
;; Page 103
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) 'o+)
      (o+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'o*)
      (o* (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     (else
      (oexpt (value (car nexp))
             (value (car (cdr (cdr nexp)))))))))

(test "value"
      (value '(1 o+ 3)) 4)

(test "value"
      (value '(1 o+ (3 oexpt 4))) 82)

;; value: [aexp] -> [number]
;; Returns the natural value of an arithmetic expression.
;;
;; This time an [aexp] is represented as an
;; - [atom] or
;; - (list [a-operator] [aexp] [aexp])
;;
;; But this version has a bug.
;; 1. revision of value
;; Page 103
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) 'o+)
      (o+ (value (car (cdr nexp)))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) 'o*)
      (o* (value (car (cdr nexp)))
          (value (car (cdr (cdr nexp))))))
     (else
      (oexpt (value (car (cdr nexp)))
             (value (car (cdr (cdr nexp)))))))))

(test "value - 1.revision"
      (value '(o+ 1 3)) 4)

(test "value - 1.revision"
      (value '(o+ 1 (oexpt 3  4))) 82)

;; first-sub-exp: [aexp] -> [aexp]
;; 
;; Changed '1st-sub-exp' to 'first-sub-exp' because r6rs does not
;; permit identifier names beginning with a digit.
;; 
;; Page 105
(define first-sub-exp
  (lambda (aexp)
    (cond
     (else (car (cdr aexp))))))

(test "first-sub-exp"
      (first-sub-exp '(o+ 1 3)) 1)

;; first-sub-exp: [aexp] -> [aexp]
;; 1. revision of first-sub-exp
;; 
;; Page 105
(define first-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(test "first-sub-exp - 1.revision"
      (first-sub-exp '(o+ 1 3)) 1)

;; second-sub-exp: [aexp] -> [aexp]
;; Changed '2nd-sub-exp' to 'second-sub-exp' because r6rs does not
;; permit identifier names beginning with a digit.
;;
;; Page 106
(define second-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(test "second-sub-exp"
      (second-sub-exp '(1 o+ 3)) 3)

(test "second-sub-exp"
      (second-sub-exp '(o+ 1 3)) 3)

;; operator: [aexp] -> [s-exp]
;;
;; Page 106
(define operator
  (lambda (aexp)
    (car aexp)))

(test "operator"
      (operator '(o+ 1 3)) 'o+)

;; value: [aexp] -> [number]
;; 2. revision of value
;;
;; Page: 106
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) 'o+)
      (o+ (value (first-sub-exp nexp))
          (value (second-sub-exp nexp))))
     ((eq? (operator nexp) 'o*)
      (o* (value (first-sub-exp nexp))
          (value (second-sub-exp nexp))))
     (else
      (oexpt (value (first-sub-exp nexp))
             (value (second-sub-exp nexp)))))))

(test "value - 2.revision"
      (value '(o+ 1 3)) 4)

(test "value - 2.revision"
      (value '(o+ 1 (oexpt 3  4))) 82)

;; first-sub-exp: [aexp] -> [aexp]
;; This version works on the previous [aexp] abstract data type.
;; 2. revision of first-sub-exp
;;
;; Example:
;; (first-sub-exp '(2 o+ 5))
;; -> 2
;; 
;; Page 105
(define first-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(test "first-sub-exp - 2.revision"
      (first-sub-exp '(o+ 2 5)) 2)

;; operator: [aexp] -> [a-operator]
;; This version works on the previous [aexp] abstract data type.
;; 1. revision of operator
;;
;; Example:
;; (operator '(2 o* 3)
;; -> o*
;;
;; Page 106
(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(test "operator - 1.revision"
      (operator '(1 o+ 3)) 'o+)

(test "operator - 1.revision"
      (operator '(1 o* 3)) 'o*)

;; sero?: [ln] -> boolean
;;
;; Page 108
(define sero?
  (lambda (n)
    (null? n)))

(test "sero?"
      (sero? '()) #t)

(test "sero?"
      (sero? '(())) #f)

;; edd1: [ln] -> [ln]
;;
;; Page 108
(define edd1
  (lambda (n)
    (cons '() n)))

(test "edd1"
      (edd1 '()) '(()))

(test "edd1"
      (edd1 '(())) '(() ()))

;; zub1: [ln] -> boolean
;;
;; Page 108
(define zub1
  (lambda (n)
    (cdr n)))

(test "zub1"
      (zub1 '(())) '())

(test "zub1"
      (zub1 '(() ())) '(()))

;; o+: [ln] [ln] -> [ln]
;;
;; This version of 'o+' works not with the data type [number] but with
;; [ln].
;;
;; Example:
;; (o+ '(()()) '(()()()))
;; -> (() () () () ())
;;
;; Page 108
(define o+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (o+ n (zub1 m)))))))

(test "o+"
      (o+ '(() ()) '(() () ())) '(() () () () ()))
