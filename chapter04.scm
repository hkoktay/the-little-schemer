;; Chapter 4: Numbers Games

(load "test-check.scm")

;; add1: number -> number
(define add1
  (lambda (n)
    (+ n 1)))

(test "add1"
      (add1 67) 68)

(test "add1"
      (add1 0) 1)

(test "add1"
      (add1 -4) -3)

;; sub1: number -> number
(define sub1
  (lambda (n)
    (- n 1)))

(test "sub1"
      (sub1 10) 9)

(test "sub1"
      (sub1 -4) -5)

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

;; o-: number number -> number
;; Returns the subtractions of 'n' and 'm'
;;
;; Page 61
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(test "o-"
      (o- 5 2)  3)

(test "o-"
      (o- 2 5)  -3)

;; addtup: [tup] -> number
;; Page 64
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

(test "addtup"
      (addtup '(3 5 2 8))
      18)

(test "addtup"
      (addtup '(15 6 7 12 3))
      43)

(test "addtup with empty list"
      (addtup '())
      0)

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

;; tup+: [tup] [tup] -> [tup]
;; Adds the numbers of two tups of the same length and conses the
;; result into a list.
;; Works only for lists of the same length.
;; 
;; Page 69
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     (else (cons (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

(test "tup+"
      (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
      '(11 11 11 11 11))

;; This test expression returns an error, because when the end of 'tup2' is
;; reached 'tup+' tries to get the car of the empty list. The procedure test-exception catches the error 
;; and returns #t ,
(test "tup+"
      (test-exception
       (lambda ()
         (tup+ '(3 6 9 11 4) '(8 5 2))))
      #t)

;; This expression throws an error, because when the end of 'tup1' is reached
;; 'tup+' tries to get the car of the empty list.
(test "tup+"
      (test-exception
       (lambda ()
         (tup+ '(3 6 9) '(8 5 2 0 7))))
      #t)

(test "tup+"
      (tup+ '() '())
      '())

;; tup+: [tup] [tup] -> [tup]
;; Like tup+0 but works also on tups with different length.
;; 1. revision of tup+
;;
;; Examples:
;; (top+ '(2 5 6) '(6 3 2)) -> (8 8 8)
;; (top+ '(2 5 6) '(6 3)) -> (8 8 6)
;; 
;; Page 71
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) '())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

(test "tup+ - 1.revision"
      (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
      '(11 11 11 11 11))

(test "tup+ - 1.revision"
      (tup+ '(3 6 9 11 4) '(8 5 2))
      '(11 11 11 11 4))

(test "tup+ - 1.revision"
      (tup+ '(3 6 9) '(8 5 2 0 7))
      '(11 11 11 0 7))

(test "tup+ - 1.revision"
      (tup+ '(3 6 9 11 4) '())
      '(3 6 9 11 4))

(test "tup+ - 1.revision"
      (tup+ '() '(8 5 2 0 7))
      '(8 5 2 0 7))

(test "tup+ - 1.revision"
      (tup+ '() '())
      '())

;; 2. revision of tup+
;; Page 71
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

(test "tup+ - 2.revision"
      (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
      '(11 11 11 11 11))

(test "tup+ - 2.revision"
      (tup+ '(3 6 9 11 4) '(8 5 2))
      '(11 11 11 11 4))

(test "tup+ - 2.revision"
      (tup+ '(3 6 9) '(8 5 2 0 7))
      '(11 11 11 0 7))

(test "tup+ - 2.revision"
      (tup+ '(3 6 9 11 4) '())
      '(3 6 9 11 4))

(test "tup+ - 2.revision"
      (tup+ '() '(8 5 2 0 7))
      '(8 5 2 0 7))

(test "tup+ - 2.revision"
      (tup+ '() '())
      '())

;; o>: number number -> boolean
;; Returns #t if n is bigger than m, else #f. This version is not
;; correct. It fails when 'n' and 'm' are equal.
;;
;; Page 72
(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(test "o>"
      (o> 4 2)
      #t)

(test "o>"
      (o> 2 4)
      #f)

(test "o>"
      (o> 3 3)
      #f)

;; o>: number number -> boolean
;; Correct version. Note: o> works only on the defined [number]
;; datatype. A [number] is either 0 or (add1 n).
;; 1. revision of o>
;;
;; Page 73
(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(test "o> - 1.revison"
      (o> 4 2)
      #t)

(test "o> - 1.revison"
      (o> 2 4)
      #f)

(test "o> - 1.revision"
      (o> 3 3)
      #f)

;; o<: number number -> boolean
;; Returns #t if n is smaller than m, else #f-
;;
;; Page 73
(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))

(test "o<"
      (o< 2 4)
      #t)

(test "o<"
      (o< 4 2)
      #f)

(test "o<"
      (o< 2 2)
      #f)

;; o=: number number -> boolean
;; Returns #t when 'n' and 'm' are equal, otherwise returns #f.
;; 
;; Page 74
(define o=
  (lambda (n m)
    (cond
     ((zero? n) (zero? m))
     ((zero? n) #f)
     (else (o= (sub1 n) (sub1 m))))))

(test "o="
      (o= 1 1)
      #t)

(test "o="
      (o= 1 2)
      #f)

(test "o="
      (o= 2 1)
      #f)

;; o= number number -> boolean
;; 1. revision of o=
;;
;; Page 74
(define o=
  (lambda (n m)
    (cond
     ((o> n m) #f)
     ((o< n m) #f)
     (else #t))))

(test "o= - 1.revision"
      (o= 1 1)
      #t)

(test "o= - 1.revision"
      (o= 1 2)
      #f)

(test "o= - 1.revision"
      (o= 2 1)
      #f)

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

;; oquotient: number number -> number
;; Division of 'n' and 'm'.
;; Example:
;; (oquotient 15 4) -> 3
;; 
;; Page 75
(define oquotient
  (lambda (n m)
    (cond
     ((o< n m) 0)
     (else (add1 (oquotient (o- n m) m))))))

(test "oquotient"
      (oquotient 15 4)
      3)

(test "oquotient"
      (oquotient 15 4)
      3)

(test "oquotient"
      (oquotient 1 1)
      1)

;; length: [lat] -> number
;; Returns the length of 'lat'.
;; 
;; Page 76
(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(test "length"
      (length '(a b c)) 3)

(test "length"
      (length '()) 0)

(test "length"
      (length '(a)) 1)

(test "length"
      (length '(a (b c) (d))) 3)

;; pick: number [lat] -> [atom]
;; Returns the 'n'-th atom of 'lat'.
;; Example:
;; (pick 2 '(a b c d e)) -> b
;;
;; Page 76
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(test "pick"
      (pick 4 '(lasagne spaghetti ravioli macaroni meatball))
      'macaroni)

(test "pick"
      (pick 5 '(lasagne spaghetti ravioli macaroni meatball))
      'meatball)

;; rempick: number [lat] -> [lat]
;; Returns a lat with 'n'-th atom removed.
;; Example:
;; (rempick 2 '(a b c d e)) -> (a c d e)
;;
;; Page 77
(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(test "rempick"
      (rempick 3 '(hotdogs with hot mustard))
      '(hotdogs with mustard))

;; no-nums: [lat] -> [lat]
;; Returns a [lat] with all numbers removed.
;;
;; Page 77
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (no-nums (cdr lat)))
       (else (cons (car lat) (no-nums (cdr lat)))))))))

(test "no-nums"
      (no-nums '(5 pears 6 prunes 9 dates))
      '(pears prunes dates))

;; no-nums: [lat] -> [lat]
;; 1. revision of no-nums
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(test "no-nums - 1.revision"
      (no-nums '(5 pears 6 prunes 9 dates))
      '(pears prunes dates))

;; all-nums: [lat] -> [tup]
;; Returns a [tup] with all numbers from 'lat'.
;; Page 78
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
       (else (all-nums (cdr lat))))))))

(test "all-nums"
      (all-nums '(5 pears 6 prunes 9 dates))
      '(5 6 9))

;; all-nums: [lat] -> [tup]
;; 1. revision of all-nums
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(test "all-nums - 1.revision"
      (all-nums '(5 pears 6 prunes 9 dates))
      '(5 6 9))

;; equan?: [atom] [atom] -> boolean
;; Returns #t if 'a1' and 'a2' are the same atom
;;
;; Page 78
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2) #f))
     (else (eq? a1 a2)))))

(test "eqan?"
      (eqan? 2 3) #f)

(test "eqan?"
      (eqan? 2 2) #t)

(test "eqan?"
      (eqan? 'a 'a) #t)

(test "eqan?"
      (eqan? 'a 'b) #f)

;; occur: [atom] [lat] -> number
;; Counts how many times atom 'a' occurs in 'lat'
;; Page 78
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eq? (car lat) a) (add1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))

(test "occur"
      (occur 'x '(a b c d e))
      0)

;; occur: [atom] [lat] -> number
;; 1. revision of occur
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? (car lat) a) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(test "occur - 1.revision"
      (occur 'x '(a b c d e))
      0)

;; one?: number -> boolean
;; Returns #t if 'n' is 1, else #f
;; Page 79
(define one?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (zero? (sub1 n))))))

(test "one?"
      (one? 3) #f)

(test "one?"
      (one? 0) #f)

(test "one?"
      (one? 1) #t)

;; one?: number -> boolean
;; 1. revision of one?
;; Page 79
(define one?
  (lambda (n)
    (cond
     (else (= n 1)))))

(test "one? - 1.revision"
      (one? 3) #f)

(test "one? - 1.revision"
      (one? 0) #f)

(test "one? - 1.revision"
      (one? 1) #t)

;; one?: number -> boolean
;; 2. revision of one?
;; Page 79
(define one?
  (lambda (n)
    (= n 1)))

(test "one? - 2.revision"
      (one? 3) #f)

(test "one? - 2.revision"
      (one? 0) #f)

(test "one? - 2.revision"
      (one? 1) #t)
