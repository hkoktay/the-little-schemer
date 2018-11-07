;; Chapter 8: Lambda the Ultimate

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; add1: number -> number
;; Chapter 4
(define add1
  (lambda (n)
    (+ n 1)))

;; sub1: number -> number
;; Chapter 4
(define sub1
  (lambda (n)
    (- n 1)))

;; o+: number number -> number
;; Returns the addition of 'n' and 'm'
;;
;; Page 60, Chapter 4
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
;; Page 61, Chapter 4
(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(test "o-"
      (o- 5 2)  3)

(test "o-"
      (o- 2 5)  -3)

;; o*: [number] [number] -> [number]
;; Returns the result of the multiplication of 'n' and 'm'.
;;
;; Page 65, Chapter 4
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

;; o<: number number -> boolean
;; Returns #t if n is smaller than m, else #f-
;;
;; Page 73, Chapter 4
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
;; Page 75, Chapter 4
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

;; rember-f: fun [atom] [lat] -> [lat]
;; Page 126
(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     (else (cond
            ((test? (car l) a) (cdr l))
            (else (cons (car l)
                    (rember-f test? a (cdr l)))))))))

(test "rember-f"
      (rember-f = 5 '(6 5 2 3))
      '(6 2 3))

(test "rember-f"
      (rember-f eq? 'jelly '(jelly beans are good))
      '(beans  are good))

(test "rember-f"
      (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
      '(lemonade and (cake)))

;; rember-f: fun [atom] [lat] -> [lat]
;; 1. revision of rember-f
;; Page 126
(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? (car l) a) (cdr l))
     (else (cons (car l)
             (rember-f test? a (cdr l)))))))

(test "rember-f - 1.revision"
      (rember-f = 5 '(6 5 2 3))
      '(6 2 3))

(test "rember-f - 1.revision"
      (rember-f eq? 'jelly '(jelly beans are good))
      '(beans  are good))

(test "rember-f - 1.revision"
      (rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))
      '(lemonade and (cake)))

;; eq?-c: [atom] -> fun
;; Page 127
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(test "eq?-c"
      ((eq?-c 'x) 'x) #t)

(test "eq?-c"
      ((eq?-c 'x) 'b) #f)

;; eq?-salad: [atom] -> boolean
;; Page 128
(define eq?-salad (eq?-c 'salad))

(test "eq?-salad"
      (eq?-salad 'salad) #t)

(test "eq?-salad"
      (eq?-salad 'tuna) #f)

;; rember-f: fun -> fun
;; Returns a procedure which returns a [listof sexp] where  atom 'a'
;; is removed if 'test?' is #t.
;; 2. revision of rember-f
;;
;; Page 129
(define rember-f
  (lambda (test?)
    ;; [atom] [listof sexp] -> [listof sexp]
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(test "rember-f - 2.revision"
      ((rember-f =) 5 '(6 5 2 3))
      '(6 2 3))

(test "rember-f - 2.revision"
      ((rember-f eq?) 'jelly '(jelly beans are good))
      '(beans  are good))

(test "rember-f - 2.revision"
      ((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))
      '(lemonade and (cake)))

(test "rember-f - 2.revision"
      ((rember-f eq?) 'tuna '(shrimp salad and tuna salad))
      '(shrimp salad and salad))

(test "rember-f - 2.revision"
      ((rember-f eq?) 'eq? '(equal? eq? equan? eqlist? eqpair?))
      '(equal? equan? eqlist? eqpair?))

;; rember-eq?: [atom] [lat] -> boolean
;; Page 129
(define rember-eq? (rember-f eq?))

(test "rember-eq?"
      (rember-eq? 'tuna '(tuna salad is good))
      '(salad is good))

(test "rember-eq?"
      (rember-eq? 'x '(a b x c d))
      '(a b c d))

;; insertL-f: fun -> fun
;; Page 130
(define insertL-f
  (lambda (test?)
    ;; [atom] [atom] [listof s-exp] -> [listof s-exp]
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old) (cons new (cons old (cdr l))))
       (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(test "insertL-f"
      ((insertL-f =) 2 0 '(1 2 0 3 4 0))
      '(1 2 2 0 3 4 0))

;; insertR-f: fun -> fun
;; Page 130
(define insertR-f
  (lambda (test?)
    ;; [atom] [atom] [listof s-exp] -> [listof s-exp]
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old) (cons old (cons new (cdr l))))
       (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(test "insertR-f"
      ((insertR-f =) 7 0 '(1 2 0 6 5 0))
      '(1 2 0 7 6 5 0))

;; seqL: [atom] [atom] [listof s-exp] -> fun
;; Page 131
(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(test "seqL"
      (seqL 'x 'a '(b c))
      '(x a b c))

;; seqR: [atom] [atom] [listof s-exp] -> fun
;; Page 131
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(test "seqR"
      (seqR 'x 'a '(b c))
      '(a x b c))

;; insert-g: fun -> fun
;; Page 132
(define insert-g
  (lambda (seq)
    ;; [atom] [atom] [listof s-exp] -> [listof s-exp]
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old) (seq new old (cdr l)))
       (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(test "insert-g with seqL"
      ((insert-g seqL) 'x 'o '(a b o c d o f))
      '(a b x o c d o f))

(test "insert-g with seqR"
      ((insert-g seqR) 'x 'o '(a b o c d o f))
      '(a b o x c d o f))


;; insertRg: [atom] [atom] [listof s-exp] -> [listof s-exp]
;; Page 132
(define insertRg (insert-g seqR))

(test "insertRg"
      (insertRg 'x 'o '(a b o c d o f))
      '(a b o x c d o f))

;; insertLg: [atom] [atom] [listof s-exp] -> [listof s-exp]
;; Page 132
(define insertLg (insert-g
                    (lambda (new old l)
                      (cons new (cons old l)))))

(test "insertLg"
      (insertLg 'x 'o '(a b o c d o f))
      '(a b x o c d o f))

;; subst: [atom] [atom] [listof s-exp] -> [listof s-exp]
;; Returns a new [listof s-exp] with [atom] 'old' replaced with [atom]
;; 'new'
;;
;; Page 133
(define subst
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((eq? (car l) old) (cons new (cdr l)))
     (else (cons (car l) (subst new old (cdr l)))))))

(test "subst"
      (subst 'x 'o '(a b o c d o))
      '(a b x c d o))

;; seqS: [atom] [atom] [listof s-exp] -> [listof s-exp]
;; Page 133
(define seqS
  (lambda (new old l)
    (cons new l)))

(test "seqS"
      (seqS 'a 'b '(x y z))
      '(a x y z))

;; subst: [atom] [atom] [listof s-exp] -> [listof s-exp]
;; subst implemented with insert-g and seqS
;; 1. revision of subst
;;
;; Page 133
(define subst (insert-g seqS))

(test "subst - 1.revision"
      (subst 'x 'o '(a b o c d o))
      '(a b x c d o))

;; segrem: [atom] [atom] [listof s-exp] -> [listof s-exp]
;; Page 133
(define seqrem
  (lambda (new old l) l))

(test "seqrem"
      (seqrem 'a 'b '(x y z))
      '(x y z))

;; rember: [atom] [listof s-exp] -> [listof s-exp]
;; rember implemented with insert-g.
;;
;; Page 133
(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(test "rember" (rember 'a '(a b c)) '(b c))
(test "rember" (rember 'a '(b a c)) '(b c))
(test "rember" (rember 'a '(c b a)) '(c b))
(test "rember" (rember 'a '(a b a)) '(b a))
(test "rember" (rember 'a '(x y z)) '(x y z))
(test "rember" (rember 'a '(b (a) c)) '(b (a) c))
(test "rember" (rember 'a '()) '())

;; first-sub-exp: [aexp] -> [aexp]
;; Page 105, Chapter 6
(define first-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(test "first-sub-exp"
      (first-sub-exp '(o+ 1 3)) 1)

;; second-sub-exp: [aexp] -> [aexp]
;; Page 106, Chapter 6
(define second-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(test "second-sub-exp"
      (second-sub-exp '(o+ 1 3)) 3)

;; operator: [aexp] -> [s-exp]
;; Page 106, Chapter 6
(define operator
  (lambda (aexp)
    (car aexp)))

(test "operator"
      (operator '(o* 1 3)) 'o*)

;; value: [aexp] -> [number]
;; Page 134
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

(test "value"
      (value '(o+ 1 3)) 4)

(test "value"
      (value '(o+ 1 (oexpt 3 4))) 82)

;; atom-to-function: [atom] -> fun
;; Page 134
(define atom-to-function
  (lambda (a)
    (cond
     ((eq? a 'o+) o+)
     ((eq? a 'o*) o*)
     (else oexpt))))

(test "atom-to-function"
      (atom-to-function 'o*) o*)

(test "atom-to-function"
      (atom-to-function 'o+) o+)

;; value: [aexp] -> [number]
;; 1. revision of value
;; Page 135
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
            (value (first-sub-exp nexp))
            (value (second-sub-exp nexp)))))))

(test "value - 1.revision"
      (value '(o+ 1 3)) 4)

(test "value - 1.revision"
      (value '(o+ 1 (oexpt 3 4))) 82)

;; multirember: [atom] [lat] -> [lat]
;; Page 135
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(test "multirember"
      (multirember 'a '())
      '())

(test "multirember"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; multirember-f: fun -> fun
;; Page 135
(define multirember-f
  (lambda (test?)
    ;; [atom] [lat] -> [lat]
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))


(test "multirember-f"
      ((multirember-f eq?) 'a '())
      '())

(test "multirember-f"
      ((multirember-f eq?) 'x '(x a b x c d x))
      '(a b c d))

(test "multirember-f"
      ((multirember-f eq?) '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

(test "multirember-f"
      ((multirember-f equal?) '(a b) '(x y (a b) z (a b)))
      '(x y z))

;; multirember-eq?: [atom] [lat] -> [lat]
;; Page 136
(define multirember-eq? (multirember-f eq?))

(test "multirember-eq?"
      (multirember 'a '())
      '())

(test "multirember-eq?"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember-eq?"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; eq?-tuna: [atom] -> boolean
;; Page 136
(define eq?-tuna (eq?-c 'tuna))

(test "eq?-tuna"
      (eq?-tuna 'tuna) #t)

(test "eq?-tuna"
      (eq?-tuna 'pie) #f)

;; multiremberT: func [lat] -> [lat]
;; Page 137
(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat)) (multiremberT test? (cdr lat)))
     (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(test "multiremberT"
      (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))
      '(shrimp salad salad and))

;; multirember&co: [atom] [lat] k -> [lat]
;; col is short for "collector". A collector is sometimes called a
;; "continuation".
;; Page 137
(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a (cdr lat) (lambda (newlat seen)
                                    (col newlat (cons (car lat) seen)))))
     (else
      (multirember&co a (cdr lat) (lambda (newlat seen)
                                    (col (cons (car lat) newlat) seen)))))))

;; a-friend: any any -> boolean
;; Page 138
(define a-friend
  (lambda (x y)
    (null? y)))

(test "a-friend"
      (a-friend '(a b c) '()) #t)

(test "a-friend"
      (a-friend '(a b) '(x)) #f)

(test "multirember&co"
      (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)
      #f)

(test "multirember&co"
      (multirember&co 'tuna '(tuna) a-friend)
      #f)

;; new-friend: [lat] [s-exp] -> boolean
;; Page 139
(define new-friend
  (lambda (newlat seen)
    (a-friend newlat (cons '(tuna) seen))))

(test "new-friend"
      (new-friend '(a b c) '(x y z))
      #f)

(test "new-friend"
      (new-friend '(a b c) '(x y z))
      #f)

(test "new-friend"
      (new-friend '(a b c) '())
      #f)

;; latest-friend: [lat] [lat] -> boolean
;; Page 139
(define latest-friend
  (lambda (newlat seen)
    (a-friend (cons 'and newlat) seen)))

(test "latest-friend"
      (latest-friend '(a b c) '(x y))
      #f)

;; last-friend: [lat] [lat] -> [number]
;; Page 140
(define last-friend
  (lambda (x y)
    (length x)))

(test "last-friend"
      (last-friend '(a b c) '(x y z)) 3)

(test "last-friend"
      (last-friend '() '(x y z)) 0)

;; multiinsertL: [atom] [atom] [lat] -> [lat]
;; Page 141
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new
                            (cons old
                              (multiinsertL new old (cdr lat)))))
     (else (cons (car lat)
             (multiinsertL new old (cdr lat)))))))

(test "multiinsertL"
      (multiinsertL 'x 'o '(a b o c d o))
      '(a b x o c d x o))

;; multiinsertR: [atom] [atom] [lat] -> [lat]
;; Page 141
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons old
                            (cons new
                              (multiinsertR new old (cdr lat)))))
     (else (cons (car lat)
             (multiinsertR new old (cdr lat)))))))

(test "multiinsertR"
      (multiinsertR 'x 'o '(a b o c d o))
      '(a b o x c d o x))

;; multiinsertLR: [atom] [atom] [atom] [lat] -> [lat]
;; Returns a [lat] with [atom] 'new' inserted to the left of
;; 'oldL' and to the right of 'oldR'
;;
;; Page 141
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(test "multiinsertLR"
      (multiinsertLR 'x 'o 'd '(a b o c d o))
      '(a b x o c d x x o))

;; multiinsertLR&co: [atom] [atom] [atom] [lat] k -> [lat]
;; Example:
;; (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) list)
;; -> ((chips salty and salty fish or salty fish and chips salty) 2 2)
;; (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (x y z) (list y z)))
;; -> (2 2)
;; 
;; Page 134
(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new (cons oldL newlat)) (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR (cons new newlat)) L (add1 R)))))
     (else
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R) (col (cons (car lat) newlat) L R)))))))

(test "multiinsertLR&co"
      (multiinsertLR&co 'salty 'fish 'chips
                        '(chips and fish or fish and chips)
                        list)
      '((chips salty and salty fish or salty fish and chips salty) 2 2))

(test "multiinsertLR&co"
      (multiinsertLR&co 'salty 'fish 'chips
                        '(chips and fish or fish and chips)
                        (lambda (x y z) (list y z)))
      '(2 2))

;; even?: [number] -> boolean
;; Page 144
(define even?
  (lambda (n)
    (= (o* (oquotient n 2) 2) n)))

(test "even?"
      (even? 3) #f)

(test "even?"
      (even? 4) #t)

;; evens-only*: [sexp-of number] -> [sexp-of number]
;; Returns a [sexp-of number] with all odd numbers removed.
;; Page 144
(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(test "evens-only*"
      (evens-only* '(1 2 3 4 5 6 7 8))
      '(2 4 6 8))

(test "evens-only*"
      (evens-only* '(1 2 (3 4 (5 6)) 7 8))
      '(2 (4 (6)) 8))

;; evens-only*&co: [sexp-of number] k -> [sexp-of number] [number] [number]
;; Returns a [sexp-of number] with all odd numbers removed, the sum of
;; the odd numbers and the result of multiplying the even numbers.
;; 1.revision of evens-only*&co
;; Page 145
(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col (cons (car l) newl)
                               (o* (car l) p)
                               s))))
       (else
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col newl
                               p
                               (o+ (car l) s)))))))
     (else
      (evens-only*&co (car l)
                      (lambda (al ap as)
                        (evens-only*&co (cdr l)
                                        (lambda (dl dp ds)
                                          (col (cons al dl)
                                               (o* ap dp)
                                               (o+ as ds))))))))))

(test "evens-only*&co"
      (evens-only*&co '(1 2 3 4 5 6 7 8) list)
      '((2 4 6 8) 384 16))

;; [sexp-of number] [number] [number] -> [sexp-of number]
;; Page 146
(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))

(test "the-last-friend"
      (the-last-friend '(a b) '(x y) '(e f))
      '((e f) (x y) a b))

(test "evens-only*&co with the-last-friend"
      (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
      '(38 1920 (2 8) 10 (() 6) 2))

