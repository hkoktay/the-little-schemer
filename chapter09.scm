;; Chapter 9: ...and Again, and Again and Again,...
;; 
;; A [pora] is an
;; - atom or
;; - pair
;;
;; A [sorn] is a
;; - symbol or
;; - [number]

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; add1: [number] -> [number]
;; Chapter 4
(define add1
  (lambda (n)
    (+ n 1)))

;; sub1: [number] -> [number]
;; Chapter 4
(define sub1
  (lambda (n)
    (- n 1)))

;; o+: [number] [number] -> [number]
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

;; pick: [number] [lat] -> [atom]
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

(test "pick"
      (pick 6 '(6 2 4 cavier 5 7 3)) 7)

(test "pick"
      (pick 7 '(6 2 4 cavier 5 7 3)) 3)

;; keep-looking: [sorn] -> boolean
;; Uses the numbers in 'lat' as indices to recur over 'lat'. 
;; This is an unnatural recursion because keep-looking does not recur
;; on a part of lat.
;;
;; Page 150
(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

(test "keep-looking"
      (keep-looking 'cavier 3 '(6 2 4 cavier 5 7 3)) #t)

(test "keep-looking"
      (keep-looking 'cavier 3 '(6 2 4 cavier 5 7 3)) #t)

;; looking: [atom] [lat] -> boolean
;; looking is a partial function. The functions we have seen so far
;; are called total functions.
;;
;; Page 149
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(test "looking"
      (looking 'cavier '(6 2 4 cavier 5 7 3)) #t)

(test "looking"
      (looking 'cavier '(6 2 grits cavier 5 7 3)) #f)

;; Never ending loop
;; (test "looking"
;;       (looking 'cavier '(7 1 2 cavier 5 6 3)) 'does-not-stop)


;; eternity: any -> any
;; eternity is the "most" partial function. It reaches its goal for
;; none of its arguments.
(define eternity
  (lambda (x)
    (eternity x)))

;; build: [s-exp] [s-exp] -> [pair]
;; Page 119, Chapter 7
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(test "build"
      (build 1 2) '(1 2))

;; first: [pair] -> [s-exp]
;; Page 119, Chapter 7
(define first
  (lambda (p)
    (car p)))

(test "first"
      (first '(1 2)) 1)

;; second: [pair] -> [s-exp]
;; Page 119, Chapter 7
(define second
  (lambda (p)
    (car (cdr p))))

(test "second"
      (second '(1 2)) 2)

;; shift: (list [pair] [s-exp]) -> [pair]
;; Takes a (list [pair] [s-exp])  whose first component is a pair and makes a new
;; pair by shifting the second part of the first component into the
;; second component.
;; 
;; Examples:
;; (shift '((a b) (c d)))
;; -> (a (b (c d)))
;; (shift '((a b) c))
;; -> (a (b c))
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(test "shift"
      (shift '((a b) c)) '(a (b c)))

(test "shift"
      (shift '((a b) (c d))) '(a (b (c d))))

;; a-pair?: any -> boolean
;; Page 118, Chapter 7
(define a-pair?
  (lambda (l)
    (cond
     ((atom? l) #f)
     ((null? l) #f)
     ((null? (cdr l))#f)
     ((null? (cdr (cdr l))) #t)
     (else #f))))

(test "a-pair?"
      (a-pair? '(pear pear)) #t)

(test "a-pair?"
      (a-pair? '(3 7)) #t)

(test "a-pair?"
      (a-pair? '((2) (pair))) #t)

(test "a-pair?"
      (a-pair? '()) #f)

(test "a-pair?"
      (a-pair? '()) #f)

(test "a-pair?"
      (a-pair? 'a) #f)

;; align: [pora] -> [pair]
;; Recursivly 'shifts' pairs in 'pora.
;; In the second cond-line shift creates an argument for align that
;; is not a part of the orignal argument, so it is not guaranteed
;; that align makes progress. This violates the seventh commandment.
;; Align is not a partial function because it yields a value for
;; every argument.
(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora)) (align (shift pora)))
     (else
      (build (first pora) (align (second pora)))))))

;; length*: [pora] -> [number]
;; Page 153
(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (o+ (length* (first pora))
          (length* (second pora)))))))

(test "length*"
      (length* '(a b)) 2)

(test "length*"
      (length* 'a) 1)

(test "length*"
      (length* '(a (b c))) 3)

;; weight*: [pora] -> [number]
;; Examples:
;; (weight* '((a b) c))
;; -> 7
;; (weight* '(a (b c)))
;; -> 5
;;
;; Page 154
(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (o+ (o* (weight* (first pora)) 2)
          (weight* (second pora)))))))

(test "weight*"
      (weight* '((a b) c)) 7)

(test "weight*"
      (weight* '(a (b c))) 5)

;; revpair: [pair] -> [pair]
;; Returns a pair with its elements reversed
;;
;; Page 121, Chapter 7
(define revpair
  (lambda (p)
    (build (second p) (first p))))

(test "revpair"
      (revpair '((a b) (c d))) '((c d) (a b)))

;; shuffle: [pora] -> [pora]
;; Like align but implemented with revpair.
;; 
;; Examples:
;; (shuffle '(a (b c))) -> (a (b c))
;;
;; Page 154
(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora)) (shuffle (revpair pora)))
     (else
      (build (first pora) (shuffle (second pora)))))))

(test "shuffle"
      (shuffle '(a (b c))) '(a (b c)))

(test "shuffle"
      (shuffle '(a b)) '(a b))

;; one?: [number] -> boolean
;; Returns #t if 'n' is 1, else #f
;; Page 79, Chapter 4
(define one?
  (lambda (n)
    (= n 1)))

(test "one?"
      (one? 3) #f)

(test "one?"
      (one? 0) #f)

(test "one?"
      (one? 1) #t)

;; o-: [number] [number] -> [number]
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

;; o<: [number] [number] -> boolean
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
      (o< 2 4) #t)

(test "o<"
      (o< 4 2) #f)

(test "o<"
      (o< 2 2) #f)

;; oquotient: [number] [number] -> [number]
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
      (oquotient 15 4) 3)

(test "oquotient"
      (oquotient 15 4) 3)

(test "oquotient"
      (oquotient 1 1) 1)

;; C: [number] -> void
;; Page 155
(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (oquotient n 2)))
       (else (C (add1 (o* 3 n)))))))))

;; [number] [number] -> void
;; Examples:
;; (A 1 0) -> 2
;; (A 1 1) -> 3
;; (A 2 2) -> 7
;; A is a total function because it gives always an answer.
;; (A 4 3) -> takes too long to compute
;;
;; Page 156
(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n) (A n (sub1 m)))))))

(test "A"
      (A 1 1) 3)

(test "A"
      (A 2 2) 7)

;; The Halting Problem
;; -------------------
;;
;; Page 157
;;
;; will-stop?: fun -> boolean
;; Note this is only a hypothetical function not a real one.
;; Returns #t if function 'f' will stop or #f if 'f' will never stop.
;; (define (will-stop? f)
;;   ...)
;;
;; Example:
;; (will-stop? eternity) -> would be #f
;; 
;; (define (last-try x)
;;   (and (will-stop? last-try)
;;        (eternity x)))
;;
;; Application of last-try:
;; (last-try '())
;;
;; The value of (last-try '()) depends on the expression:
;; (and (will-stop? last-try) (eternity '()))
;; And the value of this expression depends on the value of
;; (will-stop? last-try)
;;
;; The value of (will-stop? last-try) can either be #t or #f.
;; 1. (will-stop? last-try) -> #f
;;    => (and #f (eternity '()))
;;    => #f
;; 
;; Thus last-try stopped but this is in conflict with the value of
;; (will-stop? last-try) which was #f, which means that last-try
;; should *not* stop.
;; 
;; 2. (will-stop? last-try) -> #t
;;    => (and #t (eternity '()))
;;    => does not stop
;;
;; This is again in conflict with the result of (will-stop? last-try)
;; which should be #t, meaning that last-try should stop.
;;
;; Conclusion
;;
;; The function will-stop? cannot be defined. In other words you
;; cannot define a function which tests if a function will stop.


;; length0: [listof sexp] -> [number]
;; Determines the length of the empty list.
;; This function gives no answer for non-empty lists.
;; (lambda (l)
;;   (cond
;;    ((null? l) 0)
;;    (else (add1 (eternity (cdrl))))))
;;
;; You can try this with engines. Engines are a high-level process abstraction
;; supporting timed preemption. You can use engines to run a function a specific
;; amount of time. If the computation of the engine finishes before the "fuel"
;; runs out, it returns the amount of fuel left and the values of the
;; computation. If the fuel runs out (the number of ticks), it returns #t.  Thus
;; this test returns always #t, because the engine runs always out of ticks and
;; returns #t. No matter how much ticks the engine gets the computation never
;; finishes, because (eternity (cdr l) never finishes. Try it out by changing
;; the amount of fuel.
;;
;; You can get more information about engines in the chez scheme documentation:
;; https://cisco.github.io/ChezScheme/csug9.5/control.html#./control:h4
(test "length0"
      (letrec ((e (make-engine
                   (lambda ()
                     ((lambda (l)
                        (cond
                         ((null? l) 0)
                         ;; (eternity (cdr l)) runs forever so add1 is never
                         ;; called.
                         (else (add1 (eternity (cdr l))))))
                      '(1 2 3))))))
        ;; We supply the engine with 100 ticks
        (e 100 list (lambda (eng) #t)))
      #t)


(test "length0"
      ((lambda (l)
         (cond
          ((null? l) 0)
          (else (add1 (eternity (cdr l))))))
       '())
      0)

;; length<=1: [listof sexp] -> [number]
;; (lambda (l)
;;   (cond
;;    ((null? l) 0)
;;    (else
;;     (add1
;;      ;; length0
;;      ((lambda (l)
;;         (cond
;;          ((null? l) 0)
;;          (else (add1 (eternity (cdr l))))))
;;       (cdr l))))))

;; lenght<=2: [listof sexp] -> [number]
;; (lambda (l)
;;   (cond
;;    ((null? l) 0)
;;    (else
;;     (add1
;;      ;; length<=1
;;      ((lambda (l)
;;         (cond
;;          ((null? l) 0)
;;          (else
;;           (add1
;;            ;; length0
;;            ((lambda (l)
;;               (cond
;;                ((null? l) 0)
;;                (else
;;                 (add1 (eternity (cdr l))))))
;;             (cdr l))))))
;;       (cdr l))))))

;; length0: fun -> fun
;;  ((lambda (length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else (add1 (length (cdr l)))))))
;;   eternity)

;; length<=1: fun -> fun
;; ((lambda (f)
;;   (lambda (l)
;;     (cond
;;      ((null? l) 0)
;;      (else (add1 (f (cdr l)))))))
;;  ((lambda (g)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else (add1 (g (cdr l)))))))
;;   eternity))

;; length<=2: fun -> fun
;; ((lambda (length)
;;   (lambda (l)
;;     (cond
;;      ((null? l) 0)
;;      (else (add1 (length (cdr l)))))))
;;  ((lambda (length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else (add1 (length (cdr l)))))))
;;   ((lambda (length)
;;      (lambda (l)
;;        (cond
;;         ((null? l) 0)
;;         (else (add1 (length (cdr l)))))))
;;    eternity)))

;; mk-length, length0
;; ((lambda (mk-length)
;;    (mk-length eternity))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else (add1 (length (cdr l))))))))

;; make-lenght, length<=1
;; ((lambda (mk-length)
;;    (mk-length
;;     (mk-length eternity)))
;;  (lambda (length)
;;    (lambda (l)
;;       (cond
;;       ((null? l) 0)
;;       (else (add1 (length (cdr l))))))))

;; make-length, length<=2
;; ((lambda (mk-length)
;;    (mk-length
;;     (mk-length
;;      (mk-length eternity))))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else (add1 (length (cdr l))))))))

;; make-length, length<=3
;; ((lambda (mk-length)
;;   (mk-length
;;    (mk-length
;;     (mk-length
;;      (mk-length eternity)))))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else (add1 (length (cdr l))))))))

;; make-length, length0
;; ((lambda (mk-length)
;;   (mk-length mk-length))
;;  (lambda (mk-length)
;;    (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else (add1 (mk-length (cdr l))))))))

;; mk-length, length<=1
;; 
;; ((lambda (mk-length)
;;    (mk-length mk-length))
;;  (lambda (mk-length)
;;    (lambda (l)
;;      (cond
;;       ((null l) 0)
;;       (else
;;         (add1 ((mk-length eternity) (cdr l))))))))

;; Example
;; Page 166
;; 
;; (((lambda (mk-length)
;;    (mk-length mk-length))
;;   (lambda (mk-length)
;;     (lambda (l)
;;       (cond
;;        ((null? l) 0)
;;        (else
;;          (add1 ((mk-length eternity) (cdr l))))))))
;;  '(apples))

;; Apply mk-length to itself
;; 
;; ((lambda (mk-length)
;;   (mk-length mk-length))
;;  (lambda (mk-length)
;;    (lambda (l)
;;      (cond
;;       ((null l) 0)
;;       (else (add1 ((mk-length mk-length)
;;                    (cdr l))))))))

;; Simplification
;; Page 167
;; 
;; ((lambda (mk-length)
;;   (mk-length mk-length))
;;  (lambda (mk-length)
;;    ((lambda (length)
;;       (lambda (l)
;;         (cond
;;          ((null? l) 0)
;;          (else (add1 (length (cdr l)))))))
;;     (mk-length mk-length))))

;; Application of this function does not end! 
;; 
;;  (((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     ((lambda (length)
;;        (lambda (l)
;;          (cond
;;           ((null? l) 0)
;;           (else (add1 (length (cdr l)))))))
;;      (mk-length mk-length))))
;; '(apples))


;;  ((lambda (mk-length)
;;   (mk-length mk-length))
;; (lambda (mk-length)
;;   (lambda (l)
;;     (cond
;;      ((null? l 0))
;;      (else
;;       (add1
;;        ((lambda (x)
;;           ((mk-length mk-length)) x)
;;         (cdr l))))))))

;; Page 171
;;  ((lambda (mk-length)
;;   (mk-length mk-length))
;; (lambda (mk-length)
;;   ((lambda (length)
;;      (lambda (l)
;;        (cond
;;         ((null? l 0))
;;         (else
;;          (add1 (length (cdr l)))))))
;;    (lambda (x)
;;      ((mk-length mk-length)) x))))

;; Abstraction of the "length" lambda
;; Page 172
;; 
;; ((lambda (le)
;;   ((lambda (mk-length)
;;      (mk-length mk-length))
;;    (lambda (mk-length)
;;      (le (lambda (x)
;;            ((mk-length mk-length) x))))))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond
;;       ((null? l) 0)
;;       (else (add1 (length (cdr l))))))))

;; The "le" lambda
;; 
;; (lambda (le)
;;  ((lambda (mk-length)
;;     (mk-length mk-length))
;;   (lambda (mk-length)
;;     (le (lambda (x)
;;           ((mk-length mk-length) x))))))

;; The applicative-order Y combinator
;; Page 172
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(test "Y"
      ((Y (lambda (length)
            (lambda (l)
              (cond
               ((null? l) 0)
               (else (add1 (length (cdr l))))))))
       '(a b c))
      3)

(test "Y"
      ((Y (lambda (length)
            (lambda (l)
              (cond
               ((null? l) 0)
               (else (add1 (length (cdr l))))))))
       '(a b c d e f))
      6)

(test "Y"
      ((Y (lambda (leftmost)
            (lambda (l)
              (cond
               ((atom? l) l)
               (else (leftmost (car l)))))))
       '((potato) (chips ((with) fish) (chips))))
      'potato)

