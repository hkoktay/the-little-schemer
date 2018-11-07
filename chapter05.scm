;; Chapter 5: *Oh My Gawd*: It's Full of Stars

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

;; rember*: [atom] [s-exp] -> [s-exp]
;; Returns a [s-exp] with [atom] 'a' removed.
;;
;; Page 81
(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(test "rember*"
      (rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
      '((coffee) ((tea)) (and (hick))))

(test "rember*"
      (rember* 'sauce '(((tomato sauce))
                        ((bean) sauce)
                        (and ((flying)) sauce)))
      '(((tomato))
        ((bean))
        (and ((flying)))))

(test "rember*"
      (rember* 'x '((a) (b (c))))
      '((a) (b (c))))

;; insertR*: [atom] [atom] [s-exp] -> [s-exp]
;; Returns a s-exp with 'new' inserted everywhere rightafter every
;; occurence of 'old.
;;
;; Page 82
(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(test "insertR*"
      (insertR* 'roast 'chuck '((how much (wood))
                                could
                                ((a (wood) chuck))
                                (((chuck)))
                                (if (a) (wood chuck))
                                could chuck wood))
      '((how much (wood))
        could
        ((a (wood) chuck roast))
        (((chuck roast)))
        (if (a) (wood chuck roast))
        could chuck roast wood))

;; occur*: [atom] [s-exp] -> [number]
;; Counts how often the atom 'a' occurs in 'l'.
;;
;; Page 85
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(test "occur*"
      (occur* 'banana '((banana)
                        (split ((((banana ice)))
                                (cream (banana))
                                sherbet))
                        (banana)
                        (bread)
                        (banana brandy)))
      5)

(test "occur*"
      (occur* 'x '(a (b (c d)) e))
      0)

;; subst*: [atom] [atom] [s-exp] -> [s-exp]
;; Returns a s-exp with all occurences of atom 'old' replaced with
;; 'new'
;;
;; Page 85
(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(test "subst*"
      (subst* 'orange 'banana '((banana)
                                (split ((((banana ice)))
                                        (cream (banana))
                                        sherbet))
                                (banana)
                                (bread)
                                (banana brandy)))
      '((orange)
        (split ((((orange ice)))
                (cream (orange))
                sherbet))
        (orange)
        (bread)
        (orange brandy)))

;; insertL*: [atom] [atom] [s-exp] -> [s-exp]
;; Returns a s-exp with 'new' inserted before every occurence of 'old'
;;
;; Page 86
(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(test "insertL*"
      (insertL* 'pecker 'chuck '((how much (wood))
                                 could
                                 ((a (wood) chuck))
                                 (((chuck)))
                                 (if (a) (wood chuck))
                                 could chuck wood))
      '((how much (wood))
        could
        ((a (wood) pecker chuck))
        (((pecker chuck)))
        (if (a) (wood pecker chuck))
        could pecker chuck wood))

;; member*: [atom] [s-exp] -> boolean
;; Returns #t if 'a' is a member of 's-exp'
;;
;; 87
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a) (member* a (cdr l))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(test "member*"
      (member* 'chips '((potato) (chips ((with) fish) (chips))))
      #t)

;; leftmost: [s-exp] -> [atom]
;; Returns the leftmost atom of 'l'. Assumes that there are no empty
;; lists in s-exp.
;;
;; Page 88
(define leftmost
  (lambda (l)
    (cond
     ((atom? l) l)
     (else (leftmost (car l))))))

(test "leftmost"
      (leftmost '((potato) (chips ((with) fish) (chips))))
      'potato)

(test "leftmost"
      (leftmost '(((hot) (tuna (and))) cheese))
      'hot)

(test "leftmost"
      (test-exception
       (lambda ()
         (leftmost '(((() four)) 17 (seventeen)))))
      #t)

;; eqan?: [atom] [atom] -> boolean
;; Returns #t if 'a1' and 'a2' are the same atom
;;
;; Page 78 (Chapter 4)
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

;; eqlist?: [s-exp] [s-exp] -> boolean
;; Returns #t when 'l1' and 'l2' have the same elements.
;;
;; Page 91
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((and (null? l1) (atom? (car l2))) #f)
     ((null? l1) #f)
     ((and (atom? (car l1)) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2))))
     ((atom? (car l1)) #f)
     ((null? l2) #f)
     ((atom? (car l2)) #f)
     (else
      (and (eqlist? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))

(test "eqlist?"
      (eqlist? '(strawberry ice cream) '(strawberry ice cream))
      #t)

(test "eqlist?"
      (eqlist? '(strawberry ice cream) '(strawberry cream ice))
      #f)

(test "eqlist?"
      (eqlist? '(banana ((split))) '((banana) (split)))
      #f)

(test "eqlist?"
      (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
      #f)

(test "eqlist?"
      (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
      #t)

;; eqlist?: [s-exp] [s-exp] -> boolean
;; 1. Revision of eqlist?
;;
;; Page 92
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else
      (and (eqlist? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))

(test "eqlist? - 1.revision"
      (eqlist? '(strawberry ice cream) '(strawberry ice cream))
      #t)

(test "eqlist? - 1.revision"
      (eqlist? '(strawberry ice cream) '(strawberry cream ice))
      #f)

(test "eqlist? - 1.revision"
      (eqlist? '(banana ((split))) '((banana) (split)))
      #f)

(test "eqlist? - 1.revision"
      (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
      #f)

(test "eqlist? - 1.revision"
      (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
      #t)

;; equal?: any any -> boolean
;; Compares two arbitrary expressions and returns #t if they are
;; equal.
;;
;; Page 92
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((atom? s1) #f)
     ((atom? s2) #f)
     (else (eqlist? s1 s2)))))

(test "equal?"
      (equal? 'a 'a) #t)

(test "equal?"
      (equal? 'a 'b) #f)

(test "equal?"
      (equal? '(a b) '(a b)) #t)

(test "equal?"
      (equal? '(a b) '(a c)) #f)

(test "equal?"
      (equal? '(a b) 'a) #f)

;; equal?: any any -> boolean
;; 1. revision of equal?
;;
;; Page 93
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(test "equal? - 1.revsion"
      (equal? 'a 'a) #t)

(test "equal? - 1.revsion"
      (equal? 'a 'b) #f)

(test "equal? - 1.revsion"
      (equal? '(a b) '(a b)) #t)

(test "equal? - 1.revsion"
      (equal? '(a b) '(a c)) #f)

(test "equal? - 1.revsion"
      (equal? '(a b) 'a) #f)

;; eqlist?: [listof s-exp] [listof s-exp] -> boolean
;; 2. revision of eqlist?
;;
;; Page 93
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))

(test "eqlist? - 2.revision"
      (eqlist? '(strawberry ice cream) '(strawberry ice cream))
      #t)

(test "eqlist? - 2.revision"
      (eqlist? '(strawberry ice cream) '(strawberry cream ice))
      #f)

(test "eqlist? - 2.revision"
      (eqlist? '(banana ((split))) '((banana) (split)))
      #f)

(test "eqlist? - 2.revision"
      (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
      #f)

(test "eqlist? - 2.revision"
      (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
      #t)

;; rember0: [s-exp] [s-exp] -> [s-exp]
;; Removes the [s-exp] 's' from the [s-exp] 'l'.
;;
;; Page 94
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
                       ((equal? (car l) s) (cdr l))
                       (else (cons (car l)
                               (rember s (cdr l))))))
     (else (cond
            ((equal? (car l) s) (cdr l))
            (else (cons (car l)
                    (rember s (cdr l)))))))))

(test "rember" (rember 'a '(a b c)) '(b c))
(test "rember" (rember 'a '(b a c)) '(b c))
(test "rember" (rember 'a '(c b a)) '(c b))
(test "rember" (rember 'a '(a b a)) '(b a))
(test "rember" (rember 'a '(x y z)) '(x y z))
(test "rember" (rember 'a '(b (a) c)) '(b (a) c))
(test "rember" (rember 'a '()) '())

(test "rember"
      (rember 'mint '(lamb chops and mint jelly))
      '(lamb chops and jelly))

(test "rember"
      (rember 'mint '(lamb chops and mint flavored mint jelly))
      '(lamb chops and flavored mint jelly))

(test "rember"
      (rember 'toast '(bacon lettuce and tomato))
      '(bacon lettuce and tomato))

(test "rember"
      (rember 'cup '(coffee cup tea cup and hick cup))
      '(coffee tea cup and hick cup))

(test "rember"
      (rember 'bacon '(bacon lettuce and tomato))
      '(lettuce and tomato))

(test "rember"
      (rember 'and '(bacon lettuce and tomato))
      '(bacon lettuce tomato))

;; rember1: [s-exp] [s-exp] -> [s-exp]
;; 1. revision of rember
;;
;; Page 94
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     (else (cond
            ((equal? (car l) s) (cdr l))
            (else (cons (car l)
                    (rember s (cdr l)))))))))

(test "rember - 1.revision" (rember 'a '(a b c)) '(b c))
(test "rember - 1.revision" (rember 'a '(b a c)) '(b c))
(test "rember - 1.revision" (rember 'a '(c b a)) '(c b))
(test "rember - 1.revision" (rember 'a '(a b a)) '(b a))
(test "rember - 1.revision" (rember 'a '(x y z)) '(x y z))
(test "rember - 1.revision" (rember 'a '(b (a) c)) '(b (a) c))
(test "rember - 1.revision" (rember 'a '()) '())

(test "rember - 1.revision"
      (rember 'mint '(lamb chops and mint jelly))
      '(lamb chops and jelly))

(test "rember - 1.revision"
      (rember 'mint '(lamb chops and mint flavored mint jelly))
      '(lamb chops and flavored mint jelly))

(test "rember - 1.revision"
      (rember 'toast '(bacon lettuce and tomato))
      '(bacon lettuce and tomato))

(test "rember - 1.revision"
      (rember 'cup '(coffee cup tea cup and hick cup))
      '(coffee tea cup and hick cup))

(test "rember - 1.revision"
      (rember 'bacon '(bacon lettuce and tomato))
      '(lettuce and tomato))

(test "rember - 1.revision"
      (rember 'and '(bacon lettuce and tomato))
      '(bacon lettuce tomato))

;; rember: [s-exp] [s-exp] -> [s-exp]
;; 2. revision of rember
;;
;; Page 95
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else
      (cons (car l)
        (rember s (cdr l)))))))

(test "rember - 2.revision" (rember 'a '(a b c)) '(b c))
(test "rember - 2.revision" (rember 'a '(b a c)) '(b c))
(test "rember - 2.revision" (rember 'a '(c b a)) '(c b))
(test "rember - 2.revision" (rember 'a '(a b a)) '(b a))
(test "rember - 2.revision" (rember 'a '(x y z)) '(x y z))
(test "rember - 2.revision" (rember 'a '(b (a) c)) '(b (a) c))
(test "rember - 2.revision" (rember 'a '()) '())

(test "rember - 2.revision"
      (rember 'mint '(lamb chops and mint jelly))
      '(lamb chops and jelly))

(test "rember - 2.revision"
      (rember 'mint '(lamb chops and mint flavored mint jelly))
      '(lamb chops and flavored mint jelly))

(test "rember - 2.revision"
      (rember 'toast '(bacon lettuce and tomato))
      '(bacon lettuce and tomato))

(test "rember - 2.revision"
      (rember 'cup '(coffee cup tea cup and hick cup))
      '(coffee tea cup and hick cup))

(test "rember - 2.revision"
      (rember 'bacon '(bacon lettuce and tomato))
      '(lettuce and tomato))

(test "rember - 2.revision"
      (rember 'and '(bacon lettuce and tomato))
      '(bacon lettuce tomato))
