;; Chapter 7: Friends and Relations
;; 
;; A [set] is
;; - () or
;; - [listof atom]
;; - There are no atoms in [listof atom] that are 'equal?'.
;;
;; A [l-set] is
;; - () or
;; - (cons [set] [l-set])
;;
;; A [pair] is one of
;; - (cons [s-exp] [s-exp])
;; 
;; A relation [rel] is a
;; - [setof pairs]
;;
;; A function [fun] is
;; - [setof pairs] where (firsts rel) is a [set]
;;
;; A [fullfun] is
;; - [setof pairs] where (seconds rel) is a [set]

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; firsts: [s-exp] -> [s-exp]
;; Returns a list of the first sexps of the [listof s-exp]
;; 
;; Examples:
;; (firsts '()) -> '()
;; (firsts '((a b) (c) (d)) -> (a c d)
;; (firsts '(((a b) c) (d e f) ((g) h))) -> ((a b) d (g))
;;
;; Page 46
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
             (firsts (cdr l)))))))

(test "firsts" (firsts '((a 1) (b 2))) '(a b))
(test "firsts" (firsts '(((f) a) ((g) b))) '((f) (g)))
(test "firsts" (firsts '((a) (b))) '(a b))

(test "firsts"
      (firsts '((apple peach pumpkin)
                (plum pear cherry)
                (grape raisin pea)
                (bean carrot eggplant)))
      '(apple plum grape bean))

(test "firsts"
      (firsts '((a b) (c d) (e f)))
      '(a c e))

(test "firsts" (firsts '()) '())

(test "firsts"
      (firsts '((five plums)
                (four)
                (eleven green organges)))
      '(five four eleven))

(test "firsts"
      (firsts '(((five plums) four)
                (eleven green organges)
                ((no) more)))
      '((five plums) eleven (no)))

;; member?: [atom] [list-of atoms] -> boolean
;; Checks if [atom] 'a' is a member of [list-of sexp]. Return #t if
;; yes.
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
               (member? a (cdr lat)))))))

(test "member?" (member? 'x '(a b c)) #f)
(test "member?" (member? 'x '(a x c)) #t)
(test "member?" (member? 'x '(x b c)) #t)
(test "member?" (member? 'x '(a b x)) #t)
(test "member?" (member? '2 '(a 2 c)) #t)
(test "member?" (member? 'b '(a (b) c)) #f)
(test "member?" (member? 'x '()) #f)

;; multirember: [atom] [listof atom] -> [listof atom]
;; Page 53
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((equal? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(test "multirember"
      (multirember 'a '())
      '())

(test "multirember"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y z))

;; set?: [lat] -> boolean
;; Returns #t if 'lat' is a set.
;;
;; Page 111
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else
      (cond
       ((member? (car lat) (cdr lat)) #f)
       (else (set? (cdr lat))))))))

(test "set?"
      (set? '(apple peaches apple plum)) #f)

(test "set?"
      (set? '(apple peaches pears plums)) #t)

(test "set?"
      (set? '()) #t)

(test "set?"
      (set? '(apple 3 pear 4 9 apple 3 4)) #f)

(test "set?"
      (set? '(apple banana apple)) #f)

(test "set?"
      (set? '(apple banana peaches banana)) #f)

;; set?: [lat] -> boolean
;; 1. revision of set?
(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(test "set? - 1.revision"
      (set? '(apple peaches apple plum)) #f)

(test "set? - 1.revision"
      (set? '(apple peaches pears plums)) #t)

(test "set? - 1.revision"
      (set? '()) #t)

(test "set? - 1.revision"
      (set? '(apple 3 pear 4 9 apple 3 4)) #f)

(test "set? - 1.revision"
      (set? '(apple banana apple)) #f)

(test "set? - 1.revision"
      (set? '(apple banana peaches banana)) #f)

;; makeset: [lat] -> [set]
;; Returns a set from a lat.
;;
;; Page 112
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(test "makeset"
      (makeset '(apple peach pear peach plum apple lemon peach))
      '(pear plum apple lemon peach))

(test "makeset"
      (makeset '(banana peach banana))
      '(peach banana))

;; makeset: [lat] -> [set]
;; makeset implemented with multirember from chapter 3, page 53
;; 1. revision of makeset
;;
;; Page 112
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat)
             (makeset
              (multirember (car lat) (cdr lat))))))))

(test "makeset - 1.revision"
      (makeset '(apple peach pear peach plum apple lemon peach))
      '(apple peach pear plum lemon))

(test "makeset - 1.revision"
      (makeset '(banana peach banana))
      '(banana peach))

(test "makeset - 1.revision"
      (makeset '(apple 3 pear 4 9 apple 3 4))
      '(apple 3 pear 4 9))

;; subset?: [set] [set] -> boolean
;; Returns #t if all atoms in 's1' are also in 's2'
;;
;; Page 113
(define subset?
  (lambda (s1 s2)
    (cond
     ((null? s1) #t)
     (else
      (cond
       ((member? (car s1) s2) (subset? (cdr s1) s2))
       (else #f))))))

(test "subset?"
      (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
      #t)

(test "subset?"
      (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))
      #f)

;; subset?: [set] [set] -> boolean
;; 1. revision of subset?
;;
;; Page 114
(define subset?
  (lambda (s1 s2)
    (cond
     ((null? s1) #t)
     ((member? (car s1) s2) (subset? (cdr s1) s2))
     (else #f))))

(test "subset? - 1.revision"
      (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
      #t)

(test "subset? - 1.revision"
      (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))
      #f)

;; subset?: [set] [set] -> boolean
;; 2. revision of subset?
;;
;; Page 114
(define subset?
  (lambda (s1 s2)
    (cond
     ((null? s1) #t)
     (else
      (and (member? (car s1) s2) (subset? (cdr s1) s2))))))

(test "subset? - 2.revision"
      (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))
      #t)

(test "subset? - 2.revision"
      (subset? '(4 pounds of horseradish) '(four pounds chicken and 5 ounces horseradish))
      #f)

;; eqset?: [set] [set] -> boolean
;; Returns #t if 'set1' and 'set2' are equal. Two sets, set1 and set2, are equal if
;; set1 is a subset of set2 and set2 is a subset of set1.
;;
;; Page 114
(define eqset?
  (lambda (set1 set2)
    (cond
     ((subset? set1 set2) (subset? set2 set1))
     (else #f))))

(test "eqset?"
      (eqset? '(6 large chickens with wings) '(6 chickens with large wings))
      #t)

(test "eqset?"
      (eqset? '(6 large chickens with wings) '(6 chickens with wings))
      #f)

;; eqset?1: [set] [set] -> boolean
;; 1. revision of eqset?
;;
;; Page 114
(define eqset?
  (lambda (set1 set2)
    (cond
     (else
      (and (subset? set1 set2) (subset? set2 set1))))))

(test "eqset? - 1.revison"
      (eqset? '(6 large chickens with wings) '(6 chickens with large wings))
      #t)

(test "eqset? - 1.revison"
      (eqset? '(6 large chickens with wings) '(6 chickens with wings))
      #f)

;; eqset?: [set] [set] -> boolean
;; 2.revision of eqset?
;;
;; Page 115
(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(test "eqset? - 2.revison"
      (eqset? '(6 large chickens with wings) '(6 chickens with large wings))
      #t)

(test "eqset? - 2.revison"
      (eqset? '(6 large chickens with wings) '(6 chickens with wings))
      #f)

;; intersect?: [set] [set] -> boolean
;; An intersection of two sets is a set whose members are a member of
;; both sets. 'intersect?' returns #t if 'set1' has elements which are
;; also members of 'set2'.  Returns #t if there is an intersection
;; between set1 and set2.
;;
;; Page 115
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (cond
            ((member? (car set1) set2) #t)
            (else (intersect? (cdr set1) set2)))))))

(test "intersect?"
      (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
      #t)

(test "intersect?"
      (intersect? '(a b c) '(x y z))
      #f)

(test "intersect?"
      (intersect? '(a b c) '())
      #f)

(test "intersect?"
      (intersect? '() '(a b c))
      #f)

(test "intersect?"
      (intersect? '() '())
      #f)

;; intersect?1: [set] [set] -> boolean
;; 1. revision of intersect?
;;
;; Page 115
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else (intersect? (cdr set1) set2)))))

(test "intersect? - 1.revision"
      (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
      #t)

(test "intersect? - 1.revision"
      (intersect? '(a b c) '(x y z))
      #f)

(test "intersect? - 1.revision"
      (intersect? '(a b c) '())
      #f)

(test "intersect? - 1.revision"
      (intersect? '() '(a b c))
      #f)

(test "intersect? - 1.revision"
      (intersect? '() '())
      #f)

;; intersect?: [set] [set] -> boolean
;; 2. revision of intersect?
;; Implementation with 'or'.
;;
;; Page 115
(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))

(test "intersect? - 2.revision"
      (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese))
      #t)

(test "intersect? - 2.revision"
      (intersect? '(a b c) '(x y z))
      #f)

(test "intersect? - 2.revision"
      (intersect? '(a b c) '())
      #f)

(test "intersect? - 2.revision"
      (intersect? '() '(a b c))
      #f)

(test "intersect? - 2.revision"
      (intersect? '() '())
      #f)

;; intersect: [set] [set] -> [set]
;; Returns the intersection of two sets, set1 and set2.
;;
;; Page 116
(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(test "intersect"
      (intersect '(stewed tomatoes and macaroni) '(macaroni and cheese))
      '(and macaroni))

(test "intersect"
      (intersect '(a b c) '())
      '())

(test "intersect"
      (intersect '() '(x y z))
      '())

(test "intersect"
      (intersect '() '(x y z))
      '())

;; union: [set] [set] -> [set]
;; Returns the union of two sets, set1 and set2.
;;
;; Page 116
(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(test "union"
      (union '(stewed tomatoes and macaroni casserole) '(macaroni and cheese))
      '(stewed tomatoes casserole macaroni and cheese))

(test "union"
      (union '(a b c) '())
      '(a b c))

(test "union"
      (union '() '(x y z))
      '(x y z))

(test "union"
      (union '() '())
      '())

;; difference: [set] [set] -> [set]
;; Returns all atoms in set1 that are not in set2.
;;
;; Page 117
(define difference
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (difference (cdr set1) set2))
     (else (cons (car set1) (difference (cdr set1) set2))))))

(test "difference"
      (difference '(a b c) '(b c)) '(a))

(test "difference"
      (difference '(a b c) '(a b c)) '())

(test "difference"
      (difference '(a b c) '(d e)) '(a b c))

;; intersectall: [l-set] -> [set]
;; Returns the intersection of all sets in 'l-set'. Instead of (null?
;; l-set), we test (null? (cdr l-set)) and return (car l-set) as
;; value because (intersect '(a b c) '()) returns '().
;;
;; Page 117
(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(test "intersectall"
      (intersectall '((6 pears and)
                      (3 peaches and 6 peppers)
                      (8 pears and 6 plums)
                      (and 6 prunes with some apples)))
      '(6 and))

(test "intersectall"
      (intersectall '((6 pears and)
                      (3 peaches and 6 peppers)
                      ()
                      (and 6 prunes with some apples)))
      '())

;; a-pair?: any -> boolean
;; Page 118
(define a-pair?
  (lambda (l)
    (cond
     ((atom? l) #f)
     ((null? l) #f)
     ((null? (cdr l)) #f)
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

;; first: [pair] -> [s-exp]
;; Page 119
(define first
  (lambda (p)
    (car p)))

(test "first"
      (first '(a b)) 'a)

;; second: [pair] -> [s-exp]
;; Page 119
(define second
  (lambda (p)
    (car (cdr p))))

(test "second"
      (second '(a b)) 'b)

;; build: [s-exp] [s-exp] -> [pair]
;; Page 119
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(test "build"
      (build 'a 'b) '(a b))

;; third: [listof s-exp] -> [s-exp]
;; The supplied list argument 'l' must have at least length 3.
;; Page 119
(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(test "third"
      (third '(a b c d e)) 'c)

;; fun?: [rel] -> boolean
;; Needs procedures 'firsts' from chapter 3
;;
;; Page 120
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(test "fun?"
      (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
      #t)

(test "fun?"
      (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))
      #f)

;; revrel: [rel] -> [rel]
;; Returns a relation with the elements of the pairs reversed
;; Example:
;; (revrel '((a 1) (b 2) (c 3)))
;; -> ((1 a) (2 b) (3 c))
;;
;; Page 120
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (car rel))
                        (first (car rel)))
             (revrel (cdr rel)))))))

(test "revrel"
      (revrel '((8 a) (pumpkin pie) (got sick)))
      '((a 8) (pie pumpkin) (sick got)))

(test "revrel"
      (revrel '((a 1) (b 2) (c 3)))
      '((1 a) (2 b) (3 c)))

;; revrel: [rel] -> [rel]
;; revrel version without build, first and second.
;; 1. revision of revrel
;;
;; Page 121
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (cons (car (cdr (car rel)))
                   (cons (car (car rel)) '()))
             (revrel (cdr rel)))))))

(test "revrel - 1.revision"
      (revrel '((8 a) (pumpkin pie) (got sick)))
      '((a 8) (pie pumpkin) (sick got)))

(test "revrel - 1.revision"
      (revrel '((a 1) (b 2) (c 3)))
      '((1 a) (2 b) (3 c)))

;; revpair: [pair] -> [pair]
;; Returns a pair with its elements reversed
;;
;; Page 121
(define revpair
  (lambda (p)
    (build (second p) (first p))))

(test "revpair"
      (revpair '(a b)) '(b a))

;; revrel: [rel] -> [rel]
;; Returns a relation with the elements of the pairs reversed
;; 2. revision of revrel
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
             (revrel (cdr rel)))))))

(test "revrel - 2.revision"
      (revrel '((8 a) (pumpkin pie) (got sick)))
      '((a 8) (pie pumpkin) (sick got)))

(test "revrel - 2.revision"
      (revrel '((a 1) (b 2) (c 3)))
      '((1 a) (2 b) (3 c)))

;; seconds: [listof s-exp] -> [listof s-exp]
(define seconds
  (lambda (s)
    (cond
     ((null? s) '())
     (else (cons (car (cdr (car s)))
             (seconds (cdr s)))))))

(test "seconds"
      (seconds '((a 1) (b 2) (c 3)))
      '(1 2 3))

;; fullfun?: [rel] -> boolean
;; Returns #t if [rel] 'fun' is a fullfun.
;; Page 122
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(test "fullfun?"
      (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
      #t)

(test "fullfun?"
      (fullfun? '((grape raisin) (plum prune) (stewed prune)))
      #f)

;; one-to-one?: [rel] -> boolean
;; This is another implementation of fullfun?.
;; Page 122
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(test "one-to-one?"
      (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
      #t)

(test "one-to-one?"
      (fullfun? '((grape raisin) (plum prune) (stewed prune)))
      #f)
