;; Chapter 3: Cons the Magnificant

(load "test-check.scm")

;; rember: [atom] [lat] -> [lat]
;; Returns a list of atoms [lat] with atom 'a' removed.
;; This version of 'rember' is not correct. See tests.
;;
;; Page 34
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) a) (cdr lat))
       (else (rember a (cdr lat))))))))

;; Test rember
(test "rember" (rember 'a '(a b c)) '(b c))
(test "rember" (rember 'a '(b a c)) '(c))
(test "rember" (rember 'a '(c b a)) '())
(test "rember" (rember 'a '(a b a)) '(b a))
(test "rember" (rember 'a '(x y z)) '())
(test "rember" (rember 'a '(b (a) c)) '())
(test "rember" (rember 'a '()) '())

;; rember: [atom] [lat] -> [lat]
;; 1. revision of rember
;; 
;; Page 37
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
                    (rember a (cdr lat)))))))))

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


;; rember: [atom] [lat] -> [lat]
;; 2. revision of rember
;; Page 41
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((eq? (car l) s) (cdr l))
     (else
      (cons (car l) (rember s (cdr l)))))))

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

;; insertR: [atom] [atom] [listof atom] -> [listof atom]
;; Builds a [listof atom] with 'new' inserted to the right of the
;; first occurrence of 'old'.
;; This version does not work. See tests.
;; Page 49
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cdr lat))
       (else (cons (car lat) (insertR new old (cdr lat)))))))))

(test "insertR"
      (insertR 'topping 'fudge '(ice cream with fudge for dessert))
      '(ice cream with for dessert))

(test "insertR"
      (insertR 'jalapeno 'and '(tacos tamales and salsa))
      '(tacos tamales  salsa))

(test "insertR"
      (insertR 'e 'd '(a b c d f g d h))
      '(a b c f g d h))

;; insertR: [atom] [atom] [listof atom] -> [listof atom]
;; Builds a [list-of atom] with 'new' inserted to the right of the
;; first occurrence of 'old'. This version does not work too. See tests.
;; 1. revision of insertR
;; Page 49
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new (cdr lat)))
       (else (cons (car lat) (insertR new old (cdr lat)))))))))

(test "insertR - 1.revision"
      (insertR 'topping 'fudge '(ice cream with fudge for dessert))
      '(ice cream with topping for dessert))

(test "insertR - 1.revision"
      (insertR 'jalapeno 'and '(tacos tamales and salsa))
      '(tacos tamales jalapeno salsa))

(test "insertR - 1.revision"
      (insertR 'e 'd '(a b c d f g d h))
      '(a b c e f g d h))

;; insertR: [atom] [atom] [listof atom] -> [listof atom]
;; Builds a [list-of atom] with 'new' inserted to the right of the
;; first occurrence of 'old'
;; 2. revision of insertR
;; Page 50
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons old (cons new (cdr lat))))
       (else (cons (car lat) (insertR new old (cdr lat)))))))))

(test "insertR - 2.revision"
      (insertR 'topping 'fudge '(ice cream with fudge for dessert))
      '(ice cream with fudge topping for dessert))

(test "insertR - 2.revision"
      (insertR 'jalapeno 'and '(tacos tamales and salsa))
      '(tacos tamales and jalapeno salsa))

(test "insertR - 2.revision"
      (insertR 'e 'd '(a b c d f g d h))
      '(a b c d e f g d h))

;; insertR: [atom] [atom] [listof atom] -> [listof atom]
;; Builds a [listof atom] with 'new' inserted to the right of the
;; first occurrence of 'old'
;; 3. revision of insertR
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons old (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(test "insertR - 3.revision"
      (insertR 'topping 'fudge '(ice cream with fudge for dessert))
      '(ice cream with fudge topping for dessert))

(test "insertR - 3.revision"
      (insertR 'jalapeno 'and '(tacos tamales and salsa))
      '(tacos tamales and jalapeno salsa))

(test "insertR - 3.revision"
      (insertR 'e 'd '(a b c d f g d h))
      '(a b c d e f g d h))

;; insertL0: [atom] [atom] [listof atom] -> [listof atom]
;; Builds a [listof atom] with 'new' inserted to the left of the
;; first occurrence of 'old'.
;; Page 51
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new lat))
       (else (cons (car lat) (insertL new old (cdr lat)))))))))

(test "insertL"
      (insertL 'O 'x '(a b x c d x e f))
      '(a b O x c d x e f))

;; insertL: [atom] [atom] [listof atom] -> [listof atom]
;; Builds a [listof atom] with 'new' inserted to the left of the
;; first occurrence of 'old'.
;; 1. revision of insertL
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(test "insertL - 1.revision"
      (insertL 'O 'x '(a b x c d x e f))
      '(a b O x c d x e f))

;; subst: [atom] [atom] [listof atom] -> [listof atom]
;; Replaces the first occurence of 'old' atom with 'new' atom and
;; returns a [list-of atom]
;; Page 51
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new (cdr lat)))
       (else (cons (car lat) (subst new old (cdr lat)))))))))

(test "subst"
      (subst 'topping 'fudge '(ice cream with fudge for dessert))
      '(ice cream with topping for dessert))

(test "subst"
      (subst 'a 'x '(x b c d x e))
      '(a b c d x e))

(test "subst"
      (subst 'a 'x '())
      '())

;; subst: [atom] [atom] [listof atom] -> [listof atom]
;; Replaces the first occurence of 'old' atom with 'new' atom and
;; returns a [listof atom]
;; 1. revision of subst
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(test "subst - 1.revision"
      (subst 'topping 'fudge '(ice cream with fudge for dessert))
      '(ice cream with topping for dessert))

(test "subst - 1.revision"
      (subst 'a 'x '(x b c d x e))
      '(a b c d x e))

(test "subst - 1.revision"
      (subst 'a 'x '())
      '())

;; subst2: [atom] [atom] [atom] [listof atom] -> [listof atom]
;; Replaces the fist occurence of 'o1' or 'o2'
;; Page 52
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) o1) (cons new (cdr lat)))
       ((eq? (car lat) o2) (cons new (cdr lat)))
       (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

(test "subst2"
      (subst2 'x 'a 'c '(a b c d e))
      '(x b c d e))

(test "subst2"
      (subst2 'x 'a 'c '(g b c d e))
      '(g b x d e))

;; subst2: [atom] [atom] [atom] [listof atom] -> [listof atom]
;; Simplified version of subst2
;; 1. revision of subst2
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? (car lat) o1) (eq? (car lat) o2))
      (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(test "subst2 - 1.revision"
      (subst2 'x 'a 'c '(a b c d e))
      '(x b c d e))

(test "subst2 - 1.revision"
      (subst2 'x 'a 'c '(g b c d e))
      '(g b x d e))

;; multirember: [atom] [listof atom] -> [listof atom]
;; Returns a [listof atom] with all occurences of atom 'a' removed
;; from 'lat'
;;
;; Page 53
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) a) (multirember a (cdr lat)))
            (else (cons (car lat)
                    (multirember a (cdr lat)))))))))

(test "multirember"
      (multirember 'a '())
      '())

(test "multirember"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; multirember: [atom] [listof atom] -> [listof atom]
;; 1. revision of multirember
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(test "multirember - 1.revision"
      (multirember 'a '())
      '())

(test "multirember - 1.revision"
      (multirember 'x '(x a b x c d x))
      '(a b c d))

(test "multirember - 1.revision"
      (multirember '(a b) '(x y (a b) z (a b)))
      '(x y (a b) z (a b)))

;; multiinsertR: [atom] [atom] [listof atom] -> [listof atom]
;; Returns a [listof atom] with atom 'new' inserted rightafter all
;; occurences of atom 'old'
;; Page 56
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons (car lat)
                              (cons new
                                (multiinsertR new old (cdr lat)))))
       (else (cons (car lat)
               (multiinsertR new old (cdr lat)))))))))

(test "multiinsertR"
      (multiinsertR 'x 'c '(a b c d e c g h))
      '(a b c x d e c x g h))

(test "multiinsertR"
      (multiinsertR 'x 'c '())
      '())

(test "multiinsertR"
      (multiinsertR 'x 'g '(a b c d c))
      '(a b c d c))

;; multiinsertR: [atom] [atom] [listof atom] -> [listof atom]
;; 1. revision of multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(test "multiinsertR - 1.revision"
      (multiinsertR 'x 'c '(a b c d e c g h))
      '(a b c x d e c x g h))

(test "multiinsertR - 1.revision"
      (multiinsertR 'x 'c '())
      '())

(test "multiinsertR - 1.revision"
      (multiinsertR 'x 'g '(a b c d c))
      '(a b c d c))

;; multiinsertL: [atom] [atom] [listof atom] -> [listof atom]
;;
;; Returns a [listof atom] with atom 'new' inserted before all
;; occurences of atom 'old'. This version does not work because in the
;; first clause of the second 'cond' clause the natural recusion does
;; not advance. 
;; 
;; Page 56
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new (cons old (multiinsertL new old lat))))
       (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))

;; (test "multiinsertL"
;;       (multiinsertL 'x 'c '(a b c d e c g h))
;;       '(a b x c d e x c g h))

;; (test "multiinsertL"
;;       (multiinsertL 'x 'c '())
;;       '())

;; (test "multiinsertL"
;;       (multiinsertL 'x 'g '(a b c d c))
;;       '(a b c d c))

;; multiinsertL1: [atom] [atom] [listof atom] -> [listof atom]
;; 1. revision of multiinsertL1
;; 
;; Page 57
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old) (cons new
                                   (cons old
                                     (multiinsertL new old (cdr lat)))))
            (else (cons (car lat)
                    (multiinsertL new old (cdr lat)))))))))

(test "multiinsertL - 1.revision"
      (multiinsertL 'x 'c '(a b c d e c g h))
      '(a b x c d e x c g h))

(test "multiinsertL - 1.revision"
      (multiinsertL 'x 'c '())
      '())

(test "multiinsertL - 1.revision"
      (multiinsertL 'x 'g '(a b c d c))
      '(a b c d c))

;; multiinsertL: [atom] [atom] [listof atom] -> [listof atom]
;; 2. revision of multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(test "multiinsertL - 2.revision"
      (multiinsertL 'x 'c '(a b c d e c g h))
      '(a b x c d e x c g h))

(test "multiinsertL - 2.revision"
      (multiinsertL 'x 'c '())
      '())

(test "multiinsertL - 2.revision"
      (multiinsertL 'x 'g '(a b c d c))
      '(a b c d c))

;; multisubst: [atom] [atom] [listof atom] -> [listof atom]
;; 
;; Returns a [list-of atom] with all occurences of 'old' replaced
;; with atom 'new'
;;
;; Page 57
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
       (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(test "multisubst"
      (multisubst 'x 'c '(a b c d e c f g))
      '(a b x d e x f g))

(test "multisubst"
      (multisubst 'x 'c '(c b c d e c f c))
      '(x b x d e x f x))

(test "multisubst"
      (multisubst 'x 'c '())
      '())

(test "multisubst"
      (multisubst 'x 'c '(a b d e))
      '(a b d e))

;; multisubst: [atom] [atom] [listof atom] -> [listof atom]
;; 1. revision of multisubst
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(test "multisubst - 1.revision"
      (multisubst 'x 'c '(a b c d e c f g))
      '(a b x d e x f g))

(test "multisubst - 1.revision"
      (multisubst 'x 'c '(c b c d e c f c))
      '(x b x d e x f x))

(test "multisubst - 1.revision"
      (multisubst 'x 'c '())
      '())

(test "multisubst - 1.revision"
      (multisubst 'x 'c '(a b d e))
      '(a b d e))
