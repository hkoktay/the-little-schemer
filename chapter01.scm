;; Chapter 1: Toys

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; sexp?: any -> boolean
(define sexp?
  (lambda (exp)
    (or (atom? exp) (list? exp))))

;; These tests show you some usage examples
(test "atom?" (atom? 'turkey) #t)
(test "atom?" (atom? 1492) #t)
(test "atom?" (atom? 'u) #t)
(test "atom?" (atom? '(atom)) #f)
(test "atom?" (atom? '(atom turkey or)) #f)
(test "atom?" (atom? '((atom turkey) or)) #f)

(test "list?" (list? '(atom)) #t)
(test "list?" (list? '(atom turkey or)) #t)
(test "list?" (list? '((atom turkey) or)) #t)
(test "list?" (list? '()) #t)
(test "list?" (list? '(() () () ())) #t)

(test "car" (car '(a b c)) 'a)
(test "car" (car '((a b c) x y z)) '(a b c))
(test "car" (car '(((hotdogs)) (adn) (pickle) relish)) '((hotdogs)))
(test "car" (car (car '(((hotdogs)) (and)))) '(hotdogs))
(test "cdr" (cdr '(a b c)) '(b c))
(test "cdr" (cdr '((a b c) x y z)) '(x y z))
(test "cdr" (cdr '(hamburger)) '())
(test "cdr" (cdr '((x) t r)) '(t r))

(test "car and cdr" (car (cdr '((b) (x y) ((c))))) '(x y))
(test "car and cdr" (cdr (cdr '((b) (x y) ((c))))) '(((c))))
(test "car and cdr" (cdr (cdr '((b) (x y) ((c))))) '(((c))))

(test "cons" (cons 'peanut '(butter and jelly)) '(peanut butter and jelly))
(test "cons"
      (cons '(banana and) '(peanut butter and jelly))
      '((banana and) peanut butter and jelly))
(test "cons"
      (cons '((help) this) '(is very ((hard) to learn)))
      '(((help) this) is very ((hard) to learn)))
(test "cons" (cons '(a b (c)) '()) '((a b (c))))
(test "cons" (cons 'a '()) '(a))
(test "cons" (cons 'a (car '((b) c d))) '(a b))
(test "cons" (cons 'a (cdr '((b) c d))) '(a c d))
(test "null?" (null? '()) #t)
(test "null?" (null? (quote ())) #t)
(test "null?" (null? '(a b c)) #f)

(test "atom?" (atom? 'Harry) #t)
(test "atom?" (atom? '(Harry had a heap of apples)) #f)
(test "atom?" (atom? (car '(Harry had a heap of apples))) #t)
(test "atom?" (atom? (cdr '(Harry had a heap of apples))) #f)
(test "atom?" (atom? (cdr '(Harry))) #f)
(test "atom?" (atom? (car (cdr '(swing low sweet cherry oat)))) #t)
(test "atom?" (atom? (car (cdr '(swing (low sweet) cheery oat)))) #f)

(test "eq?" (eq? 'Harry 'Harry) #t)
(test "eq?" (eq? 'margarine 'butter) #f)
(test "eq?" (eq? (car '(Mary had a little lamb chop)) 'Mary) #t)
(test "eq?" (eq? (car '(beans beans we need jelly beans))
                 (car (cdr '(beans beans we need jelly beans))))
      #t)
