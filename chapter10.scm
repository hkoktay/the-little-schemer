;; Chapter 10: What Is the Value of All of This?
;;
;; An [entry] is a
;; - [pairof [set] list]
;;
;; A [table] is a
;; - [listof entry]
;;
;; A [closure] is
;; - (list non-primitive (list [table] list any))

(load "test-check.scm")

;; atom?: any -> boolean
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; add1: number -> number
(define add1
  (lambda (n)
    (+ n 1)))

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

;; third: [listof s-exp] -> [s-exp]
;; Page 119
(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(test "third"
      (third '(a b c d e)) 'c)

;; build: [s-exp] [s-exp] -> [pair]
;; Page 119
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(test "build"
      (build 'a 'b) '(a b))

;; new-entry: [s-exp] [s-exp] -> [pair]
;; Page 175
(define new-entry build)

(test "new-entry"
      (new-entry 'a 'b) '(a b))

;; lookup-in-entry-help: symbol [set] list fun -> any | void
;; Page 176
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else (lookup-in-entry-help name
                                 (cdr names)
                                 (cdr values)
                                 entry-f)))))

(test "lookup-in-entry-help"
      (lookup-in-entry-help 'foo '(me foo too) '(1 2 3) list)
      '2)

(test "lookup-in-entry-help"
      (lookup-in-entry-help 'no '(me foo too) '(1 2 3) list)
      '(no))

;; lookup-in-entry: symbol [entry] fun -> symbol | void
;; Page 176
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(test "lookup-in-etnry"
      (lookup-in-entry 'foo '((me foo too) (1 2 3)) list)
      2)

(test "lookup-in-entry"
      (lookup-in-entry 'no '((me foo too) (1 2 3)) list)
      '(no))

;; extend-table: symbol [table] -> [table]
;; Page 176
(define extend-table cons)

(test "extend-table"
      (extend-table 'a '()) '(a))

(test "extend-table"
      (extend-table 'a '(b c)) '(a b c))

;; lookup-in-table: symbol [table] fun -> symbol
;; Recursivle looks for symbol in [table] 'table'. If the symbol is
;; not found the function 'table-f' is applied to the table otherwise
;; it retunrs the bound value.
;;
;; Page 177
(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f table))
     (else (lookup-in-entry name
                            (car table)
                            (lambda (name)
                              (lookup-in-table name (cdr table) table-f)))))))

(test "lookup-in-table"
      (lookup-in-table 'entree
                       '(((entree dessert)
                          (spaghetti spumoni))
                         ((appetizer entree beverage)
                          (food tastes good)))
                       list)
      'spaghetti)

(test "lookup-in-table"
      (lookup-in-table 'foo '(((me too foo) (1 2 3)) ((one two three) (a b c))) list)
      3)

(test "lookup-in-table"
      (lookup-in-table 'two '(((me too foo) (1 2 3)) ((one two three) (a b c))) list)
      'b)

(define *const #f)
(define *identifier #f)
(define *quote #f)
(define *lambda #f)
(define *cond #f)
(define *application #f)

;; atom-to-action: [s-exp] -> any
;; Page 181
(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))



;; list-to-action: [s-exp] -> any
;; Page 182 
(define list-to-action
  (lambda (e)
    (cond
     ((atom? e)
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

;; expression-to-action: [s-exp] -> any
;; Page 181
(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

;; meaning: [s-exp] [table] -> any
;; Page 182
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

;; value: [s-exp] -> any
;; The function 'value' together with all the functions it uses, is
;; called an interpreter. '() is the empty table. The function value
;; approximates the scheme function 'eval'.
;; Page 182
(define value
  (lambda (e)
    (meaning e '())))

;; *cons: [s-exp] [table] -> any
;; Page 183
(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

(test "*const"
      (*const 3 '()) 3)

(test "*const"
      (*const #t '()) #t)

(test "*const"
      (*const #f '()) #f)

(test "*const"
      (*const 'test '()) '(primitive test))

;; text-of: [pair] -> [s-exp]
;; Page 183
(define text-of second)

(test "text-of"
      (text-of '(a b)) 'b)

;; *quote: [s-exp] [table] -> [s-exp]
;; Page 183
(define *quote
  (lambda (e table)
    (text-of e)))

(test "*quote"
      (*quote '((x y) (a b c)) '()) '(a b c))

;; initial-table: symbol -> void
;; Page 183
(define initial-table
  (lambda (name)
    (car '())))

;; *identifier: [s-exp] [table] -> symbol
;; Page 183
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

;; *lambda: [s-exp] [table] -> [pair]
;; Page 184
(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

;; Example:
;; (meaning (lambda (x) (cons x y)) '(((y z) ((8) 9))))
;; -> (non-primitive
;;      ((((y z) ((8) 9))) (x) (cons x y)))
;; 
;; (((y z) ((8) 9))) is the table
;; (x) is the formal argument
;; (cons x y) is the body

;; *lambda expression observers
;; Page 184

;; table-of: list -> any
(define table-of first)
;; formals-of: list -> any
(define formals-of second)
;; body-of: list -> any
(define body-of third)

;; evcon: [pair] [table] -> any
;; Page 185
(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

;; *cond expression observers
;; Page 185

;; question-of: list -> any
(define question-of first)
;; answer-of: list -> any
(define answer-of second)

;; *cond example:
;; (*cond
;;  '(cond
;;    (coffee klatsch)
;;    (else 'party))
;;  '(((coffee) (#t))
;;    ((klatsch party) (5 (6)))))

;; else?: [s-exp] -> boolean
;; Page 185
(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

(test "else?"
      (else? 'else) #t)

(test "else?"
      (else? 'test) #f)

;; *cond: [s-exp] [table] -> any
;; Page 185
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

;; cond-lines-of: list -> list
;; Page 185
(define cond-lines-of cdr)

;; evlis: [listof s-exp] -> [listof s-exp]
;; Takes a list of (representations of) arguments and a table, and
;; returns a list composed of the meaning of each argument.
;; Page 186
(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
        (evlis (cdr args) table))))))

;; *application: [s-exp] [table] -> any
;; Page 186
(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

;; *application observers
;; Page 187
;;
;; function-of: list -> any
(define function-of car)
;; arguments-of: list -> any
(define arguments-of cdr)

;; A Representation of a function is one of
;; - (primitive primitive-name)
;; - (non-primitive (table formals body))
;; (table formals body) is called a closure record

;; primitive?: [s-exp] -> boolean
;; Page 187
(define primitive?
  (lambda (e)
    (eq? (first e) 'primitive)))

;; non-primitive?: [s-exp] -> boolean
;; Page 187
(define non-primitive?
  (lambda (e)
    (eq? (first e) 'non-primitive)))

;; apply: fun list -> any
;; Page 187
(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))

;; apply-primitive: symbol list -> any
;; apply-primitive could be extended to check for application of cdr
;; to the empty list or sub1 to 0 etc.
;; 
;; Page 188
(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons) (cons (first vals) (second vals)))
     ((eq? name 'car) (car (first vals)))
     ((eq? name 'cdr) (cdr (first vals)))
     ((eq? name 'null?) (null? (first vals)))
     ((eq? name 'eq?) (eq? (first vals) (second vals)))
     ((eq? name 'p-atom?) (atom? (frist vals)))
     ((eq? name 'add1) (add1 (first vals)))
     ((eq? name 'sub1) (sub1 (first vals)))
     ((eq? name 'number?) (number? (first vals))))))

;; p-atom?: any -> boolean
;; Page 188
(define p-atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

;; apply-closure: [closure] list -> any
;; Page 189
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))