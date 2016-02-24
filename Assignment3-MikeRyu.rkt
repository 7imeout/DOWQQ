;; Mike Ryu
;; Winter 2016 CPE 430
;; Assignment 3

#lang plai-typed

(require plai-typed/s-exp-match)

(print-only-errors #t)


#||||||||||||||||||||#
#| Data Definitions |#
#||||||||||||||||||||#

;; ExprC (core language) is either:
;; - a numC (number),
;; - an idC (identifier),
;; - an appC (function application),
;; - a binopC (binary operation), or
;; - a ifleq0 (conditional <= 0).
(define-type ExprC
  [numC (n : number)]
  [boolC (b : boolean)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : (listof ExprC))]
  [binopC (op : symbol) (l : ExprC) (r : ExprC)]
  [ifC (tst : ExprC) (thn : ExprC) (els : ExprC)]
  [lamC (arg : (listof symbol)) (body : ExprC)])

;; Binding (from PLAI Ch. 6) is:
;; - a bind from a name (symbol) to a value (number)
(define-type Binding
  [bind (name : symbol) (val : Value)])

;; An environment is a list of bindings
(define-type-alias Env (listof Binding))

;; An empty environment is just an empty list
(define mt-env empty)

;; A value is:
;; - a number or,
;; - a boolean
(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (arg : (listof symbol)) 
         (body : ExprC) 
         (env : Env)])


#|||||||||||||||||||||||||||||||||||||||||||#
#| Interfaces and Other Required Funcitons |#
#|||||||||||||||||||||||||||||||||||||||||||#

;; Combines parsing and evaluation, as provided in spec
(define (top-eval [s : s-expression]) : string
  (serialize (interp (parse s) mt-env)))

;; Consumes a Value type and maps the value
;; to its corresponding string representation
(define (serialize [val : Value]) : string
  (type-case Value val
    [numV (n) (to-string n)]
    [boolV (b) (cond [b "true"] [else "false"])]
    [closV (a b evn) "#<procedure>"]))

;; Consumes an AST and an environment,
;; and interprets the result into a Value
(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [numC (n) (numV n)]
    [boolC (b) (boolV b)]
    [idC (n) (lookup n env)]
    [appC (f as)
          (local 
            ([define f-value (interp f env)])
            (cond [(closV? f-value) 
                   (interp (closV-body f-value) 
                           (extend-env (closV-arg f-value) 
                                       (map (lambda ([arg : ExprC]) (interp arg env)) as) 
                                       (closV-env f-value)))]
                  [else (error 'interp "function application with non-closure")]))]
    [binopC (o l r) (binop o (interp l env) (interp r env))]
    [ifC (t thn els)
         (local ([define tst (interp t env)])
           (cond [(boolV? tst) 
                  (cond [(boolV-b (interp t env)) (interp thn env)]
                        [else (interp els env)])]
                 [else (error 'interp "if's condition did not evaluate to a boolean value")]))]
    [lamC (a b) (closV a b env)]))

;; Consumes s-exp of OWQQ3 and produces an AST
(define (parse [s : s-expression]) : ExprC
  (cond [(s-exp-number? s) (numC (s-exp->number s))]
        [(s-exp-match? `true s) (boolC #t)]
        [(s-exp-match? `false s) (boolC #f)]
        [(s-exp-symbol? s) 
         (local [(define symb (s-exp->symbol s))]
           (cond [(reserved? symb) 
                  (error 'parse 
                    (string-append "using a reserved symbol as an identifer: "
                                   (to-string symb)))]
                 [else (idC symb)]))]
        [(not (s-exp-list? s))
         (error 'parse (string-append "invalid s-expression: " (to-string s)))]
        [(s-exp-match? `{with {SYMBOL = ANY} ... ANY} s)
         (local [(define l (s-exp->list s))]
           (parse (desugar (rest l))))]
        [(s-exp-match? `{if ANY ANY ANY} s)
         (local [(define l (s-exp->list s))]
           (ifC (parse (second l)) (parse (third l)) (parse (fourth l))))]
        [(s-exp-match? `{func {SYMBOL ...} ANY} s)
         (local [(define l (rest (s-exp->list s)))]
           (lamC (map-unique (s-exp->list (first l)) (hash empty))
                 (parse (first (rest l)))))]
        [(and (not (first-reserved? (s-exp->list s))) (s-exp-match? `{ANY ANY ...} s))
         (local [(define l (s-exp->list s))]
           (appC (parse (first l)) (map parse (rest l))))] 
        [(and (first-binop? (s-exp->list s)) (s-exp-match? `{SYMBOL ANY ANY} s))
         (local [(define l (s-exp->list s))]
           (binopC (s-exp->symbol (first l)) (parse (second l)) (parse (third l))))]
        [else (error 'parse (string-append "invalid s-expression: " (to-string s)))]))

;; Consumes a list of s-expressions (local variable declartions and their
;; application), and desugars them into a literal function application
(define (desugar [l : (listof s-expression)]) : s-expression
  (cond [(empty? l) (list->s-exp empty)]
        [else
         (cond [(s-exp-match? `{SYMBOL = ANY} (first l))
                     (local [(define dsl (s-exp->list (desugar (rest l))))]
                       (list->s-exp 
                        (cons
                         (list->s-exp 
                          (cons`func 
                               (cons (list->s-exp
                                      (cons (first (s-exp->list (first l))) ; new varref (SYMBOL)
                                            (s-exp->list (second (s-exp->list (first dsl)))))) ; existing varrefs
                                     (list (third (s-exp->list (first dsl))))))) ; existing body
                                      
                         (cons (third (s-exp->list (first l))); new argument
                               (rest dsl)))))] ; existing arguments   
                    [(s-exp-match? `{ANY ...} (first l)) 
                     (list->s-exp
                      (list (list->s-exp (list `func `{} (first l)))))]
                    [else (error 'parse (string-append "invalid local variable declaration: "
                                                       (to-string (first l))))])]))


#|||||||||||||||||||||||||#
#| Core Helper Functions |#
#|||||||||||||||||||||||||#

;; An acition to extend an environment is to
;; append a new binding at the beginning
(define (extend-env [refs : (listof symbol)]
                    [args : (listof Value)] ; MAKE IT ACCPET LIST OF VALUES 
                    [env : Env]) : Env
  (cond [(= (length refs) (length args))
         (cond [(empty? args) env]
               [else (cons (bind (first refs) (first args)) ; mt-env ???
                           (extend-env (rest refs) (rest args) env))])]
        [else (error 'interp 
                     (string-append 
                      (string-append "wrong arity (expected vs given): "
                                     (to-string (length refs)))
                      (string-append " vs " (to-string (length args)))))]))

;; Consumes a symbol, and returns the corresponding Racket procedure;
;; if the symbol given is not a binary operator, throws an error
(define (binop-subst [o : symbol]) : (number number -> number)
  (cond [(symbol=? o '+) +]
        [(symbol=? o '-) -]
        [(symbol=? o '*) *]
        [(symbol=? o '/) /]
        [else (error o "not a binary operator")]))

;; Consumes a symbol, and returns the corresponding Racket comparison procedure;
;; if the symbol given is not a binary comparison operator, throws an error
(define (binop-comp-subst [o : symbol]) : ('a 'a -> boolean)
  (cond [(symbol=? o '<=) <=]
        [(symbol=? o 'eq?) eq?]
        [else (error o "not a binary comparison operator")]))

;; Consumes a symbol and checks whether this symbol is
;; one of the binary operator symbol for an identifier
(define (binop? [s : symbol]) : boolean
  (or (binop-arith? s) (binop-comp? s)))

;; Consumes a symbol and checks whether this symbol is
;; one of the arithmetic binary operator symbol
(define (binop-arith? [s : symbol]) : boolean
  (member s (list '+ '- '* '/)))

;; Consumes a symbol and checks whether this symbol is
;; one of the comparison binary operator symbol 
(define (binop-comp? [s : symbol]) : boolean
  (member s (list '<= 'eq?)))

;; Consumes a list of s-expressions and determines whether 
;; the first of the list contains a reserved operator
(define (first-binop? [l : (listof s-expression)]) : boolean
  (cond [(first-binop-arith? l) #t]
        [(first-binop-comp? l) #t]
        [else #f]))

;; Consumes a list of s-expressions and determines whether 
;; the first of the list contains a reserved operator
(define (first-binop-arith? [l : (listof s-expression)]) : boolean
  (cond [(not (s-exp-symbol? (first l))) #f]
        [(binop-arith? (s-exp->symbol (first l))) #t]
        [else #f]))

;; Consumes a list of s-expressions and determines whether 
;; the first of the list contains a reserved operator
(define (first-binop-comp? [l : (listof s-expression)]) : boolean
  (cond [(not (s-exp-symbol? (first l))) #f]
        [(binop-comp? (s-exp->symbol (first l))) #t]
        [else #f]))

;; Consumes a symbol (one of binops) and performs 
;; corresponding binary operation with the given values
(define (binop [o : symbol] [l : Value] [r : Value]) : Value
  (cond [(and (symbol=? o 'eq?) (or (not (numV? l)) (not (numV? r))))
         (nonnum-eq? l r)]
        [(and (binop-comp? o) (numV? l) (numV? r))
         (boolV ((binop-comp-subst o) (numV-n l) (numV-n r)))] 
        [(and (symbol=? '/ o) (numV? l) (= (numV-n r) 0))
         (error '/ "division by zero (OWQQ3)")]
        [(and (binop-arith? o) (numV? l) (numV? r))
         (numV ((binop-subst o) (numV-n l) (numV-n r)))]
        [(not (binop-arith? o))
         (error 'binop (string-append "not a binop: " (to-string o)))] 
        [else
         (error 'binop "one or more argument was not a number")]))

;; Consumes two non-numerical Values 
;; and evaluates their value equality
(define (nonnum-eq? [l : Value] [r : Value]) : Value
  (cond [(or (closV? l) (closV? r)) (boolV #f)]
        [(and (boolV? l) (boolV? r)) (boolV (eq? (boolV-b l) (boolV-b r)))]
        [else (boolV #f)]))

;; Consumes a symbol and checks whether this symbol is
;; a keyword for OWQQ3 (thus illegal for an identifier)
(define (keyword? [s : symbol]) : boolean
  (member s (list 'true 'false 'with 'if 'func)))

;; Consumes a symbol and checks whether this symbol is
;; a reserved (thus illegal) symbol for an identifier
(define (reserved? [s : symbol]) : boolean
  (cond [(or (binop-comp? s) (binop-arith? s) (keyword? s)) #t]
        [else #f]))

;; Consumes a list of s-expressions and determines whether 
;; the first of the list contains a reserved operator
(define (first-reserved? [l : (listof s-expression)]) : boolean
  (cond [(not (s-exp-symbol? (first l))) #f]
        [(reserved? (s-exp->symbol (first l))) #t]
        [else #f]))

;; Given a list of s-expressions (candiates for fdC-arg's), produces 
;; a list of symbols while recursively checking for duplicates 
(define (map-unique [l : (listof s-expression)] 
                    [ht : (hashof symbol boolean)]) : (listof symbol)
  (cond [(empty? l) empty]
        [else (local [(define s (s-exp->symbol (first l)))]
                (type-case (optionof boolean) (hash-ref ht s)
                  [some (bool)
                        (error 'map-unique 
                               (string-append
                                "duplicate identifier: "
                                (to-string s)))]
                  [none ()
                        (cond [(reserved? s)
                               (error 'parse 
                                      (string-append
                                       "using a reserved symbol as an identifer: "
                                       (to-string s)))]
                              [else (cons s
                                          (map-unique (rest l)
                                                      (hash-set ht s #t)))])]))]))

;; Consumes a symbol (varref) and an environment, 
;; and performs a lookup of the given symbol (varref)  
(define (lookup [ref : symbol] [env : Env]) : Value
  #;(raise-runtime-error "env with 'double" env)
  (cond
    [(empty? env) (error 'lookup 
                          (string-append "unbound variable reference: " 
                                         (to-string ref)))]
    [else (cond [(symbol=? ref (bind-name (first env)))
                 (bind-val (first env))]
                [else (lookup ref (rest env))])]))

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#| Extra Helper Functions for Debugging and Development |#
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

;; Raise runtime error the check the value of a given expression
(define (raise-runtime-error [expected : 'a] [actual : 'a])
  (error 'runtime (foldr string-append ""
                         (list (to-string expected) " was expected, but got " (to-string actual) ""))))

;; Evaluator that returns Value instead of
;; the string for more debugging information
(define (eval [s : s-expression]) : Value
  (interp (parse s) mt-env))


#|||||||||||||||||||||||||||||||||||||||||||||||||||||#
#| Tests for Interfaces and Other Required Functions |#
#|         (in the order of the definitions)         |#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||#

;; Test cases for top-eval
(test (top-eval `{+ 1 2}) 
      "3")
(test (top-eval `{/ 2 3}) 
      "2/3")
(test (top-eval `{{func {x y} {+ x y}} 1 2}) 
      "3")

(test (top-eval `{{func {x} {if (<= x 0) true false}} 1})
      "false")
(test (top-eval `{{func {x} {if (<= x 0) true false}} 0})
      "true")

(test/exn (top-eval `{{func {x y} {+ x y}} 1 2 3}) 
          "interp: wrong arity (expected vs given): 2 vs 3")
(test/exn (top-eval `{{func {x y} {+ x y}}}) 
          "interp: wrong arity (expected vs given): 2 vs 0")
(test/exn (top-eval `{{func {x} {if (+ x 0) true false}} 1})
          "interp: if's condition did not evaluate to a boolean value")

;; Test cases for serialize
(test (serialize (numV 0)) "0")
(test (serialize (boolV #t)) "true")
(test (serialize (boolV #f)) "false")
(test (serialize (closV (list 'a 'b) 
                        (binopC '+ (idC 'a) (idC 'b))
                        (list (bind 'a (numV 2))
                              (bind 'b (numV -2))))) 
      "#<procedure>")

;; New test cases for parse:
(test (parse `{{func {x} {+ 1 x}} 1})
      (appC (lamC (list 'x) (binopC '+ (numC 1) (idC 'x))) 
            (list (numC 1))))
(test (parse `{{func {x} {+ x 1}} 1 2})
      (appC (lamC (list 'x) (binopC '+ (idC 'x) (numC 1))) (list (numC 1) (numC 2))))
(test (parse `{<= {+ 1 2} {- 3 4}})
      (binopC '<= (binopC '+ (numC 1) (numC 2)) (binopC '- (numC 3) (numC 4))))
(test (parse `{<= 
               {* {+ 1 2} {- 3 4}} 
               {/ {- 3 4} {+ 1 2}}})
      (binopC '<= 
              (binopC '* 
                      (binopC '+ (numC 1) (numC 2)) 
                      (binopC '- (numC 3) (numC 4))) 
              (binopC '/ 
                      (binopC '- (numC 3) (numC 4)) 
                      (binopC '+ (numC 1) (numC 2)))))
(test (parse `{with {z = 1}
                    {+ z 3}})
      (appC (lamC (list 'z) (binopC '+ (idC 'z) (numC 3))) 
            (list (numC 1))))
(test (parse `{with {sq = {sq {x} {* x x}}}
                    {sq 3}})
      (appC (lamC (list 'sq) (appC (idC 'sq) (list (numC 3)))) 
            (list (appC (idC 'sq) 
                        (list (appC (idC 'x) (list)) 
                              (binopC '* (idC 'x) (idC 'x)))))))
(test/exn (parse `{with z})
      "parse: invalid local variable declaration: 'z")
(test/exn (parse `{{func {x x} {+ x x}} 1})
          "map-unique: duplicate identifier: 'x")
(test/exn (parse `{{func {x +} {+ x +}} 1})
          "parse: using a reserved symbol as an identifer: '+")
(test/exn (parse `{{func {if +} {+ if +}} 1})
          "parse: using a reserved symbol as an identifer: 'if")

;; Test cases for interp's exceptional cases
;; (non-exceptional cases are coverd by other test cases) 
(test/exn (interp (appC (numC 1) (list (numC 2))) empty)
          "interp: function application with non-closure")
(test/exn (interp (ifC (idC 'a) (numC 1) (numC 0)) 
                  empty) 
          "lookup: unbound variable reference: 'a")
(test/exn (interp (binopC '* (numC 1) (idC 'i)) 
                  empty) 
          "lookup: unbound variable reference: 'i")

;; Old test cases for parse:
(test (parse `1)
      (numC 1))
(test (parse `-0)
      (numC 0))
(test (parse `-3)
      (numC -3))
(test (parse `true)
      (boolC #t))
(test (parse `false)
      (boolC #f))
(test (parse `f)
      (idC 'f))
(test (parse `symbol)
      (idC 'symbol))
(test (parse `5ymb0l)
      (idC '5ymb0l))
(test (parse `{myFunc 1})
      (appC (idC 'myFunc) (list (numC 1))))
(test (parse `{yourFunc sym})
      (appC (idC 'yourFunc) (list (idC 'sym))))
(test (parse `{outerFunc {innerFunc arg}})
      (appC (idC 'outerFunc) (list (appC (idC 'innerFunc) (list (idC 'arg))))))
(test (parse `{outerFunc {middleFunc {innerFunc 123}}})
      (appC (idC 'outerFunc) (list (appC (idC 'middleFunc)
                                   (list (appC (idC 'innerFunc) 
                                               (list (numC 123))))))))
(test (parse `{+ 1 2})
      (binopC '+ (numC 1) (numC 2)))
(test (parse `{- 3 4})
      (binopC '- (numC 3) (numC 4)))
(test (parse `{* {+ 1 2} {- 3 4}})
      (binopC '* (binopC '+ (numC 1) (numC 2)) (binopC '- (numC 3) (numC 4))))
(test (parse `{/ 
               {* {+ 1 2} {- 3 4}} 
               {/ {- 3 4} {+ 1 2}}})
      (binopC '/ 
              (binopC '* 
                      (binopC '+ (numC 1) (numC 2)) 
                      (binopC '- (numC 3) (numC 4))) 
              (binopC '/ 
                      (binopC '- (numC 3) (numC 4)) 
                      (binopC '+ (numC 1) (numC 2)))))

(test (parse `{if false 2 3})
      (ifC (boolC #f) (numC 2) (numC 3)))
(test (parse `{if true {myFunc 2} {yourFunc sym}})
      (ifC (boolC #t) 
           (appC (idC 'myFunc) (list (numC 2))) 
           (appC (idC 'yourFunc) (list (idC 'sym)))))
(test (parse `{if false 
              {myFunc {if true 2 3}} 
              {yourFunc {if true 
                            {func1 2} 
                            {func2 sym}}}})
      (ifC (boolC #f) 
           (appC (idC 'myFunc) (list (ifC (boolC #t) (numC 2) (numC 3)))) 
           (appC (idC 'yourFunc) (list (ifC (boolC #t)
                                       (appC (idC 'func1) (list (numC 2)))
                                       (appC (idC 'func2) (list (idC 'sym))))))))

(test (parse `{hi})
      (appC (idC 'hi) (list)))
(test (parse `{myFunc 1 2 3})
      (appC (idC 'myFunc) (list (numC 1) (numC 2) (numC 3))))
(test (parse `{yourFunc sym bol s})
      (appC (idC 'yourFunc) (list (idC 'sym) (idC 'bol) (idC 's))))
(test (parse `{outerFunc {innerFunc arg1 arg2}})
      (appC (idC 'outerFunc) 
            (list (appC (idC 'innerFunc) 
                        (list (idC 'arg1) 
                              (idC 'arg2))))))
(test (parse `{outerFunc {middleFunc {innerFunc 123} {fun 0}}})
      (appC (idC 'outerFunc) 
            (list (appC (idC 'middleFunc) 
                        (list (appC (idC 'innerFunc) 
                                    (list (numC 123)))
                              (appC (idC 'fun) 
                                    (list (numC 0))))))))
(test/exn (parse `#t)
          "invalid s-expression: #t")
(test/exn (parse `#f)
          "invalid s-expression: #f")
(test/exn (parse `{/ 2 3 4}) 
          "invalid s-expression: '(/ 2 3 4)")
(test/exn (parse `{+ / 3})
          "using a reserved symbol as an identifer: '/")

;; Test cases for desugar
(test (desugar empty)
  `{})
(test (desugar (s-exp->list `{{+ x y}})) 
      `{{func {} {+ x y}}})
(test (desugar (s-exp->list `{{z = 1} {+ z 1}})) 
      `{{func {z} {+ z 1}} 1})
(test (desugar (s-exp->list `{{x = 1} {y = 2} {+ x y}})) 
      `{{func {x y} {+ x y}} 1 2})
(test (desugar (s-exp->list `{{z = {+ 9 14}} {y = 98} {+ z y}}))
      `{{func {z y} {+ z y}} {+ 9 14} 98})
(test (desugar (s-exp->list `{{sq = {func {x} {* x x}}} {sq 2}}))
      `{{func {sq} {sq 2}} {func {x} {* x x}}})
(test/exn (desugar (s-exp->list `{z = {+ 9 14}}))
      "parse: invalid local variable declaration: 'z")


#|||||||||||||||||||||||||||||||||||||#
#|  Tests for Core Helper Functions  |#
#| (in the order of the definitions) |#
#|||||||||||||||||||||||||||||||||||||#

;; Normal test cases for extned-env
;; (exceptional cases covered by other tests)
(test (extend-env (list) (list) mt-env) 
      mt-env)
(test (extend-env (list 'x) 
                  (list (numV 0)) 
                  mt-env) 
      (list (bind 'x (numV 0))))
(test (extend-env (list 'w 'x 'y 'z) 
                  (list (numV 0) (numV 0) (boolV #t) (closV (list) (numC 1) mt-env)) 
                  mt-env) 
      (list (bind 'w (numV 0))
            (bind 'x (numV 0)) 
            (bind 'y (boolV #t)) 
            (bind 'z (closV (list) (numC 1) (list)))))
(test (extend-env (list) (list) (list (bind 'x (numV 0)))) 
      (list (bind 'x (numV 0))))
(test (extend-env (list 'x) 
                  (list (numV 0)) 
                  (list (bind 'y (numV 0)))) 
      (list (bind 'x (numV 0))
            (bind 'y (numV 0))))
(test (extend-env (list 'x) 
                  (list (closV (list) (numC 1) mt-env)) 
                  (list (bind 'y (numV 3)))) 
      (list (bind 'x (closV (list) (numC 1) mt-env))
            (bind 'y (numV 3))))


;; Test cases for binop-subst
(test (binop-subst '+) +)
(test (binop-subst '-) -)
(test (binop-subst '*) *)
(test (binop-subst '/) /)
(test/exn (binop-subst '!) "!: not a binary operator")
(test/exn (binop-subst '?) "?: not a binary operator")
(test/exn (binop-subst '%) "%: not a binary operator")
(test/exn (binop-subst '&) "&: not a binary operator")

;; Test cases for binop-subst
(test (binop-comp-subst '<=) <=)
(test (binop-comp-subst 'eq?) eq?)
(test/exn (binop-comp-subst '+) "+: not a binary comparison operator")
(test/exn (binop-comp-subst '-) "-: not a binary comparison operator")
(test/exn (binop-comp-subst '*) "*: not a binary comparison operator")
(test/exn (binop-comp-subst '/) "/: not a binary comparison operator") 
(test/exn (binop-comp-subst '!) "!: not a binary comparison operator")
(test/exn (binop-comp-subst '?) "?: not a binary comparison operator")
(test/exn (binop-comp-subst '%) "%: not a binary comparison operator")
(test/exn (binop-comp-subst '&) "&: not a binary comparison operator")

;; Test cases for binop-comp-subst
(test (binop-comp-subst '<=) <=)
(test (binop-comp-subst 'eq?) eq?)
(test/exn (binop-comp-subst '+) "+: not a binary comparison operator")
(test/exn (binop-comp-subst '-) "-: not a binary comparison operator")
(test/exn (binop-comp-subst '*) "*: not a binary comparison operator")
(test/exn (binop-comp-subst '/) "/: not a binary comparison operator")
(test/exn (binop-comp-subst 'ifleq0) "ifleq0: not a binary comparison operator")
(test/exn (binop-comp-subst 'legal) "legal: not a binary comparison operator")

;; Test cases for binop?
(test (binop? '+) #t)
(test (binop? '-) #t)
(test (binop? '*) #t)
(test (binop? '/) #t)
(test (binop? 'ifleq0) #f)
(test (binop? 'legal) #f)
(test (binop? '>=) #f)
(test (binop? '+*) #f)
(test (binop? '-/) #f)
(test (binop? '<=) #t)
(test (binop? 'eq?) #t)
(test (binop? 'ifleq0) #f)
(test (binop? 'legal) #f)
(test (binop? '>=) #f)
(test (binop? '+*) #f)
(test (binop? '-/) #f)

;; Test cases for binop-arith?
(test (binop-arith? '+) #t)
(test (binop-arith? '-) #t)
(test (binop-arith? '*) #t)
(test (binop-arith? '/) #t)
(test (binop-arith? 'ifleq0) #f)
(test (binop-arith? 'legal) #f)
(test (binop-arith? '>=) #f)
(test (binop-arith? '+*) #f)
(test (binop-arith? '-/) #f)

;; Test cases for binop-comp?
(test (binop-comp? '<=) #t)
(test (binop-comp? 'eq?) #t)
(test (binop-comp? '+) #f)
(test (binop-comp? '-) #f)
(test (binop-comp? '*) #f)
(test (binop-comp? '/) #f)
(test (binop-comp? 'ifleq0) #f)
(test (binop-comp? 'legal) #f)
(test (binop-comp? '>=) #f)
(test (binop-comp? '+*) #f)
(test (binop-comp? '-/) #f)

;; Test cases for first-binop?
(test (first-binop? (s-exp->list `{+ 0 1})) #t)
(test (first-binop? (s-exp->list `{- 0 1 2})) #t)
(test (first-binop? (s-exp->list `{* 0 1 2 3})) #t)
(test (first-binop? (s-exp->list `{/ 0 1 2 3 4})) #t)
(test (first-binop? (s-exp->list `{! 0 1 2 3 4})) #f)
(test (first-binop? (s-exp->list `{? 0 1 2 3})) #f)
(test (first-binop? (s-exp->list `{% 0 1 2 })) #f)
(test (first-binop? (s-exp->list `{& 0 1})) #f)
(test (first-binop? (s-exp->list `{{1 2}})) #f)
(test (first-binop? (s-exp->list `{<= 0 1})) #t)
(test (first-binop? (s-exp->list `{eq? 0 1})) #t)
(test (first-binop? (s-exp->list `{? 0 1 2 3})) #f)
(test (first-binop? (s-exp->list `{% 0 1 2 })) #f)
(test (first-binop? (s-exp->list `{0 1 2 })) #f)

;; Test cases for first-binop-arith?
(test (first-binop-arith? (s-exp->list `{+ 0 1})) #t)
(test (first-binop-arith? (s-exp->list `{- 0 1 2})) #t)
(test (first-binop-arith? (s-exp->list `{* 0 1 2 3})) #t)
(test (first-binop-arith? (s-exp->list `{/ 0 1 2 3 4})) #t)
(test (first-binop-arith? (s-exp->list `{! 0 1 2 3 4})) #f)
(test (first-binop-arith? (s-exp->list `{? 0 1 2 3})) #f)
(test (first-binop-arith? (s-exp->list `{% 0 1 2 })) #f)
(test (first-binop-arith? (s-exp->list `{& 0 1})) #f)
(test (first-binop-arith? (s-exp->list `{{1 2}})) #f)

;; Test cases for first-binop-comp?
(test (first-binop-comp? (s-exp->list `{<= 0 1})) #t)
(test (first-binop-comp? (s-exp->list `{eq? 0 1})) #t)
(test (first-binop-comp? (s-exp->list `{+ 0 1})) #f)
(test (first-binop-comp? (s-exp->list `{- 0 1 2})) #f)
(test (first-binop-comp? (s-exp->list `{? 0 1 2 3})) #f)
(test (first-binop-comp? (s-exp->list `{% 0 1 2 })) #f)
(test (first-binop-comp? (s-exp->list `{0 1 2 })) #f)

;; Test cases for binop
(test (binop '+ (numV -1) (numV 1)) (numV 0))
(test (binop '- (numV -1) (numV 1)) (numV -2))
(test (binop '* (numV -1) (numV 1)) (numV -1))
(test (binop '/ (numV -1) (numV 1)) (numV -1))
(test (binop '<= (numV 1) (numV 1)) (boolV #t))
(test (binop '<= (numV -1) (numV 1)) (boolV #t))
(test (binop '<= (numV 1) (numV -1)) (boolV #f))
(test (binop 'eq? (numV 1) (numV 1)) (boolV #t))
(test (binop 'eq? (numV -1) (numV 1)) (boolV #f))
(test (binop 'eq? (numV 1) (numV -1)) (boolV #f))
(test (binop 'eq? (boolV #t) (boolV #t)) (boolV #t))
(test (binop 'eq? (boolV #t) (boolV #f)) (boolV #f))
(test (binop 'eq? (boolV #t) (numV 0)) (boolV #f))
(test (binop 'eq? (numV 0) (boolV #f)) (boolV #f))
(test (binop 'eq? (numV 0) 
                  (closV empty (numC 0) mt-env)) 
      (boolV #f))
(test (binop 'eq? (boolV #t) 
                  (closV empty (numC 0) mt-env)) 
      (boolV #f))
(test (binop 'eq? (closV empty (numC 0) mt-env) 
                  (closV empty (numC 0) mt-env)) 
      (boolV #f))
(test/exn (binop '= (numV -1) (numV 1)) "binop: not a binop: '=")
(test/exn (binop '? (numV 1) (numV -1)) "binop: not a binop: '?")
(test/exn (binop '+ (numV -1) (boolV #t)) "binop: one or more argument was not a number")

;; Test cases for nonnum-eq?
(test (nonnum-eq? (boolV #t) (boolV #t)) (boolV #t))
(test (nonnum-eq? (boolV #t) (boolV #f)) (boolV #f))
(test (nonnum-eq? (boolV #t) (numV 0)) (boolV #f))
(test (nonnum-eq? (numV 0) (boolV #f)) (boolV #f))
(test (nonnum-eq? (numV 0) 
                  (closV empty (numC 0) mt-env)) 
      (boolV #f))
(test (nonnum-eq? (boolV #t) 
                  (closV empty (numC 0) mt-env)) 
      (boolV #f))
(test (nonnum-eq? (closV empty (numC 0) mt-env) 
                  (closV empty (numC 0) mt-env)) 
      (boolV #f))

;; Test cases for keyword?
(test (keyword? 'true) #t)
(test (keyword? 'false) #t)
(test (keyword? 'with) #t)
(test (keyword? 'if) #t)
(test (keyword? 'func) #t)
(test (keyword? '+) #f)
(test (keyword? '-) #f)
(test (keyword? '*) #f)
(test (keyword? '/) #f)

;; Test cases for reserved?
(test (reserved? '+) #t)
(test (reserved? '-) #t)
(test (reserved? '*) #t)
(test (reserved? '/) #t)
(test (reserved? '<=) #t)
(test (reserved? 'eq?) #t)
(test (reserved? 'true) #t)
(test (reserved? 'false) #t)
(test (reserved? 'with) #t)
(test (reserved? 'if) #t)
(test (reserved? 'func) #t)
(test (reserved? 'ifleq0) #f)
(test (reserved? 'legal) #f)
(test (reserved? '+*) #f)
(test (reserved? '-/) #f)

;; Test cases for first-reserved?
(test (first-reserved? (s-exp->list `{+ 0 1})) #t)
(test (first-reserved? (s-exp->list `{- 0 1 2})) #t)
(test (first-reserved? (s-exp->list `{* 0 1 2 3})) #t)
(test (first-reserved? (s-exp->list `{/ 0 1 2 3 4})) #t)
(test (first-reserved? (s-exp->list `{if 0 1 2 3 4})) #t)
(test (first-reserved? (s-exp->list `{ifleq0 0 1 2})) #f)
(test (first-reserved? (s-exp->list `{ifleq1 0 1 2})) #f)
(test (first-reserved? (s-exp->list `{! 0 1 2 3 4})) #f)
(test (first-reserved? (s-exp->list `{? 0 1 2 3})) #f)
(test (first-reserved? (s-exp->list `{% 0 1 2 })) #f)
(test (first-reserved? (s-exp->list `{& 0 1})) #f)
(test (first-reserved? (s-exp->list `{{1 2}})) #f)

;; Test cases for map-unique
(test (map-unique empty (hash empty))
      empty)
(test (map-unique (list `a `b `c `d) (hash empty))
      (list 'a 'b 'c 'd))
(test/exn (map-unique (list `a `b `c `a) (hash empty))
      "map-unique: duplicate identifier: 'a")
(test/exn (map-unique (list `a `a) (hash empty))
      "map-unique: duplicate identifier: 'a")

;; Test cases for lookup
(test (lookup 'double 
              (list (bind 'quadruple 
                          (closV  (list 'x)
                                  (appC (idC 'double) (list (appC (idC 'double) (list (idC 'x))))) 
                                  (list (bind 'double 
                                              (closV  (list 'x) 
                                                      (binopC '+ (idC 'x) (idC 'x)) 
                                                      mt-env))))) 
                    (bind 'double 
                          (closV (list 'x) 
                                 (binopC '+ (idC 'x) (idC 'x)) 
                                 mt-env))))
      (closV (list 'x) 
             (binopC '+ (idC 'x) (idC 'x)) 
             mt-env))
(test/exn (lookup 'a mt-env) 
          "lookup: unbound variable reference: 'a")
(test/exn (lookup 'x (list (bind 'a (boolV #f)))) 
          "lookup: unbound variable reference: 'x")


#||||||||||||||||||||||||||||||||||||||#
#|  Tests for Extra Helper Functions  |#
#|  (in the order of the definitions) |#
#||||||||||||||||||||||||||||||||||||||#

;; Test cases for raise-runtime-error:
(test/exn (raise-runtime-error 'a 'b) "'a was expected, but got 'b")

;; Test cases for eval:
(test (eval `{+ 1 2}) 
      (numV 3))
(test (eval `{/ 2 3}) 
      (numV 2/3))
(test (eval `{{func {x y} {+ x y}} 1 2}) 
      (numV 3))

(test (eval `{{func {x} {if (<= x 0) true false}} 1})
      (boolV #f))
(test (eval `{{func {x} {if (<= x 0) true false}} 0})
      (boolV #t))
(test (eval `{with {sq = {func {x} {* x x}}}
                   {sq 2}})
      (numV 4))

(test (eval `{with {sq = {func {x} {* x x}}}
                   {a = 2}
                   {sq a}})
      (numV 4))

(test (eval `{with {add = {func {x y} {+ x y}}}
                   {a = -1}
                   {b = 1}
                   {add a b}})
      (numV 0))

(test (eval `{with {double = {func {x} {+ x x}}}
                   {with {quadruple = {func {x} {double {double x}}}}
                         {quadruple 10}}})
      (numV 40))
(test/exn (eval `{/ 1 0}) 
          "/: division by zero (OWQQ3)")
(test/exn (eval `{with {quadruple = {func {x} {double {double x}}}}
                   {with {double = {func {x} {+ x x}}}
                         {quadruple 10}}})
          "lookup: unbound variable reference: 'double")
(test/exn (eval `{+ 1 true})
          "binop: one or more argument was not a number")
(test/exn (eval `{{func {x y} {+ x y}} 1 2 3}) 
          "interp: wrong arity (expected vs given): 2 vs 3")
(test/exn (eval `{{func {x y} {+ x y}}}) 
          "interp: wrong arity (expected vs given): 2 vs 0")
(test/exn (eval `{{func {x} {if (+ x 0) true false}} 1})
          "interp: if's condition did not evaluate to a boolean value")


#||||||||||||||||||||||||||||||||||||||#
#| Test Case for Captain Teach (Mine) |#
#||||||||||||||||||||||||||||||||||||||#

;; Test case for top-eval that contains a call to a function 
;; where the caller and callee have disjoint nonempty environments. 
;; That is, nothing in scope at the call site is in scope in the 
;; body of the caller, but both environments contain something

(test (top-eval `{with {add1 = {func {x1} {+ 1 x1}}}
                       {with {add3 = {func {x2} {+ 2 {add1 x2}}}}
                             {add3 1}}})
      "4")

; (test (parse `{with {add1 = {func {x1} {+ 1 x1}}}
;                        {with {add3 = {func {x2} {+ 2 {add1 x2}}}}
;                              {add3 1}}})
;       (numC 1))

; (test (desugar (s-exp->list `{{add3 = {func {x2} {+ 2 {add1 x2}}}}
;                              {add3 1}}))
;       `{})

; (appC (lamC '(add1) 
;              (appC (lamC '(add3) 
;                           (appC (idC 'add3) 
;                                 (list (numC 1)))) 
;                    (list (lamC '(x2) 
;                                 (binopC '+ 
;                                         (numC 2) 
;                                         (appC (idC 'add1) (list (idC 'x2)))))))) 
;       (list (lamC '(x1) (binopC '+ (numC 1) (idC 'x1)))))