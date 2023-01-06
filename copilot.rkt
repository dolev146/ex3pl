#lang pl

#|
 1. Introduction
 In class we saw the FLANG language and interpreter. It allows simple arithmetic
 expressions, it allows binding identifiers to expressions, and it provides a first
 class treatment for functions. It does not, however, allow for conditional
 expressions and Boolean values.
 In the following, we will expand the FLANG (see here) language to also treat
 conditionals. Our goal is to allow logical operators and expressions to deal with
 Boolean (logical) values. In general, our treatment will have a very similar
 semantics to that of the pl language. Specifically, we will add if expressions, the
 binary (numeric to Boolean) operators <, >, =, the unary operator (Boolean to
 Boolean) not, and the Boolean values True and False (the expressions #t and #f
 will not be part of our language). In addition, we will allow the run interface to
 return to the user non-numeric values.
 Here are some tests that should work after you are done:
 ;; tests
 (test (run "True") => true)
 (test (run "{not True}") => false)
 (test (run "{> 3 44}") => false)
 (test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
 (test (run "{with {x 8}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
 (test (run "{with {x 0}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
 (test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
 (test (run "{with {c True}
 {if c {then-do {> 2 1}} {else-do 2}}}")
 => true)
 (test (run "{with {foo {fun {x}
 {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
 => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Divide (Id 'x) (Num 2)))))
 (test (run "{with {x 0}
 {if {> x 0} {/ 2 x} x}}")
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
 (test (run "true") =error> "eval: free identifier: true")
 (test (run "{< false 5}") =error> "eval: free identifier: false")
 (test (run "{< False 5}")
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")
 A remark about semantics of if expressions: Note that a conditional if
 expression cannot be replaced by a function. That is, you cannot just define a
 function named 'if' and apply it to three arguments. This is because the
 semantics of an if expression is “lazy” in the sense that it only evaluates the
 “then-do” part if the condition is satisfied and the “else-do” part if the condition is
 not satisfied (but never evaluates both).
|#



(define-type FLANG
;; FLANG is the type of FLANG expressions
 [Num Number]
;; Num is the type of numeric values
 [Plus FLANG FLANG]
;; Plus is the type of addition expressions
 [Minus FLANG FLANG]
;; Minus is the type of subtraction expressions
 [Multipilication FLANG FLANG]
;; Multipilication is the type of multiplication expressions
 [Divide FLANG FLANG]
;; Divide is the type of division expressions
 [With Symbol FLANG FLANG]
;; With is the type of with expressions
 [Id Symbol]
;; Id is the type of identifiers
 [Fun Symbol FLANG]
;; Fun is the type of function expressions 
 [Call FLANG FLANG]
;; Call is the type of function calls
 [Bool Boolean]
;; Bool is the type of Boolean values
 [Bigger FLANG FLANG]
;; Bigger is the type of greater-than expressions
 [Smaller FLANG FLANG]
;; Smaller is the type of smaller-than expressions
 [Equal FLANG FLANG]
;; Equal is the type of equal expressions
 [Not FLANG]
;; Not is the type of not expressions
 [If FLANG FLANG FLANG]
;; If is the type of if expressions
 )
 



(: parse-sexpr : Sexpr -> FLANG)
;; declare parse-sexpr function
 (define (parse-sexpr sexpr)
;; define parse-sexpr function
 (match sexpr
 ;; look for the following patterns in sexpr: number #t #f symbol and return the corresponding FLANG
 [(number: n) (Num n)]
 ['True (Bool true)]
 ['False (Bool false)]
 [(symbol: name) (Id name)]
 ;; if sexpr is a list, look for the following patterns in sexpr: + - * / 
 ;; with fun call = > < if not and return the corresponding FLANG
 [(cons 'with more)
  ( match sexpr
   [(list 'with (list (symbol: name) named-expr) body) (With name (parse-sexpr named-expr)(parse-sexpr body))]
   [else (error 'parse-sexpr "Invalid syntax for with")])]
 [(cons 'fun more)
  ( match sexpr
   [(list 'fun (list (symbol: name)) body)(Fun name (parse-sexpr body))]
   [else (error 'parse-sexpr "invalid syntax for fun")])]
 [(list '+ l r) (Plus (parse-sexpr l) (parse-sexpr r))]
 ;; parse-sexpr : Sexpr -> FLANG
 [(list '- l r) (Minus (parse-sexpr l) (parse-sexpr r))]
 ;; parse-sexpr : Sexpr -> FLANG
 [(list '* l r) (Multipilication (parse-sexpr l) (parse-sexpr r))]
 ;; parse-sexpr : Sexpr -> FLANG
 [(list '/ l r) (Divide (parse-sexpr l) (parse-sexpr r))]
 ;; devide sexpr split the numbers to the left and right of the operator
 [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
 ;; call sexpr split the function and the argument
 [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
 ;; equal sexpr split the numbers to the left and right of the operator
 [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
 ;; greater than sexpr , check if the number on the left is bigger than the number on the right
 [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
 ;; smaller than sexpr , check if the number on the left is smaller than the number on the right
 [(list 'not exp) (Not (parse-sexpr exp))]
 ;; not sexpr , check if the number is not equal to 0
 [(cons 'if more)
 ;; if sexpr , check if the condition is true or false and return the corresponding FLANG for example 
  (match sexpr
    [(list 'if con (list 'then-do then) (list 'else-do else))(If (parse-sexpr con) (parse-sexpr then) (parse-sexpr else))]
    [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
;; if the condition is true return the then-do part and if the condition is false return the else-do part
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
 ;; if the syntax is not correct return an error message


(: parse : String -> FLANG)
;; declare parse function
(define (parse code)
;; define parse function
 (parse-sexpr (string->sexpr code))
;; parse-sexpr : Sexpr -> FLANG
)

(test (parse "{if {= 3 3} {then-do 4} {else-do 5}}") => (If (Equal (Num 3) (Num 3)) (Num 4) (Num 5)))


#|
 2. Expanding the FLANG BNF language
 Extend your BNF and Parser to Support this syntax. Note that `then-do’ and
 `else-do` are terminals and are part of the syntax of the “if” special form.
 #| The grammar:
 <FLANG> ::= <num> ;; Rule 1
 | { + <FLANG> <FLANG> } ;; Rule 2
 | { - <FLANG> <FLANG> } ;; Rule 3
 | { * <FLANG> <FLANG> } ;; Rule 4
 | { / <FLANG> <FLANG> } ;; Rule 5
 | { with { <id> <FLANG> } <FLANG> } ;; Rule 6
 | <id> ;; Rule 7
 | { fun { <id> } <FLANG> } ;; Rule 8
 | { call <FLANG> <FLANG> } ;; Rule 9
 | —«fill-in 1»— ;; add rule for True ;; Rule 10
 | —«fill-in 2»— ;; Rule 11
 | —«fill-in 3»— ;; add rule for = ;; Rule 12
 | —«fill-in 4»— ;; Rule 13
 | —«fill-in 5»— ;; Rule 14
 | —«fill-in 6»— ;; Rule 15
 | —«fill-in 7»— ;; add rule 16 for (the above) if
 expressions
 |#
|#



(: subst : FLANG Symbol FLANG -> FLANG)
;; declare subst function
 (define (subst expr from to)
;; define subst function
 (cases expr
;; cases expr
    [(Num n) expr]
;; if expr is a number return expr
    [(Plus l r) (Plus (subst l from to) (subst r from to))]
;; if expr is a plus return the sum of the left and right of the operator
    [(Minus l r) (Minus (subst l from to) (subst r from to))]
;; if expr is a minus return the difference of the left and right of the operator
    [(Multipilication l r) (Multipilication (subst l from to) (subst r from to))]
;; if expr is a multiplication return the product of the left and right of the operator
    [(Divide l r) (Divide (subst l from to) (subst r from to))]
;; if expr is a division return the quotient of the left and right of the operator
    [(With name named body)(With name (subst named from to)(if (eq? from name) body (subst body from to)))]
;; if expr is a with return the body of the with
    [(Fun name body)(Fun name (if (eq? name from) body (subst  body from to)))]
;; if expr is a function return the body of the function
   [(Call fun-expr arg-expr)(Call (subst fun-expr from to) (subst arg-expr from to))]
;; if expr is a call return the function and the argument
   [(Id name) (if (eq? from name) to expr)]
;; if expr is an id return the name
   [(Bool b) expr]
;; if expr is a boolean return expr
   [(Equal l r) (Equal (subst l from to) (subst r from to))]
;; if expr is an equal return the equality of the left and right of the operator
   [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
;; if expr is a bigger return the bigger of the left and right of the operator
   [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
;; if expr is a smaller return the smaller of the left and right of the operator
   [(Not exp) (Not (subst exp from to))]
;; if expr is a not return the not of the expression
   [(If con then else) (If (subst con from to) (subst then from to) (subst else from to))])) 


(: Num->number : FLANG -> Number)
;; declare Num->number function
(define (Num->number e)
;; define Num->number function
 (cases e
;; cases e
 [(Num n) n]
;; if e is a number return n
 [else (error 'Num->number "expected a number, got: ~s" e)]))
;; if e is not a number return an error message

 (: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; declare arith-op function
 (define (arith-op op expr1 expr2)
;; define arith-op function
 (Num (op (Num->number expr1) (Num->number expr2))))
;; if expr1 and expr2 are numbers return the operation of the operator and the numbers
 (: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; declare logic-op function
 (define (logic-op op expr1 expr2)
;; define logic-op function
 (Bool (op (Num->number expr1) (Num->number expr2))))
;; if expr1 and expr2 are numbers return the operation of the operator and the numbers
 (: flang->bool : FLANG -> Boolean)
;; declare flang->bool function
 (define (flang->bool e)
;; define flang->bool function
 (cases e
;; cases e
   [(Bool b) b]
;; if e is a boolean return b
   [else #t]))
;; if e is not a boolean return #t



#|
 3. Extending the Parser
 Use the above test examples to complete the missing parts of the FLANG
 type definition and the parse-sexpr procedure.
 (define-type FLANG
 [Num Number]
 ... Original interpreter's code omitted...
 [Call FLANG FLANG]
 [Bool <--fill in 1 -->]
 [Bigger <--fill in 2 -->]
 [Smaller <--fill in 3 -->]
 [Equal <--fill in 4 -->]
 [Not <--fill in 5 -->]
 [If <--fill in 6 -->])
 (: parse-sexpr : Sexpr -> FLANG)
 ;; to convert s-expressions into FLANGs
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
 ['True (Bool true)]
 ['False <--fill in 1-->])
 [(symbol: name) (Id name)]
 ... Original interpreter's code omitted...
 [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
 [(list '= lhs rhs) (Equal <--fill in 2 -->)]
 [(list '> lhs rhs) <--fill in 3 -->]
 [<--fill in 4 -->]
 [(list 'not exp) <--fill in 5 -->]
 [(cons 'if <--fill in 6 -->)]
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])
|#


#|
 4. Extending subst and eval
 Use the following formal rules to complete the code for the subst procedure.
 Formal Substitution rules:
 subst:
 N[v/x] = N
 {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]}
 {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]}
 {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]}
 {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]}
 y[v/x] = y
 x[v/x] = v
 {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
 {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
 {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]}
 {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x
 {fun {x} E}[v/x] = {fun {x} E}
 B[v/x] = B ;; B is Boolean
 {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
 {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
 {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
 { not E}[v/x] = {not E[v/x]}
 {if Econd {then-do Edo} {else-do Eelse}}[v/x]
 = {if Econd[v/x] {then-do Edo[v/x]} {else-do
 Eelse[v/x]}}
 (: subst : FLANG Symbol FLANG -> FLANG)
 ;; substitutes the second argument with the third argument in the
 ;; first argument, as per the rules of substitution; the resulting
 ;; expression contains no free instances of the second argument
 (define (subst expr from to)
 (cases expr
 ... Original interpreter's code omitted...
 [(Bool b) <--fill in 1 -->]
 [(Equal l r) <--fill in 2 -->]
 [<--fill in 3 -->]
 [<--fill in 4 -->]
 [<--fill in 5 -->]
 [<--fill in 6 -->]))
 Use the following provided procedures and the formal rules below to complete
 the code below for the logic-op procedure and for the flang->bool procedure.
 ;; The following function is used in multiple places below,
 ;; hence, it is now a top-level definition
 (: Num->number : FLANG -> Number)
 ;; gets a FLANG -- presumably a Num variant -- and returns the
 ;; unwrapped number
 (define (Num->number e)
 (cases e
 [(Num n) n]
 [else (error 'Num->number "expected a number, got: ~s" e)]))
 (: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
 ;; gets a Racket numeric binary operator, and uses it within a FLANG
 ;; `Num' wrapper
 (define (arith-op op expr1 expr2)
 (Num (op (Num->number expr1) (Num->number expr2))))
 (: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
 ;; gets a Racket Boolean binary operator (on numbers), and applies it
 ;; to two `Num' wrapped FLANGs
 (define (logic-op op expr1 expr2)
 <--fill in 1 -->)
 (: flang->bool : FLANG -> Boolean)
 ;; gets a Flang E (of any kind) and returns a its appropiate
 ;; Boolean value -- which is true if and only if E does not
 ;; represent false
 ;; Remark: the `flang->bool` function will also be top-level
 ;; since it's used in more than one place.
 (define (flang->bool e)
 (cases e
 [<--fill in 2 -->]
 [else <--fill in 3 -->]))
|#


#|
 Use the above defined procedures and the formal rules below to complete the
 code below for the eval procedure. Consult the provided tests at the introduction
 part of the assignment.
 eval: Evaluation rules:
 eval(N) = N ;; N is an expression for a numeric value
 eval({+ E1 E2}) = eval(E1) + eval(E2) \ if both E1 and E2
 eval({- E1 E2}) = eval(E1) - eval(E2) \ evaluate to numbers
 eval({* E1 E2}) = eval(E1) * eval(E2) / otherwise error!
 eval({/ E1 E2}) = eval(E1) / eval(E2) /
 eval(id) = error! eval({with {x E1} E2}) =
 eval(E2[eval(E1)/x])
 eval(FUN) = FUN ; assuming FUN is a function expression
 eval({call E1 E2}) = eval(Ef[eval(E2)/x])
 if eval(E1)={fun {x} Ef}
 = error! otherwise
 eval(B) = B ;; B is an expression for a Boolean value
 eval({= E1 E2}) = eval(E1) = eval(E2) \ if both E1 and E2
 eval({> E1 E2}) = eval(E1) > eval(E2) \ evaluate to
 numbers
 eval({< E1 E2}) = eval(E1) < eval(E2) / otherwise error!
 eval({not E}) = not(eval(E)) /E may be anything
 eval({if Econd {then-do Edo} {else-do Eelse}})
 = eval(Edo) if eval(Econd) =/= false,
 eval(Eelse), otherwise.
 Remark: The semantics of the not operation is defined by the `not’ operation of
 pl.
 (: eval : FLANG -> FLANG)
 ;; evaluates FLANG expressions by reducing them to *expressions*
 (define (eval expr)
 (cases expr
 ... Original interpreter's code omitted...
 [(Bool b) <--fill in 1 -->]
 [<--fill in 2 -->]
 [<--fill in 3 -->]
 [<--fill in 4 -->]
 [(If l m r)
 (let ([<--fill in 5 -->])
 (<--fill in 6 -->))]
 [(Not exp) [<--fill in 7 -->]]))
|#


#|
 5. Extending the run procedure
 Finally, we will allow the interface procedure to return any one of the three
 possible types of the extended language. Use the above test examples to
 complete the code for the run procedure.
 (: run : String -> (U Number Boolean FLANG))
 ;; evaluate a FLANG program contained in a string
 (define (run str)
 (let ([result (eval (parse str))])
 (cases result
 [<--fill in 1 -->]
 [<--fill in 2 -->]
 [<--fill in 3 -->])))
|#


(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
 (define (eval expr)
 ;; (displayln expr)
 (cases expr
 ;; Original interpreter's code omitted...
   [(Num n) expr]
   ;; if both E1 and E2 evaluate to numbers
   [(Plus l r)  (arith-op + (eval l) (eval r))]
   ;; if plus evaluates to a number and the other evaluates to a boolean
   [(Minus l r) (arith-op - (eval l) (eval r))]
   ;; if minus evaluates to a number and the other evaluates to a boolean
   [(Multipilication l r) (arith-op * (eval l) (eval r))]
   ;; if multiplication evaluates to a number and the other evaluates to a boolean
   [(Divide l r) (arith-op / (eval l) (eval r))]
   ;; if divide evaluates to a number and the other evaluates to a boolean
   [(With name named body) (eval (subst body name (eval named)))]
   ;; if the name is not defined
   [(Id name) (error 'eval "free identifier: ~s" name)]
   ;; if the name is defined
   [(Fun name body) expr]
   ;; fun is a function expression
   [(Call fun-expr arg-expr) (let ([fval (eval fun-expr)]) (cases fval [(Fun name body) 
   ;; call the function 
   (eval (subst body name (eval arg-expr)))]
   ;; if the function is not defined
    [else (error 'eval "expected a function, got: ~s" fval)]))]
    ;; if the function is defined
  [(Bool b) expr]
  ;; if both E1 and E2 evaluate to numbers
  [(Equal l r) (logic-op = (eval l) (eval r))]
  ;; if plus evaluates to a number and the other evaluates to a boolean
  [(Bigger l r) (logic-op > (eval l) (eval r))]
  ;; if minus evaluates to a number and the other evaluates to a boolean
  [(Smaller l r) (logic-op < (eval l) (eval r))]
  ;; if multiplication evaluates to a number and the other evaluates to a boolean
  [(If l m r)
  ;; if the condition is true
   (let ([ival (eval l)])
   ;; if the condition is false
     (if (eq? (flang->bool ival) #t) (eval m) (eval r)))]
    ;; if the condition is not a boolean 
  [(Not exp) (Bool (not (flang->bool (eval exp))))]))


(: run : String -> (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string
 (define (run str)
 ;; (displayln str)
 (let ([output (eval (parse str))])
 ;; (displayln output)
(cases output
;; if the output is a number
 [(Num n) n]
 ;; if the output is a boolean
 [(Bool b) b]
;; if the output is a FLANG expression
 [else output])))




