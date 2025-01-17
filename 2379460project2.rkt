; Jeff Cailteux
; KUID: 2379460
; Project 2, Part 1
; EECS 662
; CFAE

#lang plai

;Define CFAE
(define-type CFAE
	(num (n number?))
	(id (name symbol?))
	(op (oper symbol?) (lhs CFAE?) (rhs CFAE?))
	(fun (param symbol?) (body CFAE?))
	(app (funct CFAE?) (arg CFAE?))
	(if0 (c CFAE?) (t CFAE?) (e CFAE?)))

;Define Deferred Substitution
(define-type DefrdSub
	(mtSub)
	(aSub (name symbol?) (value number?) (ds DefrdSub?))
   (afSub (name symbol?) (func CFAE?) (ds DefrdSub?)))

;operators
;list of symbol operator pairs
(define binop
  (list (list 'add +)
        (list 'sub -)
        (list 'mul *)
        (list 'div /)
        ))

;get operator
;turns operator symbols into operator text by looking up binop type
(define get-binop
  (lambda (type binop)
    (cond ((empty? binop)
           (error 'get-binop "Bad Operator"))
          (else (if (symbol=? type (first (first binop))) 
                    (cadar binop)(get-binop type (cdr binop)))
                )
          )))

;Lookup for Deferred Sub
(define lookup
	(lambda (name ds)
		(type-case DefrdSub ds
			(mtSub () (error 'lookup "no binding for identifier"))
			(aSub (n v rds)
				(if (symbol=? n name)
				     v
				    (lookup name rds)))
                  (afSub (n f rds)
				(if (symbol=? n name)
				     f
				    (lookup name rds))))))

;Interpreter
(define interp-cfae
  (lambda (expr ds)
		(type-case CFAE expr
			(num (n) n)
			(id (v) (lookup v ds))
			(op (oper l r) (if (or (fun? l) (fun? r))
							(error 'interp-cfae "Cannot perform arithmetic expression on function")
				((get-binop oper binop) 
                            (interp-cfae l ds) 
                            (interp-cfae r ds))))
			(fun (id body) expr)
			(app (func arg)
				(local((define fun-val (interp-cfae func ds)))
					(interp-cfae (fun-body fun-val)
						(aSub (fun-param fun-val)
                                                  (interp-cfae arg ds)
                                                   ds))))
			(if0 (c t e) (cond ((= (interp-cfae c ds) 0)
							(interp-cfae t ds))
							(else (interp-cfae e ds)))))))
			
			

;Evaluator
(define eval-cfae
  (lambda (exp)
		(interp-cfae exp (mtSub))))

;My Test Cases
(test (eval-cfae (num 3)) 3)
(test (eval-cfae (op 'add (num 1) (num 2))) 3)
(test (eval-cfae (fun 'x (op 'mul (id 'x) (num 2)))) (fun 'x (op 'mul (id 'x) (num 2))))
(test (eval-cfae (if0 (op 'sub (num 1) (num 1)) (num 10) (num 0))) 10)
(test (eval-cfae (if0 (op 'sub (num 2) (num 1)) (num 10) (num 0))) 0)
(test (eval-cfae (app (fun 'x (id 'x)) (num 5))) 5)
(test (eval-cfae (if0 (app (fun 'y (op 'div (num 10) (id 'y))) (num 2)) (num 1) (num 2))) 2)
;Provided Test Cases(Adapted)
(test (eval-cfae (op 'add (num 1) (num 2))) 3)
(test (eval-cfae (op 'mul (num 2) (num 2))) 4)
(test (eval-cfae (if0 (num 0) (num 1) (num 2))) 1)
(test (eval-cfae (app (fun 'x (id 'x)) (num 5))) 5)
(test (eval-cfae (app (fun 'x (op 'add (id 'x) (num 1))) (num 1))) 2)
(test (eval-cfae (if0 (app (fun 'x (op 'sub (id 'x) (num 2))) (num 3))
                       (app (fun 'x (op 'mul (id 'x) (num 2))) (num 10))
                       (app (fun 'x (op 'div (id 'x) (num 2))) (num 8)))) 4)
(test (eval-cfae (app (if0 (num 0)
                            (fun 'x (op 'add (id 'x) (num 1)))
                            (fun 'x (op 'add (id 'x) (num 2))))
                       (num 0))) 1)

;Could not get this test case to work, not sure if valid syntax
;(eval-cfae (app (app (fun 'x (fun 'y (op 'add (id 'x) (id 'y)))) (num 3)) (num 2)))

(test (eval-cfae (fun 'x (op 'add (id 'x) (num 1)))) (fun 'x (op 'add (id 'x) (num 1))))
(test (eval-cfae (fun 'x (id 'x))) (fun 'x (id 'x)))

;Testing the Error function in the Interpreter
;(eval-cfae (op 'add (num 1) (fun 'x (op 'mul (id 'x) (num 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Jeff Cailteux
; KUID: 2379460
; Project 2, Part 2
; EECS 662
; CFWAE

;#lang plai

;Define CFWAE
(define-type CFWAE
	(numW (n number?))
	(idW (name symbol?))
	(opW (oper symbol?) (lhs CFWAE?) (rhs CFWAE?))
	(funW (param symbol?) (body CFWAE?))
	(appW (funct CFWAE?) (arg CFWAE?))
	(if0W (c CFWAE?) (t CFWAE?) (e CFWAE?))
   (withW (id symbol?) (expr CFWAE?) (body CFWAE?))
   (cond0W (ct list?) (ed CFWAE?)))

;Define Deferred Substitution
(define-type DefrdSubW
	(mtSubW)
	(aSubW (name symbol?) (value number?) (ds DefrdSubW?))
   (afSubW (name symbol?) (func CFWAE?) (ds DefrdSubW?)))

;Lookup for Deferred Sub
(define lookupW
	(lambda (name ds)
		(type-case DefrdSubW ds
			(mtSubW () (error 'lookup "no binding for identifier"))
			(aSubW (n v rds)
				(if (symbol=? n name)
				     v
				    (lookupW name rds)))
                    (afSubW (n f rds)
				(if (symbol=? n name)
				     f
				    (lookupW name rds))) )))
;Elaborator
(define elab-cfwae
  (lambda (expr)
    (type-case CFWAE expr
      (numW (n) (num n))
      (idW (v) (id v))
      (opW (oper l r) (op oper (elab-cfwae l) (elab-cfwae r)))
      (funW (fun-name body) (fun fun-name (elab-cfwae body)))
      (appW (func arg) (app (elab-cfwae func) (elab-cfwae arg)))
      (if0W (c t e) (if0 (elab-cfwae c) (elab-cfwae t) (elab-cfwae e)))
      (withW (id expr body) (app (fun id (elab-cfwae body)) (elab-cfwae expr)))
      (cond0W (ct ed) (elab-cond0W ct (elab-cfwae ed))))))
 
     
;Elab Helper for cond0
(define elab-cond0W
  (lambda(ct ed)
    (cond ((empty? ct) ed)
          (else (if0 (elab-cfwae (caar ct))
                     (elab-cfwae (cadar ct))
                     (elab-cond0W (cdr ct) ed))))))


;Prelude
(define prelude
  (aSub 'pi 3.1415927
        (afSub 'area (fun 'r (op 'mul (num 3.1415927) (op 'mul (id 'r) (id 'r))))
              (afSub 'inc (fun 'i (op 'add (id 'i) (num 1))) (mtSub))))) 
			

;Evaluator
(define eval-cfwae
  (lambda (expr)
		(interp-cfae (elab-cfwae expr) prelude)))

;My Test Cases
(test (eval-cfwae (numW 3)) 3)
(test (eval-cfwae (opW 'add (numW 3) (numW 2))) 5)
(test (eval-cfwae (funW 'x (opW 'mul (idW 'x) (numW 2)))) (fun 'x (op 'mul (id 'x) (num 2))))
(test (eval-cfwae (appW (funW 'x (opW 'mul (idW 'x) (numW 2))) (numW 5))) 10)
(test (eval-cfwae (if0W (opW 'sub (numW 1) (numW 1)) (numW 10) (numW 0))) 10)
(test (eval-cfwae (if0W (opW 'add (numW 22) (numW 1)) (numW 10) (numW 0))) 0)
(test (eval-cfwae (withW 'x (numW 5) (opW 'add (idW 'x) (numW 6)))) 11)
(test (eval-cfwae (appW (idW 'area) (numW 5))) 78.5398175)
(test (eval-cfwae (opW 'mul (numW 2) (idW 'pi))) 6.2831854)
(test (eval-cfwae (opW 'mul (withW 'x (numW 5) (opW 'add (idW 'x) (numW 6))) (numW 2))) 22)
(test (eval-cfwae (appW (idW 'inc) (numW 3))) 4)
(test (eval-cfwae (cond0W (list (list (numW 0) (numW 2)) (list (numW 1) (numW 7))) (numW 4))) 2)
(test (eval-cfwae (cond0W (list (list (numW 1) (numW 2)) (list (numW 1) (numW 7))) (numW 4))) 4)
;Provided Test Cases(adapted)
(test (eval-cfwae (cond0W (list (list(numW 1) (numW 2))(list (numW 0) (numW 15))) (numW 0))) 15)
(test (eval-cfwae (withW 'x (numW 10) (opW 'add (idW 'x) (numW 5)))) 15)


;Could not adapt this to my syntax for some reason
;(eval-cfwae (withW 'add1 (funW 'x (opW 'add (idW 'x) (numW 1)))
;                      (cond0W (list (list (appW (idW 'add1) (numW 0)) (numW 5))
;                            (list (numW 3) (numW 4))
;                            (list (numW 0) (appW (idW 'add1) (numW 2))))(numW 2))))