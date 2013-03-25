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
	(aSub (name symbol?) (value number?) (ds DefrdSub?)))

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

;Testing
(test (eval-cfae (num 3)) 3)
(test (eval-cfae (op 'add (num 1) (num 2))) 3)
(test (eval-cfae (fun 'x (op 'mul (id 'x) (num 2)))) (fun 'x (op 'mul (id 'x) (num 2))))
(test (eval-cfae (if0 (op 'sub (num 1) (num 1)) (num 10) (num 0))) 10)
(test (eval-cfae (if0 (op 'sub (num 2) (num 1)) (num 10) (num 0))) 0)
(test (eval-cfae (app (fun 'x (id 'x)) (num 5))) 5)
(test (eval-cfae (if0 (app (fun 'y (op 'div (num 10) (id 'y))) (num 2)) (num 1) (num 2))) 2)

;Testing Error in Interpreter
(eval-cfae (op 'add (num 1) (fun 'x (op 'mul (id 'x) (num 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Jeff Cailteux
; KUID: 2379460
; Project 2, Part 2
; EECS 662
; CFWAE

#lang plai

;Define CFWAE
(define-type CFWAE
	(num (n number?))
	(id (name symbol?))
	(op (oper symbol?) (lhs CFWAE?) (rhs CFWAE?))
	(fun (param symbol?) (body CFWAE?))
	(app (funct CFWAE?) (arg CFWAE?))
	(if0 (c CFWAE?) (t CFWAE?) (e CFWAE?))
    (with (id symbol?) (expr CFWAE?) (body CFWAE?))
    (cond0 (ct list?) (ed CFWAE?)))

;Define Deferred Substitution
(define-type DefrdWSub
	(mtSub)
	(aSub (name symbol?) (value number?) (ds DefrdWSub?))
    (afSub (name symbol?) (func CFWAE?) (ds DefrdWSub?)))

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
				    (lookup name rds)))
                  )
          
          ))

;Prelude-not tested
(define prelude
  (aSub 'pi (num 3.1415927)
        (aSub 'area (fun 'r (mul (num 3.1415927) (mul (id 'r) (id 'r))))
              (aSub 'inc (fun 'i (add (id 'i) (num 1))) (mtSub))))) 
			

;Evaluator
(define eval-cfwae
  (lambda (expr)
		(interp-cfwae expr (mtSub))))

;Testing


