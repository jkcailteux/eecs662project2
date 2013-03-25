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
(define-type DefrdSub
	(mtSub)
	(aSub (name symbol?) (value number?) (ds DefrdSub?))
    (afSub (name symbol?) (func CFWAE?) (ds DefrdSub?))

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
(define interp-cfwae
  (lambda (expr ds)
		(type-case CFWAE expr
			(num (n) n)
			(id (v) (lookup v ds))
			(op (oper l r) ((get-binop oper binop) (interp-cfwae l ds) (interp-cfwae r ds)))
			(fun (id body) expr)
			(app (func arg)
				(local((define fun-val (interp-cfwae func ds)))
					(interp-cfwae (fun-body fun-val)
						(aSub (fun-param fun-val)
                                                  (interp-cfwae arg ds)
                                                   ds))))
			(if0 (c t e) (cond ((= (interp-cfwae c ds) 0)
							(interp-cfwae t ds))
							(else (interp-cfwae e ds)))))))

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
;(test (eval-cfwae (num 3)) 3)
;(test (eval-cfwae (op 'add (num 1) (num 2))) 3)
;(test (eval-cfwae (fun 'x (op 'mul (id 'x) (num 2)))) (fun 'x (op 'mul (id 'x) (num 2))))
;(test (eval-cfwae (if0 (op 'sub (num 1) (num 1)) (num 10) (num 0))) 10)
;(test (eval-cfwae (if0 (op 'sub (num 2) (num 1)) (num 10) (num 0))) 0)
;(test (eval-cfwae (app (fun 'x (id 'x)) (num 5))) 5)
;(test (eval-cfwae (if0 (app (fun 'y (op 'div (num 10) (id 'y))) (num 2)) (num 1) (num 2))) 2)

