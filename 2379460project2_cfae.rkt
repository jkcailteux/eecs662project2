; Jeff Cailteux
; KUID: 2379460
; Project 2, Part 1
; EECS 662
; CFAE

#lang plai

;;;Define CFAE
(define-type CFAE
	(num (n number?))
	(id (name symbol?))
	(op (oper symbol?) (lhs CFAE?) (rhs CFAE?))
	(fun (fun-name symbol?) (body CFAE?))
	(app (funct CFAE?) (arg CFAE?))
	(if0 (c CFAE?) (t CFAE?) (e CFAE?)))

;;;Define Deferred Substitution
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

;;Lookup for Deferred Sub
(define lookup
	(lambda (name ds)
		(type-case DefrdSub ds
			(mtSub () (error 'lookup "no binding for identifier"))
			(aSub (n v rds)
				(if (symbol=? n name)
				     v
				    (lookup name rds))))))

;;; Interpreter
(define interp-cfae
	(lambda (expr ds)
		(type-case CFAE expr
			(num (n) n)
			(id (v) (lookup v ds))
			(op (oper l r) ((get-binop oper binop) (interp-cfae l ds) (interp-cfae r ds)))
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
			
			

;;; Evaluator
(define eval-cfae
	(lambda (cfae)
		(interp-cfae cfae mtSub)))

;;;Testing
