; Jeff Cailteux
; KUID: 2379460
; Project 2, Part 1
; EECS 662
; CFAE

#lang plai

(define-type CFAE
	(num (n number?))
	(id (name symbol?))
	(add (lhs CFAE?) (rhs CFAE?))
	(sub (lhs CFAE?) (rhs CFAE?))
	(mul (lhs CFAE?) (rhs CFAE?))
	(div (lhs CFAE?) (rhs CFAE?))
	(fun (fun-name symbol?) (body CFAE?))
	(app (funct CFAE?) (arg CFAE?))
	(if0 (c CFAE?) (t CFAE?) (e CFAE?))
)

(define-type DefrdSub
	(mtSub)
	(aSub (name symbol?) (value number?) (ds DefrdSub?))
)

