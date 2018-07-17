; Функция расстановки операторов и операндов в зависимости от приоритетов
; ae - список вида (операция, операнд, операнд)
; operators - операторы
; operands - операнды
; return - список готовой части выражения
(defun inf-iter (ae operators operands)
	(prog nil 
		(cond 
			((and (null ae) (null operators)) 
				(return (car operands))
			)
		)
		(cond 
			((and (not (null ae)) (or (null operators) (> (weight (car ae)) (weight (car operators)))))
				(return (inf-aux (cdr ae) (cons (car ae) operators) operands))
			)
		
		)
		(return (inf-iter ae (cdr operators) (cons (list (opcode (car operators)) (cadr operands) (car operands)) (cddr operands))))
	)
)