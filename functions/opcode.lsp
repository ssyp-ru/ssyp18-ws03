; Функция возврата оператора в виде атома
; op - оператор в виде атома
; return - оператор в виде атома
(defun opcode (op) 
	(cond ((eq op (quote +)) (quote +))
		((eq op (quote -)) (quote -))
		((eq op (quote <=)) (quote <=))
		((eq op (quote >=)) (quote >=)) 
		((eq op (quote =)) (quote =))
		((eq op (quote !=)) (quote !=))  
		((eq op (quote *)) (quote *))
		((eq op (quote /)) (quote /))
		((eq op (quote ^)) (quote ^)) 
		((eq op (quote >)) (quote >))
		((eq op (quote <)) (quote <))
	)
)