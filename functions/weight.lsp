; Функция возврата веса оператора
; x - оператор
; return - вес
(defun weight (x) 
   (cond 
		((eq x (quote +)) 1) 
		((eq x (quote -)) 1) 
		((eq x (quote <=)) 0)
		((eq x (quote >=)) 0 )
		((eq x (quote =)) 0)
		((eq x (quote !=)) 0)  
		((eq x (quote *)) 2) 
		((eq x (quote <)) 0)
		((eq x (quote >)) 0)
		((eq x (quote /)) 2) 
		((eq x (quote ^)) 3) 
		(t 5)
	)
)