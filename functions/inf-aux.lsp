; Функция взятия очередной операции и операнда
; ae - список вида (операция, операнд, операнд)
; operators - операторы
; operands - операнды
; return - список готовой части выражения
(defun inf-aux (ae operators operands)


	(inf-iter (cdr ae) operators (cons (car ae) operands))
)