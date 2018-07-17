; Функция создания локального массива 
; name - имя
; kol - длина
; return - строку из листа с нулями
(defun createArray(name kol)
	(let ((nnres nil))
		(for i 1 (input kol)
			(
				(setq nnres (append nnres (list 0)))

			)
		)
		(setq nnres (append (list name) (list (list 'quote nnres))))
		nnres
	)
)