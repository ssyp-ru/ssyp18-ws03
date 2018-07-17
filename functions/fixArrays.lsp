; Функция обработки массивом при вызове функций
; lst - список
; return - обработанный список
(defun fixArrays (lst)
	(let ((tmp ()) (res "") (elements ()) (wasArray nil))
		(iter (for i from 0 to (- (length lst) 1))
			(if (eq (nth i lst) "[") ; Начало массива
				(setq wasArray t)
				(if (eq (nth i lst) "]")
					(progn
						(setq wasArray nil)
						(setq res "(list ")
						(iter (for i in elements)
							(setq res (strcat res i " "))
						)
						(setq elements ())
						(setq res (strcat res ")"))
						(setq tmp (append tmp (list res)))

					)
				)
			)

			(when (and (null (eq (nth i lst) "[")) wasArray (null (eq (nth i lst) "]")))
				(setq elements (append elements (list (nth i lst))))
			)

			(when (and (null (eq (nth i lst) "[")) (null wasArray) (null (eq (nth i lst) "]")))
				(setq tmp (append tmp (list (nth i lst))))
			)
		)
		tmp
	)
)