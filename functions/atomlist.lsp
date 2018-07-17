; Функция взятия отдельного атома из списка
; x - список
; return - отдельный атом
(defun atomlist (x)
	(cond ((null x) nil)
		((atom (car x)) (atomlist (cdr x)))
		(t nil)
	)
)