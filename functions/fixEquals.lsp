; Функция обработки знаков равенства
; str - строка
; return - обработанный список
(defun fixEquals (str)
	(let ((i 2) (signs "<>!") (leftstr "") (midstr " = ") (rightstr "") (wasReplace nil))
		(loop
			(setq wasReplace nil)
			(when (eq "=" (strmid str i 1))
				(when (and (= 0 (strind signs (strmid str (- i 1) 1))) (= 0 (strind signs (strmid str (+ i 1) 1))))
					(setq leftstr (strmid str 1 (- i 1)))
					(setq rightstr (strmid str (+ 1 i)))
					(setq str (strcat leftstr midstr rightstr))
					(setq wasReplace t)
				)
			)
			(when (>= i (- (strlen str) 1))
				(return 0)
			)
			(if wasReplace
				(setq i (+ i 2))
				(incf i)
			)
		)
		str
	)
)