; Обработка исключений вида (>= <= !=)
; str - строка
; return - обработанная строка
(defun fixeq (str) 
	(let ((current 2) (symbols "<>!") (place 0) (midstr "") (leftstr "") (rightstr "") (wasreplace nil)) 
		(loop
			(setq place (strind symbols (strmid str (- current 1) 1))) ; Место символа в строке
			(setq wasreplace nil)
			( when (and (/= 0 place) (eq "=" (strmid str current 1)))
				(setq leftstr (strmid str 1 (- (- current 1) 1))) ; Левая часть
				(setq rightstr (strmid str (+ 1 current))) ; Правая часть
				(setq midstr (strcat " " (strmid str (- current 1) 1) "=" " ")) ; Середина
				(setq str (strcat leftstr midstr rightstr))
				(setq wasreplace t)
			)
			; Выход из цикла
			(if (>= current (+ 1 (strlen str)))
				(return t)
			)
			; Инкремент счётчика
			(if wasreplace
				(setq current (+ 2 current))
				(setq current (+ 1 current))
				)
		)
		str
	)
)