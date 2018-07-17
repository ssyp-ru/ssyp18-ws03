
; Функция парсинга строки
; str - строка
; return - список из строк
( defun split (str)
	(setq str (fixminus str))
	(setq str (fixeq str))
	(setq str (fixEquals str))
	(when (eq "-" (strmid str 1 1))
		(setq str (strmid str 2))	
		(setq str (strcat "0 - " str))
	)	
	(setq str (strrep str "(" " { "))
	(setq str (strrep str "+" " + "))
	(setq str (strrep str "-" " - "))
	(setq str (strrep str "*" " * "))
	(setq str (strrep str "/" " / "))
	(setq str (strrep str "^" " ^ "))
	(setq str (strrep str "," " , "))
	(setq str (strrep str ">" " > "))
	(setq str (strrep str "<" " < "))
	(setq str (strrep str "]" " ] "))
	(setq str (strrep str "[" " [ "))
	(setq str (strrep str (strchr 9) ""))
	(setq str (strrep str ")" " } "))
	(setq str (strrep str "<>" " <> "))
	(setq str (strcat str " "))
	; Строка готова для парсинга
	( let ((begin 0) (current 1) (res ()) (isWord nil) (wasStr nil) 
		(digits (strcat (strchr 1) ",1234567890.{}+-/*^<>=!_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm[]")))
		(loop
			; Если нашли кавычку
			(if (and (null wasStr) (eq (strchr 34) (strmid str current 1)))
				(progn
					(setq begin current)
					(setq wasStr t)
				)
				(if (and wasStr (eq (strchr 34) (strmid str current 1)))
					(progn 
						(setq wasStr nil)
						(setq res (append res (list (strmid str begin (+ 1 (- current begin))))))
						(setq isWord nil)
					)
				)
			)

			; Когда встречаем новое слово после пробела
			(when (and (<> 0 (strind digits (strmid str current 1))) (null isWord) (null wasStr))
				(setq isWord t)
				(setq begin current)
			)

			; Когда встречаем пробел после слова
			(when (and (= 0 (strind digits (strmid str current 1))) isWord (null wasStr))
				(setq res (append res (list (strmid str begin (- current begin)))))
				(setq isWord nil)
			)

			; Если это конец
			(if (= current (strlen str))
				(return res)
			)
			(incf current)		
		)
		res
	)
)