; Функция форматирования строки из списка
; lst - строка из списка
; return - форматированная строка из списков
(defun conversionToListVarForExp(lst)
(setq arrKol nil)
	(when (> (strind lst "[") 0)
		(progn 

			(setq arrKol (strmid lst (+ 1 (strind lst "[")) (-(- (strind lst "]")(strind lst "[") )1) ))
		)
	)
	(when  arrKol
		(setq arrKol (strrep arrKol " " ""))
	)
	(setq lst (strrep lst "=" ""))
	(setq lst (strrep lst "," ")("))
	(setq lst (strrep lst "( " "("))
	(setq lst (strrep lst " )" ")"))
	(strcat "(" lst ")")

							
)