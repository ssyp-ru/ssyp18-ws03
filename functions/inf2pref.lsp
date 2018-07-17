; Функция перевода иза инфиксной в превиксную запись
; x - список
; return - строку с префиксной записью
(defun inf2pref (x) 
	(prog ( tl cc xx rr) 

		(cond ((atomlist x) (return (inf-aux x nil nil))))
		(setq rr nil)
		(setq xx x) 
		loop 
		(setq hd (car xx))
		(setq tl (cdr xx))
		(cond 
			((member hd funcNames)
				(setq rr (append rr (list (list hd (inf2pref (car tl))))))
				(setq tl (cdr tl))
			)
			((atom hd) 
				(setq rr (append rr (list hd)))
			)

			(t 

				(setq rr (append rr (list (inf2pref hd))))
			)
		)
		(cond 
			((null tl) 
				(return (inf-aux rr nil nil))
			)
		)
		
		(setq xx tl) 
		(go loop)
	)
)