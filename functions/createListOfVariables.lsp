; Функция создания списка переменных
; var - строка из переменных
; return - список из переменных и их значения
(defun createListOfVariables(var)
	(let ((i 0) (nres nil) (express nil) (varName 0))
		(setq var  (output var))
		(setq var (conversionToListVarForExp var))
		(setq var (input var))
		(setq arrName nil)
		(loop
			(if (> (length (nth i var)) 1)
				(progn
					(if (member (input (strcat "[ " arrKol " ]")) (nth i var))
						(progn
							(setq arrName (nth 0 (nth i var)))
							(setf (nth i var) (createArray arrName arrKol))
						)
						(progn							 	
						 	(progn
								(setq varName (list (nth 0 (nth i var))))
								(setf (nth i var) (append varName
								(list(input(translateExp (mapcar ' output(subseq (nth i var) 1 (length (nth i var)))))))))
							)
						)
					)
				)
				(progn
					(setq varName (nth i var))
					(setf (nth i var) (append varName (list 0) ) )
				)
			)
			(if (>= i (- (length var) 1))
				(return 0)
				(incf i)
			)
		)
		(strrep (output var) "(0)" "")
	)
)