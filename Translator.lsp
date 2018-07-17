; Загрузка функции fixArrays
(rds (strcat (sysHome) "\functions\fixArrays.lsp"))

; Загрузка функции conversion
(rds (strcat (sysHome) "\functions\conversion.lsp"))

; Загрузка функции atomlist
(rds (strcat (sysHome) "\functions\atomlist.lsp"))

; Загрузка функции opcode
(rds (strcat (sysHome) "\functions\opcode.lsp"))

; Загрузка функции weight
(rds (strcat (sysHome) "\functions\weight.lsp"))

; Загрузка функции inf-iter
(rds (strcat (sysHome) "\functions\inf-iter.lsp"))

; Загрузка функции inf-aux
(rds (strcat (sysHome) "\functions\inf-aux.lsp"))

; Загрузка функции inf2pref
(rds (strcat (sysHome) "\functions\inf2pref.lsp"))

; Загрузка функции translateExp
(rds (strcat (sysHome) "\functions\translateExp.lsp"))

; Загрузка функции conversionToListVarForExp
(rds (strcat (sysHome) "\functions\conversionToListVarForExp.lsp"))

; Загрузка функции createArray
(rds (strcat (sysHome) "\functions\createArray.lsp"))

; Загрузка функции createListOfVariables
(rds (strcat (sysHome) "\functions\createListOfVariables.lsp"))

; Загрузка функции consistInListVariables
(rds (strcat (sysHome) "\functions\consistInListVariables.lsp"))

; Загрузка функции consistInListFunc
(rds (strcat (sysHome) "\functions\consistInListFunc.lsp"))

; Загрузка функции fixEquals
(rds (strcat (sysHome) "\functions\fixEquals.lsp"))

; Загрузка функции fixminus
(rds (strcat (sysHome) "\functions\fixminus.lsp"))

; Загрузка функции fixeq
(rds (strcat (sysHome) "\functions\fixeq.lsp"))

; Загрузка функции split
(rds (strcat (sysHome) "\functions\split.lsp"))

; Функция перевода из bBasic в Lisp
; return - создаёт и запускает лисповский код из Crasic
(defun translate ()
	(filCloseAll)
    (setq funcNames '(sin cos log exp atn asn acs sh ch sqr sign abs ask))
	(let ((res "") (listLines ()) (currentList ()) (bracketsStack ()) (currentLine "") (temp NIL))
		; Открытие файла с исходным кодом
		(filOpen 'fi (sysGetOpenName (sysHome) "Crasic Source Code|*.csc") _InPut)

		; Чтение строк из файла
		(loop
			(setq currentLine (filgetline 'fi))
			(setq listLines (append listLines (list (split currentLine))))
			(if (fileof 'fi)
				(return 1)
			)
		)

		(setq listLines (remove nil listLines))

		(filClose 'fi) ; Закрытие файла с исходным кодом

		; Главный цикл обработки команд
		(iter (for i from 0 to (- (length listLines) 1))
			(setq currentList (nth i listLines)) ; Запоминание текущей строки
			(cond 
				; Если строка начинается на "if" ==========
				((eq "if" (nth 0 currentList))
					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)
					(setq temp (subseq currentList 2 (- (length currentList) 2)))
					(setq res (strcat res " (if "  (translateExp temp) (strchr 10)))
					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)
					(setq res (strcat res " (progn " (strchr 10)))
				)

				; Если строка начинается на "endif" ==========
				((eq "end_if" (nth 0 currentList))
					; Добавление скобок
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
				)

				; Если строка начинается на else ==========
				((eq "else" (nth 0 currentList))
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)
					(setq res (strcat res " (progn " (strchr 10)))
				)

				; Если строка начинается на for ==========
				((eq "for" (nth 0 currentList))
					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)

					(setq res (strcat res " (iter (for "))

					(setq temp (eq "step" (nth (- (length currentList) 2) currentList)))

					; Добавление итератора
					(setq res (strcat res (nth 1 currentList)))
					; Добавление начального значения
					(setq res (strcat res " from "))
					(setq res (strcat res (translateExp (subseq currentList 3 (position "to" currentList)))))
					(setq res (strcat res " to "))

					; Проверка step
					(if (and temp (null (eq "1" (nth (- (length currentList) 1) currentList))))
						; Если есть step и он не 1
						(progn
							; Добавление конечного значения
							(setq res (strcat res (translateExp (subseq currentList (+ 1 (position "to" currentList)) 
								(position "step" currentList)))))
							; Добавление шага и закрывающей скобки
							(setq res (strcat res " by " (nth (- (length currentList) 1) currentList)))
						)
						; Если нет step или он равен 1
						(progn
							; Добавление конечного значения
							(if temp
								; Если есть step и он равен 1
								(setq res (strcat res (translateExp (subseq currentList (+ 1 (position "to" currentList)) 
									(position "step" currentList)))))
								; Если step отсутствует
								(setq res (strcat res (translateExp (subseq currentList (+ 1 (position "to" currentList))))))
							)
							
						)
					)
					; Добавление закрывающей скобки
					(setq res (strcat res ")" (strchr 10)))
				)

				; Если строка начинается на end_for =========
				((eq "end_for" (nth 0 currentList))
					; Добавление скобок
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
				)

				; Если строка начинается на proc ==========
				((eq "proc" (nth 0 currentList))
					(setq currentList (remove "," currentList)) 
					(setq variables nil)		
					(setq argIsNill "")
					(dolist (curLine (subseq listLines (+ 1 i)) t)
						(when (eq (nth 0 curLine) "local")
							(setq variables (append variables (cdr curLine)))                        	                          
						)
						(when (eq (nth 0 curLine) "end_proc")
							(return)
						)
					) 
					(setq variables (append '("result" "," ) variables))
						;(setq variables (append variables '(nil))) 

				

					; push скобок в стэк
					(push "-1" bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)
					(push (strcat ")" (strchr 10)) bracketsStack)

					(setq funcName (nth 1 currentList))


					(setq funcNames (append funcNames (list (input funcName))))
					
					(setq funcArguments (subseq currentList (+ 1 (position "{" currentList )) (position "}" currentList) ))
					
					(if (eq (subseq currentList (+ 1 (position "{" currentList )) (position "}" currentList)) nil)
						(setq argIsNill " ")
						(setq argIsNill "")
					)

					(setq res (strcat res " (defun "  FuncName argIsNill (strrep (output funcArguments) """" "")  (strchr 10)))
					
					(setq setvariables variables)
					
					(setq variables (mapcar 'input variables))

					(when (null(null variables)) 
						(setq res (strcat res " (let " (output (createListOfVariables variables)) (strchr 10)))
					)
				)

				; Если строка наинается на end_proc ==========
				((eq "end_proc" (nth 0 currentList))
					(loop
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
						(setq res (strcat res (pop bracketsStack)))
					)
					(setq variables nil)
				)

				; Если строка начинается на print ==========
				((eq "print" (nth 0 currentList))
					(let ((printExp nil))
						(setq printExp (subseq currentList 1)) ; Выражение для печати
						(cond
							; Если на вывод одна переменная/цифра
							((= 1 (length printExp)) 
								(setq res (strcat res " (print " (nth 0 printExp) ")" (strchr 10)))
							)

							; Если на вывод выражение
							((and (null (member "," printExp)) (/= 1 (length printExp)))
								(setq res (strcat res " (print " (translateExp (subseq printExp 0)) ")" (strchr 10)))
							)

							; Если на вывод несколько переменных
							((member "," printExp)
								(let ((tmp nil) (r nil))
									(iter (for i in printExp)
										(if (eq "," i)
											(progn
												(push tmp r)
												(setq tmp nil)
											)
											(collecting i into tmp)
										)
									)
									(push tmp r)
									; r - необходимый список
									(iter (for i in r)
										(cond
											((= 1 (length i)) ; 1 элемент
												(setq res (strcat res " (print " (nth 0 i) ")" (strchr 10)))
											)
											((> (length i) 1) ; Больше, чем 1 элемент
												(setq res (strcat res " (print " (translateExp i) ")" (strchr 10)))
											)
										)
									)
								)
							)
						)
					)
				)

				; Если строка начинается на printline ==========
				((eq "printline" (nth 0 currentList))
					(let ((printExp nil))
						(setq printExp (subseq currentList 1)) ; Выражение для печати
						(cond
							; Если на вывод одна переменная/цифра
							((= 1 (length printExp)) 
								(setq res (strcat res " (print " (nth 0 printExp) ")" (strchr 10)))
							)

							; Если на вывод выражение
							((and (null (member "," printExp)) (/= 1 (length printExp)))
								(setq res (strcat " (print " (translateExp printExp) ")" (strchr 10)))
							)

							; Если на вывод несколько переменных
							((member "," printExp)
								(let ((tmp nil) (r nil))
									(iter (for i in printExp)
										(if (eq "," i)
											(progn
												(push tmp r)
												(setq tmp nil)
											)
											(collecting i into tmp)
										)
									)
									(push tmp r)
									; r - необходимый список
									(iter (for i in r)
										(cond
											((= 1 (length i)) ; 1 элемент
												(setq res (strcat res " (print " (nth 0 i) ")" (strchr 10)))
											)
											((> (length i) 1) ; Больше, чем 1 элемент
												(setq res (strcat res " (print " (translateExp i) ")" (strchr 10)))
											)
										)
									)
								)
							)
						)
						(setq res (strcat res " (printline)"))
					)
				)

				; Если строка начинается на say ==========
				((eq "say" (nth 0 currentList))
					(let ((printExp nil))
						(setq printExp (subseq currentList 1)) ; Выражение для печати
						(cond
							; Если на вывод одна переменная/цифра
							((= 1 (length printExp)) 
								(setq res (strcat res " (say " (nth 0 printExp) ")" (strchr 10)))
							)
							; Если на вывод выражение
							((and (null (member "," printExp)) (/= 1 (length printExp)))
								(setq res (strcat " (say " (translateExp printExp) ")" (strchr 10)))
							)
						)
					)
				)

				; Если вызвана просто функция ==========
				((and (consistInListFunc (output (input (nth 0 currentList)))) (null (member "=" currentList)))
					(let ((ListOfCrap nil) (tempres ""))
						(when (member "[" currentList)
							(setq currentList (fixArrays currentList))
							(when (> (- (length currentList) 1) 2)
								
								(iter (for i from 2 to (- (length currentList) 1))
									(if (/= (strInd (nth i currentList) "list") 0)
										(progn
											(setq ListOfCrap (append ListOfCrap (list (nth i currentList))))
											(putel currentList (+ 1 i) (strcat (strchr 164) (fix2str (- (length listOfCrap) 1))))
										)
									)
								)
							)

							(setq tempRes (translateExp currentList))

							; Замена всех crap
							(when listOfCrap
								(iter (for i from 0 to (- (length listOfCrap) 1))
									(setq tempRes (strrep tempRes (strcat (strchr 164) (fix2str i)) (nth i ListOfCrap)))	
								)
								(setq listOfCrap ())

							)

							(setq res (strcat res tempres (strchr 10)))
						)
					)
				)

				; Если в строке содержится присваивание ==========
				((member "=" currentList)
					(setq arrBefore nil)
					(let ((begArr 0) (endArr 0) (nameOfArray 0) (tempRes "") (leftList nil) (midList nil) (rightList nil) (Equally 0) 
						(listOfNth nil) (curNth 0) (ListOfCrap nil))

						(if (and (consistInListVariables (nth 0 currentList)) (eq (nth 1 currentList) "="))
							(progn
								(setq temp (translateExp (subseq currentList 2 (length currentList) )))
								(setq temp (strrep temp " NIL" ""))
								(setq res (strcat res "(setq" " " (nth 0 currentList) " "))
								(if (member "[" currentList)
									(progn
										(setq Equally (+ 1 (position "=" currentList)))
										(when (> (- (length currentList) 1) 2)
											(iter (for i from 2 to (- (length currentList) 1)) 
												(if (and (eq (nth i currentList) "[") (null (eq (nth (- i 1) currentList) "(")))
													(progn
														(setq nameOfArray (nth (- i 1) currentList))
														(setq begArr i)
														(setq endArr (position "]" currentList))
														(setq leftList (subseq currentList Equally (- i 1)))
														(setq rightList (subseq currentList (add1 endArr)))
														(setq midList (strcat " (nth " (translateExp (subseq currentList (+ begArr 1) endArr)) " " nameOfArray ") "))
														(setq listOfNth (append listOfNth (list midList)))
														(setq midList (strcat (strchr 169) (fix2str (- (length listOfNth) 1))))
														(setq currentList (append leftList (list midList) rightList))
														(setq currentList (fixArrays currentList))
													)
												)
											)
										)
										(when (> (- (length currentList) 1) 2)
											(iter (for i from 2 to (- (length currentList) 1)) 
												(if (/= (strInd (nth i currentList) "list") 0)
													(progn
														(setq ListOfCrap (append ListOfCrap (list (nth i currentList))))
														(putel currentList (+ 1 i) (strcat (strchr 164) (fix2str (- (length listOfCrap) 1))))
													)
												)
											)
										)
										(setq tempRes (translateExp currentList))

										; Замена всех nth
										(when listOfNth
											(iter (for i from 0 to (- (length listOfNth) 1))
												(setq tempRes (strrep tempRes (strcat (strchr 169) (fix2str i)) (nth i listOfNth)))	
											)
											(setq listOfNth ())
										)
										; Замена всех crap
										(when listOfCrap
											(iter (for i from 0 to (- (length listOfCrap) 1))
												(setq tempRes (strrep tempRes (strcat (strchr 164) (fix2str i)) (nth i ListOfCrap)))	
											)
											(setq listOfCrap ())
										)

										(setq res (strcat res tempres ")" (strchr 10)))										
									)
									(progn
										(setq res (strcat res temp ")" (strchr 10)))
									)
								)

							)
							
							; Обработка массивов
							(when (and (consistInListVariables (nth 0 currentList)) (eq (nth 1 currentList) "["))
								; Обработка до знака равенства
								(setq res (strcat res "(putel " (nth 0 currentList) " "
									(strcat "(+ 1 "(translateExp (subseq currentList 2 (position "]" currentList))) ")") " "))
								
								; Обработка после знака равенства

								(setq currentList (removef "[" currentList))
								(setq currentList (removef "]" currentList))

								(setq Equally (+ 1 (position "=" currentList)))
								(if (member "[" currentList)
									(progn
										(iter (for i from 3 to (- (length currentList) 1)) 
											(if (and (eq (nth i currentList) "[") (null (eq (nth (- i 1) currentList) "(")))
												(progn
													(setq nameOfArray (nth (- i 1) currentList))
													(setq begArr i)
													(setq endArr (position "]" currentList))
													(setq leftList (subseq currentList Equally (- i 1)))
													(setq rightList (subseq currentList (add1 endArr)))
													(setq midList (strcat " (nth " (translateExp (subseq currentList (+ begArr 1) endArr)) " " nameOfArray ") "))
													(setq listOfNth (append listOfNth (list midList)))
													(setq midList (strcat (strchr 169) (fix2str (- (length listOfNth) 1))))
													(setq currentList (append leftList (list midList) rightList))
													(setq currentList (fixArrays currentList))
												)

											)

										)
										(iter (for i from 3 to (- (length currentList) 1)) 
											(if (/= (strInd (nth i currentList) "list") 0)
												(progn
													(setq ListOfCrap (append ListOfCrap (list (nth i currentList))))
													(putel currentList (+ 1 i) (strcat (strchr 164) (fix2str (- (length listOfCrap) 1))))
												)
											)
										)
										(setq tempRes (translateExp currentList))
									)
									(progn
										(setq tempRes (translateExp (subseq currentList ( + 1 (position "=" currentList)))))
									)
								)
								; Замена всех nth
								(when listOfNth
									(iter (for i from 0 to (- (length listOfNth) 1))
										(setq tempRes (strrep tempRes (strcat (strchr 169) (fix2str i)) (nth i listOfNth)))	
									)
									(setq listOfNth ())

								)
								; Замена всех crap
								(when listOfCrap
									(iter (for i from 0 to (- (length listOfCrap) 1))
										(setq tempRes (strrep tempRes (strcat (strchr 164) (fix2str i)) (nth i ListOfCrap)))	
									)
									(setq listOfCrap ())
								)

								(setq res (strcat res tempres ")"))
							)
						)
					)
			))
		)
		(filOpen 'fi (strcat (sysHome) "\temp.lsp") _OUTPUT)
		(filPutLine 'fi res)
		(filClose 'fi)
		(rds (strcat (sysHome) "\temp.lsp"))
		(sysErase (strcat (sysHome) "\temp.lsp"))
		(main)
	)
)