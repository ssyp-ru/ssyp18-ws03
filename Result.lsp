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


; Функция перевода выражений
; lst - список из строк
; return - готовое префиксное выражение
(defun translateArrayExp (lst)
	(setq lst (fixArrays lst))
	(let ((res nil))
		(if (/= 0 (strind (nth 0 lst) "list"))
			(setq res (nth 0 lst))
			(setq res (output (inf2pref (conversion lst))))
		)
		res
	)
)

; Функция перевода выражений
; lst - список из строк
; return - готовое префиксное выражение
(defun translateExp (lst)
	(output (inf2pref (conversion lst)))	
)

; Функция преобразования списка в нужный формат
; lst - список из строк
; return - отформатированный список из атомов
(defun conversion(lst)
   (setq lst (mapcar 'input lst))
   (setq lst (output lst))
   (setq lst (strrep lst "{" "("))
   (setq lst (strrep lst "}" ")"))
   (input lst)
)

; Функция взятия отдельного атома из списка
; x - список
; return - отдельный атом
(defun atomlist (x)
	(cond ((null x) nil)
		((atom (car x)) (atomlist (cdr x)))
		(t nil)
	)
)

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

; Функция взятия очередной операции и операнда
; ae - список вида (операция, операнд, операнд)
; operators - операторы
; operands - операнды
; return - список готовой части выражения
(defun inf-aux (ae operators operands)


	(inf-iter (cdr ae) operators (cons (car ae) operands))
)

; Функция расстановки операторов и операндов в зависимости от приоритетов
; ae - список вида (операция, операнд, операнд)
; operators - операторы
; operands - операнды
; return - список готовой части выражения
(defun inf-iter (ae operators operands)
	(prog nil 
		(cond 
			((and (null ae) (null operators)) 
				(return (car operands))
			)
		)
		(cond 
			((and (not (null ae)) (or (null operators) (> (weight (car ae)) (weight (car operators)))))
				(return (inf-aux (cdr ae) (cons (car ae) operators) operands))
			)
		
		)


		(return (inf-iter ae (cdr operators) (cons (list (opcode (car operators)) (cadr operands) (car operands)) (cddr operands))))
	)
)

; Функция возврата веса оператора
; x - оператор
; return - вес
(defun weight (x) 
   (cond 
		((eq x (quote +)) 1) 
		((eq x (quote -)) 1) 
		((eq x (quote <=)) 0)
		((eq x (quote >=)) 0 )
		((eq x (quote =)) 0)
		((eq x (quote !=)) 0)  
		((eq x (quote *)) 2) 
		((eq x (quote <)) 0)
		((eq x (quote >)) 0)
		((eq x (quote /)) 2) 
		((eq x (quote ^)) 3) 
   ;     ((eq x (quote ¤)) 5)
		(t 5)
	)
)

; Функция возврата оператора в виде атома
; op - оператор в виде атома
; return - оператор в виде атома
(defun opcode (op) 
	(cond ((eq op (quote +)) (quote +))
		((eq op (quote -)) (quote -))
		((eq op (quote <=)) (quote <=))
		((eq op (quote >=)) (quote >=)) 
		((eq op (quote =)) (quote =))
		((eq op (quote !=)) (quote !=))  
		((eq op (quote *)) (quote *))
		((eq op (quote /)) (quote /))
		((eq op (quote ^)) (quote ^)) 
		((eq op (quote >)) (quote >))
		((eq op (quote <)) (quote <))
	;	((eq op (quote (input (strchr 164))) (quote (input (strchr 164)))))
	)
)

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
								(setf (nth i var) (creatArray arrName arrKol))
							 )
							 (progn
								 (setq varName (list (nth 0 (nth i var))))
								 (setf (nth i var) (append varName
								 (list(input(translateExp (mapcar ' output(subseq (nth i var) 1 (length (nth i var)))))))))
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
		(output var)
	)
)

; Функция проверки существования переменной
; str - строка с переменной
; return - существует ли переменная
(defun consistInListVariables(str)
	(if (> (strind  (output setvariables) str) 0)
		t
		nil
	)    
)
(defun consistInListFunc(str)
	(if (member (input str)  funcNames)
		t
		nil
	) 
)
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

(defun creatArray(name kol)
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


; Обработка исключений вида (=-)
; str - строка
; return - обработанная строка
(defun fixminus (str) 
	(let ((isEqual nil) (i 1) (leftstr "") (midstr "0-") (rightstr "") (wasReplace nil))
		(loop
			(setq wasReplace nil)
			; Встретили =
			(when (and (eq "=" (strmid str i 1)) (null isEqual))
				(setq isEqual t)
			)
			; Встретили - после =
			(when (and (eq "-" (strmid str i 1)) isEqual)
				(setq isEqual nil)
				(setq leftstr (strmid str 1 (- i 1)))
				(setq rightstr (strmid str (+ 1 i)))
				(setq str (strcat leftstr midstr rightstr))
			)
			; Встретили другой символ после =
			(when (and (null (eq " " (strmid str i 1))) isEqual (null (eq "=" (strmid str i 1))))
				(setq isEqual nil)
			)
			; Проверка выхода
			(if (>= i (- (strlen str) 1))
				(return str)
			)
			; Инкремент счётчика
			(setq i (+ i 1))
		)
		str
	)
)

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

; Функция парсинга строки
; str - строка
; return - список из строк
( defun split (str)
	(setq str (strrep str "(-" "(0 - "))
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

; Функция перевода из bBasic в Lisp
; fileDir - путь к файлу с исходным кодом bBasic
(defun translate ()
	(filCloseAll)
	(setq fileDir (sysgetopenname (sysHome) "Source code bBasic|*.txt|"))
   (setq funcNames '(sin cos log exp atn asn acs sh ch sqr sign abs ask))
	(let ((res "") (listLines ()) (currentList ()) (bracketsStack ()) (currentLine "") (temp NIL))
		; Открытие файла с исходным кодом
		(filOpen 'fi fileDir _InPut)

		; Чтение строк из файла
		(loop
			(setq currentLine (filgetline 'fi))
			(setq listLines (append listLines (list (split currentLine))))
			(if (fileof 'fi)
				(return 1)
			)
		)

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
				)

				; Если строка начинается на else ==========
				((eq "else" (nth 0 currentList))
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth (- (length bracketsStack) 1) bracketsStack))
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
					(dolist (curLine listLines t)
						(when (eq (nth 0 curLine) "local")
							(setq variables (append variables '("result" ",")))
							(setq variables (append variables (cdr curLine)))                        	                          
						)
					) 

					; push скобок в стэк
					(push "-1" bracketsStack)
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
						(push (strcat ")" (strchr 10)) bracketsStack)
					)	
				)

				; Если строка наинается на end_proc ==========
				((eq "end_proc" (nth 0 currentList))
					(setq res (strcat res "result"))
					(loop
						(setq res (strcat res (pop bracketsStack)))
						(when (eq "-1" (nth 0 bracketsStack))
							(pop bracketsStack)
							(return 1)
						)
					)
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

				; Если вызвана просто функция
				((and (consistInListFunc (output (input (nth 0 currentList)))) (null (member "=" currentList)))
					;(printline currentList)
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
							
							(when (and (consistInListVariables (nth 0 currentList)) (eq (nth 1 currentList) "["))
								; Обработка до знака равенства
								(setq res (strcat res "(putel " (nth 0 currentList) " "
									(translateExp (subseq currentList 2 (position "]" currentList))) " "))
								
								; Обработка после знака равенства

								(setq currentList (removef "[" currentList))
								(setq currentList (removef "]" currentList))

								(setq Equally (+ 1 (position "=" currentList)))

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
								; Замена всех nth
								(iter (for i from 0 to (- (length listOfNth) 1))
									(setq tempRes (strrep tempRes (strcat (strchr 169) (fix2str i)) (nth i listOfNth)))	
								)
								(setq listOfNth ())
								; Замена всех crap
								(iter (for i from 0 to (- (length listOfCrap) 1))
									(setq tempRes (strrep tempRes (strcat (strchr 164) (fix2str i)) (nth i ListOfCrap)))	
								)
								(setq listOfCrap ())

								(setq res (strcat res tempres ")"))
							)
						)
					)
			))
		)
		res
	)
)