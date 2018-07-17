; Функция проверки существования функции в списке функций
; str - строка с именем функции
; return - существует ли функция
(defun consistInListFunc(str)
	(if (member (input str)  funcNames)
		t
		nil
	) 
)