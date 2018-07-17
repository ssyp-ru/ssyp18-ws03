; Функция проверки существования переменной
; str - строка с переменной
; return - существует ли переменная
(defun consistInListVariables(str)
	(if (> (strind  (output setvariables) str) 0)
		t
		nil
	)    
)