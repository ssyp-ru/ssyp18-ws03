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