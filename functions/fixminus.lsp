; Обработка исключений вида (=-)
; str - строка
; return - обработанная строка
(defun fixminus (str) 
	(setq str (strrep str " " ""))
	(setq str (strrep str "(-" "(0-"))
	(setq str (strrep str "=-" "=0-"))
	(setq str (strrep str ">-" ">0-"))
	(setq str (strrep str "<-" "<0-"))
	str
)