
(defun getB (stmt)
  (let ((p (strInd stmt ","))
        (c 0)
        (s ""))
   (if (zerop p)
       0
       (iter (for i from p to 1 by -1)
          (setq s (strMid stmt i 1))
          (cond ((eq s ")") (setq c (+ c 1)))
                ((eq s "(") (setq c (- c 1)) (when (= c -1) (return i))))))))
                
(defun getE (stmt p)
  (let ((c 0)
        (s ""))  
   (iter (for i from p to (strLen stmt))
     (setq s (strMid stmt i 1))
     (cond ((eq s "(") (setq c (+ c 1)))
           ((eq s ")") (setq c (- c 1)) (when (zerop c) (return i)))))))
                
(defun prepro-1 (stmt)
 (if (or (eq "proc" (strLcase (strLeft stmt 4)))
         (eq "local" (strLcase (strLeft stmt 5)))) stmt 
  (let ((b (getB stmt))
        (e 0)
        (ll "")
        (rr "")
        (mm "")) 
    (cond ((zerop b) stmt)
          (t (setq e (getE stmt b))  
             (setq ll (strLeft stmt (- b 1))) 
             (setq rr (strMid stmt (+ e 1))) 
             (setq mm (strCat "((" (strRep (strMid stmt (+ b 1) (- e b 1)) "," ")(") "))"))
             (prepro-1 (strCat ll mm rr))))))) 

(defun prepro-2 (s)
  (let* ((s1 (strRep s "(-" "(0-"))
         (s2 (strRep s1 "=-" "=0-"))
         (s3 (strRep s2 ">-" ">0-"))
         (s4 (strRep s3 "<-" "<0-")))
   s4))      
      
(defun prepro (s)
   (prepro-2 (prepro-1 s)))            
             
(defun postpro (lex-list)
  (cond ((null (cdr lex-list)) lex-list)
        (t (let ((c1 (car lex-list))
                 (c2 (cadr lex-list)))
                (cond ((and (eq c1 ">") (eq c2 "=")) (cons ">=" (postpro (cddr lex-list))))
                      ((and (eq c1 "<") (eq c2 "=")) (cons "<=" (postpro (cddr lex-list))))
                      ((and (eq c1 "=") (eq c2 "=")) (cons "==" (postpro (cddr lex-list))))
                      ((and (eq c1 "<") (eq c2 ">")) (cons "<>" (postpro (cddr lex-list))))
                      ((and (eq c1 ">") (eq c2 "<")) (cons "<>" (postpro (cddr lex-list))))
                      ((and (eq c1 "!") (eq c2 "=")) (cons "/=" (postpro (cddr lex-list))))
                      ((and (eq c1 "/") (eq c2 "=")) (cons "/=" (postpro (cddr lex-list))))
                      (t (cons c1 (postpro (cdr lex-list)))))))))                      
             
(defun parser (txt &optional (d1 " ,") (d2 "()+-*/\^=<>%"))
  (let ((res nil)
        (lex "") )
   (iter (for s in-string (strCat txt (strLeft d1 1)))
     (cond ((plusp (strInd d1 s))
            (when (> (strLen lex) 0) (collecting lex into res))
                  (setq lex ""))
           ((plusp (strInd d2 s)) 
            (when (> (strLen lex) 0) (collecting lex into res))
                  (collecting s into res)  
                  (setq lex ""))
           (t (setq lex (strCat lex s))))) res))

(defun inf2ipn (f &optional (s nil) (r nil))
  (cond ((null f) (if (null s) (reverse r) (inf2ipn nil (cdr s) (cons (car s) r))))
        ((listp (car f)) (inf2ipn (cdr f) s (append (reverse (inf2ipn (car f))) r))) 
        ((numberp (car f)) (inf2ipn (cdr f) s (cons (car f) r)))
        ((not (is-op (car f))) (inf2ipn (cdr f) s (cons (car f) r)))
        (t (cond ((null s) (inf2ipn (cdr f) (cons (car f) s) r))
                 ((> (prty (car f)) (prty (car s))) (inf2ipn (cdr f) (cons (car f) s) r))
                 (t (let ((a (car s)))
                         (inf2ipn (cdr f) (cons (car f) (cdr s)) (cons a r))))))))
                         
(defun prty (OP) 
  (cond ((EQ OP (QUOTE and)) 1)
        ((EQ OP (QUOTE or))  1)
        ((EQ OP (QUOTE >))   2)
        ((EQ OP (QUOTE >=))  2)
        ((EQ OP (QUOTE <))   2)
        ((EQ OP (QUOTE <=))  2)
        ((EQ OP (QUOTE =))   2)
        ((EQ OP (QUOTE ==))  2)
        ((EQ OP (QUOTE /=))  2)
        ((EQ OP (QUOTE +))   3) 
        ((EQ OP (QUOTE -))   3) 
        ((EQ OP (QUOTE *))   4) 
        ((EQ OP (QUOTE /))   4) 
        ((EQ OP (QUOTE \))   4)
        ((EQ OP (QUOTE %))   4)
        ((EQ OP (QUOTE ^))   5)
        ((member op (mapcar 'car *oplist*)) 6)))
        
(defun is-op (o) 
  (member o (mapcar 'car *oplist*)))  
  
(defun is-unary (o)        
   (member o '(not sin cos abs exp log sqrt)))

(defun arity (o)
  (iter (for a in *oplist*) (when (eq o (car a)) (return (cadr a)))))   
   
(defun inf2pref (f &optional (s nil))
  (cond ((null f) (car s))
        ((numberp (car f)) (inf2pref (cdr f) (cons (car f) s)))
        ((is-op (car f))
         (let ((ar (arity (car f)))) 
           (inf2pref (cdr f) (cons (cons (car f) (reverse (subseq s 0 ar))) (subseq s ar)))))
        ((atom (car f)) (inf2pref (cdr f) (cons (car f) s)))
        (t (inf2pref (cdr f) (cons (list (car f) (car s)) (cdr s))))))        

(defun mk-intf (txt)
  (let ((lex (postpro (parser (prepro txt) " ," "()+-*/\^=<>%")))
        (intf ""))
   (iter (for a in lex) (setq intf (strCat intf a " ")))
   (input (strCat "(" intf ")"))))
           
(defun i2pa (f)
  (inf2pref (inf2ipn (mk-intf f))))           

(defun i2p (f)
  (inf2pref (inf2ipn f)))           

(defun getLine (fil)
  (let ((stri ""))
    (loop
      (when (filEof fil) (return ""))
      (setq *numline* (add1 *numline*)) 
      (setq stri (filGetline fil))
      
      (printsline (strCat (format *numline* "0000") " " (strRTrim stri)))
      
      (setq stri (strATrim stri)) 
      (unless (or (eq "" stri) (eq "*" (strLeft stri 1))) (return stri)))))
  
(defun post-set (meat)
  (let ((name-var (car meat))
        (r-value (i2p (cddr meat))))
    `(setq ,name-var ,r-value)))

(defun post-proc (fi)
   (let ((stmt nil)
         (proc-name nil)
         (proc-parm nil)
         (loc-var nil)
         (res nil)
         (lv (list (list 'result 0))))
    (loop
         (setq stmt (mk-intf (getLine fi))) 
         (when (null stmt) (return t))
         (cond ((eq (car stmt) 'proc) 
                    (setq proc-name (nth 1 stmt))
                    (setq proc-parm (nth 2 stmt))
                    (setq *oplist* (cons (list proc-name (length proc-parm)) *oplist*)))                    
               ((eq (car stmt) 'end_proc) (return t))
               ((eq (car stmt) 'local) 
                    (setq loc-var (append loc-var (cdr stmt))))
               ((eq (cadr stmt) '=) 
                    (setq res (append res (list (post-set stmt)))))
               ((eq (car stmt) 'if) (setq res (append res (list (post-if stmt fi)))))     
               ((eq (car stmt) 'for) (setq res (append res (list (post-for stmt fi)))))     
               ((eq (car stmt) 'print) 
                    (setq res (append res (list (list 'printline (i2p (cdr stmt)))))))
               ((eq (car stmt) 'input) 
                    (setq res (append res (list (list 'setq (cadr stmt) (list 'read) )))))
               (t (printsline (strCat "**** �������� " (output stmt) " �� ���������")) (setq *flagerr* t))))
    (iter (for a in loc-var) (collecting (list a 0) into lv))
    `(defun ,proc-name ,proc-parm (let ,lv ,@res result))))

(defun post-if (st fi)
   (let ((cond (i2p (cadr st)))
         (fe nil)
         (tt nil)
         (ff nil)
         (stmt nil))
        (loop 
          (setq stmt (mk-intf (getLine fi)))  
          (cond ((eq (car stmt) 'end_if) (return t))
                ((eq (car stmt) 'else) (setq fe t))          
                ((eq (car stmt) 'if) 
                  (let ((tmp (post-if stmt fi)))
                    (if (null fe) (setq tt (append tt (list tmp)))
                                  (setq ff (append ff (list tmp))))))
                ((eq (car stmt) 'for) 
                  (let ((tmp (post-for stmt fi)))
                    (if (null fe) (setq tt (append tt (list tmp)))
                                  (setq ff (append ff (list tmp))))))                                  
                ((eq (cadr stmt) '=) 
                  (if (null fe) (setq tt (append tt (list (post-set stmt))))                
                                (setq ff (append ff (list (post-set stmt))))))
                ((eq (car stmt) 'print)
                  (if (null fe) (setq tt (append tt (list (list 'printline (i2p (cdr stmt))))))
                                (setq ff (append ff (list (list 'printline (i2p (cdr stmt))))))))
                ((eq (car stmt) 'input)
                  (if (null fe) (setq tt (append tt (list (list 'setq (cadr stmt) (list 'read)  ))))
                                (setq tt (append ff (list (list 'setq (cadr stmt) (list 'read)  ))))))
                ((eq (car stmt) 'exit_for)
                  (if (null fe) (setq tt (append tt (list (list 'return t))))
                                (setq ff (append ff (list (list 'return t))))))
                (t (printsline (strCat "**** �������� " (output stmt) " �� ���������")) (setq *flagerr* t) (return t))))
        `(if ,cond (progn ,@tt) (progn ,@ff))))     
        
(defun post-for (st fi)
  (let ((var (nth 1 st))
        (beg (nth 3 st))
        (end (nth 5 st))
        (step (if (> (length st) 8) (nth 7 st) 1))
        (bb nil)
        (stmt nil))
    (loop
        (setq stmt (mk-intf (getLine fi)))      
        (cond ((eq (car stmt) 'end_for) (return t))
                ((eq (cadr stmt) '=) (setq bb (append bb (list (post-set stmt)))))
                ((eq (car stmt) 'print) (setq bb (append bb (list (list 'printline (i2p (cdr stmt)))))))
                ((eq (car stmt) 'input) (setq bb (append bb (list (list 'setq (cadr stmt) (list 'read)  )))))
                ((eq (car stmt) 'exit_for) (setq bb (append bb (list (list 'return t)))))
                ((eq (car stmt) 'if) (setq bb (append bb (list (post-if stmt fi)))))
                (t (printsline (strCat "**** �������� " (output stmt) " �� ���������")) (setq *flagerr* t) (return t))))
        `(iter (for ,var from ,beg to ,end by ,step) ,@bb)))
        
(defun start (&optional (fname ""))
   (setq *oplist* '((+ 2) (- 2) (* 2) (/ 2) (^ 2) (\ 2) (% 2)
                    (= 2) (== 2) (/= 2) (> 2) (>= 2) (< 2) (<= 2) 
                    (and 2) (or 2) (not 2) 
                    (sin 1) (cos 1) (abs 1) (exp 1) (log 1) (sqrt 1))) 
   (setq *numline* 0)
   (setq *flagerr* nil)
   (when (zerop (strLen fname))
         (setq fname (sysGetOpenName (sysHome) "����-������|*.bbs")))
   (let ((fi (gensym 'fi)))
        (filCloseAll)    
        (filOpen fi fname _INPUT)
        (loop
            (let ((curr-proc (post-proc fi)))
                 (when *flagerr* (return t))
                 (when (filEOF fi) (return t))
              (eval curr-proc)))
        (filClose fi)))
        
  