
//
//  ??????????? ????_Dlg_
//

(prog nil

      (try (dlgDestroy '_Dlg_) except Nil)


      (dlgCreate '_Dlg_ 511 406 "" &H8000000F)

      (dlgAddControl '_Dlg_ '_TXT_1 _TEXT 37 317 430 25 '("Tahoma" 8 1 0 0) "" 0 &H80000008 &HFFFFFF)

      (dlgAddControl '_Dlg_ '_LBL_1 _LABEL 37 93 430 213 '("Tahoma" 8,25 1 0 0) "" 0 &HFF00 &H0)

      //
      // ??? ???? ????_Dlg_
      //

      (Prog () 

      )

      //
      // ????? ???? KEYPRESSED ?? ???????_TXT_1
      // 

      (defun _TXT_1_KeyPress  (KeyAscii) 
      	(let ((text "") (fileDir "") (resTranslate nil))
                  (when (= KeyAscii 13)
                        (setq text (dlgGetText '_TXT_1))
                        (dlgPutText '_TXT_1 "")
                        (when (eq text "/open")
                              (setq fileDir (sysGetOpenName (sysHome) "Source code bBasic|*.bba|"))
                              (when fileDir
                                    (setq resTranslate (input (translate fileDir)))
                                    (eval resTranslate)
                                    (dlgPutText '_LBL_1 "Hello world")
                                    (main)
                              )
                        )
                  )
            )
      )

      //
      //   ????? ?????-???? _TXT_1_KeyPress  ???? _TXT_1
      //

      (dlgSetEvent '_TXT_1 '_TXT_1_KeyPress )


      //
      //   ??????????_Dlg_
      //

      (dlgShow '_Dlg_)
)
