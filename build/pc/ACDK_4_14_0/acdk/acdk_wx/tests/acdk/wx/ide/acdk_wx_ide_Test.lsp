

(defun acdk-main-init ()
  (if (zerop ACDKHOME)
    (progn
      (println (s+ "Error: ACDKHOME is not defined!"))
      (println (s+ "Set Environment variable or\n\t use -acdk-home=<ACDKHOME> as command line option"))
      (exit 1)
    )
  )
  (setg acdk-home ACDKHOME)
  (setg acdk-cfg-home (s+ ACDKHOME "/cfg"))
  (include (s+ acdk-cfg-home "/lib/acdk/lisp/acdkmake.lsp"))
  (if (not (acdk-make-test-args))
    (progn 
      (println (s+ "target for make is needed. See " ACDKHOME " /cfg/target"))
      (exit 1)
    )
  )
)

(defun make-main ()
  (acdk-main-init)
  
  (setg acdkmake-project-name "acdk_wx_ide_Test")
    
  (setg acdkmake-project-type "exe")
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin"))
  (setg acdkmake-project-object-dir (s+ "./tobj/" acdkmake-project-name "/" acdkmake-target))

  (setg acdkmake-project-template "ACDKCore")
  (setg acdkmake-project-includes '("../../../src" "../../../../include"))
  
  (setf wx_ide T)
  (include "../../../../wx_cfg.lsp") ;; ajust confiburation here
  
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_cfgscript"
      "acdk_tools_aunit"
	    "acdk_wx"
	    "acdk_wx_ide"
     )
  )
  (setg acdkmake-project-defines 
    '(
    )
  )
  
  (setg acdkmake-project-sourcelist
    '(
      "acdk_wx_ide_Test.cpp"
      "acdk_wx_ide_StyledTextCtrl_Test.cpp"
    )
  )
  (acdkmake-main)
)


(make-main)



