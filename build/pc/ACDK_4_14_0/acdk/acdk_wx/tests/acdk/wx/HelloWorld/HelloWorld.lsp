

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
  
  (setg acdkmake-project-name "HelloWorld")
    
  (setg acdkmake-project-type "exe")
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin"))
  (setg acdkmake-project-object-dir (s+ "./tobj/" acdkmake-project-name "/" acdkmake-target))

  (setg acdkmake-project-template "ACDKCore")
  
  (include "../../../../wx_cfg.lsp") ;; ajust confiburation here
  (append acdkmake-project-includes "../../../../src")
  
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_wx"
     )
  )
  
  (setg acdkmake-project-defines 
    '(
    )
  )
  
  (setg acdkmake-project-sourcelist
    '(
      "HelloWorld.cpp"
     
    )
  )
  (acdkmake-main)
)


(make-main)



