
(defun acdk-main-init ()
  (if (zerop ACDKHOME)
    (progn
      (println (s+ "Error: ACDKHOME is not defined!"))
      (println (s+ "Set Environment variable or\n\t use -acdk-home=<ACDKHOME> as command line option"))
      (exit 1)
    )
  )
  (setg acdk-home ACDKHOME)
  (setg acdk-cfg-home (s+ acdk-home "/cfg"))
  (include (s+ acdk-home "/cfg/lib/acdk/lisp/acdkmake.lsp"))
  (if (not (acdk-make-test-args))
    (progn 
      (println (s+ "target for make is needed. See " ACDKHOME " /cfg/target"))
      (exit 1)
    )
  )
)

(defun make-main ()
  (acdk-main-init)
  (println (s+ "ACDKHOME=" ACDKHOME))
  (setg acdkmake-project-name "acidbg")
  ;;(setg acdkmake-project-type "guiexe")
  (setg acdkmake-project-type "exe")
  
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))

  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name))

  (setg acdkmake-project-template "ACDKCore")
  (setg acdkmake-project-includes '("."))
  ;(setg acdkmake-project-includes l)
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_cfgscript"
      "acdk_aci"
      "acdk_aci_guidbg"
      "acdk_wx"
     )
  )
  (include "../../acdk_wx/wx_cfg.lsp") ;; ajust configuration here
  
  
  (setg acdkmake-project-defines 
    '(
      )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdk/tools/acidbg"
      
     )
  )
  (if (acdkmake-is-platform "win32")
    (append acdkmake-project-sourcelist "../../acdk_core/src/acdk/res/wx.rc")
  )
  (setg acdk-make-project-metainfos
    '(
      )
  )
  (acdkmake-main)
)


(make-main)

