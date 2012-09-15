
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
  (setg acdkmake-project-name "acdk_wx_ide")
  (setg acdkmake-project-type "dll")
  
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))

  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name))

  (setg acdkmake-project-template "ACDKCore")
  (setg acdkmake-project-includes '("." "./src/ext/wxscintilla/scintilla/src"))
  ;(setg acdkmake-project-includes l)
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_cfgscript"
      "acdk_wx"
     )
  )
  (setf wx_ide T)
  (include "../wx_cfg.lsp") ;; ajust configuration here
  
  
  (setg acdkmake-project-defines 
    '(
      ("IN_ACDK_WX_IDE_LIB")
      ("SCI_LEXER")
      
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdk/wx/ide"
      "acdk/wx/ide/ide_metainf"
      "acdk/wx/ide/inspector"
      "acdk/wx/ide/inspector/inspector_metainf"
     )
  )
  (setg acdk-make-project-metainfos
    '(
      "acdk/wx/ide"
      "acdk/wx/ide/inspector"
      )
  )
  (acdkmake-main)
)


(make-main)

