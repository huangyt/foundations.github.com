
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
  
  (setg acdkmake-project-name "acdk_java")
  (setg acdkmake-project-type "dll")
  
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
  
  (include "../javacfg.lsp") ;; ajust configuration here

  (println (s+ 
    "please correct the library/include path for Java if needed: " 
    "\nincs: " (toCode acdkmake-project-includes)
    "\nlibs: " (toCode acdkmake-project-libs)
    "\nldflags: " (toCode acdkmake-project-ldflags)
            )
  )
  
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_text"
     )
  )
  
  (setg acdkmake-project-defines 
    '(
      ("IN_ACDK_JAVA_LIB")
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdk/java"
      "acdk/java/awt"
      "acdk/java/awt/event"
     )
  )

  (acdkmake-main)
)


(make-main)

