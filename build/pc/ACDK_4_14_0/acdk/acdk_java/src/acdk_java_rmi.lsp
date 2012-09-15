
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
  
  (setg acdkmake-project-name "acdk_java_rmi")
  (setg acdkmake-project-type "dll")
  
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
  
  (setg acdkmake-project-includes '( "." ) )
  (setg acdkmake-project-ldflags '() ) 
  (setg acdkmake-project-libs '() )
    
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_net"
      "acdk_java_serialization"
     )
  )
  
  (setg acdkmake-project-defines 
    '(
      ("IN_ACDK_JAVA_RMI_LIB")
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdk/java/rmi"
      "acdk/java/rmi/rmi_metainf"
      "acdk/java/rmi/registry"
     )
  )
  (setg acdk-make-project-metainfos
    '(
      "acdk/java/rmi"
      )
  )
  (acdkmake-main)
)


(make-main)

