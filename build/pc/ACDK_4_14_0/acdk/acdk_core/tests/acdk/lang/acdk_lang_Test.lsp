

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
  
  (setg acdkmake-project-name "acdk_lang_Test")
    
  (setg acdkmake-project-type "exe")
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin"))
  (setg acdkmake-project-object-dir (s+ "./tobj/" acdkmake-project-name "/" acdkmake-target))

  (setg acdkmake-project-template "ACDKCore")
  (setg acdkmake-project-includes '("../src" "../include"))
  
  
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_tools_aunit"
     )
  )
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-ldflags '())

  (setg acdkmake-project-defines 
    '(
    )
  )
  
  
  
  (setg acdkmake-project-sourcelist
    '(
      "acdk_lang_Test.cpp"
      "acdk_lang_ObjectBase_Test.cpp"
	  "acdk_lang_ObjectArray_Test.cpp"
	  "acdk_lang_Character_Test.cpp"
      "acdk_lang_String_Test.cpp"
      "acdk_lang_String2_Test.cpp"
      "acdk_lang_StringBuffer_Test.cpp"
	  "acdk_lang_Throwable_Test.cpp"
	  "acdk_lang_Thread_Test.cpp"
      "acdk_lang_Thread_Test2.cpp"
	  "acdk_lang_ThreadLocal_Test.cpp"
	  "acdk_lang_ClassLoader_Test.cpp"
	  "acdk_lang_Integer_Test.cpp"
	  "acdk_lang_Double_Test.cpp"
	  "acdk_lang_Float_Test.cpp"
	  "acdk_lang_Number_Test.cpp"
	  "acdk_lang_Runtime_Test.cpp"
	  "acdk_lang_ExtObject_Test.cpp"
	  "acdk_lang_CmdLineParser_Test.cpp"
	  "acdk_lang_System_Test.cpp"
		  
    )
  )
  (acdkmake-main)
)


(make-main)



