;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_core/acdk_core.lsp,v 1.40 2005/04/15 14:50:47 kommer Exp $
;;;

;; don't edit this function
(defun acdk-project-main-init ()
 (if (zerop ACDKHOME)
    (progn
      (println (s+ "Error: ACDKHOME is not defined!"))
      (println (s+ "Set Environment variable or\n\t use -acdk-home=<ACDKHOME> as command line option"))
      (println (s+ "In common ACDKHOME should be relativ to the current directory: '..'"))
      (exit 1)
    )
  )
 (setg acdk-home ACDKHOME)
 (setg acdk-cfg-home (s+ acdk-home "/cfg"))
 (include (s+ acdk-home "/cfg/lib/acdk/lisp/acdkprojectmake.lsp"))
)


(defun make-project-main ()
  ;; don't edit this
  (acdk-project-main-init) ; just initialization
  ;; TODO Name of the package
  (setg acdkmake-project-name "acdk_core")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; TODO which platforms are supported in this package
  ;; bcc is experimental and may not work
  ;; "mingw" is experimental and may not work
  (setg acdk-project-platforms '( "dsp" "linux" "sunos-gcc" "bsd" "bcc" "cbx" "mingw" "darwin" "cygwin-static" )) 
  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdk_core" () ("lib" "dist"))
        ("src" "acdk_core_metainf" ("acdk_core") ("lib" "dist" "meta"))
        ("src" "acdk_core_dmiproxy" ("acdk_core") ("lib" "dist" "proxy"))
        
        ("src" "acdk_security" ("acdk_core") ("lib" "dist"))
        ("src" "acdk_security_metainf" ("acdk_core" "acdk_security") ("lib" "dist" "meta"))
        ("src" "acdk_security_dmiproxy" ("acdk_core" "acdk_security") ("lib" "dist" "proxy"))
        
        ("src" "acdk_cfgscript" ("acdk_core") ("lib" "dist"))
        ("src" "acdkcfgscript" ("acdk_core" "acdk_cfgscript") ("tool" "bin" "dist"))
        ("src" "acdk_tools_mc" ("acdk_core" "acdk_cfgscript") ("lib" "dist"))
        
        ("src" "acdkmc" ("acdk_security" "acdk_tools_mc" "acdk_core") ("tool" "bin" "dist"))
        
        ("src" "acdk_tools_aunit" ("acdk_core") ("lib" "dist"))
        ("src" "acdk_tools_aunit_metainf" ("acdk_core" "acdk_core_metainf" "acdk_tools_aunit") ("lib" "dist" "meta"))
        ("src" "acdk_tools_aunit_dmiproxy" ("acdk_core" "acdk_tools_aunit") ("lib" "dist" "proxy"))
        
        ("tests/acdk/lang/sys" "acdk_lang_sys_RefHolder_Test" ("acdk_core") ("bin" "otest"))
        ("tests/acdk/lang/sys" "acdk_lang_sys_Test" ("acdk_core") ("bin" "utest"))
        ("tests/acdk/tools/aunit"  "acdk_tools_aunit_TestSuite_Test"
         ("acdk_core" "acdk_tools_aunit")  ("bin" "utest")
        )
        
        ("tests/acdk/lang" "acdk_lang_Object_Test" ("acdk_core")  ("bin" "otest"))
        ("tests/acdk/lang" "acdk_lang_Test" ("acdk_core" "acdk_tools_aunit")  ("bin" "utest"))
        ("tests/acdk/lang/dmi" "acdk_lang_dmi_Test" ("acdk_core" "acdk_tools_aunit") ("bin" "utest"))
		    ("tests/acdk/lang/ref" "acdk_lang_ref_Test" ("acdk_core" "acdk_tools_aunit") ("bin" "utest"))
		    ("tests/acdk/lang/reflect" "acdk_lang_reflect_Test" ("acdk_core" "acdk_tools_aunit") ("bin" "utest"))
		
        ("tests/acdk/io" "acdk_io_Test" ("acdk_core") ("bin" "utest"))
        ("tests/acdk/util" "acdk_util_Test" ("acdk_core"  "acdk_tools_aunit") ("bin" "utest"))
        ("tests/acdk/util/logging" "acdk_util_logging_Test" ("acdk_core"  "acdk_tools_aunit") ("bin" "utest"))
        ("tests/acdk/tools/mc" "acdk_tools_mc_Test" ("acdk_core"  "acdk_tools_aunit" "acdk_tools_mc") ("bin" "utest"))
        ("tests/acdk/locale" "acdk_locale_Test" ("acdk_core"  "acdk_tools_aunit") ("bin" "utest"))
        ("tests/acdk/cfgscript" "acdk_cfgscript_Test" ("acdk_core"  "acdk_tools_aunit" "acdk_cfgscript") ("bin" "utest"))
    ) 
  )
  (setg acdk-tests 
    '(
        "acdk_lang_sys_RefHolder_Test"
        "acdk_lang_sys_Test"
        "acdk_lang_Object_Test"
        "acdk_tools_aunit_TestSuite_Test"
        "acdk_lang_Test"
		    "acdk_lang_dmi_Test" 
		    "acdk_lang_ref_Test"
		    "acdk_lang_reflect_Test"
		    "acdk_locale_Test"
        "acdk_io_Test"
        "acdk_util_Test"
        "acdk_util_logging_Test"
        "acdk_tools_mc_Test"
        "acdk_cfgscript_Test"
     )
  )
  (acdkmake-project-main)
)

(make-project-main)
