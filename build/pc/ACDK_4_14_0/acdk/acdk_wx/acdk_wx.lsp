;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_wx/acdk_wx.lsp,v 1.12 2005/04/15 14:50:47 kommer Exp $
;;;

;;; generate Meta-Makefiles 
;;; usage:
;;; ../bin/acdklisp -acdk-home=.. <thisfile>


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
  (setg acdkmake-project-name "acdk_wx")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; TODO which platforms are supported in this package
  ;; is default acdk-project-platforms

  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdk_wx" ())
        ("src" "acdk_wx_metainf" ("acdk_wx") ("lib" "dist" "meta"))
        ("src" "acdk_wx_dmiproxy" ("acdk_wx") ("lib" "dist" "proxy"))
        ("tests/acdk/wx" "acdk_wx_Test" ("acdk_wx") ("bin" "utest"))
        ("src" "guitestrunner" ("acdk_wx") ("bin"))
        ("tests/acdk/wx/HelloWxWorld" "HelloWxWorld" ("acdk_wx"))
	      ("tests/acdk/wx/HelloWorld" "HelloWorld" ("acdk_wx"))
	      
	      ("src" "acdk_wx_ide" ("acdk_wx") ("lib" "dist" "meta"))
	      ("src" "acdkcsfide" ("acdk_wx" "acdk_wx_ide") ("bin"))
	      
	      ("tests/acdk/wx/ide" "acdk_wx_ide_Test" ("acdk_wx" "acdk_wx_ide") ("bin" "utest"))
	      
		
     )
   )
    (setg acdk-tests 
    '(
      "acdk_wx_Test"
      "acdk_wx_ide_Test"
     )
  )
  (acdkmake-project-main)
)

(make-project-main)
