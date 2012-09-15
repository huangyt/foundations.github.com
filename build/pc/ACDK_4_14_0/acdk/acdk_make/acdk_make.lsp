;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_make/acdk_make.lsp,v 1.10 2004/07/18 16:10:56 kommer Exp $
;;;




(defun make-project-main ()
  ;; don't edit this
  (acdk-project-main-init) ; just initialization
  ;; TODO Name of the package
  (setg acdkmake-project-name "acdk_make")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; default acdk-project-platforms
  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdk_make" () ("lib" "dist"))
        ("src" "acdkmake" ("acdk_make") ("bin" "tool" "dist"))
        ("src" "acdk_make_dmiproxy" ("acdk_make") ("lib" "proxy" "dist"))
        ("tests/acdk/make" "acdk_make_Test" ("acdk_make") ("bin" "utest"))
    )
  )
  (setg acdk-tests 
    '(
        "acdk_make_Test"
     )
  )
  (acdkmake-project-main)
)


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

(make-project-main)
   
