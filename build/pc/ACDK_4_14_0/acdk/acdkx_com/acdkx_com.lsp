;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdkx_com/acdkx_com.lsp,v 1.12 2004/06/17 16:04:07 kommer Exp $
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
  (setg acdkmake-project-name "acdkx_com")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; default acdk-project-platforms
  (setg acdk-project-platforms 
    '(  dsp
        bcc
        cbx
        linux ; only used for generating docs
    ))
  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdkx_com" () ("lib"))
        ("tests/acdkx/com" "acdkx_com_Test" ("acdkx_com") ("bin" "utest"))
      )
  )
  (setg acdk-tests 
    '(
        "acdkx_com_Test"
        
     )
  )
  (acdkmake-project-main)
)

(make-project-main)
