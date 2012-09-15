;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdkx_orb/acdkx_orb.lsp,v 1.21 2004/06/17 16:04:51 kommer Exp $
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
  (setg acdkmake-project-name "acdkx_orb")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; default acdk-project-platforms
  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdkx_orb" () ("lib"))
        ("src" "acdkx_orb_namesrv" ("acdkx_orb") ("bin"))
        ("src" "acdkx_orb_mc" () ("lib"))
        ("src" "acdkorbidl" () ("bin" "tool"))
        ("tests/acdkx/orb" "acdkx_orb_Test" ("acdkx_orb") ("bin" "utest"))
        ("tests/acdkx/orb/acdk" "AdressServer" ("acdkx_orb") ("bin" "otest"))
        ("tests/acdkx/orb/acdk" "AdressClient" ("acdkx_orb") ("bin" "otest"))
      )
  )
  (setg acdk-tests 
        '(
          "acdkx_orb_Test"
          )
        )
  
  (acdkmake-project-main)
)

(make-project-main)











