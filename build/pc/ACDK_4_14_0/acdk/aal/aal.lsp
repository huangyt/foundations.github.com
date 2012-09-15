;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/aal/aal.lsp,v 1.8 2004/06/29 17:21:38 kommer Exp $
;;;




(defun make-project-main ()
  ;; don't edit this
  (acdk-project-main-init) ; just initialization
  ;; TODO Name of the package
  (setg acdkmake-project-name "aal")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; default acdk-project-platforms
  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdk_aci" () ("lib"))
        ("src" "acdk_aci_metainf" ("acdk_aci") ("lib" "dist" "meta"))
        ("src" "acdk_aci_dmiproxy" ("acdk_aci" "acdk_aci_dmiproxy") ("lib" "dist" "proxy"))
        
        ("src" "acdk_aal" ("acdk_aci") ("lib"))
        ("src" "acdk_aci_guidbg" ("acdk_aci" ) ("lib"))
        ("src" "acidbg" ("acdk_aci" "acdk_aci_guidbg") ("bin"))
        
        ("tests/acdk/aci" "acdk_aci_Test" ("acdk_aci") ("bin" "utest"))
        ("tests/acdk/aal" "acdk_aal_Test" ("acdk_aci" "acdk_aal") ("bin" "utest"))
    )
  )
  (setg acdk-tests 
    '(
        "acdk_aci_Test"
        "acdk_aal_Test"
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
   
