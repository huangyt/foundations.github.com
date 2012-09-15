;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_java/acdk_java.lsp,v 1.17 2004/06/17 18:16:52 kommer Exp $
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
  (setg acdkmake-project-name "acdk_java")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; TODO which platforms are supported in this package
  (setg acdk-project-platforms
    '("dsp" "linux" "sunos-gcc"  "bsd")
  )
  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ;( directory name ( depending projects ) )
        ("src" "acdk_java_serialization" ()  ("lib"))
        ("tests/acdk/java/serialization" "acdk_java_serialization_Test" ("acdk_java_serialization") ("bin" "utest"))
        ("tests/acdk/java/serialization/sample" "acdk_java_serialization_sample" ("acdk_java_serialization") ("bin" "sample"))
        ("src" "acdk_java_rmi" ("acdk_java_serialization") ("lib"))
        ("src" "acdk_java" () ("lib"))
        ("src" "acdkjava" ("acdk_java") ("bin"))
        ("tests/acdk/java" "acdk_java_Test" ("acdk_java") ("bin" "utest"))
    )
  )
  (setg acdk-tests 
    '(
        "acdk_java_serialization_Test"
        "acdk_java_Test"
     )
  )
  (acdkmake-project-main)
)

(make-project-main)
   
