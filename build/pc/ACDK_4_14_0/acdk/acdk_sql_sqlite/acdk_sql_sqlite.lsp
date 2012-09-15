;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/acdk_sql_sqlite.lsp,v 1.4 2005/04/15 14:50:47 kommer Exp $
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
  (setg acdkmake-project-name "acdk_sql_sqlite")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdk_sql_sqlite" () ("lib"))
        ("src" "sqlite3" ( "acdk_sql_sqlite" ) ( "bin" "tool" ))
        ("src" "acdk_sql_sqlite_metainf" ("acdk_sql_sqlite") ("lib" "meta"))
        ("tests/acdk/sql/sqlite" "acdk_sql_sqlite_Test" ( "acdk_sql_sqlite" ) ("bin" "utest"))
    )
  )
  (setg acdk-tests 
    '(
        "acdk_sql_sqlite_Test"
        
     )
  )
  (acdkmake-project-main)
)

(make-project-main)
