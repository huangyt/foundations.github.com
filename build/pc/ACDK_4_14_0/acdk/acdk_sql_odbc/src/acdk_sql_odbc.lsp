;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_sql_odbc/src/acdk_sql_odbc.lsp,v 1.10 2005/03/31 20:55:25 kommer Exp $
;;;

;;; generate Makefiles 
;;; usage:
;;; > ../../bin/acdklisp -acdk-home=..../ <thisfile> [dsp | linux | sunos-gcc]
;;;
;;; generates: project or makefiles files <thisfile>.<projecttype>
;;;

;;;
;;;
;;; don't change this
;;;
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

(defun isNilorEmpty (__arg)
  (if (isNil __arg)
    (return TRUE)
  )
  (invoke __arg 'equals "")
)

(defun isWin32Target ()
  (if (isNil acdkmake-target)
    (return FALSE)
  )
  (or (invoke acdkmake-target 'equals "nmake") 
      (invoke acdkmake-target 'equals "dsp")
      (invoke acdkmake-target 'equals "bcc"))
)

(defun make-main ()
  (acdk-main-init)
  
  (setg acdkmake-project-name  "acdk_sql_odbc") 
  (setg acdkmake-project-type "dll") ;; exe or dll
  
  ;; normally you doesn't need to modify this
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
   
  (include "../odbc_cfg.lsp")
                       
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"  ; the list of used acdk-libraries
      "acdk_text"
      "acdk_sql"
     )
  )
  (setg acdkmake-project-defines 
   '(
      ("IN_ACDK_SQL_ODBC_LIB")
    )
  )
  (setg acdkmake-project-sourcelist ; directory and/or source files
   '(
      "acdk/sql/odbc"
      "acdk/sql/odbc/odbc_metainf"
    )
  )
  (setg acdk-make-project-metainfos
    '(
       "acdk/sql/odbc"
      )
  )
  (acdkmake-main) ; generate the make/projekt files
)


(make-main) ; just call make-main above


