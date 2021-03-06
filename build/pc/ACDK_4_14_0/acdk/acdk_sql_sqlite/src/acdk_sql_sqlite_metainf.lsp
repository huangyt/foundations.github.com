;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk_sql_sqlite_metainf.lsp,v 1.1 2005/04/05 13:33:23 kommer Exp $
;;;

;;; generate Makefiles 
;;; usage:
;;; > ../../bin/acdklisp -acdk-home=..../ <thisfile> [dsp | linux | sunos-gcc]
;;;
;;; generates: project or makefiles files <thisfile>.<projecttype>
;;;

(defun acdk-main-init ()
 (if (zerop ACDKHOME)
    (setg ACDKHOME (getCanonicalPath "../.."))
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

(defun make-main ()
  (acdk-main-init)
  
  (setg acdkmake-project-name "acdk_sql_sqlite_metainf")
  (setg acdkmake-project-type "dll")
  
  ;; normally you doesn't need to modify this
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
  
  
  (setg acdkmake-project-includes '("."))
  ;(setg acdkmake-project-includes l)
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_text"
      "acdk_sql"
      "acdk_sql_sqlite"
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
      "acdk/sql/sqlite/sqlite_metainf"
     )
  )
   (setg acdk-make-project-metainfos
    '(
      )
  )
  (acdkmake-main)
)


(make-main)

