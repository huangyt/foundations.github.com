;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/acdk_sql_sqlite.lsp,v 1.4 2005/04/05 13:03:41 kommer Exp $
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

(defun make-main ()
  (acdk-main-init)
  
  (setg acdkmake-project-name  "acdk_sql_sqlite") 
  (setg acdkmake-project-type "dll") ;; exe or dll
  
  ;; normally you doesn't need to modify this
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
   
   (setg acdkmake-project-includes '("."))
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-ldflags '())
  
  
                       
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"  ; the list of used acdk-libraries
      "acdk_text"
      "acdk_sql"
     )
  )
  (setg acdkmake-project-defines 
   '(
      ("IN_ACDK_SQL_SQLITE_LIB")
      ("THREADSAFE" "1") ; for sqlite
    )
  )
  (setg acdkmake-project-sourcelist ; directory and/or source files
   '(
      "acdk/sql/sqlite"
      "sqlitesrc/alter.c"
      "sqlitesrc/attach.c"
      "sqlitesrc/auth.c"
      "sqlitesrc/btree.c"
      "sqlitesrc/build.c"
      "sqlitesrc/date.c"
      "sqlitesrc/delete.c"
      "sqlitesrc/expr.c"
      "sqlitesrc/func.c"
      "sqlitesrc/hash.c"
      "sqlitesrc/insert.c"
      "sqlitesrc/legacy.c"
      "sqlitesrc/main.c"
      "sqlitesrc/opcodes.c"
      "sqlitesrc/os_unix.c"
      "sqlitesrc/os_win.c"
      "sqlitesrc/pager.c"
      "sqlitesrc/parse.c"
      "sqlitesrc/pragma.c"
      "sqlitesrc/printf.c"
      "sqlitesrc/random.c"
      "sqlitesrc/select.c"
      ;;"sqlitesrc/shell.c"
      "sqlitesrc/table.c"
      "sqlitesrc/tokenize.c"
      "sqlitesrc/trigger.c"
      "sqlitesrc/update.c"
      "sqlitesrc/utf.c"
      "sqlitesrc/util.c"
      "sqlitesrc/vacuum.c"
      "sqlitesrc/vdbe.c"
      "sqlitesrc/vdbeapi.c"
      "sqlitesrc/vdbeaux.c"
      "sqlitesrc/vdbemem.c"
      "sqlitesrc/where.c"
      
      ;"acdk/sql/sqlite/sqlite_metainf"
    )
  )
  (setg acdk-make-project-metainfos
    '(
       "acdk/sql/sqlite"
      )
  )
  (acdkmake-main) ; generate the make/projekt files
)


(make-main) ; just call make-main above


