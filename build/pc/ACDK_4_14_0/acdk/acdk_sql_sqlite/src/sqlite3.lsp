;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_sql_sqlite/src/sqlite3.lsp,v 1.1 2005/04/05 12:32:37 kommer Exp $
;;;

;;; generate Makefiles 
;;; usage:
;;; > ../../bin/acdklisp -acdk-home=..../ <thisfile> [dsp | linux | sunos-gcc]
;;;
;;; generates: project or makefiles files <thisfile>.<projecttype>
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
  
  (setg acdkmake-project-name "sqlite3")
  (setg acdkmake-project-type "exe")

  ;; normally you doesn't need to modify this
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
  
  (setg acdkmake-project-includes '("../src" "../include"))
  
  
  (setg acdkmake-project-acdklibs 
    '(
     
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
      "sqlitesrc/shell.c"
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
    )
  )
  (acdkmake-main)
)


(make-main)



