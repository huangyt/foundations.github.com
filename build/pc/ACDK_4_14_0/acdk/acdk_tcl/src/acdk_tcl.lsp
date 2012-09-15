;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_tcl/src/acdk_tcl.lsp,v 1.13 2005/02/07 17:16:04 kommer Exp $
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
  
  (setg acdkmake-project-name "acdk_tcl")
  (setg acdkmake-project-type "dll")
  
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
  
  
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_text"
     )
  )
  (include "../tcl_cfg.lsp") ;; ajust confiburation here
  (setg acdkmake-project-defines 
    '(
      ("IN_ACDK_TCL_LIB")
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdk/tcl"
      "acdk/tcl/tcl_metainf"
     )
  )
  (setg acdk-make-project-metainfos
    '(
       "acdk/tcl"
      )
  )
  (acdkmake-main)
)


(make-main)

