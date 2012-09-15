;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/aal/src/acdk_aci_metainf.lsp,v 1.2 2005/02/07 17:11:23 kommer Exp $
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
  (setg acdkmake-project-name "acdk_aci_metainf")
  (setg acdkmake-project-type "dll")

  ;; normally you doesn't need to modify this
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
  
  (setg acdkmake-project-includes '("../src" "../include"))
  (setg acdkmake-project-libs '())
  (setg acdkmake-project-ldflags '())

  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_cfgscript"
      "acdk_text"
      "acdk_aci"
     )
  )
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-defines 
   '(
    )
  )
  (setg acdkmake-project-sourcelist
    '(
      "acdk/aci/aci_metainf"
      "acdk/aci/ast/ast_metainf"
      "acdk/aci/parser/parser_metainf"
      "acdk/aci/vm/vm_metainf"
      "acdk/aci/util/util_metainf"
    )
  )
  (setg acdk-make-project-metainfos
    '(
      
      )
  )
  (acdkmake-main)
)


(make-main)



