;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk_core.lsp,v 1.10 2002/05/23 15:03:53 kommer Exp $
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
  (setg acdkmake-project-name "acdk_core")
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
     )
  )
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-defines 
   '(
      ("IN_ACDK_CORE_LIB")
    )
  )
  (setg acdkmake-project-sourcelist
    '(
      "acdk"
      "acdk/lang"
      "acdk/lang/dmi"
      "acdk/lang/sys"
      "acdk/lang/ref"
      "acdk/lang/reflect"
      "acdk/io"
      "acdk/util"
      "acdk/util/logging"
      "acdk/locale"
    )
  )
  (setg acdk-make-project-metainfos
    '(
      "acdk/lang"
      "acdk/lang/dmi"
      "acdk/lang/ref"
      "acdk/lang/reflect"
      "acdk/io"
      "acdk/util"
      "acdk/util/logging"
      "acdk/locale"
      )
  )
  (acdkmake-main)
)


(make-main)



