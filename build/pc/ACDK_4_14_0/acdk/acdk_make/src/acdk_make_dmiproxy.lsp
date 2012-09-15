;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_make/src/acdk_make_dmiproxy.lsp,v 1.2 2005/02/07 17:15:10 kommer Exp $
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
  
  (set _acdkmake (s+ acdk-home "/cfg/lib/acdk/lisp/acdkmake.lsp"))
  (println (s+ "including: " _acdkmake))
  (include _acdkmake)
  (if (not (acdk-make-test-args))
    (progn 
      (println (s+ "target for make is needed. See " ACDKHOME " /cfg/target"))
      (exit 1)
    )
  )
)

(defun make-main ()
  (acdk-main-init)
  
  (setg acdkmake-project-name  "acdk_make_dmiproxy") ;;; replace this name
  (setg acdkmake-project-type "dll") ;; exe or dll
  
  ;; normally you doesn't need to modify this
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
  
  
  (setg acdkmake-project-includes '(".")) ; add this list of directories in to include compile statement
                                          ; acdk include will included automatically
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"  ; the list of used acdk-libraries
      "acdk_cfgscript"
      "acdk_make"  
     )
  )
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-ldflags '())

  (setg acdkmake-project-defines 
    '(
      ("IN_ACDK_MAKE_DMIPROXY_LIB")
    )
  )
  (setg acdkmake-project-sourcelist ; directory and/or source files
    '(
      "acdk/make/make_dmiproxy"
     )
  )
  (setg acdk-make-project-metainfos
    '(
      )
  )
  (acdkmake-main) ; generate the make/projekt files
)


(make-main) ; just call make-main above

