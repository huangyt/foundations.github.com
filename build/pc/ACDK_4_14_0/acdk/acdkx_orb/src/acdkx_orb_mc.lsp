;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx_orb_mc.lsp,v 1.3 2005/02/07 17:18:02 kommer Exp $
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
  
  (setg acdkmake-project-name "acdkx_orb_mc")
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
      "acdk_tools_mc"
     )
  )
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-ldflags '())

  (setg acdkmake-project-defines 
    '(
    	( "IN_ACDKX_ORB_MC_LIB" )
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdkx/orb/mc"
      "acdkx/orb/mc/mc_metainf"
     )
  )
  (setg acdk-make-project-metainfos
    '(
      "acdkx/orb/mc"
      )
  )
  (acdkmake-main)
)


(make-main)

