;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/aal/src/acdk_aci_guidbg.lsp,v 1.3 2005/02/07 17:11:23 kommer Exp $
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
  
  (setg acdkmake-project-name  "acdk_aci_guidbg") ;;; replace this name
  (setg acdkmake-project-type "dll") ;; exe or dll
  
  ;; normally you doesn't need to modify this
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (getobjectdir "../tobj"))
  (setg acdkmake-project-template "ACDKCore")
  
  
  (setg acdkmake-project-includes '(".")) ; add this list of directories in to include compile statement
                                          ; acdk include will included automatically
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"  ; the list of used acdk-libraries
      "acdk_cfgscript"
      "acdk_text"
      "acdk_aci"
      "acdk_wx"
     )
  )
  (include "../../acdk_wx/wx_cfg.lsp") ;; ajust configuration here
  
  (setg acdkmake-project-defines 
    '(
      ("IN_ACDK_ACI_GUIDBG_LIB")
    )
  )
  (setg acdkmake-project-sourcelist ; directory and/or source files
    '(
      "acdk/aci/guidbg"
     )
  )
  (setg acdk-make-project-metainfos
    '(
      "acdk/aci/guidbg"
      )
  )
  (acdkmake-main) ; generate the make/projekt files
)


(make-main) ; just call make-main above

