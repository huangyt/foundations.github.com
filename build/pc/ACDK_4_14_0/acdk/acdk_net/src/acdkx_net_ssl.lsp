;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_net/src/acdkx_net_ssl.lsp,v 1.3 2005/03/31 15:10:14 kommer Exp $
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
  
  (setg acdkmake-project-name "acdkx_net_ssl")
  (setg acdkmake-project-type "dll")
  
   ;; normally you doesn't need to modify this
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")
  
  (setg acdkmake-project-includes 
  	'(
    	"."
    	"$(ACDK_OPENSSL_INCLUDE)"
    )
   )
  ;(setg acdkmake-project-includes l)
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_net"
     )
  )
  (include "../openssl_cfg.lsp") 

  (setg acdkmake-project-defines 
    '(
      ("IN_ACDKX_NET_SSL_LIB")
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdkx/net/ssl"
     )
  )
  (setg acdk-make-project-metainfos
    '(
      "acdkx/net/ssl"
      
      )
  )
  (acdkmake-main)
)


(make-main)

