;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_java/src/acdkjava.lsp,v 1.9 2005/02/07 17:14:41 kommer Exp $
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
  
  (setg acdkmake-project-name "acdkjava")
  (setg acdkmake-project-type "exe")
  
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))
  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))
  (setg acdkmake-project-template "ACDKCore")

  (include "../javacfg.lsp") ;; ajust configuration here
  (println (s+ 
    "please correct the library/include path for Perl if needed: " 
    "\nincs: " (toCode acdkmake-project-includes)
    "\nlibs: " (toCode acdkmake-project-libs)
    "\nldflags: " (toCode acdkmake-project-ldflags)
            )
  )
  
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_text"
      "acdk_java"
     )
  )
  (setg acdkmake-project-defines 
    '(
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdk/java/interpreter"
     )
  )

  (acdkmake-main)
)


(make-main)
