;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_xml/src/org_xml.lsp,v 1.10 2005/02/07 17:17:12 kommer Exp $
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
  (println (s+ "ACDKHOME=" ACDKHOME))
  (setg acdkmake-project-name "org_xml")
  (setg acdkmake-project-type "dll")
  
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))

  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name "/" acdkmake-target))

  (setg acdkmake-project-template "ACDKCore")
  (setg acdkmake-project-includes '("."))
  ;(setg acdkmake-project-includes l)
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
      "acdk_text"
     )
  )
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-ldflags '())

  (setg acdkmake-project-defines 
    '(
      ("IN_ORG_XML_LIB")
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "org/xml/sax"
      "org/xml/sax/helpers"
      "org/w3c/dom"
      "org/w3c/dom/xpath"
      "org/w3c/dom/traversal"
     )
  )
  (setg acdk-make-project-metainfos
    '(
      "org/xml/sax"
      "org/xml/sax/helpers"
      "org/w3c/dom"
      "org/w3c/dom/xpath"
      "org/w3c/dom/traversal"
      )
  )
  (acdkmake-main)
)


(make-main)

