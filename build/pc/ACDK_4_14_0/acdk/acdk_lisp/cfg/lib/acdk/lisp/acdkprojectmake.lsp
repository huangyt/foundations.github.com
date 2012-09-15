;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_lisp/cfg/lib/acdk/lisp/acdkprojectmake.lsp,v 1.7 2004/06/09 17:59:08 kommer Exp $
;;;
;; acdkprojectmake.lsp


(defun acdk-make-generate-makes (pt)
  "This routine calles to have a clean environment for a specific cross-plattform a new LispInterpreter. \
  acdk-pmake does the real work"
  
  (set li (new 'acdk.lisp.LispEnvironment))
  (invoke li 'init)
  ;(set c (s+ "(setg acdk-project-distribution-version '" (invoke acdk-project-distribution-version 'toString)))
  ;(invoke li 'parseEval c)
  (invoke li 'bindGlobal "acdk-project-distribution-version" (new 'acdk.lisp.LispAtom acdk-project-distribution-version))
  (invoke li 'bindGlobal "acdk-project-package-version" acdk-project-package-version)
  (invoke li 'bindGlobal "acdk-project-platforms" acdk-project-platforms)
  (invoke li 'bindGlobal "acdk-projects" acdk-projects)
  (invoke li 'bindGlobal "acdk-project-platform" pt)
  (invoke li 'bindGlobal "acdk-home" acdk-home)
  (invoke li 'bindGlobal "acdkmake-project-name" acdkmake-project-name)
  (if (isdef acdk-tests)
     (invoke li 'bindGlobal "acdkmake-project-tests" acdk-tests)
     (invoke li 'bindGlobal "acdkmake-project-tests" '())
  )
  ;(println (s+ "set tests: " (toString acdk-tests)))

  (setf acdk-pmake (s+ acdk-home "/cfg/lib/acdk/lisp/acdk-pmake.lsp"))
  (invoke li 'load acdk-pmake) ; calling submain with specific target
)

(defun acdkmake-project-main ()
  (if (or (not (isdef acdk-project-platforms)) (not acdk-project-platforms))
    (setg acdk-project-platforms '("dsp" "linux" "sunos-gcc" "bsd" "mingw" "bcc" "cbx"))
  )
  (dolist (pt acdk-project-platforms)
    (acdk-make-generate-makes pt)
  )
)

