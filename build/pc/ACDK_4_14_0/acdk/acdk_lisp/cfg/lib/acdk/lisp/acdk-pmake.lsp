
;;; acdk-pmake.lsp


(defun acdkmake-target-get-pmakesuffix (acdkmake-default-targetsuffux)
  acdkmake-default-targetsuffux
)


(defun acdk-pmake-main ()
  (println (s+ "Called acdk-pmake.lsp: " 
    "acdk-project-distribution-version=[" acdk-project-distribution-version "]\n"
    "acdk-project-package-version=[" acdk-project-package-version "]\n"
    "acdk-project-platforms=[" acdk-project-platforms "]\n"
    "acdk-projects=[" acdk-projects "]\n"
    "acdkmake-project-tests=[" acdkmake-project-tests "]\n"
    "acdk-project-platform=[" acdk-project-platform "]\n"
    
  ))
  (setg acdk-cfg-home (s+ acdk-home "/cfg"))
  (setg acdkmake-target acdk-project-platform)
  
  (setf t (s+ acdk-cfg-home "/targets/" acdkmake-target "/target.lsp"))
  (include t)
  (setf erg (acdkmake-load-eval-template (s+ acdk-cfg-home "/targets/" acdkmake-target "/PMakefile.template")))
  (setf makef (s+ acdkmake-project-name "."  (acdkmake-target-get-pmakesuffix acdkmake-target)))
  (writeStringToFile makef (make-convert-text erg))
  (println (s+ "\nwrote file: [" makef "]"))
  (setf args (getargs))
  (if (zerop (caddr args))
    (return)
  )
  (setg curopt (caddr args))
)

(acdk-pmake-main)
