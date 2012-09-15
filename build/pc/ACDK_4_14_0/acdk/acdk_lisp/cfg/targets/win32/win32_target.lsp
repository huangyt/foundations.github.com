(include "../general/target.lsp")

(setg acdkmake-target-binext ".exe")
(setg acdkmake-target-dllext ".dll")
(setg acdkmake-target-objext ".obj")
(setg acdkmake-target-libext ".lib")
(setg acdkmake-target-implibext ".lib")


(setg acdkmake-target-defines '(("OS_WIN32")))

(defun acdkmake-target-get-clean ()
  (setf executable (acdkmake-convert-file-if-needed (s+ acdkmake-project-exec-dir (slash) acdkmake-project-name (acdkmake-get-target-binext))))
  (setf erg (s+ "del " executable "\n"))
  
  (dolist (o acdkmake-project-objects)
    (setf erg (s+ erg "\tdel " (acdkmake-convert-file-if-needed o) "\n"))
  )
  erg
)

