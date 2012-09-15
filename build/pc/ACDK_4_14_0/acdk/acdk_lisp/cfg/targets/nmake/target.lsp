(include "../win32/win32_target.lsp")


(setg acdkmake-target-compile-exe "cl.exe")
(setg acdkmake-target-link-exe "cl.exe")
(setg acdkmake-target-cflags-exe "")
(setg acdkmake-target-ldflags-exe " /SUBSYSTEM:CONSOLE /INCREMENTAL:NO")
(setg acdkmake-target-binext-exe acdkmake-target-binext)

(setg acdkmake-target-compile-dll "cl.exe")
(setg acdkmake-target-link-dll "cl.exe")
(setg acdkmake-target-cflags-dll "")
(setg acdkmake-target-ldflags-dll " /dll /INCREMENTAL:NO ")
(setg acdkmake-target-binext-dll acdkmake-target-dllext)

;; (setg acdkmake-target-make-fqname 1)
;; (setg acdkmake-target-make-quoted-filenames 1)

(setg acdkmake-target-defines '(("OS_WIN32")))

(defun acdkmake-is-platform (label)
  (if (or (streql label "win32") (streql label "dsp"))
    t
    NIL
  )
)

(defun acdkmake-target-make-include (incl)
  (if (not incl) 
    (return "")
  )
  (s+ "/I \"" incl "\" ")
)

(defun acdkmake-target-make-define (label &optional setdefine)
  (if (zerop label)
    (return "")
  )
  (if (isdef setdefine)
    (s+ "-D \""label "=" setdefine "\"")
    (s+ "/D \"" label "\"")
  )
)


(defun acdkmake-target-getobjectlist ()
  (acdkmake-target-getfilelist acdkmake-project-object)
)

(defun acdkmake-target-make-object (src)
  (setf obj (acdkmake-convert-file-if-needed (objectfromsource src acdkmake-target-objext)))
  (setf src (acdkmake-convert-file-if-needed src))
  (setf erg (s+ obj ": " src "\n\t"))
  (setf erg (s+ erg "$(CPP) -c $(CPP_PROJ) " src "\n\n"))
  erg
)


(defun acdkmake-target-make-objects ()
  (setf erg "")
  (dolist (s acdkmake-project-sources)
    (setf erg (s+ erg (acdkmake-target-make-object s)))
  )
  erg
)



;; overwrite this function, if not unix system
;; use the original
(defun no-acdkmake-target-get-clean ()
  (setf executable (s+ acdkmake-project-name (acdkmake-get-target-binext)))
  (setf erg (s+ "\t-@erase \"$(OUTDIR)\\" executable "\"\n"))
  (dolist (o acdkmake-project-objects)
    (setf erg (s+  erg "\t-@erase \"" o "\"\n"))
  )
  erg
)

(defun acdkmake-get-ldflags ()
  (setf erg (s+ " " (acdkmake-get-target-ldflags) " "))
  (setf erg (s+ erg "/incremental:no"))
  (if (acdkmake-project-is-type "dll")
    (progn 
      (setf erg (s+ erg " /pdb:\"$(OUTDIR)\\" acdkmake-project-name ".pdb\" /debug /machine:I386 /out:\"$(OUTDIR)\\" acdkmake-project-name ".dll\" "))
      (setf erg (s+ erg " /implib:\"$(OUTDIR)\\" acdkmake-project-name ".lib\ " "/pdbtype:sept"))
    )
    (progn 
      (setf erg (s+ erg " /pdb:\"$(OUTDIR)\\" acdkmake-project-name ".pdb\" /debug /machine:I386 /out:\"$(OUTDIR)\\" acdkmake-project-name ".exe\" "))
      (setf erg (s+ erg " /pdbtype:sept"))
    )
  )
  erg
)



(defun acdk-make-target-getimplib ()
  (setf outf (getCanonicalPath (s+ acdkmake-project-exec-dir "/" acdkmake-project-name )))
  (s+ "implib -c -w " outf ".lib " outf (acdkmake-get-target-binext))
)


(if (not (isdef org-acdkmake-get-defines))
  (setg org-acdkmake-get-defines acdkmake-get-defines)
)

(defun acdkmake-get-defines ()
  (setf erg (org-acdkmake-get-defines))
  
  (setf erg (s+ erg " /D \"WIN32\" /D \"_DEBUG\" /D \"_WINDOWS\" /D \"_MT\" /D \"ACDK_DEBUG\""))
  (if (acdkmake-project-is-type "dll")
    (setf erg (s+ erg " /D \"_USRDLL\""))
  )
  erg
)


