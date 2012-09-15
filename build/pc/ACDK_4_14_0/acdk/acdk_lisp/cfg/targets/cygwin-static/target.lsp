
(include "../gmake/target.lsp")

(setg acdkmake-target-cflags-dll "-g")
(setg acdkmake-target-ldflags-dll "-g")


;; overwrites io.lsp
(defun slash ()
  (print ".") (flush)
  "/"
)

(setg acdkmake-target-defines '(( "OS_CYGWIN32" )  ("_REENTRANT")))

(defun acdkmake-is-platform (label)
  (if (or (streql label "win32") (streql label "cygwin"))
    t
    NIL
  )
)

(if (not (isdef gmake_getBindModule))
  (setg gmake_getBindModule getBindModule)
)

(setg gmake_getBindModule getBindModule)

(setg default_acdkmake-get-target-binext acdkmake-get-target-binext)
(defun acdkmake-get-target-binext ()
  (setf ext (default_acdkmake-get-target-binext))
  (if (streql ext ".so")
    (setf ext ".a")
  )
  ext
) 

(defun acdkmake-target-get-target-executable ()
  (setf executable (s+ acdkmake-project-name (acdkmake-get-target-binext)))
  (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) executable))
)

(defun acdkmake-target-get-bindmodule ()
  (if (invoke acdkmake-project-type 'endsWith "exe")
    (return (gmake_getBindModule))
  )
  (setf executable (s+ acdkmake-project-name (acdkmake-get-target-binext)))
  (setf fqexecutable  (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) executable)))
  (setf erg (s+  fqexecutable  ": $(OBJECTS)\n"))
  (setf erg (s+ erg "\t$(AR) $@ $(OBJECTS)\n\t-@ ($(RANLIB) $@ || true) >/dev/null 2>&1"))
  ;;(setf erg (s+ erg "\t$(LINK) $(LDFLAGS) " executable " -o " fqexecutable " $(OBJECTS) $(LDFLAGSX)"))
  erg    
)

(if (not (isdef acdkmake-get-libs))
  (setg acdkmake-get-libs-org acdkmake-get-libs)
)


(defun acdkmake-get-libs ()
  (if (or (not (isdef acdkmake-project-acdklibs)) (= (length acdkmake-project-acdklibs) 0))
    (return "")
  )
  (setf erg " ")
  (dolist (l acdkmake-project-acdklibs)
    (setf erg (s+ erg " " (acdkmake-convert-file-if-needed (s+ acdk-home (slash) "bin" 
      (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "") (slash) l acdkmake-target-binext-lib))))
  )
  (dolist (l acdkmake-project-libs)
    (setf l (eval-if-needed l))
    (if (isdef l)
      (if (l 'startsWith "$")
        (setf erg (s+ erg l " "))
        (setf erg (s+ erg " -l" (acdkmake-convert-file-if-needed l)))
      )
    )
  )
  (s+ erg (acdkmake-get-libs2))
)

