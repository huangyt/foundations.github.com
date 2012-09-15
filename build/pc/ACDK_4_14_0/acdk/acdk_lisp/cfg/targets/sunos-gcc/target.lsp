(include "../linux/target.lsp")

(setg acdkmake-target-binext-exe "")

(setg acdkmake-target-cflags-dll "-g -fPIC")
(setg acdkmake-target-ldflags-dll "-g -fPIC")


(setg acdkmake-target-link-dll "g++")	; default is with arg -shared, which fails on solaris with non-gnu-ld
(setg acdkmake-target-ldflags-dll "-Wl,-G -Wl,-t -Wl,-R -Wl,$(LD_LIBRARY_PATH)")
(setg acdkmake-target-link-exe "g++")
(setg acdkmake-target-ldflags-exe "-Wl,-t -Wl,-R -Wl,$(LD_LIBRARY_PATH)")



(setg acdkmake-target-makef "$(MAKE) ") ;; not used
(setg acdkmake-target-targetscriptnl "; \\\n")


(defun acdkmake-is-platform (label)
  (if (or (streql label "unix") (streql label "solaris"))
    t
    NIL
  )
)

(defun acdkmake-target-get-bindmodule ()
  
  (if (invoke acdkmake-project-type 'endsWith "exe")
    (return (gmake_getBindModule))
  )
  
  (setf executable (s+ "lib" acdkmake-project-name (acdkmake-get-target-binext)))
  (setf fqexecutable  (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) executable)))
  (setf erg (s+  fqexecutable  ": $(OBJECTS)\n"))
  (setf erg (s+ erg "\t$(LINKSO) $(SYSLDFLAGS) $(LDFLAGS) -o " fqexecutable " $(OBJECTS) $(LIBS) $(LDFLAGSX)"))
  erg    
)

(defun make-convert-text (text)
  (dos2unix text)
)


(if (not (isdef acdkmake-get-libs))
  (setg acdkmake-get-libs-org acdkmake-get-libs)
)

(defun acdkmake-get-libs ()
  (setf erg "")
  (if (and (isdef acdkmake-project-libs) (> (length acdkmake-project-libs) 0))
    (dolist (l acdkmake-project-libs)
      (if (isdef l)
        (if (l 'startsWith "$")
          (setf erg (s+ erg l " "))
          (setf erg (s+ erg "-l" l " "))
        )
      )
    )
  )
  (if (and (isdef acdkmake-project-acdklibs) (> (length acdkmake-project-acdklibs) 0))
    (progn
      (setf erg (s+ erg "-Wl,-Bdynamic -L$(BINDIR) "))
      (dolist (l acdkmake-project-acdklibs)
        ;(println (s+ "Library: " l))
        (if (isdef l)
          (if (l 'startsWith "$")
            (setf erg (s+ erg l " "))
            (setf erg (s+ erg "-l" l " "))
          )
        )
      )
    )
  )
  (s+ erg (acdkmake-get-libs2))
)

;        (setf erg (s+ erg " " (acdkmake-convert-file-if-needed (s+ acdk-home (slash) "bin" (slash) acdkmake-target (slash) l acdkmake-target-libext))))

(setg acdkmake-target-defines '( ("OS_SOLARIS") ("_REENTRANT")))

