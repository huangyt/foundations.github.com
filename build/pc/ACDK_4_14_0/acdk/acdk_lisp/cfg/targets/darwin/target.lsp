
(include "../gmake/target.lsp")


(setg acdkmake-target-link-exe "CC -Wall -Wno-unused -dynamic")
(setg acdkmake-target-compile-exe "CC -Wall -Wno-unused -dynamic")
(setg acdkmake-target-link-exe "CC -dynamic")
(setg acdkmake-target-cflags-exe "-g -D_REENTRANT")
(setg acdkmake-target-ldflags-exe "-g")
(setg acdkmake-target-binext-exe "")

(setg acdkmake-target-compile-dll "CC -Wall -Wno-unused")
(setg acdkmake-target-link-dll "CC -dynamic -dynamiclib")
(setg acdkmake-target-cflags-dll "-g -fPIC")
(setg acdkmake-target-ldflags-dll "-g -fPIC")
(setg acdkmake-target-binext-dll ".dylib")

(setg acdkmake-target-compile-lib "CC -Wall -Wno-unused")
(setg acdkmake-target-link-lib "CC ")
(setg acdkmake-target-cflags-lib "-g ")
(setg acdkmake-target-ldflags-lib "-g ")
(setg acdkmake-target-binext-lib ".a")
(setg acdkmake-target-targetscriptnl "; \\\n")


;; overwrites io.lsp

(defun slash ()
  (print ".") (flush)
  "/"
)

(defun acdkmake-is-platform (label)
  (if (or (streql label "unix") (streql label "darwin"))
    t
    NIL
  )
)

(setg acdkmake-target-defines '(( "OS_DARWIN" )  ("_REENTRANT")))


(if (not (isdef gmake_getBindModule))
  (setg gmake_getBindModule getBindModule)
)

(setg gmake_getBindModule getBindModule)



(defun acdkmake-target-get-bindmodule ()
  (setf erg (gmake_getBindModule))
  (if (invoke acdkmake-project-type 'endsWith "exe")
    (return erg)
  )
  (setf erg (s+ erg " -install_name @executable_path/lib" acdkmake-project-name (acdkmake-get-target-binext)))
  (return erg)
  
  
  ;; not executed
  
  ;;(if (invoke acdkmake-project-type 'endsWith "exe")
  ;  (return (gmake_getBindModule))
  ;)
  
  ;(setf executable (s+ acdkmake-project-name (acdkmake-get-target-binext)))
  ;(setf fqexecutable  (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) executable)))
  ;(setf ldfags (acdkmake-get-project-ldflags))
  ;(setf erg (s+  fqexecutable  ": $(OBJECTS)\n"))
  ;(setf erg (s+ erg "\t$(LINK) $(LDFLAGS) " ldfags " -Wl,-soname," executable " -o " fqexecutable " $(OBJECTS)"))
  ;erg    
)

(if (not (isdef acdkmake-get-libs))
  (setg acdkmake-get-libs-org acdkmake-get-libs)
)


(defun acdkmake-get-libs ()
  (setf erg "")
  (if (and (isdef acdkmake-project-libs) (> (length acdkmake-project-libs) 0))
    (dolist (l acdkmake-project-libs)
      (if (isdef l)
        (setf erg (s+ erg "-l" l " "))
      )
    )
  )
  (if (and (isdef acdkmake-project-acdklibs) (> (length acdkmake-project-acdklibs) 0))
    (progn
      (setf erg (s+ erg "-L$(BINDIR) "))
      (dolist (l acdkmake-project-acdklibs)
        ;(println (s+ "Library: " l))
        (if (isdef l)
          (setf erg (s+ erg " -l" l))
        )
      )
    )
  )
  (s+ erg (acdkmake-get-libs2))
)



