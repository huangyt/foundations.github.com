
(include "../gmake/target.lsp")

(setg acdkmake-target-binext ".exe")
(setg acdkmake-target-soext ".dll")
(setg acdkmake-target-objext ".o")
(setg acdkmake-target-libext ".lib")
(setg acdkmake-target-implibext ".lib")

(setg acdkmake-target-link-exe "g++ -Wall -Wno-unused")
(setg acdkmake-target-compile-exe "g++ -Wall -Wno-unused")
(setg acdkmake-target-link-exe "g++")
(setg acdkmake-target-cflags-exe "-g -D_REENTRANT -O2 ")
(setg acdkmake-target-ldflags-exe "-g ")
(setg acdkmake-target-binext-exe ".exe")
(setg acdkmake-target-binext-guiexe ".exe")

(setg acdkmake-target-compile-dll "g++ -Wall -Wno-unused")
(setg acdkmake-target-link-dll "g++ -shared")
(setg acdkmake-target-cflags-dll "-g -O2 ")
(setg acdkmake-target-ldflags-dll "-g")
(setg acdkmake-target-binext-dll ".dll")

(setg acdkmake-target-compile-lib "g++ -Wall -Wno-unused")
(setg acdkmake-target-link-lib "g++ ")
(setg acdkmake-target-cflags-lib "-g -O2 ")
(setg acdkmake-target-ldflags-lib "-g ")
(setg acdkmake-target-binext-lib ".lib")


;; overwrites io.lsp

(defun slash ()
  (print ".") (flush)
  "/"
)

(defun acdkmake-is-platform (label)
  (if (or (streql label "win32"))
    t
    NIL
  )
)

(setg acdkmake-target-defines 
  '(
    ( "OS_WIN32" )  
    ("_REENTRANT")
    ("WIN32_THREADS")
    ("ACDK_OS_WIN32")
    ("ACDK_MINGW")
   )
)


(if (not (isdef gmake_getBindModule))
  (setg gmake_getBindModule getBindModule)
)


(defun acdkmake-bind-dll ()
  (setf fqpname (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) "lib" acdkmake-project-name)))
  (setf executable (s+ "lib" acdkmake-project-name (acdkmake-get-target-binext)))
  (setf deffile (s+ fqpname ".def"))
  (setf libfile (s+ fqpname ".lib"))
  (setf fqexecutable  (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) executable)))
  (setf ldfags (acdkmake-get-project-ldflags))
  (setf erg (s+  fqexecutable  ": $(OBJECTS)\n"))
  (setf implib (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) (s+ "lib" acdkmake-project-name ".a"))))
  
  (setf erg (s+ 
    erg "\t$(LINK) -Wl,--enable-auto-image-base -Wl,--out-implib," implib " $(SYSLDFLAGS) $(LDFLAGS) -o " fqexecutable " $(OBJECTS) $(LIBS) $(LDFLAGSX)\n"))

  ;;(setf erg (s+ 
  ;  erg "\t$(LINK) -Wl,--enable-auto-image-base -Wl,--output-def," deffile
  ;      " $(SYSLDFLAGS) $(LDFLAGS) -o " fqexecutable " $(OBJECTS) $(LIBS) $(LDFLAGSX); \\\n"
  ;      "\tdlltool --dllname " fqexecutable " --input-def " deffile " -l  " libfile))
  erg    
)

(defun acdkmake-bind-exe ()
  (setf executable (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) acdkmake-project-name (acdkmake-get-target-binext))))
  (setf erg (s+ executable ": $(OBJECTS)\n"))
  (setf ldfags (acdkmake-get-project-ldflags))
  (setf erg (s+ erg "\t$(LINKEXE) $(SYSLDFLAGS) $(LDFLAGS) " ldfags " $(OBJECTS) -o "  executable " $(LIBS) $(LDFLAGSX)"))
  erg
)

(defun acdkmake-target-get-bindmodule ()
  (if (invoke acdkmake-project-type 'endsWith "exe")
    (acdkmake-bind-exe)
    (acdkmake-bind-dll)
  )
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
      (setf erg (s+ erg "-Wl,-Bdynamic -L$(BINDIR) "))
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



