
(setg acdkmake-target-exeext "")
(setg acdkmake-target-soext ".so")
(setg acdkmake-target-objext ".o")
(setg acdkmake-target-libext ".a")



(setg acdkmake-target-link-exe "ld")
(setg acdkmake-target-compile-exe "CC")
(setg acdkmake-target-link-exe "ld")
(setg acdkmake-target-cflags-exe "-g -w -mt -D_REENTRANT -D__sunos__")
(setg acdkmake-target-ldflags-exe "-g")
(setg acdkmake-target-binext-exe "")

(setg acdkmake-target-compile-dll "CC")
(setg acdkmake-target-link-dll "CC -shared -Bdynamic -PIC -G")
(setg acdkmake-target-cflags-dll "-PIC -w -mt -D_REENTRANT -D__sunos__")
(setg acdkmake-target-ldflags-dll "")
(setg acdkmake-target-binext-dll ".so")

(setg acdkmake-target-defines '(("OS_SOLARIS")))

(defun slash ()
  "/"
)

(defun acdkmake-target-make-include (incl)
  (if (not incl) 
    (return "")
  )
  (s+ "-I" incl " ")
)

(defun acdkmake-target-make-define (label &optional set)
  (if (zerop label)
    (return "")
  )
  (if (zerop set)
    (s+ "-D" label)
    (s+ "-D" label "=" set)
  )
)

(defun nonsense-acdkmake-target-getfilelist (al)
  (setf erg "")
  (dolist (o al)
    (if erg
      (setf erg (s+ erg "\t" o " \\\n"))
      (setf erg (s+ "  " o " \\\n"))
    )
  )
  erg
)

(defun acdkmake-target-getobjectlist ()
  (acdkmake-target-getfilelist acdkmake-project-object)
)

(defun getMakeObject (src)
  (setf obj (objectfromsource src acdkmake-target-objext))
  (setf erg (s+ obj ":" src "\n\t"))
  (setf erg (s+ erg "$(CC) -c $(CCOPTS) $(CCINCLUDE) $(DEFINES) $(CFLAGSX) -o $@ $<\n\n"))
  erg
)

(defun getMakeObjects ()
  (setf erg "")
  (dolist (s acdkmake-project-sources)
    (if erg
      (setf erg (s+ erg (getMakeObject s)))
      (setf erg (getMakeObject s))
    )
  )
  erg
)


(defun getBindModule ()
  (setf executable (s+ acdkmake-project-name (acdkmake-target-binext)))
  (setf erg (s+ executable ": $(OBJECTS)\n"))
  (setf erg (s+ erg "\t$(LINK) $(OBJECTS) $(LIBS) $(LIBS) -o "  executable))
  erg
)

