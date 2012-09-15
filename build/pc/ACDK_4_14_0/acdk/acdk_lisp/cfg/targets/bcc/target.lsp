
(include "../win32/win32_target.lsp")



(setg acdkmake-target-compile-exe "bcc32.exe")
(setg acdkmake-target-link-exe "ilink32.exe")
;;(setg acdkmake-target-cflags-exe "-w-8070 -w-8030 -w-8057 -w-8026 -w-8027 -w-8066 -tWC -tWR -tWM -q -R -Od ") ;; debug -v -vi- ;; advanced -Hc  -X- -r- -a8 -b- -k

(setg acdkmake-target-cflags-exe "-v -w-8070 -w-8030 -w-8057 -w-8026 -w-8027 -w-8066 -w-8008  -tWC -tWR -tWM -Od ") ;; advanced -Hc -Vx -X- -r- -a8 -b- -k

(setg acdkmake-target-ldflags-exe "-L$(OUTDIR) -Tpe -ap -c -v")
(setg acdkmake-target-binext-exe ".exe")
(setg acdkmake-target-binext-guiexe ".exe")

(setg acdkmake-target-compile-dll "bcc32.exe")
(setg acdkmake-target-link-dll "ilink32.exe")
(setg acdkmake-target-cflags-dll "-v -w-8070 -w-8030 -w-8057 -w-8026 -w-8027 -w-8066 -w-8008 -tWD -tWR -tWM -q -R -Od ") ;; advanced -Hc  -X- -r- -a8 -b- -k
(setg acdkmake-target-ldflags-dll "-L$(OUTDIR) -Tpd -aa -c -Gi -D\"\" -v")
(setg acdkmake-target-binext-dll ".dll")

;; (setg acdkmake-target-make-fqname 1)
(setg acdkmake-target-make-quoted-filenames 1)
(setg acdkmake-target-defines '(("OS_WIN32")))

(setg acdkmake-target-targetscriptnl "\n")

(setg compiletypesuffix "")
(setg acdkmake-target-makef "make ")
(setg acdkmake-target-makefile-opt "-f ")
(setg acdkmake-target-makenoabort-opt "-i ")
(setg acdkmake-target-cdpathprefix "")
(setg acdkmake-target-compileprefix "")

(defun acdkmake-is-platform (label)
  (if (or (streql label "win32") (streql label "bcc"))
    t
    NIL
  )
)

(defun acdkmake-target-make-include (incl)
  (if (not incl) 
    (return "")
  )
  (s+ "-I" incl " ")
)

(defun acdkmake-target-make-define (label &optional optset)
  (if (zerop label)
    (return "")
  )
  (if (isdef optset)
    (s+ "-D"label "=" optset)
    (s+ "-D"label)
  )
)

(defun make-convert-text (text)
  (unix2dos text)
)

(defun acdkmake-target-getobjectlist ()
  (acdkmake-target-getfilelist acdkmake-project-object)
)

(defun getMakeObject (src)
  (setf obj (acdkmake-convert-file-if-needed (objectfromsource src acdkmake-target-objext)))
  (setf src (acdkmake-convert-file-if-needed src))
  (setf erg (s+ obj ": " src "\n\t"))
  (setf erg (s+ erg "$(CC) -c $(CCOPTS) $(CCINCLUDE) $(DEFINES) $(CFLAGSX) -n" (acdkmake-convert-file-if-needed acdkmake-project-object-dir) " " src "\n\n"))
  erg
)

(defun acdkmake-get-libs ()
  (setf erg "")
  (if (isdef acdkmake-project-acdklibs)
    (progn
      (dolist (l acdkmake-project-acdklibs)
        (if (isdef l)
          ; (setf erg (s+ erg " " (acdkmake-convert-file-if-needed (s+ acdk-home (slash) "bin" (slash) acdkmake-target (slash) l acdkmake-target-libext))))
          (setf erg (s+ erg " " (acdkmake-convert-file-if-needed (s+ acdk-home (slash) "bin" (slash) l acdkmake-target-libext))))
        )
      )
    )
  )
  (println (s+ "acdkmake-project-libs: " (toCode acdkmake-project-libs)))
  (if (isdef acdkmake-project-libs)
    (progn
      (dolist (l acdkmake-project-libs)
        (if (isdef l)
          (progn 
            ;(println (s+ "Lib: " l))
            (setf erg (s+ erg " " l))
          )
        )
      ) 
    )
  )
  (if (not (invoke compiletypesuffix 'equals "_r"))
    (if (isdef acdkmake-project-ldflags2-debug)
      (setf erg (s+ erg " " acdkmake-project-ldflags2-debug))
    )
    (if (isdef acdkmake-project-ldflags2-release)
      (setf erg (s+ erg " " acdkmake-project-ldflags2-release))
    )
  )
  (s+ erg (acdkmake-get-libs2))
)

(defun acdkmake-target-make-objects ()
  (setf erg "")
  (dolist (s acdkmake-project-sources)
    (if erg
      (setf erg (s+ erg (getMakeObject s)))
      (setf erg (getMakeObject s))
    )
  )
  erg
)

(setg org-slash slash)

(defun slash ()
  (print ".")(flush)
  (org-slash)
)


(defun acdkmake-target-get-outfile ()
  (acdkmake-convert-file-if-needed (s+ acdkmake-project-exec-dir (slash) acdkmake-project-name (acdkmake-get-target-binext)))
)

(defun acdk-make-target-getimplib ()
  (return "")
  ;; rest not needed
  (if (not (invoke acdkmake-project-type 'equals "dll"))
    (progn
      (println (s+ "acdkmake-project-type: " acdkmake-project-type))
      (return "")
    )
  )
  (setf outf (s+ acdkmake-project-exec-dir (slash) acdkmake-project-name))
  (setf outl (acdkmake-convert-file-if-needed (s+ outf ".lib")))
  (setf oute (acdkmake-convert-file-if-needed (s+ outf (acdkmake-get-target-binext))))
  (s+ "implib -c -w " outl " " oute)
)

(defun acdkmake-target-get-startupobj ()
  (if (= (invoke acdkmake-project-type 'compareTo "dll") 0)
    "c0d32.obj"
    "c0x32.obj"
  )
) 

(defun uppath (path)
  (getRevercedCDPath path)
)


(defun acdkmake-get-project-ldflags ()
  (setf erg "")
  (dolist (ldf acdkmake-project-ldflags)
    (setf key (car ldf))
    (setf val (cadr ldf))
    (if key
    	(setf erg (s+ erg key "\"" val "\""))
    )
  )
  erg
)

(defun acdk-pmake-generate-make ()
  (set erg "compile:: ")
  (dolist (ps acdk-projects)
    (set erg (s+ erg (cadr ps) " "))
  )
  (set erg (s+ erg "\n"))
  (set erg (s+ erg "clean:: "))
  (dolist (ps acdk-projects)
    (set erg (s+ erg (cadr ps) "-clean "))
  )
   (set erg (s+ erg "\n"))
  (set erg (s+ erg "metainfo:: "))
  (dolist (ps acdk-projects)
    (set erg (s+ erg (cadr ps) "-metainfo "))
  )
  (set erg (s+ erg "\n"))
  (dolist (ps acdk-projects)
    (set name (cadr ps))
    (set path (car ps))
    (set erg (s+ erg name ": "))
    (set deps (car (cdr (cdr ps))))
    (if deps
      (progn
        (dolist (d deps)
          (set erg (s+ erg " " d))
        )  
      )
    )
    (set targetFlags (car (cdr (cdr (cdr ps)))))
    (setf addMakeFlag "");
    (if (and (isdef targetFlags) (strInList targetFlags "optional"))
    	(setf addMakeFlag acdkmake-target-makenoabort-opt)
    )
    (set upp (uppath path))
    (set erg (s+ erg 
      "\n\tcd " acdkmake-target-cdpathprefix path "\n"
      "\t$(MAKE)" addMakeFlag " -f " name "." acdkmake-target "\n"
      "\tcd " upp "\n"
      "\n\n"))
    
    (set erg (s+ erg name "-clean:\n"))
    (set erg (s+ erg 
            "\tcd " acdkmake-target-cdpathprefix path "\n"
            "\t$(MAKE) -f " name "." acdkmake-target " clean\n"
            "\tcd " upp "\n"
            "\n\n"))
    
    (set erg (s+ erg name "-metainfo:\n"))
    (set erg (s+ erg 
          "\tcd " acdkmake-target-cdpathprefix path "\n"
          "\t$(MAKE) -f " name "." acdkmake-target " metainfo\n"
          "\tcd " upp "\n"
          "\n\n"))
  )
  erg
)



(defun acdk-pmake-generate-tests ()
  (set erg "test::\n")
  (set testext "")
  (if (and (> (length acdkmake-project-tests) 0) (car acdkmake-project-tests))
      (progn 
        
        (dolist (_tt acdkmake-project-tests) 
          (set erg (s+ erg "\t$(ACDKHOME)\\bin\\" _tt testext " $(AUNIT_TESTOPTS)\n\n"))
          )
        )
     (set erg (s+ erg "\techo no test cases defined\n"))
   )
  erg
)
