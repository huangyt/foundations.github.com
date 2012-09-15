
(include "../general/target.lsp")

(setg acdkmake-target-binext "")
(setg acdkmake-target-soext ".so")
(setg acdkmake-target-objext ".o")
(setg acdkmake-target-libext ".a")
(setg acdkmake-target-implibext ".so")

(setg acdkmake-target-link-exe "g++ -Wall -Wno-unused")
(setg acdkmake-target-compile-exe "g++ -Wall -Wno-unused")
(setg acdkmake-target-link-exe "g++")
(setg acdkmake-target-cflags-exe "-g -D_REENTRANT")
(setg acdkmake-target-ldflags-exe "-g")
(setg acdkmake-target-binext-exe "")

(setg acdkmake-target-compile-dll "g++ -Wall -Wno-unused")
(setg acdkmake-target-link-dll "g++ -shared")
(setg acdkmake-target-cflags-dll "-g -fpic")
(setg acdkmake-target-ldflags-dll "-g -fpic")
(setg acdkmake-target-binext-dll ".so")

(setg acdkmake-target-compile-lib "g++ -Wall -Wno-unused")
(setg acdkmake-target-link-lib "g++ ")
(setg acdkmake-target-cflags-lib "-g ")
(setg acdkmake-target-ldflags-lib "-g ")
(setg acdkmake-target-binext-lib ".a")
(setg acdkmake-target-targetscriptnl "; \\\n")

(setg acdkmake-target-makef "$(MAKE) ")
(setg acdkmake-target-makefile-opt "-f ")
(setg acdkmake-target-makenoabort-opt "-k ")
(setg acdkmake-target-cdpathprefix "")
(setg acdkmake-target-compileprefix "")

;; overwrite this from io.lsp to enable cross-development

(defun slash ()
  (print ".") (flush)
  "/"
)




(defun acdkmake-target-make-include (incl)
  (if (not incl) 
    (return "")
  )
  (s+ "-I " incl " ")
)

(defun acdkmake-target-make-define (label &optional set)
  (if (zerop label)
    (return "")
  )
  (if (isdef set)
    (s+ "-D" label)
    (s+ "-D" label "=" set)
  )
)

(defun acdkmake-target-get-target-executable ()
  (setf prefix "")
  (if (invoke acdkmake-project-type 'equals "dll")
    (setf prefix "lib")
  )
  (setf executable (s+ prefix acdkmake-project-name (acdkmake-get-target-binext)))
  (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) executable))
)


(defun acdkmake-target-getobjectlist ()
  (acdkmake-target-getfilelist acdkmake-project-objects)
)

(defun getMakeObject (src)
  (setf obj (acdkmake-convert-file-if-needed (objectfromsource src acdkmake-target-objext)))
  (setf src (acdkmake-convert-file-if-needed src))
  (setf erg (s+ obj ": " src "\n\t"))
  (setf erg (s+ erg acdkmake-target-compileprefix "$(CCOMPILER) $(CCOPTS) $(CCINCLUDE) $(DEFINES) $(CCOPTSX) $(CFLAGSX) -o " obj " -c " src "\n\n"))
  erg
)

(defun getMakeObjects ()
  (setf erg "")
  (dolist (s acdkmake-project-sources)
    (setf erg (s+ erg (getMakeObject s)))
  )
  erg
)


(defun acdkmake-get-project-ldflags ()
  (setf erg "")
  ;(println (s+ "ldflgs: " acdkmake-project-ldflags))
  (dolist (ldf acdkmake-project-ldflags)
    (setf erg (s+ erg " " ldf))
  )
  erg
)

(defun acdkmake-bind-so ()
  (setf executable (s+ "lib" acdkmake-project-name (acdkmake-get-target-binext)))
  (setf fqexecutable  (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) executable)))
  (setf ldfags (acdkmake-get-project-ldflags))
  (setf erg (s+  fqexecutable  ": $(OBJECTS)\n"))
  (setf erg (s+ erg "\t" acdkmake-target-compileprefix "$(LINKSO) $(SYSLDFLAGS) $(LDFLAGS) -o " fqexecutable " $(OBJECTS) $(LIBS) $(LDFLAGSX)"))
  erg    
)

(defun acdkmake-bind-exe ()
  (setf executable (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) acdkmake-project-name (acdkmake-get-target-binext))))
  (setf erg (s+ executable ": $(OBJECTS)\n"))
  (setf ldfags (acdkmake-get-project-ldflags))
  (setf erg (s+ erg "\t" acdkmake-target-compileprefix "$(LINKEXE) $(SYSLDFLAGS) $(LDFLAGS) " ldfags " $(OBJECTS) -o "  executable " $(LIBS) $(LDFLAGSX)"))
  erg
)
 
(defun getBindModule ()
  (if (invoke acdkmake-project-type 'endsWith "exe")
    (acdkmake-bind-exe)
    (acdkmake-bind-so)
  )
)

(defun make-convert-text (text)
  ;(println "convert text")
  (dos2unix text)
)

(defun acdkmake-target-get-make-metainfo ()
  (set erg "metainfo:\n")
  
  (dolist (el acdk-make-project-metainfos)
    (if el
      (set erg (s+ erg "\t" acdkmake-target-compileprefix "$(ACDKMC) " el ";\\\n"))
    )
  )
  erg
)

;;; ==============================================================================================
;;; Meta-Projectfiles
;;;




(defun acdk-pmake-gen-make-depend ()
  (set erg "depend: ")
  (dolist (ps acdk-projects)
    (set erg (s+ erg (cadr ps) "-depend "))
  )
  (set erg (s+ erg "\n"))
  
  (dolist (ps acdk-projects)
    (set name (cadr ps))
    (set path (car ps))
    (set erg (s+ erg name "-depend:\n"))
    (set erg (s+ erg "\tcd " acdkmake-target-cdpathprefix path "; " 
    	acdkmake-target-makef acdkmake-target-makefile-opt name "." acdkmake-target " depend\n\n"))
  )
  erg
)



(defun acdk-pmake-generate-make ()
  (set erg "compile: ")
  (dolist (ps acdk-projects)
    (set erg (s+ erg (cadr ps) " "))
  )
  (set erg (s+ erg "\n"))
  
  (setf erg (s+ erg (acdk-pmake-generate-ptype-targets "lib" )))
  (setf erg (s+ erg (acdk-pmake-generate-ptype-targets "bin")))
  (setf erg (s+ erg (acdk-pmake-generate-ptype-targets "utest")))
  (setf erg (s+ erg (acdk-pmake-generate-ptype-targets "tool")))
  (setf erg (s+ erg (acdk-pmake-generate-ptype-targets "dist")))
  
  
  (set erg (s+ erg "clean: "))
  (dolist (ps acdk-projects)
    (set erg (s+ erg (cadr ps) "-clean "))
  )
   (set erg (s+ erg "\n"))
  (set erg (s+ erg "metainfo: "))
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
    ;;(invoke (peek-static 'acdk.lang.System 'out) 'println (s+ "targetFlags: " (toCode targetFlags)))
    (setf addMakeFlag "");
    (if (and (isdef targetFlags) (strInList targetFlags "optional"))
    	(setf addMakeFlag acdkmake-target-makenoabort-opt)
    )
    (set erg (s+ erg "\n\tcd " acdkmake-target-cdpathprefix path "; " acdkmake-target-makef addMakeFlag acdkmake-target-makefile-opt name "." acdkmake-target "\n\n"))
    
    (set erg (s+ erg name "-clean:\n"))
    (set erg (s+ erg "\tcd " acdkmake-target-cdpathprefix path "; " acdkmake-target-makef acdkmake-target-makefile-opt name "." acdkmake-target " clean\n\n"))
    
    (set erg (s+ erg name "-metainfo:\n"))
    (set erg (s+ erg "\tcd " acdkmake-target-cdpathprefix path "; " acdkmake-target-makef acdkmake-target-makefile-opt name "." acdkmake-target " metainfo\n\n"))
  )
 
  
  (s+ erg (acdk-pmake-gen-make-depend))
)

(defun cdup-from-cd (path)
  (if (streql path ".")
    (return ".")
  )
  (setf mcount (invoke path 'elementCount "/"))
  (setf erg "..")
  (while (> mcount 0)
    (progn
      (setf erg (s+ erg "/.."))
      (setf mcount (- mcount 1))
    )
  )
  erg  
)

(defun acdk-pmake-generate-tests ()
  (set erg "test::\n")
  (if (and (> (length acdkmake-project-tests) 0) (car acdkmake-project-tests))
      (progn
        (dolist (_tt acdkmake-project-tests) 
          (set erg (s+ erg "\t$(ACDKHOME)/bin/" _tt " $(AUNIT_TESTOPTS); \\\n"))
          )
        )
    (set erg (s+ erg "\techo no test cases defined\n"))
   )
  erg
)
