;;;
;;; This file is part of ACDK
;;; Copyrighted (c) by Roger Rene Kommer 2000
;;; 

(include "../win32/win32_target.lsp")

(setg acdkmake-target-compile-exe "cl.exe")
(setg acdkmake-target-link-exe "cl.exe")
(setg acdkmake-target-cflags-exe "")
(setg acdkmake-target-ldflags-exe " /SUBSYSTEM:CONSOLE /INCREMENTAL:NO")
(setg acdkmake-target-binext-exe ".exe")
(setg acdkmake-target-ldflags-guiexe " /SUBSYSTEM:windows /INCREMENTAL:NO")
(setg acdkmake-target-binext-guiexe ".exe")


(setg acdkmake-target-compile-dll "cl.exe")
(setg acdkmake-target-link-dll "cl.exe")
(setg acdkmake-target-cflags-dll "")
(setg acdkmake-target-ldflags-dll " /dll /INCREMENTAL:NO ")
(setg acdkmake-target-binext-dll acdkmake-target-dllext)

(setg acdkmake-target-targetscriptnl "\n")

(setg compiletypesuffix "_r")

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


(defun make-convert-text (text)
  (unix2dos text)
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
  ;(setf executable (s+ acdkmake-project-name (acdkmake-get-target-binext)))
  (acdkmake-convert-file-if-needed (s+ acdkmake-project-exec-dir compiletypesuffix)) ;; wrong
  (setf erg (s+ "\t-@erase \"$(OUTDIR)\\" executable "\"\n"))
  (dolist (o acdkmake-project-objects)
    (setf erg (s+  erg "\t-@erase \"" o "\"\n"))
  )
  erg
)

(defun no-acdkmake-get-ldflags ()
  (setf erg (s+ " " (acdkmake-get-target-ldflags) " "))
  (setf erg (s+ erg "/incremental:no"))
  (if (acdkmake-project-is-type "dll")
    (progn 
      (setf erg (s+ erg " /pdb:\"$(OUTDIR)\\" acdkmake-project-name ".pdb\" /debug /machine:I386 /out:\"$(OUTDIR)\\" acdkmake-project-name compiletypesuffix ".dll\" "))
      (setf erg (s+ erg " /implib:\"$(OUTDIR)\\" acdkmake-project-name compiletypesuffix ".lib\ " "/pdbtype:sept"))
    )
    (progn 
      (setf erg (s+ erg " /pdb:\"$(OUTDIR)\\" acdkmake-project-name ".pdb\" /debug /machine:I386 /out:\"$(OUTDIR)\\" acdkmake-project-name compiletypesuffix ".exe\" "))
      (setf erg (s+ erg " /pdbtype:sept"))
    )
  )
  erg
)

(defun acdk-make-target-getimplib ()
  (setf outf (getCanonicalPath (s+ acdkmake-project-exec-dir "/" acdkmake-project-name compiletypesuffix)))
  (s+ "implib -c -w " outf  ".lib " outf (acdkmake-get-target-binext))
)


(if (not (isdef org-acdkmake-get-defines))
  (setg org-acdkmake-get-defines acdkmake-get-defines)
)

(defun acdkmake-get-defines ()
  (setf erg (org-acdkmake-get-defines))
  (if (invoke compiletypesuffix 'equals "_d")
    (setf erg (s+ erg " /D \"WIN32\" /D \"_DEBUG\" /D \"_WINDOWS\" /D \"_MT\" /D \"ACDK_DEBUG\""))
    (setf erg (s+ erg " /D \"WIN32\" /D \"NDEBUG\" /D \"_WINDOWS\" /D \"_MT\""))
  )
  (if (acdkmake-project-is-type "dll")
    (setf erg (s+ erg " /D \"_USRDLL\""))
  )
  erg
)


(defun acdkmake-get-outputdir ()
  (acdkmake-convert-file-if-needed acdkmake-project-exec-dir)
)

(defun acdkmake-get-objdir ()
  (s+ (acdkmake-project-get-object-dir) compiletypesuffix)
)

(defun acdkmake-get-dsp-sl (sl)
  (setf erg "")
  (dolist (f sl)
    (set erg (s+ erg "\n# Begin Source File\nSOURCE=" f "\n# End Source File\n"))
  )
  erg
)

(defun acdkmake-target-get-mergedPath (f s)
  "return (common_path f_dir s_dir)"
  ;(println (s+ "acdkmake-target-get-mergedPath: [" (toCode f) "][" (toCode s) "]"))
  (setf common_path NIL)
  ;(println (s+ "common_path  == NIL: " common_path " und '() " '()))
  (setf f_dir NIL)
  (setf s_dir NIL)
  (if (not f)
    (progn
      (return (append (append (append NIL NIL) NIL) s))
    )
  )
  (setf tf (clone f))
  (setf ts (clone s))
  (setf emptylist NIL)
  (while (and tf ts)
    (progn
      ;(println (s+ "common_path1: [" (toCode common_path) "] tf[" (toCode tf) "] ts[" (toCode ts)))
      (if (and (and (car tf) (car ts) (not (invoke (car tf) 'equals (car ts)))))
        (progn
          ;(println (s+ "not same: [" (car tf) "] and [" (car ts) "]"))
          (setf erg NIL)
          (setf erg (append (append (append erg common_path) tf) ts))
          ;(println (s+ "erg1: " (toCode erg)))
          (return erg)
        )
      )
      (setf common_path (append common_path (car ts)))
      (setf tf (cdr  tf))
      (setf ts (cdr  ts))
    )
  )
  ;(println (s+ "common_path2: [" (toCode common_path) "] tf[" (toCode tf) "] ts[" (toCode ts)))
  ;(break)
  (setf erg NIL)
  (setf erg (append (append (append erg common_path) tf) ts))
  ;(println (s+ "erg1: " (toCode erg)))
  erg
)

(defun acdkmake-get-rel-parent (f)
  (setf idx (invoke f 'lastIndexOf "/"))
  
  (if (= idx -1)
    f
    (invoke f 'substr 0 idx)
  )
)

(defun print-list (liste space)
  (println (s+ "el: " (toCode liste)))
  (dolist el (liste)
    (if (listp el)
      (print-list el (s+ space " "))
      (println (s+ space (toCode el)))
    )
  )
)

(defun acdkmake-get-dsp-sourcefiles ()
  (setf erg "")
  (setf curdir NIL)
  (setf last_subdirlist NIL)
  (setf subdirlist NIL)
  (setf lslist NIL)
  (setf last_sf "")
  (dolist (sl acdkmake-project-sourcelist) 
    ;(println (s+ "sourcelist: " sl))
    (setf is_source_dir Nil)
    (setf orgls sl)
    (setf fqsl (acdkmake-convert-file-if-needed (s+ acdk-project-dir (slash) sl)))
    ;(println (s+ "isfile " fqsl ": " (isFile fqsl)))
    (if (isFile fqsl)
      (setf sl (acdkmake-get-rel-parent sl))
      (setf is_source_dir t)
    )
    
    (setf sl (invoke sl 'replace (slash) "/"))
    
    (setf subdirlist (split sl "/" t))
    ;(println (s+ "subdirlist: " (toCode subdirlist)))
    
    ;; acdkmake-target-get-mergedPath can only handle path correctly,
    ;; therefore don't try to create folder for files.
    
    (setf is_source_without_dir NIL)
    ;(println (s+ orgls " idx of " (indexof orgls "/")))
    (if (and (not is_source_dir) (= (indexof orgls "/") -1))
      (setf is_source_without_dir T)
    )
    (if is_source_without_dir
      (setf lslist '(NIL NIL NIL))
      (setf lslist (acdkmake-target-get-mergedPath last_subdirlist subdirlist))
    )
    ;(println "ListList1:")
    
    (setf common_path (car lslist))
    (setf f_dir (cadr lslist))
    (setf s_dir (caddr lslist))
    #|
    (println (s+ 
        "common_path=[" (toCode common_path) 
        "] f_dir=[" (toCode f_dir) "] s_dir=[" (toCode s_dir) 
        "] lslist=[" (toCode lslist) "]"
        ))
     |#
    (if f_dir (progn
      (setf downpath f_dir)
      (dolist (thp downpath)
        ;(println (s+ "thp: " thp))
        (if thp (progn
          (setf erg (s+ erg "# end Group\n"))
          ;(println (s+ "downpath1: " thp))
        ))
      )
    ))
    (setf uppapath (car (cddr lslist)))
    ;(println (s+ "uppapath: " uppapath))
    (dolist (dir uppapath)
      ;(println (s+ "uppath: " dir))
      ;(if is_source_dir (progn
        (setf curdir (append curdir dir))
        (setf erg (s+ erg "# Begin Group \"" dir "\"\n"))
        (setf erg (s+ erg "# PROP Default_Filter \"\"\n"))
      ;))
    )
    (setf erg (s+ erg (acdkmake-get-dsp-sl (collectHeader (append NIL orgls)))))
    (setf erg (s+ erg (acdkmake-get-dsp-sl (collectImplementations (append NIL orgls)))))
    (setf last_subdirlist subdirlist)
    (setf last_sf sl)
    (setf last_is_source_without_dir is_source_without_dir)
  )
  ;(println (s+ "ListList2: " (toCode lslist)))
  ;(setf is_source_dir t) ;### hack for acdk_text: may not work in other environment
  (setf downpath (join2list (car lslist) (caddr lslist)))
  ;(println (s+ "downpath2: " (toCode downpath)))
  (while (not (empty downpath))
    (progn
      (setf thp (pop downpath))
      (if thp
        (progn 
          ;(if is_source_dir
            (setf erg (s+ erg "# end Group\n"))
          ;)
        )
      )
    )
  )
  erg
)

(defun acdkmake-get-dsp-sourcefiles_old ()
  (s+ (acdkmake-get-dsp-sl acdkmake-project-header) (acdkmake-get-dsp-sl acdkmake-project-sources))
)

(defun acdkmake-get-project-name ()
  acdkmake-project-name
)


(setg acdkmake-get-target-cflags-org acdkmake-get-target-cflags)



(defun acdkmake-get-target-cflags ()
  (s+ (acdkmake-get-defines) (acdkmake-get-includes) (acdkmake-get-target-cflags-org))
)

(setg acdkmake-get-target-ldflags-org acdkmake-get-target-ldflags)

(defun acdkmake-get-project-ldflags ()
  (setf erg "")
  (dolist (ldf acdkmake-project-ldflags)
    (setf key (car ldf))
    (setf val (cadr ldf))
    ;(println (s+ "key: [" key "] val[" val "]"))
    (if (streql key "-L")
      (setf key " /libpath:")
    )
    (setf erg (s+ erg key "\"" val "\""))
  )
  (if (not (isdef compiletypesuffix))
    (setg compiletypesuffix "_r")
  )
  (if (not (invoke compiletypesuffix 'equals "_r"))
    (if (isdef acdkmake-project-ldflags2-debug)
      (setf erg (s+ erg " " acdkmake-project-ldflags2-debug))
    )
    (if (isdef acdkmake-project-ldflags2-release)
      (setf erg (s+ erg " " acdkmake-project-ldflags2-release))
    )
  )
  erg
)


(defun acdkmake-get-target-ldflags ()
  (setf erg "kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib")
  (setf erg (s+ erg (acdkmake-get-libs)))
  (if (not (invoke compiletypesuffix 'equals "_r"))
    (setf erg (s+ erg " /nologo /debug /pdbtype:sept /machine:I386 "))
    (setf erg (s+ erg " /nologo /debug /debugtype:coff /machine:I386 "))
  )
  (if (or (invoke acdkmake-project-type 'equals "exe") (invoke acdkmake-project-type 'equals "guiexe"))
    NIL
    (setf erg (s+ erg " /dll "))
  )
  (setf erg (s+ erg " /out:\"" (acdkmake-get-outputdir) (slash) (acdkmake-get-project-name) compiletypesuffix (acdkmake-get-target-binext) "\""))
  (setf erg (s+ erg " " (acdkmake-get-project-ldflags)))
  erg
)



(setf org-acdkmake-get-libs acdkmake-get-libs)

(defun acdkmake-get-libs ()
  (if (or (not (isdef acdkmake-project-acdklibs)) (= (length acdkmake-project-acdklibs) 0))
    (return "")
  )
  (setf erg "")
  
  (dolist (l acdkmake-project-acdklibs)
    (if (isdef l)
      (setf erg (s+ erg " " (acdkmake-convert-file-if-needed (s+ acdk-home (slash) "bin" (slash) l compiletypesuffix acdkmake-target-libext))))
    )
  )
  ;;(println (s+ "acdk-make-get-libs: " (toCode acdkmake-project-libs-debug)))
  (setf plibs '())
  (if (not (invoke compiletypesuffix 'equals "_r"))
    (progn
      (if (isdef acdkmake-project-libs-debug)
        (setf plibs acdkmake-project-libs-debug)
        (setf plibs acdkmake-project-libs)
      )
    )
    (progn
      (if (isdef acdkmake-project-libs-release)
        (setf plibs acdkmake-project-libs-release)
        (setf plibs acdkmake-project-libs)
      )
    )  
  )
  ;;(println (s+ "acdk-make-get-libs: " compiletypesuffix ", " (toCode plibs)))
  (dolist (l plibs)
    (setf l (eval-if-needed l))
    (if (isdef l)
      (progn 
        (setf l (s+ l ".lib"))
        (setf erg (s+ erg " " (acdkmake-convert-file-if-needed l)))
      )
    )
  )
  erg
)

(defun acdkmake-target-get-TARGTYPE ()
  (if (invoke acdkmake-project-type 'equals "exe")
    "\"Win32 (x86) Console Application\" 0x0103"
    (if (invoke acdkmake-project-type 'equals "guiexe")
      "\"Win32 (x86) Application\" 0x0101"
      "\"Win32 (x86) Dynamic-Link Library\" 0x0102"
    )
  )
)


(defun acdkmake-target-get-dsp-project-name (type)
  (setf erg (s+ "\"" (acdkmake-get-project-name)))
  (if (invoke acdkmake-project-type 'equals "exe")
    (setf erg (s+ erg " - Win32 " type "\" (basierend auf  \"Win32 (x86) Console Application\""))
    (if (invoke acdkmake-project-type 'equals "guiexe")
      (setf erg (s+ erg " - Win32 " type "\" (basierend auf  \"Win32 (x86) Application\""))
      (setf erg (s+ erg " - Win32 " type "\" (basierend auf  \"Win32 (x86) Dynamic-Link Library\""))
    )
  )
  erg
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
(defun acdkmake-target-get-pmakesuffix (acdkmake-default-targetsuffux)
  "dsw"
)

(defun acdk-pmake-generate-dsw ()
  (set erg "")
  (dolist (ps acdk-projects)
    (set name (cadr ps))
    (set path (car ps))
    (set deps (car (cdr (cdr ps))))
    (set erg 
      (s+ erg "\n"
        "###############################################################################\n"
        "Project: \"" name "\"=" path (slash) name ".dsp - Package Owner=<4>\n"
        "\n"
        "Package=<5>\n"
        "{{{\n"
        "}}}\n"
        "\n"
        "Package=<4>\n"
        "{{{\n"
        
      )
     )
     ;(println (s+ "deps: " (toCode deps)))
     (dolist (d deps)
        (set erg (s+ erg "    Begin Project Dependency\n"))
        (set erg (s+ erg "    Project_Dep_Name " d "\n"))
        (set erg (s+ erg "    End Project Dependency\n"))
     )
     (set erg (s+ erg
      "}}}\n"
      "\n"
    ))
  )
  (generate-wrapper-nmake)
  erg
)

(defun generate-wrapper-nmake ()
  
  (setf erg (acdkmake-load-eval-template (s+ acdk-cfg-home "/targets/" acdkmake-target "/pnmake.template")))
  (setf makef (s+ acdkmake-project-name ".nmake"))
  (writeStringToFile makef erg)
  (println (s+ "\nwrote file: [" makef "]"))
    
  ;(setf erg (acdkmake-load-eval-template (s+ acdk-cfg-home "/targets/" acdkmake-target "/pnmake7.template")))
  ;(setf makef (s+ acdkmake-project-name ".nmake7"))
  ;(writeFile makef erg)
  ;(println (s+ "\nwrote file: [" makef "]"))
)


(defun acdk-pmake-generate-tests ()
  (set erg "test::\n")
  (set testext "_d")
  (if (and (> (length acdkmake-project-tests) 0) (car acdkmake-project-tests))
      (progn 
        
        (dolist (_tt acdkmake-project-tests) 
          (set erg (s+ erg "\t$(ACDKHOME)\\bin\\" _tt testext " $(AUNIT_TESTOPTS)\n"))
          )
        )
     (set erg (s+ erg "\techo no test cases defined\n"))
   )
  erg
)
