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
  (if (or (streql label "win32") (streql label "cbx"))
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
    (s+ label "=" setdefine)
    (s+ label)
  )
)

(defun acdkmake-get-defines ()
  (setf erg "")
  (if (isdef acdkmake-target-defines)
    (dolist (dl acdkmake-target-defines)
      (setf s (car dl))
      (if (and (cdr dl) (cadr dl))
        (setf erg (s+ erg ";" (acdkmake-target-make-define (car dl) (cadr dl))))
        (setf erg (s+ erg ";" (acdkmake-target-make-define (car dl))))
      )
    )
  )
  (if (isdef acdkmake-project-defines)
    (dolist (dl acdkmake-project-defines)
      (setf s (car dl))
      (if (and (cdr dl) (cadr dl))
        (setf erg (s+ erg ";" (acdkmake-target-make-define (car dl) (cadr dl))))
        (setf erg (s+ erg ";" (acdkmake-target-make-define (car dl))))
      )
    )
  )
  erg
)


(defun make-convert-text (text)
  (unix2dos text)
)

(defun normalize-fn (s)
  (s 'replace "\\" "/")
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

(defun acdkmake-get-defines (release)
  (setf erg (org-acdkmake-get-defines))
  (if (invoke release 'equals "debug")
    (setf erg (s+ erg ";WIN32;_DEBUG;_WINDOWS;_MT;ACDK_DEBUG"))
    (setf erg (s+ erg ";WIN32;NDEBUG;_WINDOWS;_MT"))
  )
  (if (acdkmake-project-is-type "dll")
    (setf erg (s+ erg ";_USRDLL"))
  )
  erg
)

(defun make-define (defn categorie counter)
	(s+ "<property category=\"" categorie "\" name=\"option.D_MACRO_VALUE.arg."
			(invoke-static 'acdk.lang.Integer 'toString counter) "\" value=\"" defn "\"/>\n")
)

(defun acdkmake-get-gcc-defines (categorie counter)
  (setf erg "")
  (if (isdef acdkmake-target-defines)
    (dolist (dl acdkmake-target-defines)
      (setf s (car dl))
      (if (and (cdr dl) (cadr dl))
      	(setf erg (s+ erg (make-define (s+ (car dl) "=" (cadr dl)) categorie counter)))
      	(setf erg (s+ erg (make-define (s+ (car dl)) categorie counter)))
      	
        ;(setf erg (s+ erg " " (acdkmake-target-make-define (car dl) (cadr dl) counter)))
        ;(setf erg (s+ erg " " (acdkmake-target-make-define (car dl) counter)))
      )
      (setf counter (+ counter 1))
    )
  )
  (if (isdef acdkmake-project-defines)
    (dolist (dl acdkmake-project-defines)
      (setf s (car dl))
      (if (and (cdr dl) (cadr dl))
        (setf erg (s+ erg (make-define (s+ (car dl) "=" (cadr dl)) categorie counter)))
      	(setf erg (s+ erg (make-define (s+ (car dl)) categorie counter)))
      )
      (setf counter (+ counter 1))
    )
  )
  erg
)


(defun acdkmake-get-outputdir ()
  (acdkmake-convert-file-if-needed acdkmake-project-exec-dir)
)

(defun acdkmake-get-objdir ()
  (s+ (acdkmake-project-get-object-dir) compiletypesuffix)
)

(defun get-target-binext (platform)
	(setf x (acdkmake-get-target-binext))
	(if (platform 'equals "unix")
		(if (x 'equals ".exe")
			"" 
			".so"
		)
		x
	)
)

(defun get-target-binsuffix (platform)
	(if (platform 'equals "unix")
		(progn
			(setf x (acdkmake-get-target-binext))
		  (if (x 'equals ".exe")
		  	""
		  	(return "lib")
		  )
		)
	)
	""
)

(defun acdkmake-get-exec-outputname (compiletypesuffix platform)
	(normalize-fn (s+ (acdkmake-get-outputdir) (slash) (get-target-binsuffix platform) (acdkmake-get-project-name) (get-target-binext platform)))
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
	(setf f (normalize-fn f))
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




(defun acdkmake-get-cbxo-sourcefiles ()
  (setf erg "")
  (setf curdir NIL)
  (setf last_subdirlist NIL)
  (setf subdirlist NIL)
  (setf lslist NIL)
  (setf last_sf "")
  (setf sourcecounter 1)
  
  (dolist (sl acdkmake-project-header) 
  	(setf sl (normalize-fn sl))
  	(setf erg (s+ erg "  <file path=\"" sl "\">\n    <property category=\"unique\" name=\"id\" value=\"" 
  		(invoke-static 'acdk.lang.Integer 'toString sourcecounter) "\"/>\n  </file>\n"))
  	(setf sourcecounter (+ sourcecounter 1))
  )
  (dolist (sl acdkmake-project-sources) 
  	(setf sl (normalize-fn sl))
  	(setf erg (s+ erg "  <file path=\"" sl "\">\n    <property category=\"unique\" name=\"id\" value=\"" 
  		(invoke-static 'acdk.lang.Integer 'toString sourcecounter) "\"/>\n  </file>\n"))
  	(setf sourcecounter (+ sourcecounter 1))
  )
  erg
)

(defun acdkmake-get-rel-parent (f)
  (setf idx (invoke f 'lastIndexOf "/"))
  
  (if (= idx -1)
    f
    (invoke f 'substr 0 idx)
  )
)

(setg idcounter 1000)
(defun acdkmake-cbx-getuniq ()
	(setf erg (s+ "  <property category=\"unique\" name=\"id\" value=\"" 
  		(invoke-static 'acdk.lang.Integer 'toString idcounter) "\"/>\n"))
  (setg idcounter (+ idcounter 1))
  erg
)

(defun acdkmake-cbx2-sl (sl)
  (setf erg "")
  (dolist (f sl)
  	(setf f (normalize-fn f))
  	(setf erg (s+ erg "  <file path=\"" f "\">\n" (acdkmake-cbx-getuniq) "  </file>\n"))
  	;;(set erg (s+ erg "\n# Begin Source File\nSOURCE=" f "\n# End Source File\n"))
  )
  erg
)



(defun acdkmake-get-cbx-sourcefiles ()
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
      (if (isDirectory fqsl)
      	(setf is_source_dir t)
      	(println (s+ "ERROR: Is not file or directory: " fqsl))
     	)
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
        ;;(println (s+ "thp: " thp))
        (if thp (progn
          (setf erg (s+ erg "  </node>\n"))
          ;;(setf erg (s+ erg "# end Group\n"))
          ;;(println (s+ "downpath1: " thp))
        ))
      )
    ))
    (setf uppapath (car (cddr lslist)))
    ;(println (s+ "uppapath: " uppapath))
    (dolist (dir uppapath)
      ;;(println (s+ "uppath: " dir))
      ;(if is_source_dir (progn
        (setf curdir (append curdir dir))
        (setf erg (s+ erg "  <node name=\"" dir "\" type=\"Folder\">\n"))
        (setf erg (s+ erg (acdkmake-cbx-getuniq)))
        ;;(setf erg (s+ erg "# Begin Group \"" dir "\"\n"))
        ;;(setf erg (s+ erg "# PROP Default_Filter \"\"\n"))
      ;))
    )
    (setf erg (s+ erg (acdkmake-cbx2-sl (collectHeader (append NIL orgls)))))
    (setf erg (s+ erg (acdkmake-cbx2-sl (collectImplementations (append NIL orgls)))))
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
            ;;(setf erg (s+ erg "# end Group\n"))
            (setf erg (s+ erg "  </node>\n"))
            ;;(println (s+ "downpath2: " thp))
          ;)
        )
      )
    )
  )
  erg
)


(defun acdkmake-get-project-name ()
  acdkmake-project-name
)


(setg acdkmake-get-target-cflags-org acdkmake-get-target-cflags)



(defun acdkmake-get-target-cflags ()
  (s+ (acdkmake-get-defines) (acdkmake-get-includes) (acdkmake-get-target-cflags-org))
)

(defun acdkmake-get-includes (catalog startnum)
  (setf erg "")
  
  (if (not (zerop acdkmake-project-includes))
    (progn 
      (dolist (f acdkmake-project-includes)
      	(setf erg (s+ erg "  <property category=\"" catalog "\" name=\"option.I.arg." 
      		(invoke-static 'acdk.lang.Integer 'toString startnum) "\" value=\"" f "\"/>\n"))
      		
      	;; <property category="win32.Debug_Build.win32b.bcc32" name="option.I.arg.3" value="..\include"/>
        ;; (setf erg (s+ erg " " (acdkmake-target-make-include f)))
        (setf startnum (+ startnum 1))
      )
    )
  )
  (setf f (s+ ACDKHOME "/include"))
  (setf erg (s+ erg "  <property category=\"" catalog "\" name=\"option.I.arg." 
      		(invoke-static 'acdk.lang.Integer 'toString startnum) "\" value=\"" f "\"/>\n"))
  ;;(setf erg (s+ erg " " (acdkmake-target-make-include (s+ ACDKHOME "/include"))))
  erg
)


(defun acdkmake-get-cbx-libs (catalog startnum)
  (println (s+ "acdkmake-project-acdklibs: " (toCode acdkmake-project-acdklibs)))
  (setf erg "")
  (if (isdef acdkmake-project-acdklibs)
    (progn
      (dolist (l acdkmake-project-acdklibs)
        (if (isdef l)
        (progn
          (setf libname (acdkmake-convert-file-if-needed (s+ acdk-home (slash) "bin" (slash) l acdkmake-target-libext)))
          (setf erg (s+ erg " <property category=\"" catalog "\" name=\"param.libfiles." 
          		(invoke-static 'acdk.lang.Integer 'toString startnum) "\" value=\"" libname "\"/>\n"))
          ;;(setf erg (s+ erg " " ))
          (setf startnum (+ startnum 1))
         )
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
          	(setf erg (s+ erg " <property category=\"" catalog "\" name=\"param.libfiles." 
          		(invoke-static 'acdk.lang.Integer 'toString startnum) "\" value=\"" l "\"/>\n"))
            ;(if (l 'startsWith "$")
            ;  (setf erg (s+ erg l " "))
            ;  (setf erg (s+ erg "-l" l " "))
            ;)
            (setf startnum (+ startnum 1))
          )
        )
      ) 
    )
  )
  erg
)


(defun acdkmake-get-gcc-libs (catalog startnum)
  (setf erg "")
  (if (isdef acdkmake-project-acdklibs)
    (progn
      (dolist (l acdkmake-project-acdklibs)
        (if (isdef l)
        (progn
          (setf libname l)
          (setf erg (s+ erg " <property category=\"" catalog "\" name=\"option.l.arg." 
          		(invoke-static 'acdk.lang.Integer 'toString startnum) "\" value=\"" libname "\"/>\n"))
          (setf startnum (+ startnum 1))
         )
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
          	(setf erg (s+ erg " <property category=\"" catalog "\" name=\"option.l.arg." 
          		(invoke-static 'acdk.lang.Integer 'toString startnum) "\" value=\"" l "\"/>\n"))
            (setf startnum (+ startnum 1))
          )
        )
      ) 
    )
  )
  erg
)

(defun acdkmake-get-gcc-libpath (catalog startnum)
	(setf lp (s+ acdk-home (slash) "bin"))
	(setf lp (normalize-fn lp))
	(setf erg "")
	(setf erg (s+ erg " <property category=\"" catalog "\" name=\"option.L.arg." 
          		(invoke-static 'acdk.lang.Integer 'toString startnum) "\" value=\"" lp "\"/>\n"))
  (setf startnum (+ startnum 1))
  
  (dolist (ldf acdkmake-project-ldflags)
    (setf key (car ldf))
    (setf val (cadr ldf))
    ;(println (s+ "key: [" key "] val[" val "]"))
    (if (streql key "-L")
      (progn
      	(setf erg (s+ erg " <property category=\"" catalog "\" name=\"option.L.arg." 
          		(invoke-static 'acdk.lang.Integer 'toString startnum) "\" value=\"" val "\"/>\n"))
        (setf startnum (+ startnum 1))
      )
      ;;(setf key " /libpath:")
    )
    ;;(setf erg (s+ erg key "\"" val "\""))
  )
  erg
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
  "bpgr"
)

(defun acdk-pmake-generate-bpgr ()
	(set erg "")
	(dolist (ps acdk-projects)
    (set name (cadr ps))
    (set path (car ps))
    (set deps (car (cdr (cdr ps))))
    (setf erg (s+ erg "  <project path=\"" path "/" name ".cbx\"/>\n"))
	)
	;; (generate-wrapper-nmake)
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
