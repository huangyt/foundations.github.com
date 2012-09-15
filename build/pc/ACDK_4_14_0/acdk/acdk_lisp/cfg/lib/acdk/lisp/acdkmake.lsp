;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: acdkmake.lsp,v 1.15 2005/03/31 21:07:05 kommer Exp $

(include "Exceptions.lsp")

(setg acdkmake-xcompile-enabled NIL)

(defun acdkmake-target-binext ()
  (if (invoke acdkmake-project-type 'endsWith "exe")
    acdkmake-target-exeext
    acdkmake-target-soext
  )
)

(defun acdkmake-get-target-spec (spec)
  
  (setf x (s+ "acdkmake-target-" spec "-" acdkmake-project-type))
  (setf erg "")
  (if (invoke env 'lookupVar x NIL)
    (progn
      ;(println (s+ "varT: " x " = " (invoke env 'lookupVar x)))
      (setf erg (invoke env 'lookupVar x))
      (return erg)
    )
  )
  (setf x (s+ "acdkmake-target-" spec))
  ;(println x)
  (if (invoke env 'lookupVar x NIL)
    (progn
      ;(println (s+ "varT: " x " = " (invoke env 'lookupVar x)))
      (setf erg (s+ erg (invoke env 'lookupVar x)))
    )
  )
  erg
)

(defun eval-if-needed (s)
  (if (or (symbolp s) (listp s))
    (eval s)
    s
  )
)


(defun acdkmake-get-target-compile ()
  (acdkmake-get-target-spec "compile")  
)

(defun acdkmake-get-target-link ()
  (acdkmake-get-target-spec "link")  
)

(defun acdkmake-get-target-ldflags ()
  (acdkmake-get-target-spec "ldflags")  
)
(defun acdkmake-get-target-cflags ()
  (acdkmake-get-target-spec "cflags")
)

(defun acdkmake-get-target-binext ()
  (acdkmake-get-target-spec "binext")  
)


(defun acdkmake-get-outputdir ()
  (if acdkmake-project-exec-dir
    acdkmake-project-exec-dir
    "bin"
  )
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
            (if (l 'startsWith "$")
              (setf erg (s+ erg l " "))
              (setf erg (s+ erg "-l" l " "))
            )
          )
        )
      ) 
    )
  )
  erg
)

(defun acdkmake-get-locallib (lib)
  (s+ (acdkmake-get-outputdir) (slash) lib acdkmake-target-implibext)
)

(defun stripExtension (fname)
  (if (zerop fname)
    (return NIL)
  )
  (setf idx (invoke fname 'indexOf "."))
  (if (!= idx -1)
    (return (invoke fname 'substr 0 idx))
  )
  fname
)

(defun acdkmake-convert-file-if-needed (fname)
  
  (if (isdef acdkmake-target-make-fqname)
    (set erg (getCanonicalPath fname))
  )
  (if (= (invoke (slash) 'compareTo "\\") 0)
    (set erg (invoke fname 'replace "/" "\\"))
    (set erg (invoke fname 'replace "\\" "/"))
  )
  (if (isdef acdkmake-target-make-quoted-filenames)
    (s+ "\"" erg "\"")
    erg
  )
  ;;(println (s+ "acdkmake-convert-file-if-needed: fname[" fname "] erg[" erg "]"))
) 

(defun genObjectList (srclist objectext)
  "create an List of Object for Make:\nOBJECTS=$[(genObjectList source-list)]$"
  (setf erg NIL)
  (dolist (el srclist NIL)
    (setf erg (append erg (acdkmake-convert-file-if-needed (s+ (stripExtension el) objectext))))
  )
  erg
)

(defun getobjectdir (tobj)
  
  (setf x acdkmake-target)
  (if (x 'equals "dsp_r") 
    (setf x "vc\\release")
    (if (x 'equals "dsp_d")
      (setf x "vc\\debug")
    )
  )
  (setf erg (s+ tobj (slash) acdkmake-project-name (slash) x))
  (println (s+ "objectdir: " erg))
  erg
)

(defun print-full-filename (fname)
  (println (invoke (new 'acdk.io.File fname) 'getCanonicalPath))  
)

(defun collectFileTypes (dirorfilelist pattern withfiles)
  (setf erg NIL)
  (dolist (el dirorfilelist)
    (setf fqel el)
    (if (not (isAbsoluteFile el))
      (setf fqel (getCanonicalPath (s+ acdkmake-project-dir (slash) el)))
      (setf fqel (getCanonicalPath el))
    )
    
    ;(print-full-filename fqel)
    ;(break)
    (if (isFile fqel)
      (progn
        (if withfiles
          (setf erg (append erg el))
        )
      )
      (if (isDirectory fqel)
        (progn
          (dolist (tf (glob fqel pattern))
            (if (not (invoke tf 'startsWith ".")) ;; strip hidden unix files
              (setf erg (append erg (s+ el (slash) tf)))
            )
          )
        )
      )
    )
  )
  erg
)

(defun collectImplementations (dirorfilelist)
  (setf erg (collectFileTypes dirorfilelist "*.cpp" t))
  ;;(setf ce (collectFileTypes dirorfilelist "*.c" t))
  ;;(dolist (el ce)
  ;;  (setf erg (append erg el))
  ;;)
  erg
)

(defun collectHeader (dirorfilelist)
  (collectFileTypes dirorfilelist "*.h" NIL)
)

(defun acdkmake-get-includes ()
  (setf erg "")
  (if (not (zerop acdkmake-project-includes))
    (progn 
      (dolist (f acdkmake-project-includes)
        (setf erg (s+ erg " " (acdkmake-target-make-include f)))
      )
    )
  )
  (setf erg (s+ erg " " (acdkmake-target-make-include (s+ ACDKHOME "/include"))))
  erg
)

(defun acdkmake-get-defines ()
  (setf erg "")
  (if (isdef acdkmake-target-defines)
    (dolist (dl acdkmake-target-defines)
      (setf s (car dl))
      (if (and (cdr dl) (cadr dl))
        (setf erg (s+ erg " " (acdkmake-target-make-define (car dl) (cadr dl))))
        (setf erg (s+ erg " " (acdkmake-target-make-define (car dl))))
      )
    )
  )
  (if (isdef acdkmake-project-defines)
    (dolist (dl acdkmake-project-defines)
      (setf s (car dl))
      (if (and (cdr dl) (cadr dl))
        (setf erg (s+ erg " " (acdkmake-target-make-define (car dl) (cadr dl))))
        (setf erg (s+ erg " " (acdkmake-target-make-define (car dl))))
      )
    )
  )
  erg
)

(defun acdkmake-project-get-object-dir ()
  (if (isdef acdkmake-project-object-dir)
    (acdkmake-convert-file-if-needed acdkmake-project-object-dir) 
    "."
  )
)

(defun objectfromsources (sourcelist objext)
  (setf erg NIL)
  (dolist (el sourcelist)
    (if (or (invoke el 'endsWith ".cpp") (invoke el 'endsWith ".c"))
      (if (or (not (isdef acdkmake-project-object-dir)) (zerop acdkmake-project-object-dir)) 
        (setf erg (append erg (s+ (stripExtension el) objext)))
        (setf erg (append erg (concatpath acdkmake-project-object-dir (s+ (stripExtension (basename el)) objext))))
      )
    )
  )
  erg
)

(defun objectfromsource (source objext)
  (if (not (isdef acdkmake-project-object-dir))
    (s+ (stripExtension source) objext)
    (concatpath acdkmake-project-object-dir (s+ (stripExtension (basename source)) objext))
  )
)

(defun acdkmake-load-sources ()
  (setg acdkmake-project-sources (collectImplementations acdkmake-project-sourcelist))
  (setg acdkmake-project-header (collectHeader acdkmake-project-sourcelist))
  (setg acdkmake-project-objects (objectfromsources acdkmake-project-sources acdkmake-target-objext))
)

(defun acdkmake-getsourcedirs ()
  "returns a list of for this project defined source list"

  (setf erglist NIL)
  (dolist (el acdkmake-project-sourcelist)
    (setf fqel el)
    (if (not (isAbsoluteFile el))
      (setf fqel (getCanonicalPath (s+ acdkmake-project-dir (slash) el)))
    )
    
    (if (isDirectory fqel) 
        (progn
          (print-full-filename fqel)
          (setf erglist (append erglist el))
        )
    )
  )
  erglist
)


(defun acdkmake-load-eval-template (tname)
  (require "acdk.text.Template")
  (setf f (new 'acdk.io.File tname))
  (if (not (invoke f 'exists))
    (die (s+ "Template file " (getCanonicalPath tname) " does not exists"))
  )
  (setf erg (invoke-static 'acdk.lisp.LispTemplateFilter 'filter env f))
  (if (zerop erg)
    (return NIL)
  )
  erg
)

;; overwrite this function, if not unix system
(defun acdkmake-target-get-clean ()
  (setf executable (s+ acdkmake-project-name (acdkmake-get-target-binext)))
  (setf erg (s+ "rm -f " (acdkmake-convert-file-if-needed (s+ (acdkmake-get-outputdir) (slash) executable " $(OBJECTS)"))))
)


(defun acdkmake-project-is-type (type)
  (= (invoke acdkmake-project-type 'compareTo type) 0)
)

(defun acdkmake-target-getfilelist (al)
  (setf erg "")
  (dolist (o al)
    (if erg
      (setf erg (s+ erg "\t" (acdkmake-convert-file-if-needed o) " \\\n"))
      (setf erg (s+ "  " (acdkmake-convert-file-if-needed o) " \\\n"))
    )
  )
  erg
)


(defun acdk-make-load-templates ()
  (if (not (strcmp acdkmake-target "cygwin-shared"))
    (setg acdkmake-project-type "lib")
  )
  (setf t (s+ acdk-cfg-home "/targets/" acdkmake-target "/target.lsp"))
  ;(println (s+ "include: " t))
  (include t)
)

(defun acdk-make-test-args ()
  
  (setg acdkmake-project-template "ACDKCore") ; default 
  
  (setf args (getargs))
  (println (s+ "args are: " (toCode args)))
  (setf _target "")
  (setg acdk-project-dir ".")
 
  (while (car args)
    (progn
      (setf curarg (car args))

      (if (streql  curarg "-pdir")
        (progn 
          (setf args (cdr args))
          (setg acdk-project-dir (car args))
        )
        (progn
          (setf _target curarg)
        )
      )
      (setf args (cdr args))
    )
  )
  (if (streql _target "")
    (progn
      (println "target must be specified")
      (return Nil)
    )
  )
  (setg acdkmake-target _target)
  (setf uppath "")
  (if (not (streql  acdk-project-dir "."))
    (progn 
      (setf subdircount (invoke acdk-project-dir 'elementCount "/"))
      (setf uppath "..")
      (while (> subdircount 0) 
        (progn
          (setf uppath (s+ uppath "/.."))
          (setf subdircount (- subdircount 1))
        )
      )
      ;(println (s+ "old acdk-home" acdk-home " ACDKHOME " ACDKHOME))
      (setg ACDKHOME (s+ ACDKHOME "/" uppath))
      (setg acdk-home ACDKHOME)
      ;(setg acdk-home (s+ acdk-home "/" uppath))
      
      
    )
  )
  (println (s+ "make with target: " acdkmake-target " in " acdk-project-dir " acdk-home: " acdk-home))
  (acdk-make-load-templates)
  (return t)
)


(defun acdk-make-test-args2 ()
  (setf args (getargs))
  (if (zerop (caddr args))
    (progn
      (println "target must be specified")
      (return Nil)
    )
  )
  (setg acdkmake-target (caddr args))
  (set ta (split acdkmake-target "-"))
  
)

(defun acdkmake-project-get-acdk-home ()
  ACDKHOME
)

(defun acdkmake-target-get-makesuffix (acdkmake-default-targetsuffux)
  acdkmake-default-targetsuffux
)

(defun acdkmake-main ()
  ;(acdk-make-load-templates)
  
  (setf t (s+ acdk-cfg-home "/projects/" acdkmake-project-template "/Template.lsp"))
  (include t)
  
  (setg acdkmake-project-dir (s+ acdkmake-project-dir-root (slash) acdk-project-dir))
  
  (acdkmake-load-sources)
  (setf erg (acdkmake-load-eval-template (s+ acdk-cfg-home "/targets/" acdkmake-target "/Makefile.template")))
  (setf makef (s+  acdkmake-project-dir (slash) acdkmake-project-name "." (acdkmake-target-get-makesuffix acdkmake-target)))
  
  (writeStringToFile makef (make-convert-text erg))
  (println (s+ "\nwrote file: [" makef "]"))
)

