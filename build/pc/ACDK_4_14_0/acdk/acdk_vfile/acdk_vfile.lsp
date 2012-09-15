;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_vfile/acdk_vfile.lsp,v 1.10 2004/06/17 16:06:46 kommer Exp $
;;;

;;; generate Meta-Makefiles 
;;; usage:
;;; ../bin/acdklisp -acdk-home=.. <thisfile>


;; don't edit this function
(defun acdk-project-main-init ()
 (if (zerop ACDKHOME)
    (progn
      (println (s+ "Error: ACDKHOME is not defined!"))
      (println (s+ "Set Environment variable or\n\t use -acdk-home=<ACDKHOME> as command line option"))
      (println (s+ "In common ACDKHOME should be relativ to the current directory: '..'"))
      (exit 1)
    )
  )
 (setg acdk-home ACDKHOME)
 (setg acdk-cfg-home (s+ acdk-home "/cfg"))
 (include (s+ acdk-home "/cfg/lib/acdk/lisp/acdkprojectmake.lsp"))
)


(defun make-project-main ()
  ;; don't edit this
  (acdk-project-main-init) ; just initialization
  ;; TODO Name of the package
  (setg acdkmake-project-name "acdk_vfile")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; TODO which platforms are supported in this package
  ;; is default acdk-project-platforms

  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdk_vfile" () ("lib" "dist"))
        ("src" "acdk_vfile_file2rc" () ("bin" "tool" "dist"))
        ("src" "acdk_vfile_convert" ("acdk_vfile") ("bin" "tool" "dist"))
		    ("tests/acdk/vfile" "acdk_vfile_Test" ("acdk_vfile") ("bin" "utest"))
		    ("tests/acdk/vfile/tar" "acdk_vfile_tar_Test" ("acdk_vfile") ("bin" "utest"))
		    ("tests/acdk/vfile/zip" "acdk_vfile_zip_Test" ("acdk_vfile") ("bin" "utest"))
		
     )
   )
    (setg acdk-tests 
    '(
		"acdk_vfile_Test"
		"acdk_vfile_tar_Test"
		"acdk_vfile_zip_Test"
     )
  )
  (acdkmake-project-main)
)

(make-project-main)
