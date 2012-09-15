;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_xml/acdk_xml.lsp,v 1.18 2004/06/17 16:05:59 kommer Exp $
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
  (setg acdkmake-project-name "acdk_xml")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; TODO which platforms are supported in this package
  ;; is default acdk-project-platforms

  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "org_xml" () ("lib"))
        ("src" "org_xml_metainf" ("org_xml") ("lib" "meta"))
        ("src" "acdk_xml" ("org_xml") ("lib" ))
        ("src" "acdk_xml_metainf" ("org_xml" "acdk_xml") ("lib" "meta"))
        
        ("tests/acdk/xml" "acdk_xml_XMLObjectReaderWriter_Test" ("org_xml" "acdk_xml") ("bin" "otest"))
		    ("tests/acdk/xml/sax" "acdk_xml_sax_Test" ("org_xml" "acdk_xml") ("bin" "utest"))
		    ("tests/acdk/xml/dom" "acdk_xml_dom_Test" ("org_xml" "acdk_xml") ("bin" "utest"))
		
    )
  )
    (setg acdk-tests 
    '(
      "acdk_xml_XMLObjectReaderWriter_Test"
	    "acdk_xml_sax_Test"
	    "acdk_xml_dom_Test"
     )
  )
  (acdkmake-project-main)
)

(make-project-main)
