;;;
;;; This file is part of ACDK
;;; Copyrighted (c) 2000 by Roger Rene Kommer - artefaktur
;;; 
;;; $Header: /cvsroot/acdk/acdk/acdk_net/acdk_net.lsp,v 1.24 2005/04/25 13:43:20 kommer Exp $
;;;

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
  (setg acdkmake-project-name "acdk_net")
  ;; TODO the version of acdk all packages in one distribution should be compatible
  (setg acdk-project-distribution-version "4.13") 

  ;; TODO the version of the current package
  (setg acdk-project-package-version "4.13.0")

  ;; TODO which platforms are supported in this package
  ;; default acdk-project-platforms
  ;(setg acdk-project-platforms
  ;  '("dsp" "linux" "sunos-gcc" "bsd")
  ;)
  
  ;; TODO list of directory/subprojectsname/dependencies
  (setg acdk-projects
    '(
        ("src" "acdk_net" () ("lib" "dist"))
        ("src" "acdk_net_metainf" ("acdk_net") ("lib" "dist" "meta"))
        
        ("src" "acdk_net_ftp" ("acdk_net") ("lib"))
        
        ; not yet ("src" "acdkx_net_ssl" ("acdk_net") ("lib"))
        
        ("src" "acdk_net_srfsys" ("acdk_net") ("lib"))
        ("src" "acdk_net_srsync" ("acdk_net" "acdk_net_srfsys") ("bin"))
        
        ("tests/acdk/net" "acdk_net_Test" ("acdk_net") ("bin" "utest"))
        ("tests/acdk/net/ftp" "acdk_net_ftp_Test" ("acdk_net" "acdk_net_ftp") ("bin" "utest"))
        
        ("tests/acdk/net" "acdk_net_EchoClient_Test" ("acdk_net") ("bin" "utest"))
        ("tests/acdk/net" "acdk_net_EchoServer_Test" ("acdk_net") ("bin" "utest"))
        ("tests/acdk/net/srfsys" "acdk_net_srfsys_Test" ("acdk_net" "acdk_net_srfsys") ("bin" "utest"))
        
        ("src" "acdkx_net_ssl" ("acdk_net") ("lib" "optional"))
        ("src" "acdkx_net_ssl_metainf" ("acdk_net" "acdk_net") ("lib" "dist" "optional" "meta"))
        
        ("tests/acdkx/net/ssl" "acdkx_net_ssl_Test" ("acdk_net" "acdkx_net_ssl") ("bin" "utest" "optional"))
    )
  )
  (setg acdk-tests 
    '(
        "acdk_net_Test"
        "acdk_net_ftp_Test"
        "acdk_net_srfsys_Test"
        "acdkx_net_ssl_Test"
     )
  )
  (acdkmake-project-main)
)

(make-project-main)
