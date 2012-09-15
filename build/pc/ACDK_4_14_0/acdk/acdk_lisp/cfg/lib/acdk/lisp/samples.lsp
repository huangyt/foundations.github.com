;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: samples.lsp,v 1.1.1.1 2000/12/11 18:05:37 kommer Exp $

(setf sb (new 'acdk.lang.StringBuffer "ACDK"))
(invoke sb 'append " Object")
; 
(setf out (peek-static 'acdk.lang.System 'out))
(setf err (peek-static 'acdk.lang.System 'err))
(setf in (peek-static 'acdk.lang.System 'in))

(setf msg "Hallo leute")
(invoke out 'println msg)
(setf hashmap (new 'acdk.util.HashMap 0 0.7))
(invoke hashmap 'put "out" out)
(invoke (invoke hashmap 'get "out") 'println "Via HashMap")

(invoke out 'println (invoke sb 'toString))

(defun getprop (name) 
  (invoke (invoke-static 'acdk.lang.System 'getProperties) 'get name))

(defun new-file (name)
  (new 'acdk.io.File name))
  
 

(defun basename (&rest filenames)
  (let ((erg '()))
    (dolist (f filenames)
          
    )
  )
)

(setf list '("asdf" "asdf"))
(setf s (invoke list 'toString))
    
