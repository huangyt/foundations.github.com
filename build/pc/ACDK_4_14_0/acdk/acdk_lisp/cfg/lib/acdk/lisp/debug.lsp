;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: debug.lsp,v 1.1.1.1 2000/12/11 18:05:35 kommer Exp $

(defun break ()
  (invoke env 'setBreak 1)
)

(defun toCode (s)
  (if s
    (invoke s 'toCode)
    "NIL"
  )
)

  
