;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: Exceptions.lsp,v 1.3 2003/06/19 13:17:25 kommer Exp $


(defun die (str)
  (println (s+ "DIE: " str))
  (throw 'acdk.lang.Exception str)
)

(defun t2 ()
  (throw 'acdk.lang.RuntimeException "Just an nested Test")
)

(defun t1 ()
  (try 
    (
      (t2)
    )
    (catch (acdk.lang.Error ex)
      (println "In T1")
      (print (invoke ex 'getMessage))
    )
  )
)

(defun t3 ()
  (try 
    (
      (t1)
    )
    (catch (acdk.lang.RuntimeException ex)
      (println "In T")
      (print (invoke ex 'getMessage))
    )
  )
)
