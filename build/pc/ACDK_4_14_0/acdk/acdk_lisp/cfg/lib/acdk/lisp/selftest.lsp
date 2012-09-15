;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2002 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: selftest.lsp,v 1.1 2001/12/19 21:19:47 kommer Exp $

(include "autoload.lsp")

(defun testAssert(cond)
  (println (s+ "evaluate: " (toCode cond)))
  (if (not (eval cond))
    (throw 'acdk.lang.RuntimeException (s+ "test failed" (toCode cond)))
  )
)

(defun testNumber()
  (testAssert '(= (+ 1 2) 3))
  (testAssert '(= (- 1 2) -1))
  (testAssert '(= (+ 0.25 0.75) 1))
  (testAssert '(= (- 0.75 0.25) 0.5))
  (testAssert '(= (/ 20 5) 4))
  (testAssert '(= (* 20 5) 100))
)

(defun testString()
  (testAssert '(invoke (s+ "Hallo " "ACDK") 'equals "Hallo ACDK"))
)

;; More tests here
;; ....

(testNumber)
(testString)

1
