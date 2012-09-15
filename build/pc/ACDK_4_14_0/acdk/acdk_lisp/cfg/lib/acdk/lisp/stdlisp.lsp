;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: stdlisp.lsp,v 1.1.1.1 2000/12/11 18:05:37 kommer Exp $


(defun cadr (x) 
  (car (cdr x))
)

(defun caddr (x) 
  (car (cdr (cdr x)))
)

(defun cddr (x) 
  (cdr (cdr x))
)

(defun atomp (x)
  (not (consp x))
)

(defun consp (x)
  (typep x 'cons)
)


(defun != (a1 a2)
  (not (= a1 a2))
)

(defun < (a1 a2)
  (not (or (> a1 a2) (= a1 a2)))

)
  
(defun >= (a1 a2)
  (or (= a1 a2) (> a1 a2))
)

(defun <= (a1 a2)
  (or (= a1 a2) (not (> a1 a2)))
)

(defun -- (n) 
  (- n 1)
)

(defun symbolp (s)
  (instanceof s 'acdk.lisp.LispSymbol)
)

(defun listp (s)
  (or (= s NIL) (instanceof s 'acdk.lisp.LispList))
)

(defun clone (s)
  (if (not s)
    (return NIL)
  )
  (unpack (invoke s 'clone))
)
