;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: utils.lsp,v 1.2 2002/06/16 13:01:27 kommer Exp $


(defun require (classsymbol) ; com.oracle.odbcdriver
  (try 
    (
      (setf classloader (new 'acdk.lang.ClassLoader))
      (invoke classloader 'findClass classsymbol)
    )
    (catch (acdk.lang.Throwable ex)
      (print (invoke ex 'getMessage))
    )
  )
) 

(defun grep (list pattern)
  (setf rx (new 'acdk.text.RegExp))
  
    
)


(defun isNil(var)
  (eql var Nil)
)

(defun sleep (msecs)
  (invoke-static 'acdk.lang.Thread 'sleep msecs)
)

(defun getenv (name)
  (invoke-static 'acdk.lang.System 'getProperty name)
)

(defun chkvar-warn (name)
  (if (not (isdef name))
    (progn
      (println (s+ "WARN: " name "is not defined."))
      (return false)
    )
  )
  true
)

(defun chkvar-err (name)
  (if (not (isdef name))
    (progn
      (println (s+ "ERR: " name "is not defined."))
      (return false)
    )
  )
  true
)

(defun parsedefsline (_line)
  (if (isNil _line)
    (return Nil)
  )
  (if (not (or (invoke-static 'acdk.lang.Character 'isLetter (invoke _line 'charAt 0)) (invoke _line 'startsWith "_")))
    (return Nil)
  )
  (setf _idx (invoke _line 'indexOf "="))
  (if (< _idx 1)
    (return Nil)
  )
  (setf _var (invoke (substr _line 0 _idx) 'trim 2))
  (if (< (invoke _var 'length) 2)
    (return Nil)
  )
  (setf _val (invoke (substr _line (+ _idx 1)) 'trim 2))
  (list _var _val)
)

(defun yesno ()
  (setf _answer (gets))
  (if (not (isNil _answer))
    (progn
      (if (or (invoke _answer 'startsWith "y") (invoke _answer 'startsWith "Y"))
        (return true)
      )
    )
  )
  Nil
)

