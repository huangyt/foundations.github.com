;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: string.lsp,v 1.4 2001/04/06 14:56:45 kommer Exp $


(defun make-string (&optional initstr)
  (if (isdef initstr)
    (new 'acdk.lang.String initstr)
    (new 'acdk.lang.String)
  )
)

(defun make-string1 (initstr)
  (new 'acdk.lang.String initstr)
)

(defun indexof (str token &optional start)
  (if (isdef start)
    (invoke str 'indexOf token start)
    (invoke str 'indexOf token 0)
  )
)

(defun substr (str start &optional end)
  (if (isdef end)
    (invoke str 'substr start end)
    (invoke str 'substr start)
  )
)

(defun toString (_obj)
  (if (truep _obj)
    (return "TRUE")
  )
  (if (instanceof _obj 'acdk.lang.String)
    _obj
    (invoke _obj 'toString)
  )
)


(defun s+ (_str &rest _other)
  (if (not _str)
    (progn
      (if (not (isdef _other))
        (return NIL)
      )
      (setf _str (car _other))
      (setf _other (cdr _other))
    )
  )
  (setf _str (toString _str))
  (progn
    (while (and (isdef _other) (listp _other))
      (progn
        (setf _t (car _other))
        (if _t
          (setf _str (invoke _str 'concat (toString _t)))
        )
        (setf _other (cdr _other))
      )
    )
  )
  _str
)

(defun strcmp (str1 str2)
  (if (and (isNil str1) (isNil str2))
    (return 0)
  )
  (if (isNil str1)
    (return -1)
  )
  (if (isNil str2)
    (return 1)
  )
  (invoke str1 'compareTo str2)
)

(defun streql (_str1 _str2)
  (if (= (strcmp _str1 _str2) 0)
    T
    Nil
  )
)

(defun strlen (_str)
  (if _str
    (invoke _str 'length)
    0
  )
)

(defun strempty (_str)
  (= (invoke _str 'length) 0)
)

(defun isNilorEmpty (str)
  (if (isNil str)
    (return true)
  )
  (if (strempty str)
    (return true)
  )
  (return false)
)

(defun atol (__str &optional __radix)
  (if (isdef __radix)
    (invoke-static 'acdk.lang.Long 'parseLong __str __radix)
    (invoke-static 'acdk.lang.Long 'decode __str)
  )
)

(defun ltoa (__val &optional __radix)
  (if (not (isdef __radix))
    (setf __radix 10)
  )
  (invoke-static 'acdk.lang.Long 'toString __val __radix)
)

(defun chkwsp (_str &optional _fatal)
  (setf _s (invoke _str 'replace "\t" " "))
  (setf _cnt (invoke _s 'elementCount " "))
  (if (> _cnt 0)
    (progn
      (if (isdef _fatal)
        (println (s+ "ERR: " _str " must not contain any spaces or tabs!"))
        (println (s+ "WARN: " _str " should not contain any spaces or tabs!"))
      )
    )
  )
  _cnt
)

(defun dos2unix (text)
  "convert a string into unix format" 
  (invoke text 'replace "\r\n" "\n")
)

(defun unix2dos (text)
  (invoke-static 'acdk.text.Format unix2dos text)
)
