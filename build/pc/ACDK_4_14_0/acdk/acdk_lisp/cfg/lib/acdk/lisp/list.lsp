;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: list.lsp,v 1.1.1.1 2000/12/11 18:05:37 kommer Exp $


(defun cadr (_x) 
  (car (cdr _x))
)

(defun cddr (_x) 
  (cdr (cdr _x))
)
(defun caddr (_x)
  (car (cdr (cdr _x)))
)

(defun atomp (_x)
  (not (consp _x))
)

(defun consp (_x)
  (typep _x 'cons)
)

(defun nth (_n _lst)
  "return the [n] element of a list\nSee also setnth"
  (if (> _n 0)
    (nth (- _n 1) (cdr _lst))
    (car _lst)
  )
)

(defun newlist ()
  (new 'acdk.lisp.LispList)
)

;; now implemented internal.
;(defun setnth (_n _lst _val)
;  "set element at index position [n] of [lst] with [val]"
;  (throw 'acdk.lang.RuntimeException "Not implemented yet")
;)


(defun join2list (_l1 _l2)
  "joins 2 list to a new list"
  (if (not _l1)
    _l2
    (if (not _l2)
      _l1
      (progn 
        (setf _erg (clone _l1))    
        (dolist (_el _l2)
          (setf _erg (append _erg _el))
        )
        _erg
      )
    )
  )
)

(defun join2string (_l _joiner)
  "(join (list joinstr)\nJoin the the [list] to one string using [joinstr] as glue between the elements"
  (setf _erg "")
  (while (and _l (car _l))
    (progn
      (if (> (invoke _erg 'length) 0)
        (setf _erg (s+ _erg _joiner))
      )
      (setf _erg (s+ _erg (car _l)))
      (setf _l (cdr _l))
    )
  )
  _erg
)


(defun asList (_obj)
  ;;(if (or (instanceof _obj 'acdk::Lang/[) (instanceof _obj 'acdk.lang.ObjectArray))
  (if (not _obj)
    (return (list))
  )
  (if Nil ; currently out of order...
    (progn
      (setf _max (invoke _obj 'length))
      (setf _i 0)
      (setf _res (list))
      (while (> _max _i)
        (progn
          (setf _res (append _res (invoke _obj 'get _i)))
          (setf _i (+ _i 1))
        )
      )
    )
    (if (instanceof _obj 'acdk.lisp.LispList)
      (setf _res _obj)
      (setf _res (list _obj))
    )
  )
  _res
)

(defun split (_liststr _splitstr &optional _ignoreempty)
  (setf _erg NIL)
  (while _liststr
    (progn
      ;(println (s+ "erg=[" _erg "] rest=[" _liststr "]"))
      (setf _idx (indexof _liststr _splitstr))
      (if (= _idx -1)
        (return (append _erg _liststr))
        (progn 
          (setf _t (substr _liststr 0 _idx))
          (if (or (not (isdef _ignoreempty)) (> (invoke _t 'length) 0))
            (setf _erg (append _erg _t))
          )
          ;(setf _erg (append _erg (substr _liststr 0 _idx)))
          (setf _liststr (substr _liststr (+ _idx 1)))
        )
      )
    )
  )
  _erg
)


(defun push (_thelist _el)
  (if _thelist
    (cons _thelist _el)
    (if _el
      (cons '() _el)
      NIL
    )
  )
)

(defun empty (_list)
  (or (not _list) (and (not (car _list)) (not (cdr _list))))
)

(defun pop (_list)
  (unpack (invoke _list 'pop))
)

(defun asArray (_list)
  (setf _l (length _list))
  (setf _oa (create-array _l))
  (setf _i 0)
  (while (> _l _i)
    (progn
      (invoke _l 'set _i (car _list))
      (setf _list (cdr _list))
    )
  )
)

(defun lcleanup (_list)
  "removes any NIL out of a list"
  (if _list
    (progn 
      (dolist (_el _list)
        (if (not (isNil _el))
          (progn
            (if (isdef _erg)
              (setf _erg (append _erg _el))
              (setf _erg (append '() _el))
            )
          )
        )
      )
    )
  )
  (if (isdef _erg)
    (setf _erg (cdr _erg))
    (setf _erg '() )
  )
  _erg
)

(defun cleanjoin (_l1 _l2)
  "joins 2 lists to a new list without any NILs"
  (setf _erg '() )
  (if _l1
    (progn
      (setf _l1 (lcleanup _l1))
      (if (not (car _l1))
        (setf _l1 Nil)
      )
    )
  )
  (if _l2
    (progn
      (setf _l2 (lcleanup _l2))
      (if (not (car _l2))
        (setf _l2 Nil)
      )
    )
  )
  (if (and _l1 _l2)
    (setf _erg (join2list _l1 _l2))
    (if _l1
      (setf _erg _l2)
      (if _l2
        (setf _erg _l2)
      )
    )
  )
  _erg
)

(defun listremove (_list _obj)
  (if (isNil _list)
    (return Nil)
  )
  (setf _tl '() )
  (dolist (_el _list)
    (if (not (eql _el _obj))
      (setf _tl (append _tl _el))
    )
  )
  (cdr _tl)
)

(defun listremovelist (_l1 _l2)
  (if (isNil _l1)
    (return Nil)
  )
  (dolist (_el _l2l)
    (if _el
      (setf _l1 (listremove _l1 _el))
    )
  )
  _l1
)

(defun lindex (_list _obj)
  (if (or (not _list) (not _obj))
    (return Nil)
  )
  (setf _idx -1)
  (setf _obj (s+ "" _obj))
  (dolist (_l _list)
    (setf _idx (+ _idx 1))
    (if (streql (s+ "" _l) _obj)
      (return _idx)
    )
  )
  -1
)

(defun unifyV (_list)
  (setf _res (list))
  (if (not _list)
    (return _res)
  )
  (if (not (instanceof _list 'acdk.lisp.LispList))
    (return (list _list))
  )
  (dolist (_l _list)
    (setf _res (join2list _res (unifyV _l)))
  )
  _res
)

(defun unify (&rest _objs)
  (unifyV _objs)
)

(defun lexpand (_list)
  (if (or (not _list) (or (not (instanceof _list 'acdk.lisp.LispList)) (not (car _list))))
    (return (list))
  )
  (setf _res (list))
  (if (instanceof (car _list) 'acdk.lisp.LispSymbol)
    (setf _res (asList (eval _list)))
    (progn
      (setf _item (car _list))
      (if _item
        (if (instanceof _item 'acdk.lisp.LispList)
          (setf _res (lexpand _item))
          (setf _res (list _item))
        )
      )
      (if (cdr _list)
        (setf _res (join2list _res (lexpand (cdr _list))))
      )
    )
  )
  (asList _res)
)

