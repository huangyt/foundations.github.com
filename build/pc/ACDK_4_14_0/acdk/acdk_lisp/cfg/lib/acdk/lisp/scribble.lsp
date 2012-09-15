;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: scribble.lsp,v 1.2 2001/03/25 10:48:28 kommer Exp $


(defun collectFileTypes (dirorfilelist pattern)
  (setf erg NIL)
  (dolist (el dirorfilelist)
    (if (isFile el)
      (setf erg (append erg el))
      (if (isDirectory el)
        (progn
          (dolist (tf (glob el pattern))
            (setf erg (append erg (s+ el "/" tf)))
          )
        )
      )
    )
  )
  erg
)

(defun 

(defun test ()
  (break)
  
  (add 1 2)
  (return "asdf")
)

(defun collectImplementations (dirorfilelist)
  (collectFileTypes dirorfilelist "*.cpp")
)
(collectImplementations '("M:/ACDK/src/acdk/lang/ref" "M:/ACDK/src/acdk/lang/sys"))

(trace 1)
(setf fname "acdk/lang/String.cpp")
(stripExtension fname)

(setf l '("acdk/lang/String.h" "acdk/lang/String.cpp"))
(genObjectList l ".obj")

(setf dl '("M:\\ACDK\\src\\acdk\\acdk_main.cpp" "M:\\ACDK\\src\\acdk\\lang"))
(collectImplementations dl)


(defun clonelist2 (l)
  (setf nl NIL)
  (dolist (el l)
    (setf nl (append nl el))
  )
  nl
)
(clonelist2 l)

(trace 0)
(setf dir (new 'acdk.io.File "M:\\ACDK\\cfg"))
(setf fl (invoke dir 'list))
(setf dateilist (asList fl))
(trace 1)

(join dateilist "+")

(setf tl '("A" "B"))
(join tl "+")

(defun callsub (t)
  t
)

(setf f (invoke env 'lookupFunction "join"))
(invoke f 'getHelpText)

(defun t (arg)
  (if arg
    (return arg)
  )
  (return "NO ARG")
)

(defun t (l)
  (while l
    (setf l NIL)
  )
)
(setf l '("a" "b"))
(t l)

(setf i (new 'acdk.lang.Integer 42))
(instanceof i 'acdk.lang.Integer)
(instanceof i 'acdk.lang.Object)
(instanceof i 'acdk.lang.Number)
(instanceof i 'acdk.lang.Process)

(defun abfall ()
  ;(append erg (invoke objectarray 'get i))
 )


(defun asList (objectarray)
  (setf max (invoke objectarray 'length))
  (setf i 0)
  (setf erg '() )
  (while (> max i)
    (progn
      (append erg (invoke objectarray 'get i))
      (setf i (+ i 1))
    )
  )
  erg
)

(defun asList (objectarray)
  (setf size (invoke objectarray 'length))
  (setf erg '() )
  (setf i 0)
  (while (> size i)
    (progn 
      (setf i (+ i 1))
    )
  )
)

(defun dappendx ()
  (setf erg NIL)
  (setf erg (append erg "1"))
  erg
)

(defun dune ()
  (if (zerop d)
    (setf d 1)
    (setf d (+ d 1))
  )
)

(defun list-defuns ()
  (println "buildins:")
  (setf it (invoke env 'buildinsIterator))
  (while (invoke it 'hasNext)
    (progn 
      (setf v (invoke it 'next))
      (println v)
    )
  )
  (println "\n\ndefuns:")
  (setf it (invoke env 'defunsIterator))
  (while (invoke it 'hasNext)
    (progn 
      (setf v (invoke it 'next))
      (println v)
    )
  )
)
 
(defun myfunc (arg)
  "myfunc is just a dummy func"
  (return "asdf")
)

(defun test ()
  (println "start")
  (setf lv 42)
  (break)
  (println "label1")
  (println "label2")
  (println "label3")
)
(defun test2 ()
  (setf tv 41)
  (test)
)

(test2)

(defun doit (op arg1 arg2)
  "doit evaluate op with given args"
  (op arg1 arg2)
)

(defmacro setf (varname varcontent &optional helptext)
(defmacro setf (varname varcontent &rest liste)
(defun myfunc (varname :homepath home :tempdir tempdir)

)

(myfunc "asdf" :homepath "~/")

 
(setf varname varcontent "optional helptext")


(defun dotest (&rest rl)
  (let ((erg 0))
    (while (listp rl)
      (progn 
        (setf erg (+ erg (car rl)))
        (setf rl (cdr rl))
        (dump)
      )
    )
  )
)

(defun dotest2 (&rest rl)
  (while (listp rl)
    (setf rl (cdr rl))
  )
)

(defun dotest1 (&optional x)
  (if (zerop x)
    (progn
      (+ 1 2)
      (+ 2 3)
    )
    (progn
      (+ 10 20)
    )
  )
)
