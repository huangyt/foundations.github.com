;;; -*- lisp -*-
;;;
;;; Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
;;; ALL RIGHTS RESERVED
;;; This file is part of ACDK.
;;; artefaktur provides this software "as is" without express or implied warranty.
;;; Any commercial use of this software requires a license.
;;; 
;;; $Id: environment.lsp,v 1.2 2001/03/16 12:16:07 kommer Exp $

(defun help-commands ()
  (println "try (help command) with following commands:")
  (println (join2string (list-defuns) " "))
)


(defun help (&optional _func)
  "(help) or (help functionname) or (help function)\n
    return helpstring for [function]"
  (if (not (isdef _func))
    (return (help-commands))
  )
  (if (instanceof _func 'acdk.lang.String)
    (setf _func (invoke *env* 'lookupFunction _func))
  )
  (invoke _func 'getHelpText)
)

(defun list-defuns ()
  (setf _it (invoke env 'functionIterator))
  (setf _erg NIL)
  (while (invoke _it 'hasNext)
    (setf _erg (append _erg (invoke _it 'next)))
  )
  _erg
)

(defun getargs ()
  (setf _res (list))
  (if (and (isdef *args*) *args*)
    (progn
      (setf _len (invoke *args* "length"))
      (setf _n 0)
      (while (< _n _len)
        (progn
          (setf _res (append _res (invoke *args* "get" _n)))
	        (setf _n (+ _n 1))
	      )
      )
    )
  )
  _res
)

(defun parseopts (&rest _defs)
  "parses opt-definitions of type (optname, varname[, hasarg])"
  "if an option matches, either varname is set to true, or if hasarg is true"
  "the next arg is stored in varname. this function returns a list with any"
  "non-option args, excluding arg[0], which is the program-name, and excluding"
  "optional filename for non-interactive call, or Nil for an error."
  (if (not (setf _opts (getargs)))
    (return Nil)
  )
  (setf _opts (cdr _opts)) ;ignore program-name
  ;;(println (s+ "interactive = " *interactive*))
  ;;(println (s+ "!interactive = " (not *interactive*)))
  (if (not *interactive*)
    ;;(println _opts)
    (setf _opts (cdr _opts)) ;ignore startup-file-arg
    ;;(println _opts)
  )
  (setf _var Nil)
  (setf _args (list))
  (dolist (_opt _opts)
    (if _var
      (progn
        (setv _var _opt)
        (setf _var Nil)
      )
      (if (and _opt (= (invoke _opt "charAt" 0) 45))
        (progn
          (setf _opt (substr _opt 1))
	  (if (streql _opt "help")
	    (progn
	      (setf _usage (lookup-func "usage"))
	      (if _usage
	        (call-func "usage")
		(printerr "sorry, no help available")
	      )
	      (exit 0)
	    )
	  )
          (setf _found Nil)
          (dolist (_def _defs)
             (setf _str (car _def))
             (if (streql _opt _str)
               (progn
                 (setf _var (cadr _def))
                 (setf _varopt _opt)
		 (if _var
		   (progn
                     (setf _found true)
                     (if (not (cdr (cdr _def)))
                       (progn
                         (if (and (setf _val (lookup-var _var)) (isinstanceof _val 'acdk.lang.Number))
                           (setv _var (+ _val 1))
                           (setv _var true)
                         )
                         (setf _var Nil)
                       )
		     )
		   )
		   (println (s+ "invalid option-definition for " _str))
                 )
               )
             )
          )
           (if (not _found)
             (progn
               (println (s+ "invalid option -" _opt "."))
               (return Nil)
             )
           )
        )
        (setf _args (append _args _opt))
      )
    )
  )
  (if _var
    (progn
      (println (s+ "option -" _varopt " needs an argument!"))
      (return Nil)
    )
  )
  (if _args
    _args
    (list Nil)
  )
)

(defun exit (&optional _retval)
  (if (not (isdef _retval))
    (setf _retval 0)
  )
  (invoke env 'exitNow _retval)
  NIL
)

(defun lookup-var (_name)
  (invoke *env* 'lookupVar _name Nil)
)

(defun lookup-func (_name)
  (invoke *env* 'lookupFunction _name)
)

(defun call-funcV (_name _args)
  (setf _func (lookup-func _name))
  (if (isNil _func)
    (progn
      (printerr (s+ "no such function/method with the name \"" _name "\"."))
      (return Nil)
    )
  )
  (if _args  ;; NOTE: internal eval expects the name as first item.
    (invoke _func 'eval env (cons _name _args))
    (invoke _func 'eval env (list _name))
  )
)

(defun call-func (_name &rest _args)
  (if (isdef _args)
    (call-func _name _args)
    (call-func _name Nil)
  )
)

(defun set-atom (_name _arg)
  (invoke *env* 'bindGlobal _name (new 'acdk.lisp.LispAtom _arg))
)

(defun set-list (_name _arg)
  (invoke *env* 'bindGlobal _name (new 'acdk.lisp.LispList _arg))
)

(defun set-array (_name _arg)
  (invoke *env* 'bindGlobal _name (new 'acdk.lisp.LispArray _arg))
)

;; NOTE: this implementation of setv works well, but it fails while setting
;;       a variable to true, caused by the internal representation of true.
;;       acdklisp should get a complete redesign...
(defun _setv (_name _arg)
  (setf _var (setf _type (lookup-var _name)))
  (if (not _type)
    (setf _type _arg)
  )
  (if (instanceof _type 'acdk.lisp.LispArray)
    (if (instanceof _arg 'acdk.lisp.LispArray)
      (set-array _name _arg)
      (progn
        (println (s+ "can't assign " (invoke _arg 'getName) " to Array " (invoke _type 'getName)))
         (return Nil)
      )
    )
    (if (instanceof _type 'acdk.lisp.LispList)
      (if (instanceof _arg 'acdk.lisp.LispList)
        (set-list _name _arg)
        (progn
          (println (s+ "can't assign " (invoke _arg 'getName) " to List " (invoke _type 'getName)))
           (return Nil)
         )
      )
      (if (and (or (isNil _var) (instanceof _type 'acdk.lisp.LispAtom)) (not (or (instanceof _arg 'acdk.lisp.LispList) (instanceof _arg 'acdk.lisp.LispArray))))
        (set-atom _name _arg)
         (progn
          (println (s+ "can't assign " (invoke _arg 'getName) " to Atom " (invoke _type 'getName)))
           (return Nil)
         )
      )
    )
  )
  (invoke (lookup-var _name) 'toString)
)

