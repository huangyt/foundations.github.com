
(include "../gmake/target.lsp")


(setg acdkmake-target-cdpathprefix "${.CURDIR}/")
(setg acdkmake-target-compileprefix "cd ${.CURDIR}; ")
;; overwrites io.lsp

(defun slash ()
  (print ".") (flush)
  "/"
)

(defun acdkmake-is-platform (label)
  (if (or (streql label "unix") (streql label "bsd"))
    t
    NIL
  )
)

(setg acdkmake-target-defines '(  ("_REENTRANT")))


(if (not (isdef gmake_getBindModule))
  (setg gmake_getBindModule getBindModule)
)

(setg gmake_getBindModule getBindModule)



(defun acdkmake-target-get-bindmodule ()
  (return (gmake_getBindModule))
)

(if (not (isdef acdkmake-get-libs))
  (setg acdkmake-get-libs-org acdkmake-get-libs)
)


(defun acdkmake-get-libs ()
  (setf erg "")
  (if (and (isdef acdkmake-project-libs) (> (length acdkmake-project-libs) 0))
    (dolist (l acdkmake-project-libs)
      (if (isdef l)
        (setf erg (s+ erg "-l" l " "))
      )
    )
  )
  (if (and (isdef acdkmake-project-acdklibs) (> (length acdkmake-project-acdklibs) 0))
    (progn
      (setf erg (s+ erg "-Wl,-Bdynamic -L$(BINDIR) "))
      (dolist (l acdkmake-project-acdklibs)
        ;(println (s+ "Library: " l))
        (if (isdef l)
          (if (l 'startsWith "$")
            (setf erg (s+ erg l " "))
            (setf erg (s+ erg "-l" l " "))
          )
        )
      )
    )
  )
  (s+ erg (acdkmake-get-libs2))
)



