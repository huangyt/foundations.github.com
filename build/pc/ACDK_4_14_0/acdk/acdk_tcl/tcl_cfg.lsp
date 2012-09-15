;;; tcl_cfg.lsp
(if (acdkmake-is-platform "unix") 
    (progn
    	(setg acdkmake-project-ldflags2
    		"$(ACDK_TCL_LDFLAGS)"
    	)
    	(setg acdkmake-project-cflags2
    		"$(ACDK_TCL_CFLAGS)"
    	)
      (setg acdkmake-project-ldflags 
       '(
        )
      )
      
      (setg acdkmake-project-libs
        '(
        )
      )
      (setg acdkmake-project-includes 
        '(
          "."
        )
      )
    )
    (progn    
      (setg acdkmake-project-ldflags 
        '(
          ("-L" "$(ACDK_TCL_LIBDIR)")
        )
      )
      (setg acdkmake-project-libs
        '(
          "$(ACDK_TCL_LIB)"
         )
       )
       (setg acdkmake-project-includes
        '(
          "."
          "$(ACDK_TCL_INCLUDE)"
        )
      )
     )
   )
  (println (s+ 
    "please correct the library/include path for Tcl if needed: " 
    "incs: " (toCode acdkmake-project-includes)
    "\nlibs: " (toCode acdkmake-project-libs)
    "\nldflags: " (toCode acdkmake-project-ldflags)
            )
  )
  

