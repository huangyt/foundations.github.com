;;; python_cfg.lsp

  (if (acdkmake-is-platform "unix")
    (progn
    	(setg acdkmake-project-cflags2
    		"$(ACDK_PYTHON_CFLAGS)"
    	)
    	(setg acdkmake-project-ldflags2
    		"$(ACDK_PYTHON_LDFLAGS)"
    	)
      (setg acdkmake-project-includes 
        '(
          "."
         )
       )
       (setg acdkmake-project-libs '( ) )
       (setg acdkmake-project-ldflags '( ))
    )
    (progn
      (setg acdkmake-project-includes 
      '(
          "."
          "$(ACDK_PYTHON_INCLUDE)"
          "$(ACDK_PYTHON_INCLUDE_PLATFROM)"
        )
      )
      (setg acdkmake-project-ldflags 
       '(
          ( "-L" "$(ACDK_PYTHON_LIB_PATH)")
        )
      )
      (setg acdkmake-project-ldflags2-release
            "$(ACDK_PYTHON_PYLIBS_RELEASE)"
      )
      (setg acdkmake-project-ldflags2-debug
        "$(ACDK_PYTHON_PYLIBS_DEBUG)"
      )
      (setg acdkmake-project-libs 
        '(
            ;"$(ACDK_PYTHON_PYLIBS_DEBUG)" ;; uses the debugging version
         )
      )
    )
  )
  (println (s+ 
    "please correct the library/include path for Python if needed: " 
    "\nincs: " (toCode acdkmake-project-includes)
    "\nlibs: " (toCode acdkmake-project-libs)
    "\nldflags: " (toCode acdkmake-project-ldflags)
            )
  )
