;;; python_cfg.lsp

  (if (acdkmake-is-platform "unix")
    (progn
    	(setg acdkmake-project-cflags2
    		"$(ACDK_SQL_ODBC_CFLAGS)"
    	)
    	(setg acdkmake-project-ldflags2
    		"$(ACDK_SQL_ODBC_LDFLAGS)"
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
        )
      )
      (setg acdkmake-project-ldflags 
       '(
          ; odbc32 should be in compilers path
        )
      )
      
      (setg acdkmake-project-libs 
        '(
            "odbc32"
         )
      )
    )
  )
  
