;;; openssl_cfg.lsp

(if (acdkmake-is-platform "unix")
  (progn
  	(setg acdkmake-project-ldflags '( ) )
  	(setg acdkmake-project-libs 
    	'(
    	"$(ACDK_OPENSSL_LIBS)"
     		)
      )
  )
  (progn 
  	(setg acdkmake-project-libs '( ) )
  	(setg acdkmake-project-ldflags '( ) )
  	(setg acdkmake-project-ldflags2-debug
  		"$(ACDK_OPENSSL_LIBS)"
		)
		(setg acdkmake-project-ldflags2-release
  		"$(ACDK_OPENSSL_LIBS)"
		)
  (setg acdkmake-project-ldflags 
  	'(
  		( "-L" "$(ACDK_OPENSSL_LIBPATH)")
  	 )
  	)
  )  
)
