;;; perl_cfg.lsp

  (if (acdkmake-is-platform "unix")
    (progn
    	(setg acdkmake-project-cflags2
    		" `perl -MExtUtils::Embed -e ccopts`"
    	)
    	(setg acdkmake-project-ldflags2
    		" `perl -MExtUtils::Embed -e ldopts` "
    	)
      
      (setg acdkmake-project-includes 
        '(
          "."
	      )
      )
      (setg acdkmake-project-ldflags '( ) ) 
      (setg acdkmake-project-libs '( ) )
    )
    ; else
    (progn
    	   
      (setg acdkmake-project-includes 
      '(
          "."
          "$(ACDK_PERL_INCLUDE)"
        )
      )
      (setg acdkmake-project-ldflags 
       '( 
       	( "-L" "$(ACDK_PERL_LIBPATH)" )
        )
      )
      (setg acdkmake-project-libs 
        '(
            "$(ACDK_PERL_LIB)"
        )
     )
     (println (s+ 
    "please correct the library/include path for Perl if needed: " 
    "\nincs: " (toCode acdkmake-project-includes)
    "\nlibs: " (toCode acdkmake-project-libs)
    "\nldflags: " (toCode acdkmake-project-ldflags)
            )
    )
    )
)
 
  
