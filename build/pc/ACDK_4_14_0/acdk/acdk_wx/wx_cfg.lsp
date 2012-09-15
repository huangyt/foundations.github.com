;;; wx_cfg.lsp
  (if (not (isdef wx_ide))
    (setg wx_ide NIL)
  )
  
  (if (acdkmake-is-platform "unix")
    (progn
      (setg acdkmake-project-includes 
        '(
          "."
        )
      )
      (setg acdkmake-project-cflags2 "$(WX_CFLAGS)" )
      (setg acdkmake-project-ldflags '( ) ) 
      (setg acdkmake-project-libs 
        '(
          "$(WX_LIBS)"
        )
      )
    )
    ; Windows Plattform
    
    (progn 
      (setg acdkmake-project-cflags2 "$(WX_CFLAGS)")
      (setg acdkmake-project-libs '())
      (setg acdkmake-project-includes 
      '(
          "."
          "$(ACDK_WX_INCLUDE)"
          "$(ACDK_WX_INCLUDE_PLATTFORM)"
          "$(ACDK_WX_INCLUDE_CONTRIB)"
        )
      )
      (setg acdkmake-project-ldflags 
       '(
          ( "-L" "$(ACDK_WX_LIB_PATH)")
        )
      )
      (if wx_ide
       (progn
          (setg acdkmake-project-ldflags2-release
            "$(ACDK_WX_IDE_WXLIBS_RELEASE)"
          )
          (setg acdkmake-project-ldflags2-debug
            "$(ACDK_WX_IDE_WXLIBS_DEBUG)"
          )
        )
        (progn
          (setg acdkmake-project-ldflags2-release
            "$(ACDK_WX_WXLIBS_RELEASE)"
          )
          (setg acdkmake-project-ldflags2-debug
            "$(ACDK_WX_WXLIBS_DEBUG)"
          )
        )
      )
    )
  )
  (println (s+ 
    "please correct the library/include path for wxWindows if needed: " 
    "\nincs: " (toCode acdkmake-project-includes)
    "\nlibs: " (toCode acdkmake-project-libs)
    "\nldflags: " (toCode acdkmake-project-ldflags)
            )
  )
  
