
			
 (if (acdkmake-is-platform "unix")
    (progn
    	(setg acdkmake-project-cflags2
    		"$(ACDK_JAVA_CFLAGS)"
    	)
    	(setg acdkmake-project-ldflags2
    		"$(ACDK_JAVA_LDFLAGS)"
    	)
      (setg acdkmake-project-includes 
        '(
        "."
        ;"/usr/local/j2sdk1.3.0/include" ;; you should ajust this        
        ;"/usr/local/j2sdk1.3.0/include/linux"
        ;"/usr/lib/jdk1.3.1/include"
        ;"/usr/lib/jdk1.3.1/include/linux"
        ;"/usr/lib/SunJava2-1.4.1/include"
				;"/usr/lib/SunJava2-1.4.1/include/linux"
        ;"/usr/java1.2/include"
        ;"/usr/java1.2/include/solaris"
        )
      )
      (if acdkmake-is-platform "linux")
       (progn
        (setg acdkmake-project-ldflags 
          '(
            ;("-L" "/usr/local/j2sdk1.3.0/jre/lib/i386/native_threads")
            ;("-L" "/usr/local/j2sdk1.3.0/jre/lib/i386/classic")
            ;("-L" "/usr/local/j2sdk1.3.0/jre/lib/i386")
            ;("-L" "/usr/lib/jdk1.3.1/jre/lib/i386")
            ;("-L" "/usr/lib/jdk1.3.1/jre/lib/i386/classic")
            ;("-L" "/usr/lib/jdk1.3.1/jre/lib/i386/native_threads") 
	    			;("-L" "/usr/lib/SunJava2-1.4.1/jre/lib/i386/native_threads") ; Suse 8.2
	    			; ("-L" "/usr/lib/SunJava2-1.4.1/jre/lib/i386") ; Suse 8.2
           )
         )
         (setg acdkmake-project-libs 
          '(
            ;"hpi"
            ;"jvm"
            ;"verify"
            ;"java"
            )
          )
      )
      (if (acdkmake-is-platform "solaris")
       (progn
        (setg acdkmake-project-ldflags 
          '(
            ;("-L" "/usr/java1.2/jre/lib/sparc")
          )
         )
        (setg acdkmake-project-libs 
          '(
            ;"jvm"
            ;"java"
            )
          )
        )
      ) 
    )
    (progn ;; windows
      (setg acdkmake-project-includes 
      '(
          "."
          "$(ACDK_JAVA_INCLUDE)"
          "$(ACDK_JAVA_INCLUDE_WIN32)"
          ;"d:/programr/lang/java/jdk/jdk1.3.1/include"
          ;"d:/programr/lang/java/jdk/jdk1.3.1/include/win32"
          ;"c:/programr/lang/java/jdk/jdk1.3.1/include"
          ;"c:/programr/lang/java/jdk/jdk1.3.1/include/win32"
          ;"f:/bin/jdk1.3./include" ;; you should ajust this
          ;"f:/bin/jdk1.3./include/win32" ;; you should ajust this
        )
      )
      (setg acdkmake-project-ldflags 
       '(
       		("-L" "$(ACDK_JAVA_LIBDIR)")
          ;("-L" "d:/programr/lang/java/jdk/jdk1.3.1/lib")
          ;("-L" "c:/programr/lang/java/jdk/jdk1.3.1/lib")
          ;("-L" "F:/bin/jdk1.3/lib")
        )
      )
      (setg acdkmake-project-libs 
        '(
            ;"jvm"
            "$(ACDK_JAVA_JVM_LIB)"
        )
     )
    )
  )
  
