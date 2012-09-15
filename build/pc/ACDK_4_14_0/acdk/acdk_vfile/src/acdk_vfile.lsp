
(defun acdk-main-init ()
  (if (zerop ACDKHOME)
    (progn
      (println (s+ "Error: ACDKHOME is not defined!"))
      (println (s+ "Set Environment variable or\n\t use -acdk-home=<ACDKHOME> as command line option"))
      (exit 1)
    )
  )
  (setg acdk-home ACDKHOME)
  (setg acdk-cfg-home (s+ acdk-home "/cfg"))
  (include (s+ acdk-home "/cfg/lib/acdk/lisp/acdkmake.lsp"))
  (if (not (acdk-make-test-args))
    (progn 
      (println (s+ "target for make is needed. See " ACDKHOME " /cfg/target"))
      (exit 1)
    )
  )
)

(defun make-main ()
  (acdk-main-init)
  (println (s+ "ACDKHOME=" ACDKHOME))
  (setg acdkmake-project-name "acdk_vfile")
  (setg acdkmake-project-type "dll")
  
  (setg acdkmake-project-dir-root ".")
  (setg acdkmake-project-exec-dir (s+ acdk-home "/bin" (if acdkmake-xcompile-enabled (s+ "/" acdkmake-target) "")))

  (setg acdkmake-project-object-dir (s+ "../tobj/" acdkmake-project-name))

  (setg acdkmake-project-template "ACDKCore")
  (setg acdkmake-project-includes '("."))
  ;(setg acdkmake-project-includes l)
  (setg acdkmake-project-acdklibs 
    '(
      "acdk_core"
     )
  )
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-ldflags '())

  (setg acdkmake-project-defines 
    '(
      ("IN_ACDK_VFILE_LIB")
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdk/vfile"
      "acdk/vfile/tar"
      "acdk/vfile/zip"
      "acdk/vfile/zlib/adler32.c"
      "acdk/vfile/zlib/crc32.c"
      "acdk/vfile/zlib/deflate.c"
      ;"acdk/vfile/zlib/deflate.h"
      "acdk/vfile/zlib/infblock.c"
      ;"acdk/vfile/zlib/infblock.h"
      "acdk/vfile/zlib/infcodes.c"
      ;"acdk/vfile/zlib/infcodes.h"
      "acdk/vfile/zlib/inffast.c"
      ;"acdk/vfile/zlib/inffast.h"
      "acdk/vfile/zlib/inftrees.c"
      ;"acdk/vfile/zlib/inftrees.h"
      "acdk/vfile/zlib/inflate.c"
      "acdk/vfile/zlib/infutil.c"
      ;"acdk/vfile/zlib/infutil.h"
      "acdk/vfile/zlib/trees.c"
      ;"acdk/vfile/zlib/trees.h"
      ;"acdk/vfile/zlib/zlib.h"
      "acdk/vfile/zlib/zutil.c"
      ;"acdk/vfile/zlib/zutil.h"
     )
  )
  
  (acdkmake-main)
)


(make-main)

