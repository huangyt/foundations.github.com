
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
  (setg acdkmake-project-name "acdk_xml")
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
      "acdk_text"
      "acdk_net"
      "org_xml"
     )
  )
  (setg acdkmake-project-libs 
    '(
     )
  )
  (setg acdkmake-project-ldflags '())

  (setg acdkmake-project-defines 
    '(
      ("IN_ACDK_XML_LIB")
    )
  )
  (setg acdkmake-project-sourcelist 
    '(
      "acdk/xml"
      "acdk/xml/sax"
      "acdk/xml/dom"
      "acdk/xml/libxmldom"
      "acdk/xml/parsers"
      "libxml"
      ;;"libxml2"
      
      "expat/xmlparse.c"
      "expat/xmlrole.c"
      "expat/xmltok.c"
      
      "libxml2/c14n.c"
      "libxml2/catalog.c"
      "libxml2/chvalid.c"
      "libxml2/debugXML.c"
      "libxml2/dict.c"
      "libxml2/DOCBparser.c"
      "libxml2/encoding.c"
      "libxml2/entities.c"
      "libxml2/error.c"
      "libxml2/globals.c"
      "libxml2/hash.c"
      "libxml2/HTMLparser.c"
      "libxml2/HTMLtree.c"
      "libxml2/legacy.c"
      "libxml2/list.c"
      "libxml2/nanoftp.c"
      "libxml2/nanohttp.c"
      "libxml2/parser.c"
      "libxml2/parserInternals.c"
      "libxml2/pattern.c"
      "libxml2/relaxng.c"
      "libxml2/SAX.c"
      "libxml2/SAX2.c"
      "libxml2/threads.c"
      "libxml2/tree.c"
      "libxml2/triostr.c"
      "libxml2/uri.c"
      "libxml2/valid.c"
      "libxml2/xinclude.c"
      "libxml2/xlink.c"
      "libxml2/xmlIO.c"
      "libxml2/xmlmemory.c"
      "libxml2/xmlreader.c"
      "libxml2/xmlregexp.c"
      "libxml2/xmlsave.c"
      "libxml2/xmlschemas.c"
      "libxml2/xmlschemastypes.c"
      "libxml2/xmlstring.c"
      "libxml2/xmlunicode.c"
      "libxml2/xmlwriter.c"
      "libxml2/xpath.c"
      "libxml2/xpointer.c"
     )
  )
  (setg acdk-make-project-metainfos
    '(
       "acdk/xml"
       "acdk/xml/sax"
       "acdk/xml/dom"
       "acdk/xml/parsers"
       "acdk/xml/libxmldom"
      )
  )
  (acdkmake-main)
)


(make-main)

