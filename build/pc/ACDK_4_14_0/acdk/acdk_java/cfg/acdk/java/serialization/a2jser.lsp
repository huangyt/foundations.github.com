
(setg a2jserfout NIL)

(defun getdicel (map key &optional required)
  (dolist (el map)
    ;(println (toCode el))
    ;(println (toString (car el)))
    (if (eql (car el) key)
      (return (cadr el))
    )
  )
  (if (isdef required)
    (println (s+ "parameter is required: " (toString key)))
  ) 
  Nil
)

(defun printserinfo (out el)
  
  (setf a2jser (getdicel el 'a2jser T))
  (setf classname (getdicel el 'name T))
  (invoke out println "// Is a2jser")
  (setf fieldlabels NIL)
  (dolist (field (getdicel el 'fields))
    (setf flabel (s+ "a2jser_" classname "_" (getdicel field 'name T) "_field"))
    (setf fieldlabels (append fieldlabels flabel))
    (invoke out println (s+ flabel " = {"))
    (invoke out println (s+ " \"" (getdicel field 'acdk_name T) "\","))
    (invoke out println (s+ " \"" (getdicel field 'acdk_type T) "\","))
    (invoke out println (s+ " \"" (getdicel field 'java_name T) "\","))
    (invoke out println (s+ " \"" (getdicel field 'java_type T) "\""))
    (invoke out println (s+ "};\n"))
  )
  ;(println (s+ "List has Length: " (length fieldlabels)))
  (invoke out println (s+ "a2jser_" classname "_fields = {"))
  (dolist (fieldlabel fieldlabels)
    (invoke out println (s+ " &" fieldlabel ","))
  )
  (invoke out println (s+ " 0"))
  (invoke out println (s+ "};\n"))
  
  
  (invoke out println (s+ "a2jser_" classname " = {"))
  
  (invoke out println (s+ " \"" (getdicel el 'acdk_name T) "\","))
  (invoke out println (s+ " \"" (getdicel el 'java_name T) "\","))
  (setf flags (getdicel el 'flags))
  (if (not flags)
    (setf flags "SC_SERIALIZABLE")
  )
  (invoke out println (s+ " " flags ","))
  (setf serid (getdicel el 'serid))
  (if (not serid)
    (setf serid 0)
  )
  (invoke out println (s+ " JLONG_CONSTANT(" serid "),"))
  (invoke out println (s+ " a2jser_" classname "_fields"))
  (invoke out println (s+ "};\n"))
  (invoke out println (s+ "::acdk::java::serialization::RegisterTypeMapping register_a2jser_" 
    classname "(&a2jser_" classname ");\n\n"))
  
)

(defun a2jser (mapdefinition)
  (printserinfo a2jserfout mapdefinition)
)

(defun printheader ()
  (invoke a2jserfout println "// This file is generated by ACDK Java Serialization")
  (invoke a2jserfout println "// http://www.acdk.de")
  (invoke a2jserfout println "//")
  (invoke a2jserfout println "#include <acdk/java/serialization/ClassTypeMapping.h>\n\n")
)

(defun main ()
  (setf args (getargs))
  (if (< (length args) 4)
    (progn
      (println "Need output file name and one or more a2jser defintion files")
      (return 1)
    )
  )
  (setf args (cdr (cdr args)))
  (setf filename (car args))
  (setg a2jserfout (new 'acdk.io.PrintWriter (new 'acdk.io.FileWriter filename)))
  (printheader)
  (setf args (cdr args))
  (dolist (f args)
    (println (s+ "arg: " f))
    (include f)
  )
  (return 0)
)

(main)
