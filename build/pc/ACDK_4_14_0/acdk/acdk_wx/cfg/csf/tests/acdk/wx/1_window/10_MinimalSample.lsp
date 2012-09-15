(invoke-static 'Class 'forName "acdk.wx.Window)
;;; does not work, because acdk_lisp doesn't register
;;; Lisp classes in ClazzInfo
(defclass MyApp  (acdk.wx.App) 
  (bar :onInit 
    (lambda (self) 
      ((make-instance 'acdk.wx.Frame) 'show true)
      (return true)
    )
  )
)

(setf args (make-instance 'StringArray 0))
(invoke-static 'acdk.wx.App 'createGui "MyApp" args)
