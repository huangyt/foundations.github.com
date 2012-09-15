Sub HelloWorld()
  Dim obj
  Set obj = CreateObject("Acdk.Object")
  Dim sb
  Set sb = obj.New("acdk/lang/StringBuffer", "Hallo ACDK")
  sb.append " from COM"
  
  MsgBox sb.toString()
  'alternativelly also: MsgBox sb.Invoke("toString") 
End Sub

Function testAssert(cond, msg)
   if cond = False then
      testAssert = False
      MsgBox "Failed: " & msg
   Else
      testAssert = True
   End If
End Function

Sub dmiTest()
  Dim obj
  ' for accessing static methods or member you also
  ' need an instance of an AcdkObject, becuase COM doesn't
  ' know the concept of static members/methods.
  Set obj = CreateObject("Acdk.Object")
  Dim i
  ' write access a static member
  obj.poke_static "acdk/tools/aunit/DmiTestClass", "pubStaticInt", 42
  ' read access a static member
  i = obj.peek_static("acdk/tools/aunit/DmiTestClass", "pubStaticInt")
  
  testAssert (i = 42), "poke_static or peek_static"
  'MsgBox "getPubStaticInt: " & i
  
  ' invoke a static method of a class
  i = obj.invoke_static("acdk/tools/aunit/DmiTestClass", "getPubStaticInt")
  testAssert (i = 42), "invoke_static"
  
  Dim d
  
  ' create an instance of the class acdk/tools/aunit/DmiTestClass
  Set d = obj.New("acdk/tools/aunit/DmiTestClass", "Test", 0)
  
  ' access the member pubInt of this object
  i = d.peek("pubInt")
  
  testAssert (i = 0), "peek"
  
  ' write the member value 
  d.poke "pubInt", 43
  ' and read it again
  i = d.peek("pubInt")
  testAssert (i = 43), "poke"
  
  ' object member can also be accessed directly 
  ' via name:
  
  ' write member
  d.pubInt = 42
  ' read member
  i = d.pubInt
  
  testAssert (i = 42), "poke/poke direct over name"
End Sub


'HelloWorld
dmiTest