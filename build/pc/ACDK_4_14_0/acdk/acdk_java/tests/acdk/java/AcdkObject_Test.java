
package tests.acdk.java;

import acdk.java.AcdkObject;

public class AcdkObject_Test
{
  static boolean verbose = true;
  static void testAssert(boolean condition, String descr)
  {
    if (condition == false)
    {
      if (verbose == true)
        System.err.println("***** Test Failed: " + descr);
      throw new Error(descr);
    }
    System.err.println("      Test OK: " + descr);
  }
  static void testBasic()
  {
    AcdkObject aobj = AcdkObject.New("acdk/lang/StringBuffer", "Hello");
    testAssert(aobj != null, "AcdkObject.New(\"acdk/lang/StringBuffer\", \"Hello\"");
    AcdkObject aout = (AcdkObject)AcdkObject.peek_static("acdk/lang/System", "out");
    testAssert(aout != null, "AcdkObject.peek_static(\"acdk/lang/System\", \"out\")");
    aobj.invoke("append", " ACDK from Java");
    aout.invoke("println", aobj.invoke("toString"));
    aobj.dispose();
  }
  static void testExceptions()
  {
    try {
      AcdkObject aobj = AcdkObject.New("acdk/lang/StringBuffer", "Hello");
      aobj.invoke("insert", (Object)new Integer(10000), (Object)"Another");
    } catch (IndexOutOfBoundsException ex) {
      System.out.println("Caught expected Ex: " + ex.getMessage());
    } 
  }
  static void testPeekPoke()
  {
    AcdkObject aobj = AcdkObject.New("acdk/tools/aunit/DmiTestClass");
    Integer sival = new Integer(42);
    aobj.poke("pubInt", sival);
    Integer ival = (Integer)aobj.peek("pubInt");
    testAssert(ival.intValue() == 42, "peek / poke");
    
    
    AcdkObject.poke_static("acdk/tools/aunit/DmiTestClass", "pubStaticInt", sival);
    ival = (Integer)AcdkObject.peek_static("acdk/tools/aunit/DmiTestClass", "pubStaticInt");
    testAssert(ival.intValue() == 42, "peek_static / poke_static");
  }
  public static void main(String[] sargs)
  {
    try {
      testBasic();
      testExceptions();
      testPeekPoke();
    } catch (Throwable ex) {
      System.err.println("Catched unexpected Exception:");
      System.err.println(ex.getMessage());
    }
  }
}