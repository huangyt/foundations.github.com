
package acdk.java;



/**
	Wrapper to a ACDK object
*/
public class AcdkObject
{
  public int objectHandle = 0;
  private native static int newAcdkObject(String classname, Object[] args);
  public AcdkObject(int obj)
  {
    objectHandle = obj;
  }
  public AcdkObject(String classname)
  {
    Object[] args = new Object[0];
    objectHandle = newAcdkObject(classname, args);
  }
  public AcdkObject(String classname, Object[] args)
  {
    objectHandle = newAcdkObject(classname, args);
  }
  public AcdkObject(String classname, Object arg1)
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    objectHandle = newAcdkObject(classname, args);
  }
  public AcdkObject(String classname, Object arg1, Object arg2)
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    objectHandle = newAcdkObject(classname, args);
  }
  public AcdkObject(String classname, Object arg1, Object arg2, Object arg3)
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    objectHandle = newAcdkObject(classname, args);
  }
  public AcdkObject(String classname, Object arg1, Object arg2, Object arg3, Object arg4)
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    objectHandle = newAcdkObject(classname, args);
  }
  public AcdkObject(String classname, Object arg1, Object arg2, Object arg3, Object arg4, 
                                      Object arg5)
  {
    Object[] args = new Object[5];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    objectHandle = newAcdkObject(classname, args);
  }
  public AcdkObject(String classname, Object arg1, Object arg2, Object arg3, Object arg4, 
                                      Object arg5, Object arg6)
  {
    Object[] args = new Object[6];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    args[5] = arg6;
    objectHandle = newAcdkObject(classname, args);
  }
  public AcdkObject(String classname, Object arg1, Object arg2, Object arg3, Object arg4, 
                                      Object arg5, Object arg6, Object arg7)
  {
    Object[] args = new Object[7];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    args[5] = arg6;
    args[7] = arg7;
    objectHandle = newAcdkObject(classname, args);
  }
  public static AcdkObject New(String name)
  {
    return new AcdkObject(name);
  }
  public static AcdkObject New(String name, Object arg1)
  {
    return new AcdkObject(name, arg1);
  }
  public static AcdkObject New(String name, Object arg1, Object arg2)
  {
    return new AcdkObject(name, arg1, arg2);
  }
  public static AcdkObject New(String name, Object arg1, Object arg2, Object arg3)
  {
    return new AcdkObject(name, arg1, arg2, arg3);
  }
  public static AcdkObject New(String name, Object arg1, Object arg2, Object arg3, Object arg4)
  {
    return new AcdkObject(name, arg1, arg2, arg3, arg4);
  }
  public static AcdkObject New(String name, Object arg1, Object arg2, Object arg3, Object arg4, 
                                            Object arg5)
  {
    return new AcdkObject(name, arg1, arg2, arg3, arg4, arg5);
  }
  public static AcdkObject New(String name, Object arg1, Object arg2, Object arg3, Object arg4, 
                                            Object arg5, Object arg6)
  {
    return new AcdkObject(name, arg1, arg2, arg3, arg4, arg5, arg6);
  }
  public static AcdkObject New(String name, Object arg1, Object arg2, Object arg3, Object arg4, 
                                            Object arg5, Object arg6, Object arg7)
  {
    return new AcdkObject(name, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }
  public void finalize()
  {
    dispose();
  }
  public native Object peek(String field);
  public native void poke(String field, Object value);
  public native Object invoke(String function, Object[] args);
  public Object invoke(String function)
  {
    Object[] args = new Object[0];
    return invoke(function, args);
  }
  public Object invoke(String function, Object arg1)
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return invoke(function, args);
  }
  public Object invoke(String function, Object arg1, Object arg2)
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return invoke(function, args);
  }
  public Object invoke(String function, Object arg1, Object arg2, Object arg3)
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return invoke(function, args);
  }
  public Object invoke(String function, Object arg1, Object arg2, Object arg3, Object arg4)
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return invoke(function, args);
  }
  public Object invoke(String function, Object arg1, Object arg2, Object arg3, Object arg4, 
                                        Object arg5)
  {
    Object[] args = new Object[5];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    return invoke(function, args);
  }
  public Object invoke(String function, Object arg1, Object arg2, Object arg3, Object arg4, 
                                        Object arg5, Object arg6)
  {
    Object[] args = new Object[6];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    args[5] = arg6;
    return invoke(function, args);
  }
  public Object invoke(String function, Object arg1, Object arg2, Object arg3, Object arg4, 
                                        Object arg5, Object arg6, Object arg7)
  {
    Object[] args = new Object[7];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    args[5] = arg6;
    args[6] = arg7;
    return invoke(function, args);
  }
  public static native Object peek_static(String acdkclass, String field);
  public static native void poke_static(String acdkclass, String field, Object value);
  public static native Object invoke_static(String acdkclass, String function, Object[] args);
  public static Object invoke_static(String acdkclass, String function)
  {
    Object[] args = new Object[0];
    return invoke_static(acdkclass, function, args);
  }
  public static Object invoke_static(String acdkclass, String function, Object arg1)
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return invoke_static(acdkclass, function, args);
  }
  public static Object invoke_static(String acdkclass, String function, Object arg1, Object arg2)
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return invoke_static(acdkclass, function, args);
  }
  public static Object invoke_static(String acdkclass, String function, Object arg1, Object arg2, Object arg3)
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return invoke_static(acdkclass, function, args);
  }
  public static Object invoke_static(String acdkclass, String function, Object arg1, Object arg2, Object arg3, Object arg4)
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return invoke_static(acdkclass, function, args);
  }
  public static Object invoke_static(String acdkclass, String function, Object arg1, Object arg2, Object arg3, Object arg4, 
                                                                        Object arg5)
  {
    Object[] args = new Object[5];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    return invoke_static(acdkclass, function, args);
  }
  public static Object invoke_static(String acdkclass, String function, Object arg1, Object arg2, Object arg3, Object arg4, 
                                                                        Object arg5, Object arg6)
  {
    Object[] args = new Object[6];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    args[5] = arg6;
    return invoke_static(acdkclass, function, args);
  }
  public static Object invoke_static(String acdkclass, String function, Object arg1, Object arg2, Object arg3, Object arg4, 
                                                                        Object arg5, Object arg6, Object arg7)
  {
    Object[] args = new Object[7];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    args[4] = arg5;
    args[5] = arg6;
    args[6] = arg7;
    return invoke_static(acdkclass, function, args);
  }
  public native void dispose();
  
  static 
  {
    try {
      System.loadLibrary("acdk_java");
    } catch (UnsatisfiedLinkError ex1) {
      try {
        System.loadLibrary("acdk_java_d");
      } catch (UnsatisfiedLinkError ex2) {
        System.loadLibrary("acdk_java_r");
      }
    }
  }
  /*
  public static void main(String[] args)
  {
    System.out.println("AcdkObject main");
    AcdkObject aobj = AcdkObject.New("acdk/lang/StringBuffer", "Hello");
    if (aobj == null)
    {
      System.out.println("AcdkObject not created");
    } else {
      System.out.println("AcdkObject created");
    }
    
    AcdkObject aout = (AcdkObject)AcdkObject.peek_static("acdk/lang/System", "out");
    
    aobj.invoke("append", " ACDK from Java");
    
    System.out.println((String)aobj.invoke("toString"));
    aout.invoke("println", aobj.invoke("toString"));
  }
  */
}