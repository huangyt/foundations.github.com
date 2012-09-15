
package acdk.java;
class ACDK
{
  public static native Object New(String name, Object[] args);
  public native Object peek(String field);
  public native void poke(String field, Object value);
  public native Object invoke(String function, Object[] args);
  public static native Object peek_static(String acdkclass, String field);
  public static native void poke_static(String acdkclass, String field, Object value);
  public static native Object invoke_static(String acdkclass, String function, Object[] args);
}