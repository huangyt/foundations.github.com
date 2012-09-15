
import java.io.*;

class JavaClassDeclToAcdkSer
{
  static String getJavaType(String javaname)
  {
    return javaname.replace('.', '/');
  }
  static String getAcdkType(String javaname)
  {
    if (javaname.equals("Z"))
      return "bool";
    if (javaname.equals("C"))
      return "char";
    if (javaname.equals("B"))
      return "byte";
    if (javaname.equals("S"))
      return "short";
    if (javaname.equals("I"))
      return "int";
    if (javaname.equals("J"))
      return "long";
    if (javaname.equals("F"))
      return "float";
    if (javaname.equals("D"))
      return "double";
    if (javaname.startsWith("java.") == true)
    {
      javaname = "acdk" + javaname.substring(4);
    }
    return javaname.replace('.', '/');
  }
  static String identifierFromName(String javaname)
  {
    String ret = javaname.replace('.', '_').replace('/', '_');
    if (ret.startsWith("[") == true)
      return identifierFromName(ret.substring(1) + "Array");
    return ret;
  }
  static String getTypeString(ObjectStreamField field)
  {
    if (field.isPrimitive() == true)
      return String.valueOf(field.getTypeCode());
    return field.getTypeString();
  }
  public static boolean isSerializable(Class cls)
  {
    Class ifaces[]  = cls.getInterfaces();
    for (int i = 0; i < ifaces.length; ++i)
    {
      if (ifaces[i].getName().equals("java.io.Serializable") == true)
        return true;
    }
    return false;
  }
  public static void printClassDesc(PrintStream out, String classname)
  {
    try {
      String cident = identifierFromName(classname);
      Class cls = Class.forName(classname);
      String arrayclsname;
      
      if (classname.length() == 2 && classname.charAt(0) == '[')
        arrayclsname = "[" + classname;
      else
        arrayclsname = "[L" + classname + ";";
      
      Class arraycls = Class.forName(arrayclsname);
      ObjectStreamClass os = ObjectStreamClass.lookup(cls);
      if (os == null)
      {
        out.println("// Class " + classname + " is not serializable");
        return;
      }
      ObjectStreamClass osarray = ObjectStreamClass.lookup(arraycls);
                
      //System.out.println("UID: 0x" + Long.toHexString(os.getSerialVersionUID()));
      //System.out.println("UID[]: 0x" + Long.toHexString(osarray.getSerialVersionUID()));
      out.println("\n");
      ObjectStreamField[] fields  = os.getFields();
      String fieldNames[] = new String[fields.length];
      for (int i = 0; i < fields.length; ++i)
      {
        String fieldname = "a2jser_" + cident + "_field_" + fields[i].getName();
        fieldNames[i] = fieldname;
        out.println("::acdk::java::serialization::MemberTypeMapping " + fieldname + " = \n{");
        out.println("  \"" + getJavaType(fields[i].getName()) + "\", // acdk_name");
        out.println("  \"" + getAcdkType(getTypeString(fields[i])) + "\", // acdk_type");
        out.println("  \"" + fields[i].getName() + "\", // java_name");
        out.println("  \"" + getTypeString(fields[i]) + "\" // java_type");
        out.println("};\n");
      }
      out.println("::acdk::java::serialization::MemberTypeMapping* a2jser_" + 
        cident + "_fields[] = \n{");
      for (int i = 0; i < fieldNames.length; ++i)
      {
        out.println("  &" + fieldNames[i] + ",");
      }
      out.println("  0\n};\n");
      
      out.println("::acdk::java::serialization::ClassTypeMapping a2jser_" + cident + " = \n{");
      out.println("  \"" + getAcdkType(classname) + "\", // acdk_name");
      out.println("  \"" + getJavaType(classname) + "\", // java_name");
      Class supercls = cls.getSuperclass();
      if (supercls != null && isSerializable(supercls) == true)
      {
        out.println("  \"" + getAcdkType(supercls.getName()) + "\", // acdk_super");
        out.println("  \"" + getJavaType(supercls.getName()) + "\", // java_super");
      } else {
        out.println("  \"\", // acdk_super");
        out.println("  \"\", // java_super");
      }
      out.println("  ::acdk::java::serialization::SC_SERIALIZABLE, // flags" );
      out.println("  JLONG_CONSTANT(0x" + Long.toHexString(os.getSerialVersionUID()) + "), // UID");
      out.println("  JLONG_CONSTANT(0x" + Long.toHexString(osarray.getSerialVersionUID()) + "), //[] UID");
      out.println("  a2jser_" + cident + "_fields, // Fields");
      out.println("  0, // read_func");
      out.println("  0, // write_func");
      out.println("  0 // used for internal linked list\n};\n");
      out.println("::acdk::java::serialization::RegisterTypeMapping register_a2jser_" +
                      cident + "(&a2jser_" + cident + ");\n\n");
    } catch (java.lang.ClassNotFoundException ex) {
      System.out.println("Class not found: " + ex.getMessage());
    }
  }
  public static void main(String[] args)
  {
    try {
      if (args.length < 1) 
      {
        System.err.println("Need a class name as Parameter");
        return;
      }
      System.out.println("#include <acdk.h>");
      System.out.println("#include <acdk/java/serialization/ClassDescription.h>\n");
      
      String cname = args[0];
      printClassDesc(System.out, cname);
      boolean isfalse = false;
      if (isfalse == false)
        return;
      Class cls = Class.forName(cname);
      String arrayclsname;
      if (cname.length() == 2 && cname.charAt(0) == '[')
        arrayclsname = "[" + cname;
      else
        arrayclsname = "[L" + cname + ";";
      Class arraycls = Class.forName(arrayclsname);
      ObjectStreamClass os = ObjectStreamClass.lookup(cls);
      ObjectStreamClass osarray = ObjectStreamClass.lookup(arraycls);
      
      System.out.println("UID: 0x" + Long.toHexString(os.getSerialVersionUID()));
      System.out.println("UID[]: 0x" + Long.toHexString(osarray.getSerialVersionUID()));
      
      ObjectStreamField[] fields  = os.getFields();
      for (int i = 0; i < fields.length; ++i)
      {
        System.out.println("Field: " + fields[i].getName());
        System.out.println("    tc: " + fields[i].getTypeCode());
        System.out.println("    ts: " + fields[i].getTypeString());
      }
      return;
    } catch (java.lang.ClassNotFoundException ex) {
      System.out.println("Class not found: " + ex.getMessage());
    }
    return;
  }
}