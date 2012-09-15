
import java.io.*;

class WriteIntegerArray_Test
{
  public static void main(String[] args)
  {
    try {
    Integer da[] = new Integer[10];
    Integer naint = new Integer(42);
    for (int i = 0; i < da.length; ++i)
    {
      if ((i % 2) == 0)
        da[i] = naint;
      else
        da[i] = new Integer(i);
      System.out.println("i: " + i + ": " + da[i]);
    }
    FileOutputStream ostream = new FileOutputStream("IntegerArray.dat");
	  ObjectOutputStream p = new ObjectOutputStream(ostream);
	  p.writeObject(da);
    p.flush();
	  ostream.close();
	  System.out.println("Written: " + da.toString());
  } catch (Throwable ex) {
    System.out.println("Ex: " + ex.getMessage());
  }
  }
}