
import java.io.*;

class WriteStringArray_Test
{
  public static void main(String[] args)
  {
    try {
    String da[] = new String[10];
    String  naint = new String("Bla: הüִײßים");
    for (int i = 0; i < da.length; ++i)
    {
      if ((i % 2) == 0)
        da[i] = naint;
      else
        da[i] = Integer.toString(i);
      System.out.println("i: " + i + ": " + da[i]);
    }
    FileOutputStream ostream = new FileOutputStream("StringArray.dat");
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