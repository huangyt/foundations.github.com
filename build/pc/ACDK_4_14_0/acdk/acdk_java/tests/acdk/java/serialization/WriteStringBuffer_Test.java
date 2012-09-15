
import java.io.*;

class WriteStringBuffer_Test
{
  public static void main(String[] args)
  {
    try {
    StringBuffer obj = new StringBuffer("Test for StringBuffer with Umlauts: הüִײßים");
    FileOutputStream ostream = new FileOutputStream("StringBuffer.dat");
	  ObjectOutputStream p = new ObjectOutputStream(ostream);
	  p.writeObject(obj);
    p.flush();
	  ostream.close();
  } catch (Throwable ex) {
    System.out.println("Ex: " + ex.getMessage());
  }
  }
}
