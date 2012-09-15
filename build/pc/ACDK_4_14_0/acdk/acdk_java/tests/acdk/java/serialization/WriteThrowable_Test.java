
import java.io.*;

class WriteThrowable_Test
{
  private static void throwFunc(String str) throws Throwable
  {
    throw new Throwable(str);
  }
  public static void main(String[] args)
  {
    try {
      Throwable obj = null;
      try {
        throwFunc("This is the Reason");
      } catch (Throwable exm) {
        obj = exm;
      }
    FileOutputStream ostream = new FileOutputStream("Throwable.dat");
	  ObjectOutputStream p = new ObjectOutputStream(ostream);
	  p.writeObject(obj);
    p.flush();
	  ostream.close();
  } catch (Throwable ex) {
    System.out.println("Ex: " + ex.getMessage());
  }
  }
}
