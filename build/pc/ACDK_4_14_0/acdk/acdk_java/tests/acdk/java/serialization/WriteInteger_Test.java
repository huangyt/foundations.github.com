
import java.io.*;

class WriteInteger_Test
{
  public static void main(String[] args)
  {
    try {
    Integer integer = new Integer(42);
    FileOutputStream ostream = new FileOutputStream("ReadInteger.dat");
	  ObjectOutputStream p = new ObjectOutputStream(ostream);
	  p.writeObject(integer);
    //p.writeInt(12345);
	  //p.writeObject("Today");
	  //p.writeObject(new Date());
    p.flush();
	  ostream.close();
  } catch (Throwable ex) {
    System.out.println("Ex: " + ex.getMessage());
  }
  }
}