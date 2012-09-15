
import java.io.*;

class ReadInteger_Test
{
  public static void main(String[] args)
  {
    try {
    Integer integer = new Integer(42);
    FileInputStream ostream = new FileInputStream("ReadInteger.dat.out.dat");
	  ObjectInputStream p = new ObjectInputStream(ostream);
	  integer = (Integer)p.readObject();
    if (integer.equals(new Integer(42)) == false)
      System.out.println("Test Failed!");
  } catch (Throwable ex) {
    System.out.println("Ex: " + ex.getMessage());
  }
  }
}