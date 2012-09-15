
import java.io.*;
import java.util.*;

class WriteArrayList_Test
{
  public static void main(String[] args)
  {
    try {
    ArrayList array = new ArrayList();
    array.add(new Short((short)42));
    array.add(new String("Bla"));
    //array.add(array);
    FileOutputStream ostream = new FileOutputStream("ArrayList.dat");
	  ObjectOutputStream p = new ObjectOutputStream(ostream);
	  p.writeObject(array);
    p.flush();
	  ostream.close();
  } catch (Throwable ex) {
    System.out.println("Ex: " + ex.getMessage());
  }
  }
}