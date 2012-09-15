
import java.io.*;

class WritecharArray_Test
{
  public static void main(String[] args)
  {
    try {
    char[] obj = new char[8];
    obj[0] = 'A';
    obj[1] = 'C';
    obj[2] = 'D';
    obj[3] = 'K';
    obj[4] = 'ä';
    obj[5] = 'Ö';
    obj[6] = 'ß';
    obj[7] = 'é';
    FileOutputStream ostream = new FileOutputStream("charArray.dat");
	  ObjectOutputStream p = new ObjectOutputStream(ostream);
	  p.writeObject(obj);
    p.flush();
	  ostream.close();
  } catch (Throwable ex) {
    System.out.println("Ex: " + ex.getMessage());
  }
  }
}
