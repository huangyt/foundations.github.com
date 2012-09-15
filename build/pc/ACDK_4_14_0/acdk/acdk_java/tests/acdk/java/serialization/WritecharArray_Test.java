
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
    obj[4] = '�';
    obj[5] = '�';
    obj[6] = '�';
    obj[7] = '�';
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
