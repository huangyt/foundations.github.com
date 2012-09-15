
import java.io.*;

class WriteDoubleArray_Test
{
  public static void main(String[] args)
  {
    try {
    double da[] = new double[10];
    for (int i = 0; i < da.length; ++i)
    {
      da[i] = 0.12 * i;
    }
    FileOutputStream ostream = new FileOutputStream("doubleArray.dat");
	  ObjectOutputStream p = new ObjectOutputStream(ostream);
	  p.writeObject(da);
    p.flush();
	  ostream.close();
  } catch (Throwable ex) {
    System.out.println("Ex: " + ex.getMessage());
  }
  }
}