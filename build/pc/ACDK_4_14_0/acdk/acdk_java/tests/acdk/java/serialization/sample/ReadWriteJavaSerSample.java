import java.io.*;

public class ReadWriteJavaSerSample
{
  /**
    write JavaSerSample and JavaSerSample[] to be read by ACDK
  */
  static void writeJavaSerSample() throws java.io.IOException
  {
    {
      // write single object
      JavaSerSample obj = new JavaSerSample(41, 42);
      FileOutputStream ostream = new FileOutputStream("JavaSerSample_j2a.dat");
	    ObjectOutputStream p = new ObjectOutputStream(ostream);
	    p.writeObject(obj);
      p.flush();
	    ostream.close();
	  }
	  {
	    // write array of objects
      JavaSerSample[] obj = new JavaSerSample[2];
      obj[0] = new JavaSerSample(41, 42);
      obj[1] = new JavaSerSample(43, 44);
      FileOutputStream ostream = new FileOutputStream("JavaSerSampleArray_j2a.dat");
	    ObjectOutputStream p = new ObjectOutputStream(ostream);
	    p.writeObject(obj);
      p.flush();
	    ostream.close();
	  }
  }
  /**
    read JavaSerSample and JavaSerSample[] written by ACDK
  */
  static void readJavaSerSample() throws java.io.IOException, ClassNotFoundException
  {
    {
      FileInputStream fin = new FileInputStream("JavaSerSample_a2j.dat");
      ObjectInputStream oin = new ObjectInputStream(fin);
      Object obj = oin.readObject();
      System.out.println("Read Java Object written by ACDK serialization: " + obj.toString());
      JavaSerSample sample = (JavaSerSample)obj;
      sample.getX();
    }
	  {
      FileInputStream fin = new FileInputStream("JavaSerSampleArray_a2j.dat");
      ObjectInputStream oin = new ObjectInputStream(fin);
      Object obj = oin.readObject();
      System.out.println("Read Java Object Array written by ACDK serialization: " + obj.toString());
      JavaSerSample[] sample = (JavaSerSample[])obj;
      sample[0].getX();
    }
  }
  public static void main(String[] args)
  {
   try {
    writeJavaSerSample();
    readJavaSerSample();
  } catch (Throwable ex) {
    System.out.println("Exception in serialization: " + ex.getMessage());
  }
  }
}