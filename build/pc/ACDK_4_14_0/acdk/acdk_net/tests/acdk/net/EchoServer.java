
import java.io.*;
import java.net.*;


class EchoServer
{
  public static void main(String[] args)
  {
    if (args.length < 1) {
      System.out.println("Need port");
      return;
    }
    int port = Integer.parseInt(args[0]);
    try {
      ServerSocket server = new ServerSocket (port);
      System.out.println("Login on " 
            + server.getInetAddress().toString() + ":" 
            + server.getLocalPort() 
            + ".\nExit with a single x");
      Socket client = server.accept();
      LineNumberReader in = new LineNumberReader(new InputStreamReader(client.getInputStream()));
      PrintWriter out = new PrintWriter(client.getOutputStream());
      while (true) {
        String str = in.readLine();
        System.out.println(str);
        if (str.equals("x") == true) {
          client.close();
          server.close();
          
          break;
        }
        out.println(str);
        out.flush();
      }
    } catch (IOException ex) {
    }
   }
}