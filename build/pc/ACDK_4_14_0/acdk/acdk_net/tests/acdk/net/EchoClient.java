
import java.io.*;
import java.net.*;


class EchoClient
{
  public static void main(String[] args)
  {
    if (args.length < 1) {
      System.out.println("Need port");
      return;
    }
    int port = Integer.parseInt(args[0]);
    try {
    Socket socket = new Socket(InetAddress.getLocalHost(), port);
    LineNumberReader in = new LineNumberReader(new InputStreamReader(socket.getInputStream()));
    PrintWriter out = new PrintWriter(socket.getOutputStream());
    LineNumberReader sin = new LineNumberReader(new InputStreamReader(System.in));
    while (true) {
      System.out.print("> "); System.out.flush();
      String line = sin.readLine();
      out.println(line);
      out.flush();
      String retline = in.readLine();
      System.out.println("< " + retline);
    }
    } catch (SocketException ex) {
      System.out.println(ex.getMessage());
    } catch (IOException ex) {
    }
  }
}