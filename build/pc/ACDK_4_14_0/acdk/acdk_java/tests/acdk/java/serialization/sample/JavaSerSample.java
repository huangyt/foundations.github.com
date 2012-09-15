
public
class JavaSerSample
implements java.io.Serializable
{
  private int _x;
  private int _y;
  transient String privateData;
  public JavaSerSample(int x, int y)
  {
    _x = x;
    _y = y;
  }
  public int getX() { return _x; }
  public int getY() { return _y; }
  public String toString()
  {
    return "x=" + _x + ", y=" + _y;
  }
}
