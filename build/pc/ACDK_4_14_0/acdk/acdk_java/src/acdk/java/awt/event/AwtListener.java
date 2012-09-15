
package acdk.java.awt.event;
import java.awt.*;
import java.awt.event.*;

public class AwtListener
  extends acdk.java.AcdkObject
  implements ActionListener,
             AWTEventListener,
             WindowListener,
             KeyListener,
             MouseListener 
{
  public AwtListener()
  {
    super(0);
  }
  public AwtListener(int objh)
  {
    super(objh);
  }
  public void actionPerformed(ActionEvent e)
  {
    actionPerformedN(e, e.getID());
  }
  native void actionPerformedN(AWTEvent event, int eid);
   
  public void eventDispatched(AWTEvent event) 
  {
   eventDispatchedN(event, event.getID());
  }
  native void eventDispatchedN(java.awt.AWTEvent event, int eid);
  
  // WindowListener
  public void windowActivated(WindowEvent e) { windowEventN(e, e.getID()); }
  public void windowClosed(WindowEvent e)  { windowEventN(e, e.getID()); }
  public void windowClosing(WindowEvent e)  { windowEventN(e, e.getID()); }
  public void windowDeactivated(WindowEvent e)  { windowEventN(e, e.getID()); }
  public void windowDeiconified(WindowEvent e)  { windowEventN(e, e.getID()); }
  public void windowIconified(WindowEvent e)  { windowEventN(e, e.getID()); }
  public void windowOpened(WindowEvent e)  { windowEventN(e, e.getID()); }
  native void windowEventN(java.awt.AWTEvent event, int eid);
  
  //KeyListener
  public void keyPressed(KeyEvent e) { keyEventN(e, e.getID());  }
  public void keyReleased(KeyEvent e) { keyEventN(e, e.getID()); }
  
  public void keyTyped(KeyEvent e) { keyEventN(e, e.getID()); }
  native void keyEventN(java.awt.AWTEvent event, int eid);
  
  // MouseListener 
  public void mouseClicked(MouseEvent e) { mouseEventN(e, e.getID()); }
  public void mouseEntered(MouseEvent e) { mouseEventN(e, e.getID()); } 
  public void mouseExited(MouseEvent e)  { mouseEventN(e, e.getID()); }
  public void mousePressed(MouseEvent e)  { mouseEventN(e, e.getID()); }
  public void mouseReleased(MouseEvent e)  { mouseEventN(e, e.getID()); }
 
  native void mouseEventN(java.awt.AWTEvent event, int eid);
}