// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Library General Public License (LGPL).
// 
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the 
// License ACDK-FreeLicense document enclosed in the distribution
// for more for more details.

// This file is part of the Artefaktur Component Development Kit:
//                         ACDK
// 
// Please refer to
// - http://www.acdk.de
// - http://www.artefaktur.com
// - http://acdk.sourceforge.net
// for more information.
// 
// $Header: /cvsroot/acdk/acdk/acdk_java/tests/acdk/java/acdk_java_Basic_Test.cpp,v 1.11 2005/02/05 10:45:12 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/java/JavaInterpreter.h>
#include <acdk/java/JavaObject.h>

#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/java/awt/AWTEvent.h>
#include <acdk/java/awt/event/ActionListener.h>
#include <acdk/java/awt/event/WindowListener.h>
#include <acdk/java/awt/event/KeyListener.h>

namespace tests {
namespace acdk {
namespace java {

using namespace ::acdk::lang;
using namespace ::acdk::java::awt;
using namespace ::acdk::java::awt::event;


BEGIN_DECLARE_TEST( Basic_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( Gui1 )
END_DECLARE_TEST( Basic_Test  )

BEGIN_DEFINE_TEST( Basic_Test )
  ADD_TEST( Basic_Test, standard ) 
  ADD_TEST( Basic_Test, Gui1 ) 
  
END_DEFINE_TEST( Basic_Test )

USING_CLASS(::acdk::java::, JavaObject);




ACDK_DECL_CLASS(TestListener);
class TestListener
: extends ::acdk::lang::Object
, implements WindowListener
, implements KeyListener
, implements ActionListener
{
  RJavaObject _frame;
  bool _quit;
  int _counter;
public:
  TestListener(IN(RJavaObject) frame)
    : _frame(frame)
    , _quit(false)
    , _counter(0)
  {
  }
  bool doQuit() { return _quit; }

  virtual void actionPerformed(IN(RActionEvent) event)
  {
    RString command = event->getActionCommand();
    System::out->println(RString("Event: ") + event->getID() + " command = " + command);
    _quit = true;
    _frame->invoke("dispose");
  }
  
  virtual void windowActivated(IN(RWindowEvent) event)
  {
    System::out->println(RString("WindowEvent::windowActivated: ") + event->getID());
  }
  virtual void windowClosed(IN(RWindowEvent) event)
  {
    System::out->println(RString("WindowEvent::windowClosed: ") + event->getID());
    _quit = true;
    _frame->invoke("dispose");
  }
  virtual void windowClosing(IN(RWindowEvent) event)
  {
    System::out->println(RString("WindowEvent::windowClosing: ") + event->getID());
    _quit = true;
  }
  virtual void windowDeactivated(IN(RWindowEvent) event)
  {
    System::out->println(RString("WindowEvent::windowDeactivated: ") + event->getID());
  }
  virtual void windowDeiconified(IN(RWindowEvent) event)
  {
    System::out->println(RString("WindowEvent::windowDeiconified: ") + event->getID());
  }
  virtual void windowIconified(IN(RWindowEvent) event)
  {
    System::out->println(RString("WindowEvent::windowIconified: ") + event->getID());
  }
  virtual void windowOpened(IN(RWindowEvent) event)
  {
    System::out->println(RString("WindowEvent::windowOpened: ") + event->getID());
  }
  virtual void keyPressed(IN(RKeyEvent) event)
  {
    int kc = event->getKeyCode();

    char ch = event->invoke("getKeyChar");// using dmi
    System::out->println(RString("keyPressed: [") + _counter + "]: " + event->getID()
                         + " KeyCode=" + kc + " KeyChar: " + ch);
  }
  virtual void keyReleased(IN(RKeyEvent) event) 
  {
    int kc = event->getKeyCode();
    char ch = event->getKeyChar(); // using dmi
    System::out->println(RString("keyReleased: ") + event->getID()
                         + " KeyCode=" + kc + " KeyChar: " + ch);
  }
  virtual void keyTyped(IN(RKeyEvent) event)
  {
    int kc = event->getKeyCode(); // more performanced way
    char ch = event->invoke("getKeyChar"); // using dmi
    System::out->println(RString("keyTyped: ") + event->getID()
                         + " KeyCode=" + kc + " KeyChar: " + ch);
  }
};


void 
Basic_Test::standard()
{  
  if (TestCase::TestInBatchMode == true)
  {
    System::out->println("Skip interactive Test");
    return;
  }
 
#if defined(ACDK_OS_LINUX)
  System::out->println("Using AWT/Swing doesn't work on Linux");
  return;
#endif
#ifndef ACDK_USE_AWT
  RJavaObject frame = new JavaObject("javax/swing/JFrame", (const char*)"Hello from ACDK");
  RJavaObject button = new JavaObject("javax/swing/JButton", (const char*)"Click to Close");
  RJavaObject pane = new JavaObject("javax/swing/JPanel");
#else
  RJavaObject frame = new JavaObject("java/awt/Frame", (const char*)"Hello from ACDK");
  RJavaObject button = new JavaObject("java/awt/Button", (const char*)"Click to Close");
  RJavaObject pane = new JavaObject("java/awt/Panel");
  pane->invoke("setSize", 400, 200);
  button->invoke("setSize", 400, 200);
#endif
  frame->invoke("setSize", 400, 200);
  
  button->invoke("setActionCommand", (const char*)"close");

  pane->invoke("add", &button);
  frame->invoke("setContentPane", &pane);
  frame->invoke("pack");
  RTestListener listener = new TestListener(frame);
  

  button->invoke("addActionListener", ::acdk::java::awt::AWTEvent::createNewListner(&listener));
  frame->invoke("addWindowListener", ::acdk::java::awt::AWTEvent::createNewListner(&listener));
  frame->invoke("addKeyListener", ::acdk::java::awt::AWTEvent::createNewListner(&listener));

  frame->invoke("setVisible", true);

  while (listener->doQuit() == false)
    Thread::sleep(200);
  return;
}

void Basic_Test::Gui1()
{
  /*
  RJavaObject jp1 = new JavaObject("javax/swing/JPanel");
  jp1->invoke("setLayout", new JavaObject("javax/swing/BorderLayout"));
  */
}

} // namespace util
} // namespace acdk
} // namespace tests

