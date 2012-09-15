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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/event/acdk_java_awt_event_AwtListener.cpp,v 1.9 2005/04/25 13:20:46 kommer Exp $

#include <acdk.h>
#include "acdk_java_awt_event_AwtListener.h"
#include <acdk/java/JavaObject.h>
#include <acdk/java/acdk2java.h>
#include "ActionListener.h"
#include "WindowListener.h"
#include "KeyListener.h"
#include "MouseListener.h"
#include "AWTEventListener.h"
#include "KeyEvent.h"
#include "ActionEvent.h"
#include "WindowEvent.h"
#include "MouseEvent.h"

using namespace acdk::java::awt::event;
using namespace acdk::java;

/*
 * Class:     acdk_java_awt_event_AwtListener
 * Method:    actionPerformedN
 * Signature: (Ljava/awt/AWTEvent;I)V
 */
extern "C" JNIEXPORT void JNICALL Java_acdk_java_awt_event_AwtListener_actionPerformedN
  (JNIEnv* jenv, jobject jobj, jobject event, jint eid)
{
  JNIEnv* senv = getjenv();
  RObject obj = ::acdk::java::getObjectHandle(jenv, jobj);
  ::acdk::java::awt::event::RActionEvent awtevent = new ::acdk::java::awt::event::ActionEvent(event, eid);
  ::acdk::java::awt::event::RActionListener alistner = (::acdk::java::awt::event::RActionListener)obj;
  alistner->actionPerformed(awtevent);
}

/*
 * Class:     acdk_java_awt_event_AwtListener
 * Method:    eventDispatchedN
 * Signature: (I)V
 */

extern "C" JNIEXPORT void JNICALL Java_acdk_java_awt_event_AwtListener_eventDispatchedN
  (JNIEnv* jenv, jobject jobj, jobject jevent, jint eid)
{
  RObject obj = ::acdk::java::getObjectHandle(jenv, jobj);
  ::acdk::java::awt::RAWTEvent awtevent = new ::acdk::java::awt::AWTEvent(jevent, eid);
  ::acdk::java::awt::event::RAWTEventListener listener = (::acdk::java::awt::event::RAWTEventListener)obj;
  listener->eventDispatched(awtevent);
}


/*
 * Class:     acdk_java_awt_event_AwtListener
 * Method:    windowEventN
 * Signature: (I)V
 */
extern "C" JNIEXPORT void JNICALL Java_acdk_java_awt_event_AwtListener_windowEventN
  (JNIEnv* jenv, jobject jobj, jobject jevent, jint eid)
{
  RObject obj = ::acdk::java::getObjectHandle(jenv, jobj);
  ::acdk::java::awt::event::RWindowEvent awtevent = new ::acdk::java::awt::event::WindowEvent(jevent, eid);
  ::acdk::java::awt::event::RWindowListener listener = (::acdk::java::awt::event::RWindowListener)obj;
  
  switch(eid)
  {
  case WINDOW_ACTIVATED:
    listener->windowActivated(awtevent);
    break;
  case WINDOW_CLOSED:
    listener->windowClosed(awtevent);
    break;
  case WINDOW_CLOSING:
    listener->windowClosing(awtevent);
    break;
  case WINDOW_DEACTIVATED:
    listener->windowDeactivated(awtevent);
    break;
  case WINDOW_DEICONIFIED:
    listener->windowDeiconified(awtevent);
    break;
  case WINDOW_ICONIFIED:
    listener->windowIconified(awtevent);
    break;
    break;
  case WINDOW_OPENED:
    listener->windowOpened(awtevent);
    break;
  }
}

/*
 * Class:     acdk_java_awt_event_AwtListener
 * Method:    keyEventN
 * Signature: (Ljava/awt/AWTEvent;I)V
 */
JNIEXPORT void JNICALL Java_acdk_java_awt_event_AwtListener_keyEventN
    (JNIEnv* jenv, jobject jobj, jobject jevent, jint eid)
{
  JStackFrame jstack(jenv, 128); 
  RObject obj = ::acdk::java::getObjectHandle(jenv, jobj);
  ::acdk::java::awt::event::RKeyEvent event = new ::acdk::java::awt::event::KeyEvent(jevent, eid);
  ::acdk::java::awt::event::RKeyListener listener = (::acdk::java::awt::event::RKeyListener)obj;
  switch (eid)
  {
  case KEY_PRESSED:
    listener->keyPressed(event);
    break;
  case KEY_RELEASED:
    listener->keyReleased(event);
    break;
  case KEY_TYPED:
    listener->keyTyped(event);
    break;
  }
}

/*
 * Class:     acdk_java_awt_event_AwtListener
 * Method:    mouseEventN
 * Signature: (Ljava/awt/AWTEvent;I)V
 */
JNIEXPORT void JNICALL Java_acdk_java_awt_event_AwtListener_mouseEventN
    (JNIEnv* jenv, jobject jobj, jobject jevent, jint eid)
{
  RObject obj = ::acdk::java::getObjectHandle(jenv, jobj);
  ::acdk::java::awt::event::RMouseEvent awtevent = new ::acdk::java::awt::event::MouseEvent(jevent, eid);
  ::acdk::java::awt::event::RMouseListener listener = (::acdk::java::awt::event::RMouseListener)obj;

  switch (eid)
  {
  case AWT_MOUSE_CLICKED:
    listener->mouseClicked(awtevent);
    break;
  case AWT_MOUSE_ENTERED:
    listener->mouseEntered(awtevent);
    break;
  case AWT_MOUSE_EXITED:
    listener->mouseExited(awtevent);
    break;
  case AWT_MOUSE_PRESSED:
    listener->mousePressed(awtevent);
    break;
  case AWT_MOUSE_RELEASED:
    listener->mouseReleased(awtevent);
    break;
  }
}

