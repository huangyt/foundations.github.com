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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/event/MouseListener.h,v 1.5 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_awt_event_MouseListener_h
#define acdk_java_awt_event_MouseListener_h

#include <acdk.h>
#include <acdk/java/Config.h>
#include <acdk/java/JavaObject.h>
#include "MouseEvent.h"

namespace acdk {
namespace java {
namespace awt {
namespace event {

const int BUTTON1 = 1;
const int BUTTON2 = 2;
const int BUTTON3 = 3;
const int AWT_MOUSE_CLICKED = 500;
const int AWT_MOUSE_DRAGGED = 506;
const int AWT_MOUSE_ENTERED = 504;
const int AWT_MOUSE_EXITED = 505;
const int AWT_MOUSE_FIRST = 500;
const int AWT_MOUSE_LAST = 507;
const int AWT_MOUSE_MOVED = 503;
const int AWT_MOUSE_PRESSED = 501;
const int AWT_MOUSE_RELEASED = 502;
const int AWT_MOUSE_WHEEL = 507;
const int NOBUTTON = 0;




ACDK_DECL_INTERFACE(MouseListener);

class ACDK_ACDK_JAVA_PUBLIC MouseListener
      ACDK_INTERFACEBASE
{
public:
  virtual void mouseClicked(IN(RMouseEvent) event) = 0;
  virtual void mouseEntered(IN(RMouseEvent) event) = 0;
  virtual void mouseExited(IN(RMouseEvent) event) = 0;
  virtual void mousePressed(IN(RMouseEvent) event) = 0;
  virtual void mouseReleased(IN(RMouseEvent) event) = 0;
};

} // namespace event 
} // namespace awt 
} // namespace java 
} // namespace acdk 


#endif //acdk_java_awt_event_MouseListener_h
