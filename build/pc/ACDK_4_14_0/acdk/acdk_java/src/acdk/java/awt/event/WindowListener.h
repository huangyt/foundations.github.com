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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/event/WindowListener.h,v 1.5 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_awt_event_WindowListener_h
#define acdk_java_awt_event_WindowListener_h

#include <acdk.h>
#include <acdk/java/Config.h>
#include <acdk/java/JavaObject.h>
#include "WindowEvent.h"

namespace acdk {
namespace java {
namespace awt {
namespace event {

//enum WindowEventTypes
const int WINDOW_ACTIVATED = 205;
const int WINDOW_CLOSED = 202;
const int WINDOW_CLOSING = 201;
const int WINDOW_DEACTIVATED = 206;
const int WINDOW_DEICONIFIED = 204;
const int WINDOW_FIRST = 200;
const int WINDOW_GAINED_FOCUS = 207;
const int WINDOW_ICONIFIED = 203;
const int WINDOW_LAST = 209;
const int WINDOW_LOST_FOCUS = 208;
const int WINDOW_OPENED = 200;
const int WINDOW_STATE_CHANGED = 209;



ACDK_DECL_INTERFACE(WindowListener);

class ACDK_ACDK_JAVA_PUBLIC WindowListener
      ACDK_INTERFACEBASE
{
public:
  virtual void windowActivated(IN(RWindowEvent) event) = 0;
  virtual void windowClosed(IN(RWindowEvent) event) = 0;
  virtual void windowClosing(IN(RWindowEvent) event) = 0;
  virtual void windowDeactivated(IN(RWindowEvent) event) = 0;
  virtual void windowDeiconified(IN(RWindowEvent) event) = 0;
  virtual void windowIconified(IN(RWindowEvent) event) = 0;
  virtual void windowOpened(IN(RWindowEvent) event) = 0;
};

} // namespace event 
} // namespace awt 
} // namespace java 
} // namespace acdk 


#endif //acdk_java_awt_event_WindowListener_h
