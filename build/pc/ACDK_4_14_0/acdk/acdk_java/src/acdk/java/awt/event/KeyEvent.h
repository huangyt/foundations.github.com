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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/event/KeyEvent.h,v 1.5 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_awt_event_KeyEvent_h
#define acdk_java_awt_event_KeyEvent_h

#include <acdk/java/JavaObject.h>

#include <acdk/java/acdk2java.h>
#include <acdk/java/awt/AWTEvent.h>

namespace acdk {
namespace java {
namespace awt {
namespace event {


ACDK_DECL_CLASS(KeyEvent);

class ACDK_ACDK_JAVA_PUBLIC KeyEvent
: extends AWTEvent 
{
  
public:
  KeyEvent(jobject event, int eid)
  : AWTEvent(event, eid)
  {
  }
  int getKeyCode();
  char getKeyChar();
  int getKeyLocation();
  static RString getKeyText(int keyCode);
  bool isActionKey();
  static jclass getThisClass();
};

} //namespace event


} // namespace awt 
} // namespace java 
} // namespace acdk 


#endif //acdk_java_awt_event_KeyEvent_h
