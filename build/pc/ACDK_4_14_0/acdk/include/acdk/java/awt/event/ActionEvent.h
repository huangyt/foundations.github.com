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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/event/ActionEvent.h,v 1.6 2005/04/10 12:52:40 kommer Exp $

#ifndef acdk_java_awt_event_ActionEvent_h
#define acdk_java_awt_event_ActionEvent_h

#include <acdk/java/JavaObject.h>

#include <acdk/java/acdk2java.h>

#include "../AWTEvent.h"

namespace acdk {
namespace java {
namespace awt {
/**
  some experimental C++ wrapper for the Java AWT events classes
*/    
namespace event {

ACDK_DECL_CLASS(ActionEvent);

class ACDK_ACDK_JAVA_PUBLIC ActionEvent
: extends AWTEvent 
{
public:
  ActionEvent(jobject event, int eid)
  : AWTEvent(event, eid)
  {
  }
  RString getActionCommand();
};



} //namespace event
} // namespace awt 
} // namespace java 
} // namespace acdk 


#endif //acdk_java_awt_event_ActionEvent_h
