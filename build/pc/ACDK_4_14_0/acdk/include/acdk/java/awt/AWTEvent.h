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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/AWTEvent.h,v 1.7 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_awt_AWTEvent_h
#define acdk_java_awt_AWTEvent_h

#include <acdk/java/JavaObject.h>

#include <acdk/java/acdk2java.h>

namespace acdk {
namespace java {
/**
  Wrapper classes to java.awt package. Experimental
*/
namespace awt {

const jlong ACTION_EVENT_MASK = JLONG_CONSTANT(128);
const jlong ADJUSTMENT_EVENT_MASK = JLONG_CONSTANT(256);
const jlong COMPONENT_EVENT_MASK = JLONG_CONSTANT(1);
const jlong CONTAINER_EVENT_MASK = JLONG_CONSTANT(2);
const jlong FOCUS_EVENT_MASK = JLONG_CONSTANT(4);
const jlong HIERARCHY_BOUNDS_EVENT_MASK = JLONG_CONSTANT(65536);
const jlong HIERARCHY_EVENT_MASK = JLONG_CONSTANT(32768);
const jlong INPUT_METHOD_EVENT_MASK = JLONG_CONSTANT(2048);
const jlong INVOCATION_EVENT_MASK = JLONG_CONSTANT(16384);
const jlong ITEM_EVENT_MASK = JLONG_CONSTANT(512);
const jlong KEY_EVENT_MASK = JLONG_CONSTANT(8);
const jlong MOUSE_EVENT_MASK = JLONG_CONSTANT(16);
const jlong MOUSE_MOTION_EVENT_MASK = JLONG_CONSTANT(32);
const jlong MOUSE_WHEEL_EVENT_MASK = JLONG_CONSTANT(131072);
const jlong PAINT_EVENT_MASK = JLONG_CONSTANT(8192);
const int RESERVED_ID_MAX =1999;
const jlong TEXT_EVENT_MASK = JLONG_CONSTANT(1024);
const jlong WINDOW_EVENT_MASK = JLONG_CONSTANT(64);
const jlong WINDOW_FOCUS_EVENT_MASK = JLONG_CONSTANT(524288);
const jlong WINDOW_STATE_EVENT_MASK = JLONG_CONSTANT(262144);


ACDK_DECL_CLASS(AWTEvent);

enum EventType
{
  AWTEventType,
  ActionEventType,
  WindowEventType,
  KeyEventType,
  MouseEventType
};

class ACDK_ACDK_JAVA_PUBLIC AWTEvent 
: extends ::acdk::java::JavaObject
{
protected:
  int _eventId;
public:
  
  AWTEvent(jobject event, int eid)
  : JavaObject(event)
  , _eventId(eid)
  {
  }
  int getID() { return _eventId; }
  
  static RObject createNewListner(IN(RObject) obj);
  
  //

  /*
  protected  void consume() 
          Consumes this event, if this event can be consumed. 
 int getID() 
          Returns the event type. 
protected  boolean isConsumed() 
          Returns whether this event has been consumed. 
 String paramString() 
          Returns a string representing the state of this Event. 
 void setSource(Object newSource) 
          Retargets an event to a new source. 
 String toString() 
 */
  

};



} // namespace awt 
} // namespace java 
} // namespace acdk 


#endif //acdk_java_awt_AWTEvent_h
