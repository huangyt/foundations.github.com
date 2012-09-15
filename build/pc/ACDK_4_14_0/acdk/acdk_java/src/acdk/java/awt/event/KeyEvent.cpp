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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/event/KeyEvent.cpp,v 1.5 2005/02/05 10:45:11 kommer Exp $

#include "KeyEvent.h"

namespace acdk {
namespace java {
namespace awt {
namespace event {


int 
KeyEvent::getKeyCode()
{
  static JMethod method(jenv(), javaObject(), "getKeyCode", "()I");
  return method.callIntMethod(javaObject());
}

char 
KeyEvent::getKeyChar()
{
  static JMethod method(jenv(), javaObject(), "getKeyChar", "()C");
  return (char)method.callCharMethod(javaObject());
}

int 
KeyEvent::getKeyLocation()
{
  static JMethod method(jenv(), javaObject(), "getKeyLocation", "()I");
  return method.callIntMethod(javaObject());
}

//static 
RString 
KeyEvent::getKeyText(int keyCode)
{
  static JMethod method(getJavaInterpreter()->jenv(), getThisClass(), "getKeyText", "(I)Ljava/lang/String;", true);
  return method.callStringMethod(0, keyCode);
}

bool 
KeyEvent::isActionKey()
{
  static JMethod method(jenv(), javaObject(), "isActionKey", "()Z");
  return method.callBooleanMethod(javaObject());
}

jclass 
KeyEvent::getThisClass()
{
  return getJavaInterpreter()->jenv()->FindClass("acdk/java/awt/event/KeyEvent");
}


} //namespace event
} // namespace awt 
} // namespace java 
} // namespace acdk 



