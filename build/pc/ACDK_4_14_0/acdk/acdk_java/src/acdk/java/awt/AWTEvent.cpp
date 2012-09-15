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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/awt/AWTEvent.cpp,v 1.5 2005/02/05 10:45:11 kommer Exp $

#include "AWTEvent.h"

namespace acdk {
namespace java {
namespace awt {

//static 
RObject 
AWTEvent::createNewListner(IN(RObject) obj)
{
  JNIEnv* jenv = getJavaInterpreter()->jenv();
  jclass jcls = jenv->FindClass("acdk/java/awt/event/AwtListener");
  jvalue jargs[2]; 
  jargs[0].i = (int)&obj;
  static JMethod AwtListner_AwtListner(jenv, jcls, "<init>", "(I)V", true);
  jobject jobj = jenv->NewObjectA(jcls, AwtListner_AwtListner.methodId(), jargs);
  //setObjectHandle(jenv, jobj, obj);
  obj->addRef();
  return new JavaObject(jobj);
}


} // namespace awt 
} // namespace java 
} // namespace acdk 



