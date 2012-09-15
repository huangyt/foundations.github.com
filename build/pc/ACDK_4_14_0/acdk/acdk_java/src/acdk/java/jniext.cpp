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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/jniext.cpp,v 1.8 2005/04/25 13:20:46 kommer Exp $


#include "jniext.h"

namespace acdk {
namespace java {

RString 
Class_getName(JNIEnv* env, jobject t)
{
  static JMethod Class_getNameF(env, t, "getName", "()Ljava/lang/String;");
  JString cname(env);
  cname = (jstring)Class_getNameF.callObjectMethod(t);
  return cname.toString();
}


jobject 
Class_getField(JNIEnv* env, jobject t, IN(RString) name)
{
  static JMethod Class_getFieldF(env, t, "getField", "(Ljava/lang/String;)Ljava/lang/reflect/Field;");
  JString jstr(env, name);

  jobject field = Class_getFieldF.callObjectMethod(t, (jstring)jstr);
  if (field == 0) 
  {
    // ####
  }
  return field;
}

bool isString(JNIEnv* env, jobject obj)
{
  typedef JClassImpl<JGlobalRes> JGlobalClass;
  static JGlobalClass cls = JGlobalClass::findClass(env, "java/lang/String");
  return env->IsAssignableFrom(JClass::getObjectClass(env, obj), cls);
}

}
}
