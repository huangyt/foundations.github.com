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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/JavaInterpreter.h,v 1.18 2005/04/08 10:53:20 kommer Exp $


#ifndef acdk_java_JavaInterpreter_h
#define acdk_java_JavaInterpreter_h

#include <acdk.h>
#include "Config.h"
#include <acdk/lang/sys/core_specific.h>



struct JNIEnv_;
typedef JNIEnv_ JNIEnv;
struct JavaVM_;
typedef JavaVM_ JavaVM;

namespace acdk {
namespace java {

using namespace acdk::lang;
using namespace acdk::io;

using acdk::lang::dmi::ScriptVar;
using acdk::lang::dmi::ScriptVarArray;


ACDK_DECL_CLASS(JavaInterpreter);
ACDK_DECL_CLASS(JavaObject);

  /*
#ifdef JNI_VERSION_1_2
#undef JNI_VERSION_1_2
#endif
  */




/** 
  Representing the Java interpreter
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.18 $
  @date $Date: 2005/04/08 10:53:20 $
*/

class ACDK_ACDK_JAVA_PUBLIC JavaInterpreter
: public acdk::lang::Object
{
protected:
#if !defined(ACDK_OS_WIN32)
  JNIEnv* _env;
#else
  ::acdk::lang::sys::specific<JNIEnv*> _env;
#endif
  JavaVM* _jvm;
  
public:
  static JavaInterpreter* curJInt;
  JavaInterpreter();
  virtual ~JavaInterpreter();
  
  /** calls the static main of given class */
  void callMain(IN(RString) clazz, IN(RStringArray) args);
  
  RJavaObject getInstance(IN(RString) classname, ScriptVarArray& args);

  
  void attachCurrentThread();
  JNIEnv* jenv() 
  { 
#if !defined(ACDK_OS_WIN32)
    return _env;
#else
    if (_env.isSet() == false || _env.get() == 0)
    {
      attachCurrentThread();
    }
    return _env.get(); 
#endif
  }
  JavaVM* jvm() { return _jvm; }
  /**
    This method should be called, if should be exit via Java not ACDK
    This is necessary if graphical (swing) functions was called
  */
  static void exitViaJava();
};

ACDK_ACDK_JAVA_PUBLIC RJavaInterpreter getJavaInterpreter();

  
} //namespace java
} // namespace acdk

inline
JNIEnv* getjenv()
{
  return ::acdk::java::getJavaInterpreter()->jenv();
}


#endif // acdk_java_JavaInterpreter_h
