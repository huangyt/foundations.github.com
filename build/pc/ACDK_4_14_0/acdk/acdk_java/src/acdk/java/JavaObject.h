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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/JavaObject.h,v 1.22 2005/03/14 17:59:13 kommer Exp $


#ifndef acdk_java_JavaObject_h
#define acdk_java_JavaObject_h

#include <acdk.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include "Config.h"

#include "jniext.h"
#include "JavaInterpreter.h"

namespace acdk {
/**
  Implements the utilisation of a Java 1.2 virtual machine
*/
namespace java {

using namespace acdk::lang;

using namespace acdk::lang::dmi;


ACDK_DECL_CLASS(JavaObject);
ACDK_DECL_CLASS(JavaInterpreter);
/**
  Implements the DMI-Interface for
  Java object as DMI server.
  A little Hello sample using the java class java.lang.StringBuffer
  @code
    RJavaObject jo = new JavaObject("java/lang/StringBuffer", new String("Hello Java "));
    jo->invoke("append", new String(" from ACDK"));
    RString erg = (RString)jo->invoke("toString");
  @codeend
  @see also 
*/

class ACDK_ACDK_JAVA_PUBLIC JavaObject
: public acdk::lang::Object
{
  //ACDK_WITH_METAINFO
protected:
  foreign RJavaInterpreter _env;
  foreign JObjectImpl<jobject, JGlobalRes> _jobj;
  foreign JObjectImpl<jclass, JGlobalRes> _jclass;

public:
  /// not using the standard meta info, becuase this class is used as a proxy 
  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return clazzInfo(); } 
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return &_clazzInfo; } 
  virtual ::acdk::lang::RClass getClass() { return GetClass(); }
  static ::acdk::lang::RClass GetClass() { return ::acdk::lang::Class::getSingeltonClass(clazzInfo()); }
  static ::acdk::lang::RObject create_array(int length = 0) { return Nil; }
  static ::acdk::lang::RObject create_array_array(int firstLength = 0, int secondLength = 0) { return Nil; }
  virtual void getCollectableFields(FieldReferences& fields) { }
  virtual ::acdk::lang::dmi::SysFields getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz = 0) 
  { return ::acdk::lang::dmi::SysFields(); }
  static const ClazzMethodInfo* _invoke_dynamic(  ::acdk::lang::Object* This, 
                                                          IN(RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf); 
  static const ClazzMethodInfo* _invoke_static(  IN(RString) fname, 
                                          ScriptVar& ret, 
                                          ScriptVarArray& args, 
                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf); 
  /*
  virtual  const ::acdk::lang::dmi::ClazzMethodInfo* standardDispatch(  const char* fname, 
                                                          ::acdk::lang::dmi::ScriptVar& ret, 
                                                          ::acdk::lang::dmi::ScriptVarArray& args, 
                                                          ::acdk::lang::dmi::DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0,
                                                          const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0);
  static const ::acdk::lang::dmi::ClazzMethodInfo* StandardDispatch(const char* fname, ::acdk::lang::dmi::ScriptVar& ret, 
                                                                ::acdk::lang::dmi::ScriptVarArray& args, 
                                                                ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, 
                                                                int flags, 
                                                                const ::acdk::lang::dmi::ClazzInfo* clazzinfo = 0,
                                                                const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0);
*/
  virtual ::acdk::lang::dmi::ScriptVar getMember(IN(RString) fieldname, ::acdk::lang::dmi::DmiClient& dc, int flags, const ::acdk::lang::dmi::ClazzInfo* type_requested = 0);
  virtual void setMember(IN(RString) fieldname, const ::acdk::lang::dmi::ScriptVar& newval, ::acdk::lang::dmi::DmiClient& dc, int flags);

  static ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient gJavaDmiClient;
  ::acdk::lang::dmi::DmiClient& getDmiClient()
  { 
    return gJavaDmiClient;
  }
private:   
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo;
  
public:
  JavaObject(jobject jobj);
/**
    Create an JavaObject as Wrapper to an existant jobject
    Used internally to wrapp arguments
  */
  JavaObject(IN(RJavaInterpreter) env, jobject jobj);
  /**
    create Object calling the JNI constructor with given args
  */
  JavaObject(IN(RJavaInterpreter) env, IN(RString) clsname, ScriptVarArray& args)
  : _env(env)
  , _jobj(_env->jenv())
  , _jclass(_env->jenv())
  {
    _jobj = getNewJObject(_env->jenv(), clsname, args);
  }
  /**
    create Object calling the JNI constructor with given args
  */
  JavaObject(IN(RString) clsname, ScriptVarArray& args)
  : _env(getJavaInterpreter())
  , _jobj(_env->jenv())
  , _jclass(_env->jenv())
  {
    _jobj = getNewJObject(_env->jenv(), clsname, args);
  }
  /**
    create Object calling the JNI constructor with given args
  */
  JavaObject(IN(RString) clsname, ScriptVar va1 = ScriptVar(), 
                                           ScriptVar va2 = ScriptVar(), 
                                           ScriptVar va3 = ScriptVar(), 
                                           ScriptVar va4 = ScriptVar(), 
                                           ScriptVar va5 = ScriptVar());
  virtual ~JavaObject();
  /**
    create Object calling the JNI constructor with given args
  */
  static RJavaObject getInstance(IN(RJavaInterpreter) env, IN(RString) classname, ScriptVarArray& args);
  /** 
    call a non static method of the Java class
  */
  virtual ScriptVar invoke(IN(RString) methodname, ScriptVarArray& args);
  
  /** 
    call a non static method of the Java class
  */
  ScriptVar invoke(IN(RString) methodname, ScriptVar va1 = ScriptVar(), 
                                           ScriptVar va2 = ScriptVar(), 
                                           ScriptVar va3 = ScriptVar(), 
                                           ScriptVar va4 = ScriptVar(), 
                                           ScriptVar va5 = ScriptVar());
  
  static ScriptVar invoke_static(IN(RString) classname, IN(RString) methodname, 
                                           ScriptVar va1 = ScriptVar(), 
                                           ScriptVar va2 = ScriptVar(), 
                                           ScriptVar va3 = ScriptVar(), 
                                           ScriptVar va4 = ScriptVar(), 
                                           ScriptVar va5 = ScriptVar());
  /** 
    call a static method of the Java class
  */
  static ScriptVar invoke_static(IN(RJavaInterpreter) env, IN(RString) classname, IN(RString) methodname, ScriptVarArray& args);
  
  //virtual ScriptVar peek(IN(RString) fieldname);
  
  static ScriptVar peek_static(IN(RJavaInterpreter) env, IN(RString) classname, IN(RString) fieldname);
  static ScriptVar peek_static(IN(RString) classname, IN(RString) fieldname)
  {
    return peek_static(getJavaInterpreter(), classname, fieldname);
  }
  //virtual void poke(IN(RString) fieldname, ScriptVar& val);
  static void poke_static(IN(RJavaInterpreter) env, IN(RString) classname, IN(RString) fieldname, const ScriptVar& val);
  static void poke_static(IN(RString) classname, IN(RString) fieldname, const ScriptVar& val)
  {
    poke_static(getJavaInterpreter(), classname, fieldname, val);
  }
  
  virtual jobject javaObject() { return _jobj; }
  virtual RString toString();

  jclass getJObjectClass();

  static jobject getNewJObject(JNIEnv* env, IN(RString) classname, ScriptVarArray& args);
  JNIEnv* jenv() { return _env->jenv(); }
};

} //namespace java
} // namespace acdk

#endif // acdk_java_JavaObject_h
