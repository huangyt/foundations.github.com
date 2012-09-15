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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/acdk2java.cpp,v 1.22 2005/04/25 13:20:46 kommer Exp $

#include "acdk2java.h"
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>

#include <acdk/lang/Error.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/ClassNotFoundException.h>
#include <acdk/lang/IllegalAccessException.h>
#include <acdk/lang/NullPointerException.h>
#include <acdk/lang/IndexOutOfBoundsException.h>

#include "JavaObject.h"
#include <acdk/lang/dmi/ScriptVar.h>
#include <acdk/io/EOFException.h>


namespace acdk {
namespace java {

using namespace ::acdk::lang::dmi;

jclass 
getAcdkObjectClass(JNIEnv *env)
{
  return env->FindClass("acdk/java/AcdkObject");
}

Object* getObjectHandle(JNIEnv* env, jobject obj)
{
  static JField jfield(env, env->GetObjectClass(obj), "objectHandle", "I");
  int handle = jfield.getInt(obj);
  // ### if handle == 0
  return (Object*)handle;
}

void setObjectHandle(JNIEnv* env, jobject jobj, IN(RObject) obj)
{
  Object* optr = obj;
  if (optr != 0)
    optr->addRef();
  static JField jfield(env, env->GetObjectClass(jobj), "objectHandle", "I");
  jfield.setInt(jobj, (int)optr);
}

jobject createNewAcdkObject(JNIEnv* env, jclass jcls, IN(RObject) obj)
{
  jvalue jargs[1]; 
  static JMethod AcdkObject_AcdkObject(env, jcls, "<init>", "()V");
  jobject jobj = env->NewObjectA(jcls, AcdkObject_AcdkObject.methodId(), jargs);
  setObjectHandle(env, jobj, obj);
  return jobj;
}

void
java2acdk(JNIEnv *env, jobject jobj, ScriptVar& sa)
{
  if (env->IsSameObject(jobj, 0) == JNI_TRUE)
  {
    sa = Nil;
    return;
  }
  JClass cls(env, env->GetObjectClass(jobj));
  RString cname = cls.getName();
  //std::cerr << "jarg: " << cname->toString()->c_str() << sys::eofl;
  if (cname->equals("java.lang.String") == true)
  {
    JString jstr(env, (jstring)jobj);
    RString str = jstr.toString();
    sa = (RObject)str;
    return;
  } 
  
  if (cname->equals("java.lang.Boolean") == true) {
    static JMethod getValue(env, jobj, "booleanValue", "()Z");
    RObject obj = new Boolean(getValue.callBooleanMethod(jobj));
    sa = obj;
    return;
  }
  if (cname->equals("java.lang.Character") == true) {
    static JMethod getValue(env, jobj, "charValue", "()C");
    RObject obj = new Character(getValue.callCharMethod(jobj));
    sa = obj;
    return;
  }
  if (cname->equals("java.lang.Byte") == true) {
    static JMethod getValue(env, jobj, "byteValue", "()B");
    RObject obj = new Character(getValue.callByteMethod(jobj));
    sa = obj;
    return;
  }
  if (cname->equals("java.lang.Short") == true) {
    static JMethod getValue(env, jobj, "shortValue", "()S");
    RObject obj = new Short(getValue.callShortMethod(jobj));
    sa = obj;
    return;
  }
  if (cname->equals("java.lang.Integer") == true) {
    static JMethod getInt(env, jobj, "intValue", "()I");
    RObject obj = new Integer(getInt.callIntMethod(jobj));
    sa = obj;
    return;
  }
  if (cname->equals("java.lang.Long") == true) {
    static JMethod getValue(env, jobj, "longValue", "()J");
    RObject obj = new Long(getValue.callLongMethod(jobj));
    sa = obj;
    return;
  }
  if (cname->equals("java.lang.Float") == true) {
    static JMethod getValue(env, jobj, "floatValue", "()F");
    RObject obj = new Float(getValue.callFloatMethod(jobj));
    sa = obj;
    return;
  }
  if (cname->equals("java.lang.Double") == true) {
    static JMethod getValue(env, jobj, "DoubleValue", "()D");
    RObject obj = new Double(getValue.callDoubleMethod(jobj));
    sa = obj;
    return;
  }
  if (cname->equals("acdk.java.AcdkObject") == true) {
    JClass jcls = JClass::getObjectClass(env, jobj);
    static JField objectHandle(env, jcls, "objectHandle", "I", false);
    int oh = objectHandle.getInt(jobj);
    RObject obj = (Object*)oh;
    sa  = obj;
    return;
  }
  RObject obj = new JavaObject(getJavaInterpreter(), jobj);
  sa = obj;
}


RObject 
jobject2aobject(JNIEnv *env, jobject jobj)
{
  ScriptVar erg;
  java2acdk(env, jobj, erg);
  return erg.getObjectVar();
}


jobject aobject2jobject(JNIEnv* env, IN(RObject) obj)
{
  if (instanceof(obj, String) == true)
  {
    RString str = (RString)obj;
    JString jstr(env, str);
    return jstr;
  }
  if (instanceof(obj, JavaObject) == true)
  {
    RJavaObject aobj = (RJavaObject)obj;
    return aobj->javaObject();
  }
  if (instanceof(obj, Boolean) == true)
  {
    JClass jcls = JClass::findClass(env, "java/lang/Boolean");
    static JMethod constructor(env, jcls, "<init>", "(Z)V");
    jvalue jargs[2]; 
    jargs[0].z = RBoolean(obj)->booleanValue();
    return env->NewObjectA(jcls, constructor.methodId(), jargs);
  }
  if (instanceof(obj, Character) == true)
  {
    JClass jcls = JClass::findClass(env, "java/lang/Character");
    static JMethod constructor(env, jcls, "<init>", "(C)V");
    jvalue jargs[2]; 
    jargs[0].c = RCharacter(obj)->charValue();
    return env->NewObjectA(jcls, constructor.methodId(), jargs);
  }
  if (instanceof(obj, Byte) == true)
  {
    JClass jcls = JClass::findClass(env, "java/lang/Byte");
    static JMethod constructor(env, jcls, "<init>", "(B)V");
    jvalue jargs[2]; 
    jargs[0].b = RByte(obj)->byteValue();
    return env->NewObjectA(jcls, constructor.methodId(), jargs);
  }
  if (instanceof(obj, Short) == true)
  {
    JClass jcls = JClass::findClass(env, "java/lang/Short");
    static JMethod constructor(env, jcls, "<init>", "(S)V");
    jvalue jargs[2]; 
    jargs[0].s = RShort(obj)->shortValue();
    return env->NewObjectA(jcls, constructor.methodId(), jargs);
  }
  if (instanceof(obj, Integer) == true)
  {
    JClass jcls = JClass::findClass(env, "java/lang/Integer");
    static JMethod constructor(env, jcls, "<init>", "(I)V");
    jvalue jargs[2]; 
    jargs[0].i = RInteger(obj)->intValue();
    return env->NewObjectA(jcls, constructor.methodId(), jargs);
  }
  if (instanceof(obj, Long) == true)
  {
    JClass jcls = JClass::findClass(env, "java/lang/Long");
    static JMethod constructor(env, jcls, "<init>", "(J)V");
    jvalue jargs[2]; 
    jargs[0].j = RLong(obj)->longValue();
    return env->NewObjectA(jcls, constructor.methodId(), jargs);
  }
  if (instanceof(obj, Float) == true)
  {
    JClass jcls = JClass::findClass(env, "java/lang/Float");
    static JMethod constructor(env, jcls, "<init>", "(F)V");
    jvalue jargs[2]; 
    jargs[0].f = RFloat(obj)->floatValue();
    return env->NewObjectA(jcls, constructor.methodId(), jargs);
  }
  if (instanceof(obj, Double) == true)
  {
    JClass jcls = JClass::findClass(env, "java/lang/Double");
    static JMethod constructor(env, jcls, "<init>", "(D)V");
    jvalue jargs[2]; 
    jargs[0].d = RDouble(obj)->doubleValue();
    return env->NewObjectA(jcls, constructor.methodId(), jargs);
  }
  
  if (instanceof(obj, JavaObject) == true)
  {
    return RJavaObject(obj)->javaObject();
  }
  return createNewAcdkObject(env, getAcdkObjectClass(env), obj);
}

jobject 
acdk2jobject(JNIEnv *env, const ScriptVar& sa)
{
  switch (sa.type) 
  {
  case ScriptVar::UnknownType: return 0;
  
  case ScriptVar::BoolType:
  case ScriptVar::BoolRefType:
  {
    jvalue jargs[2]; 
    jargs[0].z = sa.getBoolVar();
    jclass jcls = env->FindClass("java/lang/Boolean");
    static JMethod NewJObjectFunc(env, jcls, "<init>", "(Z)V", true);
    return env->NewObjectA(jcls, NewJObjectFunc.methodId(), jargs);
  }
  case ScriptVar::CharType:
  case ScriptVar::CharRefType:
  {
    jvalue jargs[2]; 
    jargs[0].c = (short)sa.getCharVar();
    jclass jcls = env->FindClass("java/lang/Character");
    static JMethod NewJObjectFunc(env, jcls, "<init>", "(C)V", true);
    return env->NewObjectA(jcls, NewJObjectFunc.methodId(), jargs);
  }
  case ScriptVar::ByteType:
  case ScriptVar::ByteRefType:
  {
    jvalue jargs[2]; 
    jargs[0].b = sa.getByteVar();
    jclass jcls = env->FindClass("java/lang/Byte");
    static JMethod NewJObjectFunc(env, jcls, "<init>", "(B)V", true);
    return env->NewObjectA(jcls, NewJObjectFunc.methodId(), jargs);
  }
  case ScriptVar::ShortType:
  case ScriptVar::ShortRefType:
  {
    jvalue jargs[2]; 
    jargs[0].s = sa.getShortVar();
    jclass jcls = env->FindClass("java/lang/Short");
    static JMethod NewJObjectFunc(env, jcls, "<init>", "(S)V", true);
    return env->NewObjectA(jcls, NewJObjectFunc.methodId(), jargs);
  }
  case ScriptVar::IntType:
  case ScriptVar::IntRefType:
  {
    jvalue jargs[2]; 
    jargs[0].i = sa.getIntVar();
    jclass jcls = env->FindClass("java/lang/Integer");
    static JMethod AcdkObject_AcdkObject(env, jcls, "<init>", "(I)V", true);
    return env->NewObjectA(jcls, AcdkObject_AcdkObject.methodId(), jargs);
  }
  case ScriptVar::LongType:
  case ScriptVar::LongRefType:
  {
    jvalue jargs[2]; 
    jargs[0].j = sa.getLongVar();
    jclass jcls = env->FindClass("java/lang/Long");
    static JMethod AcdkObject_AcdkObject(env, jcls, "<init>", "(J)V", true);
    return env->NewObjectA(jcls, AcdkObject_AcdkObject.methodId(), jargs);
  }
  case ScriptVar::FloatType:
  case ScriptVar::FloatRefType:
  {
    jvalue jargs[2]; 
    jargs[0].f = sa.getFloatVar();
    jclass jcls = env->FindClass("java/lang/Float");
    static JMethod NewJObjectFunc(env, jcls, "<init>", "(F)V", true);
    return env->NewObjectA(jcls, NewJObjectFunc.methodId(), jargs);
  }
  case ScriptVar::DoubleType:
  case ScriptVar::DoubleRefType:
  {
    jvalue jargs[2]; 
    jargs[0].d = sa.getDoubleVar();
    jclass jcls = env->FindClass("java/lang/Double");
    static JMethod NewJObjectFunc(env, jcls, "<init>", "(D)V", true);
    return env->NewObjectA(jcls, NewJObjectFunc.methodId(), jargs);
  }
  case ScriptVar::ObjectType: 
  case ScriptVar::ObjectRefType:
    return aobject2jobject(env, sa.getObjectVar());
  default:
    // ####
    return 0;
  }
}

void 
java2acdk(JNIEnv *env, JObjectArray& arguments, ScriptVarArray& sargs)
{
  for (int i = 0; i < sargs.size(); ++i)
  {
    java2acdk(env, arguments[i], sargs[i]);
  }
}

bool acdk2java(JNIEnv *jenv, jclass jcls, ScriptVar& marg, jvalue& jarg)
{
  JClass cls(jenv, jcls);
  RString jtypenam = cls.getName();
  
  if (jtypenam->equals("boolean") == true) {
    jarg.z = marg.getBoolVar();
    return true;
  }
  if (jtypenam->equals("char") == true) {
    jarg.c = marg.getCharVar();
    return true;
  }
  if (jtypenam->equals("byte") == true) {
    jarg.b = marg.getByteVar();
    return true;
  }
  if (jtypenam->equals("short") == true) {
    jarg.s = marg.getShortVar();
    return true;
  }
   
  if (jtypenam->equals("int") == true) {
    jarg.i = marg.getIntVar();
    return true;
  }
  if (jtypenam->equals("long") == true) {
    jarg.j = marg.getLongVar();
    return true;
  }
  if (jtypenam->equals("float") == true) {
    jarg.f = marg.getFloatVar();
    return true;
  }
  if (jtypenam->equals("double") == true) {
    jarg.d = marg.getDoubleVar();
    return true;
  }
  if (marg.type == ScriptVar::ObjectType) {
    RObject obj = marg.getObjectVar();
    if (instanceof(obj, String) == true) {
      jarg.l = jenv->NewStringUTF(RString(obj)->c_str());
      return true;
    }
    if (instanceof(obj, JavaObject) == true) {
      jarg.l = RJavaObject(obj)->javaObject();
      return true;
    }
  }
  return false;
}

bool
acdk2java(JNIEnv *jenv, jobjectArray classarray, ScriptVarArray& margs, jvalue* jargs)
{
  
  for (int i = 0; i < margs.size(); i++) {
    jclass param = (jclass)jenv->GetObjectArrayElement(classarray, i);          
    if (acdk2java(jenv, param, margs[i], jargs[i]) == false)
      return false;
  }
  return true;
}

bool
acdk2java(JNIEnv *env, ScriptVar& marg, jvalue& jarg)
{
  switch (marg.type) {
  case ScriptVar::BoolType : jarg.z = marg.var.boolVal; return true;
  case ScriptVar::BoolRefType : jarg.z = *marg.var.boolRef; return true;
  case ScriptVar::CharType : jarg.c = marg.var.charVal; return true;
  case ScriptVar::CharRefType : jarg.c = *marg.var.charRef; return true;
  case ScriptVar::UcCharType : jarg.c = marg.var.uccharVal; return true;
  case ScriptVar::UcCharRefType : jarg.c = *marg.var.uccharRef; return true;
  case ScriptVar::ByteType : jarg.b = marg.var.byteVal; return true; 
  case ScriptVar::ByteRefType : jarg.b = *marg.var.byteRef; return true; 
  case ScriptVar::ShortType : jarg.s = marg.var.shortVal; return true;
  case ScriptVar::ShortRefType : jarg.s = *marg.var.shortRef; return true;
  case ScriptVar::IntType : jarg.i = marg.var.intVal; return true; 
  case ScriptVar::IntRefType : jarg.i = *marg.var.intRef; return true; 
  case ScriptVar::LongType : jarg.j = marg.var.longVal; return true; 
  case ScriptVar::LongRefType : jarg.j = *marg.var.longRef; return true; 
  case ScriptVar::FloatType : jarg.f = marg.var.floatVal; return true;
  case ScriptVar::FloatRefType : jarg.f = *marg.var.floatRef; return true;
  case ScriptVar::DoubleType : jarg.d = marg.var.doubleVal; return true;
  case ScriptVar::DoubleRefType : jarg.d = *marg.var.doubleRef; return true;
  case ScriptVar::ObjectRefType :
  case ScriptVar::ObjectType : {
    RObject obj = marg.getObjectRef();
    if (obj == Nil) {
      jarg.l = 0;
      return true;
    }
    if (instanceof(obj, String) == true) {
      jarg.l = env->NewStringUTF(RString(obj)->c_str());
      return true;
    }
    if (instanceof(obj, JavaObject) == true) {
      jarg.l = RJavaObject(obj)->javaObject();
      return true;
    }
    // has no wrapper at the moment for ACDK
    return false;
  }
  case ScriptVar::UnknownType :
    // oops;
    break;
  }
  return false;

}

bool 
acdk2java(JNIEnv *env, ScriptVarArray& margs, jvalue* jargs)
{
  for (int i = 0; i < margs.size(); i++) {
    if (acdk2java(env, margs[i], jargs[i]) == false)
      return false;
  }
  return true;
}

#if 0

//virtual 
int 
JavaDmiClient::typeDistance(const ScriptVar& arg, const ClazzInfo* toType)
{
   return typeDistance(arg.getClazzInfo(), toType);

  int erg = AcdkDmiClient::typeDistance(arg, toType);
  if (erg != -1)
    return erg;
  if (arg.isObjectType() == false)
    return -1;
  RObject o = arg.getObjectVar();
  //if (instanceof(o, Acdk
  return -1;
}

//virtual 
int 
JavaDmiClient::typeDistance(const ClazzInfo* fromType, const ClazzInfo* toType)
{
  return AcdkDmiClient::typeDistance(fromType, toType);
}

//virtual 
void 
JavaDmiClient::castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType)
{

}

#endif //0



#define MAP_J2AEXCEPTION(javaname, acdkns, acdkcn) \
  if (env->IsAssignableFrom(cls, JClass::findClass(env, #javaname))) \
    THROW1_FQ(acdkns, acdkcn, msg)

#define MAP_A2JEXCEPTION(acdkex, jex) \
do { \
  if (acdkex::clazzInfo()->assignableFrom(ex->getClazzInfo()) == true) { \
    env->ThrowNew(env->FindClass(#jex), msg); \
    return; \
  } \
} while (false)


void mapException(JNIEnv* env, IN(RThrowable) ex)
{
  JString msg(env, ex->getMessage());
  MAP_A2JEXCEPTION(::acdk::io::EOFException, java/io/EOFException);
  MAP_A2JEXCEPTION(::acdk::io::IOException, java/io/IOException);

  MAP_A2JEXCEPTION(::acdk::lang::NoSuchMethodException, java/lang/NoSuchMethodException);
  MAP_A2JEXCEPTION(::acdk::lang::ClassNotFoundException, java/lang/ClassNotFoundException);
  MAP_A2JEXCEPTION(::acdk::lang::IllegalAccessException, java/lang/IllegalAccessException);
  MAP_A2JEXCEPTION(::acdk::lang::NullPointerException, java/lang/NullPointerException);
  MAP_A2JEXCEPTION(::acdk::lang::IndexOutOfBoundsException, java/lang/IndexOutOfBoundsException);
  MAP_A2JEXCEPTION(::acdk::lang::RuntimeException, java/lang/RuntimeException);
  MAP_A2JEXCEPTION(::acdk::lang::Exception, java/lang/Exception);
  //MAP_A2JEXCEPTION(::acdk::lang::Throwable, java/lang/Throwable);
  jclass thrcls = env->FindClass("java/lang/Exception");
  env->ThrowNew(thrcls, msg);
}

void 
handleJException(JNIEnv* env)
{
  
  JObjectImpl<jthrowable, JGlobalRes> throwable(env, env->ExceptionOccurred());
  //env->ExceptionDescribe();
  
  env->ExceptionClear();
  JMethod Throwable_getMessage(env, throwable, "getMessage", "()Ljava/lang/String;");
  
  JString jmsg(env, (jstring)Throwable_getMessage.callObjectMethod(throwable));
  if (env->ExceptionCheck() == 0)
  {
    sys::coreout << "rec ex" << sys::eofl;
  }
  RString msg = jmsg.toString();
  
  JClass cls = JClass::getObjectClass(env, throwable);

  RString cname = cls.getName();

  

  MAP_J2AEXCEPTION(java/io/EOFException, ::acdk::io::, EOFException);
  MAP_J2AEXCEPTION(java/io/IOException, ::acdk::io::, IOException);
  
  MAP_J2AEXCEPTION(java/lang/Error, ::acdk::lang::, Error);
  
  MAP_J2AEXCEPTION(java/lang/NoSuchMethodException, ::acdk::lang::, NoSuchMethodException);
  MAP_J2AEXCEPTION(java/lang/ClassNotFoundException, ::acdk::lang::, ClassNotFoundException);
  MAP_J2AEXCEPTION(java/lang/IllegalAccessException, ::acdk::lang::, IllegalAccessException);
  
  
  MAP_J2AEXCEPTION(java/lang/NullPointerException, ::acdk::lang::, NullPointerException);
  MAP_J2AEXCEPTION(java/lang/IndexOutOfBoundsException, ::acdk::lang::, IndexOutOfBoundsException);
  MAP_J2AEXCEPTION(java/lang/RuntimeException, ::acdk::lang::, RuntimeException);

  MAP_J2AEXCEPTION(java/lang/Exception, ::acdk::lang::, Exception);

  THROW1_FQ(::acdk::lang::, Throwable, msg);
  
}

} // namespace java 
} // namespace acdk
