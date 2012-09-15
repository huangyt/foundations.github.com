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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/JavaObject.cpp,v 1.26 2005/04/20 08:56:10 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>

#include "JavaObject.h"
#include "JavaInterpreter.h"
#include "acdk2java.h"




namespace acdk {
namespace java {

#define MAX_JAVA_ARGS 30



using namespace acdk::lang;
using acdk::lang::dmi::ScriptVar;
using acdk::lang::dmi::ScriptVarArray;
using acdk::lang::dmi::ClazzInfo;



//static 
::acdk::lang::dmi::AcdkStdWeakTypeDmiClient JavaObject::gJavaDmiClient(SVCastStdFlags);

JavaObject::JavaObject(jobject jobj)
: Object()
, _env(getJavaInterpreter())
, _jobj(_env->jenv(), jobj)
, _jclass(_env->jenv())
{
}

JavaObject::JavaObject(IN(RJavaInterpreter) env, jobject jobj)
: Object()
, _env(env)
, _jobj(env->jenv(), jobj)
, _jclass(_env->jenv())
{
}



//virtual 
JavaObject::~JavaObject()
{
}

RString getJavaSignature(ClazzInfo* clazz)
{
  StringBuffer sb(30);
  sb.append("L");
  sb.append(clazz->ns);
  sb.append("/");
  sb.append(clazz->name);
  return sb.toString();
}

RString 
getSignature(JNIEnv *env, ScriptVar& arg)
{
  switch (arg.type) {
  case ScriptVar::BoolType : return "Z";
  case ScriptVar::CharType : return "C";
  case ScriptVar::ByteType : return "B";
  case ScriptVar::ShortType : return "S";
  case ScriptVar::IntType : return "I";
  case ScriptVar::LongType : return "J";
  case ScriptVar::FloatType : return "F";
  case ScriptVar::DoubleType : return "D";
  case ScriptVar::ObjectType : {
    RObject obj = arg.getObjectRef();
    if (obj == Nil)
      return "Ljava/lang/Object;";
    if (instanceof(obj, String) == true)
      return "Ljava/lang/String;";
    if (instanceof(obj, JavaObject) == true) {
      // ####
    }
    return getJavaSignature(obj->getClazzInfo());
  }
  case ScriptVar::UnknownType :
    // oops;
    break;

  }
  return "";
}

RString 
getSignature(JNIEnv *env, ScriptVarArray& args)
{
  StringBuffer sb(20);
  for (int i = 0; i < args.size(); i++) {
    sb.append(getSignature(env, args[i]));
  }
  return sb.toString();
}





// static
jobject 
JavaObject::getNewJObject(JNIEnv* env, IN(RString) classname, ScriptVarArray& args)
{
  JStackFrame jstack(env, 128); 
  JClass jcls(env);
  jcls = env->FindClass(classname->c_str());
  if (jcls == 0) 
  {
    checkExceptions(env);
    return 0;
  }
  RString sig = "(" + getSignature(env, args) + ")V";

  jmethodID mid = env->GetMethodID(jcls, "<init>", sig->c_str());
  if (mid == 0)
  {
    checkExceptions(env);
    return 0;
  }
  jvalue jargs[MAX_JAVA_ARGS]; 
  acdk2java(env, args, jargs);
  jobject jobj = env->NewObjectA(jcls, mid, jargs);
  if (jobj == 0)
  {
    checkExceptions(env);
    return 0;
  }
  return jobj;
}

RJavaObject
getJavaObjectInstance(IN(RJavaInterpreter) jinterpreter, JNIEnv* env, IN(RString) classname, ScriptVarArray& args)
{
  jobject jobj = JavaObject::getNewJObject(env, classname, args);
  return new JavaObject(jinterpreter, jobj);
}


RString getString(JNIEnv* jenv, jstring jstr)
{
  JString s(jenv, jstr);
  return s.toString();
}


RString 
lookupFieldSignature(JNIEnv *jenv, jclass jcls, IN(RString) fieldname)
{
  JClass cls(jenv, jcls);
  JObject field(jenv, cls.getField(fieldname));
  checkExceptions(jenv);
  if (field == 0) 
    return "";
  JClass retjcls(jenv);
  static JMethod Field_getType(jenv, field, "getType", "()Ljava/lang/Class;");

  retjcls = (jclass)Field_getType.callObjectMethod(field);
  return retjcls.getName();
}


// ### old code
RJavaObject 
JavaObject::getInstance(IN(RJavaInterpreter) env, IN(RString) classname, ScriptVarArray& args)
{
  return new JavaObject(env, classname, args);
}


bool 
equal(JNIEnv* jenv, jstring str, const char* other)
{
  jboolean isCopy = false;
  const char* cptr = jenv->GetStringUTFChars(str, &isCopy);
  bool berg = strcmp(cptr, other) == 0;
  if (isCopy == JNI_TRUE)
      jenv->ReleaseStringUTFChars(str, cptr);
  return berg;
}



/** converts 'java.io.PrintStream' to 'Ljava/io/PrintStream;' */
RString
javaType2Signature(IN(RString) str)
{
  if (str->equals("boolean") == true)
    return "Z";
  if (str->equals("char") == true)
    return "C";
  if (str->equals("byte") == true)
    return "B";
  if (str->equals("short") == true)
    return "S";
  if (str->equals("int") == true)
    return "I";
  if (str->equals("long") == true)
    return "J";
  if (str->equals("float") == true)
    return "F";
  if (str->equals("double") == true)
    return "D";
  return "L" + str->replace(".", "/") + ";";
}

bool
lookupMatchingArgs(JNIEnv* jenv, jclass jcls, jobject method, ScriptVarArray& args, jobjectArray& jargs)
{
  JObjectArray oa(jenv);
  static JMethod Method_getParameterTypes(jenv, method, "getParameterTypes", "()[Ljava/lang/Class;");
  oa = (jobjectArray)(jobject)Method_getParameterTypes.callObjectMethod(method);

  int size = oa.size();
  if (args.size() != size)
    return false;
  for (int i = 0; i < size; i++) {
    JClass p(jenv);
    p = (jclass)(jobject)oa[i];//jenv->GetObjectArrayElement(oa, i);
    RString str = p.getName();

    if (str->equals("boolean") == true) {
      if (args[i].type != ScriptVar::BoolType)
        return false;
    }
    if (str->equals("char") == true) {
      if (args[i].isNumber() == false)
        return false;
    }
    if (str->equals("byte") == true) {
      if (args[i].isNumber() == false)
        return false;
    }
    if (str->equals("short") == true) {
      if (args[i].isNumber() == false)
        return false;
    }
    if (str->equals("int") == true) {
      if (args[i].isNumber() == false)
        return false;
    }
    if (str->equals("long") == true) {
      if (args[i].isNumber() == false)
        return false;
    }
    if (str->equals("float") == true) {
      if (args[i].isNumber() == false)
        return false;
    }
    if (str->equals("double") == true) {
      if (args[i].isNumber() == false)
        return false;
    }
    RObject o = args[i].getObjectVar();
    if (instanceof(o, String) == true) {
      if (str->equals("java.lang.String") == false)
        return false;
    } else if (instanceof(o, JavaObject) == true) {
      RJavaObject jobj = (RJavaObject) o;
      if (jenv->IsAssignableFrom(jobj->getJObjectClass(), p) == false)
        return false;
    }
  }

  jargs = oa;
  return true;
}



ScriptVar::Type
lookupReturnType(IN(RJavaInterpreter) env, jclass jcls, IN(RString) methodname, 
                 ScriptVarArray& args, jmethodID& jmeth, jobjectArray& jargs)
{
  
  JNIEnv* jenv = env->jenv();
  JObject methods(jenv);
  
  static JMethod Class_getMethods(jenv, jcls, "getMethods", "()[Ljava/lang/reflect/Method;");
  
  methods = Class_getMethods.callObjectMethod(jcls);
  
  JObjectArray oa(jenv);
  oa = (jobjectArray)(jobject)methods;

  int size = oa.length(); // jenv->GetArrayLength(oa);
  for (int i = 0; i < size; i++) 
  {
    JStackFrame jstack(jenv, 128); 
    JObject jmethod(jenv);
    jmethod = oa[i];

    if (jmethod == 0)
      continue;
    static  JMethod Method_getName(jenv, jmethod, "getName", "()Ljava/lang/String;");
    JString name(jenv);

    name = (jstring)(jobject)Method_getName.callObjectMethod(jmethod);

    if (name.equals(methodname) == true) {
      
      if (lookupMatchingArgs(jenv, jcls, jmethod, args, jargs) == true) 
      {
        JClass rettype(jenv);
        static JMethod Method_getReturnType(jenv, jmethod, "getReturnType", "()Ljava/lang/Class;"); 
        rettype = (jclass)Method_getReturnType.callObjectMethod(jmethod);
        JString rettypename(jenv);
        static JMethod Class_getName(jenv, rettype, "getName", "()Ljava/lang/String;");
        rettypename = (jstring)(jobject)Class_getName.callObjectMethod(rettype);
        RString rettypestr = rettypename.toString(); //getString(jenv, rettypename);
        //System::out->println("rettype is: " + rettypestr);
        jmeth = jenv->FromReflectedMethod(jmethod);
        checkExceptions(jenv);
        if (rettypestr->equals("boolean") == true)
          return ScriptVar::BoolType;
        if (rettypestr->equals("char") == true)
          return ScriptVar::CharType;
        if (rettypestr->equals("byte") == true)
          return ScriptVar::ByteType;
        if (rettypestr->equals("short") == true)
          return ScriptVar::ShortType;
        if (rettypestr->equals("int") == true)
          return ScriptVar::IntType;
        if (rettypestr->equals("long") == true)
          return ScriptVar::LongType;
        if (rettypestr->equals("float") == true)
          return ScriptVar::FloatType;
        if (rettypestr->equals("double") == true)
          return ScriptVar::DoubleType;
        if (rettypestr->equals("void") == true)
          return ScriptVar::UnknownType; // ### voidtype here 
        return ScriptVar::ObjectType;
      }
    } 
  }
  return ScriptVar::UnknownType;
}







//virtual 
void 
JavaObject::setMember(IN(RString) fieldname, const ::acdk::lang::dmi::ScriptVar& newval, ::acdk::lang::dmi::DmiClient& dc, int flags)
{
  JNIEnv *jenv = _env->jenv();
 
  RString jtypenam = lookupFieldSignature(jenv, getJObjectClass(), fieldname);
  if (jtypenam->equals("") == true) 
  {
    checkExceptions(jenv);
    return;
  }
  jtypenam = javaType2Signature(jtypenam);
  
  JField field(jenv, getJObjectClass(), &fieldname, jtypenam, false);
  char typec = jtypenam->charAt(0);

  switch (typec) 
  {
  case 'Z':
    field.setBoolean(_jobj, newval);
    break;
  case 'C':
    field.setChar(_jobj, newval);
    break;
  case 'B':
    field.setByte(_jobj, newval);
    break;
  case 'S':
    field.setShort(_jobj, newval);
    break;
  case 'I':
    field.setInt(_jobj, newval);
    break;
  case 'J':
    field.setLong(_jobj, newval);
    break;
  case 'F':
    field.setFloat(_jobj, newval);
    break;
  case 'D':
    field.setDouble(_jobj, newval);
    break;
  case 'L':
    field.setObject(_jobj, acdk2jobject(jenv, newval));
    break;
  default:
    // ### ex
    break;
  }
  checkExceptions(jenv);
}


//virtual 
::acdk::lang::dmi::ScriptVar 
JavaObject::getMember(IN(RString) fieldname, ::acdk::lang::dmi::DmiClient& dc, int flags, const ::acdk::lang::dmi::ClazzInfo* type_requested)
{
  
  JNIEnv *jenv = _env->jenv();
  RString jtypenam = lookupFieldSignature(jenv, getJObjectClass(), &fieldname);
  jtypenam = javaType2Signature(jtypenam);
  RString tfn = fieldname;
  jfieldID id	= jenv->GetFieldID(getJObjectClass(), S2JS(tfn), S2JS(jtypenam));
  char typec = jtypenam->charAt(0);
  switch (typec)
  {
  case 'Z':
    return jenv->GetBooleanField(_jobj, id);
  case 'C':
    return jenv->GetCharField(_jobj, id);
  case 'B':
    return jenv->GetByteField(_jobj, id);
  case 'S':
    return jenv->GetShortField(_jobj, id);
  case 'I':
    return (int)jenv->GetIntField(_jobj, id);
  case 'J':
    return jenv->GetLongField(_jobj, id);
  case 'F':
    return jenv->GetFloatField(_jobj, id);
  case 'D':
    return jenv->GetDoubleField(_jobj, id);
  case 'L':
  {
    ScriptVar ret;
    java2acdk(jenv, jenv->GetObjectField(_jobj, id), ret);
    return ret;
  }
  default:
    // ### ex
    break;
  }
  return ScriptVar();
/*

  JNIEnv *jenv = _env->jenv();
  JClass jcls = JClass::getObjectClass(jenv, _jobj);
 
  RString jtypenam = lookupFieldSignature(jenv, jcls, fieldname);
  if (jtypenam->equals("") == true) 
  {
    checkExceptions(jenv);
    return ScriptVar();
  }
  jtypenam = javaType2Signature(jtypenam);
  
  JField field(jenv, jcls, fieldname, jtypenam->c_str(), false);
  
  ScriptVar ret;
  if (jtypenam->equals("boolean") == true) {
    ret = field.getBoolean(_jobj);
  } else if (jtypenam->equals("char") == true) {
    ret = field.getChar(_jobj);
  } else if (jtypenam->equals("byte") == true) {
    ret = field.getByte(_jobj);
  } else if (jtypenam->equals("short") == true) {
    ret = field.getShort(_jobj);
  } else if(jtypenam->equals("int") == true) {
    ret = field.getInt(_jobj);
  } else if (jtypenam->equals("long") == true) {
    ret = field.getLong(_jobj);
  } else if (jtypenam->equals("float") == true) {
    ret = field.getFloat(_jobj);
  } else if (jtypenam->equals("double") == true) {
    ret = field.getDouble(_jobj);
  } else {

    jobject retobj = field.getObject(_jobj);
    if (isString(jenv, retobj) == true) {
      ret = RObject(getString(jenv, (jstring)retobj));
    } else {
      return RObject(new JavaObject(_env, retobj));
    }
  }
  checkExceptions(jenv);
  return ScriptVar();
  */
}


//static 
const ClazzMethodInfo* 
JavaObject::_invoke_dynamic(  ::acdk::lang::Object* This_, 
                                                          IN(RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  RJavaObject This = dynamic_cast<JavaObject*>(This_);
  JNIEnv* jenv = This->_env->jenv();
  
  /**
    bug in jvm: this crashes if called outside JNI
    and a GlobalRef is deleted outside.
  */
  //JStackFrame jstack(jenv, 128); 

  
  jobjectArray jargs; // this is a array of java/lang/Class
  jmethodID mid = 0;
  

  JClass jcls = JClass::getObjectClass(jenv, This->_jobj);
  ScriptVar::Type t = lookupReturnType(This->_env, jcls, &fname, args, mid, jargs);
  
  if (mid == 0) {
    System::out->println("cannot find return type ###");
    return 0; // ### error
  }
  // transform arguemnets
  int argscount = args.size();
  if (argscount == 0)
    argscount = 1;
  jvalue* jvalues = new jvalue[argscount];

  if (acdk2java(jenv, jargs, args, jvalues) == false) {
    System::out->println("cannot convert args ###");
    return 0; //### error
  }
  switch (t) {
  case ScriptVar::BoolType : 
    ret = jenv->CallBooleanMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::CharType : 
    ret = jenv->CallCharMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::ByteType : 
    ret = jenv->CallByteMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::ShortType : 
    ret = jenv->CallShortMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::IntType : 
    ret = (int)jenv->CallIntMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::LongType : 
    ret = jenv->CallLongMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::FloatType : 
    ret = jenv->CallFloatMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::DoubleType : 
    ret = jenv->CallDoubleMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::UnknownType : // ### at the moment is void
    jenv->CallVoidMethodA(This->_jobj, mid, jvalues);
    break;
  case ScriptVar::ObjectType : {
    jobject retobj = jenv->CallObjectMethodA(This->_jobj, mid, jvalues);
    java2acdk(jenv, retobj, ret);
    break;
  }
  }
  delete[] jvalues;
  checkExceptions(jenv);
  return (const ::acdk::lang::dmi::ClazzMethodInfo* )1;
}

//static 
const ClazzMethodInfo* 
JavaObject::_invoke_static(  IN(RString) fname, 
                                          ScriptVar& ret, 
                                          ScriptVarArray& args, 
                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  RJavaInterpreter jinterpr = getJavaInterpreter();
  if (fname->equals("New") == true || fname->equals("JavaObject") == true) 
  {
    if (args.size() < 1) 
    {
      // #### error
    }
    RString jclassname = args[0].getStringVar();
    args.erase(args.begin());
    
    ret = (RObject)getJavaObjectInstance(jinterpr, jinterpr->jenv(), jclassname, args);
    checkExceptions(jinterpr->jenv());
    return (const ::acdk::lang::dmi::ClazzMethodInfo* )1;
  } 
  else if (fname->equals("peek_static") == true) 
  {
    if (args.size() != 2) 
    {
      //####
      return 0;
    }
    RString jclassname = args[0].getStringVar();
    RString fieldname = args[1].getStringVar();
    ret = peek_static(getJavaInterpreter(), jclassname, fieldname);
    checkExceptions(jinterpr->jenv());
    return (const ::acdk::lang::dmi::ClazzMethodInfo* )1;
  } 
  else if (fname->equals("poke_static") == true) 
  {
    if (args.size() != 3) 
    {
      //####
      return 0;
    }
    RString jclassname = args[0].getStringVar();
    RString fieldname = args[1].getStringVar();
    ScriptVar v = (RObject)args[2].getStringVar();
    poke_static(jinterpr, jclassname, fieldname, v);
    checkExceptions(jinterpr->jenv());
  }
  // ### ex
  return 0;
}


ScriptVar 
JavaObject::invoke(IN(RString) methodname, ScriptVarArray& args)
{
  JStackFrame jstack(jenv(), 512); 
  jobjectArray jargs = 0; // this is a array of java/lang/Class
  jmethodID mid = 0;
  

  ScriptVar::Type t = lookupReturnType(_env, getJObjectClass(), methodname, args, mid, jargs);
  JNIEnv* jenv = _env->jenv();
  if (mid == 0) {
    System::out->println("cannot find method: " + methodname);
    return ScriptVar(); // ### error
  }
  // transform arguemnets
  int argscount = args.size();
  if (argscount == 0)
    argscount = 1;
  jvalue* jvalues = new jvalue[argscount];

  if (acdk2java(jenv, jargs, args, jvalues) == false) {
    System::out->println("cannot convert args ###");
    return false; //### error
  }
  ScriptVar erg;
  switch (t) {
  case ScriptVar::BoolType : 
    erg = jenv->CallBooleanMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::CharType : 
    erg = jenv->CallCharMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::ByteType : 
    erg = jenv->CallByteMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::ShortType : 
    erg = jenv->CallShortMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::IntType : 
    erg = (int)jenv->CallIntMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::LongType : 
    erg = jenv->CallLongMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::FloatType : 
    erg = jenv->CallFloatMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::DoubleType : 
    erg = jenv->CallDoubleMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::UnknownType : // ### at the moment is void
    jenv->CallVoidMethodA(_jobj, mid, jvalues);
    break;
  case ScriptVar::ObjectType : {
    jobject retobj = jenv->CallObjectMethodA(_jobj, mid, jvalues);
    checkExceptions(jenv);
    java2acdk(jenv, retobj, erg);
  }
  }
  delete[] jvalues;
  checkExceptions(jenv);
  return erg;
}


JavaObject::JavaObject(IN(RString) clsname, ScriptVar va1, 
                                           ScriptVar va2, 
                                           ScriptVar va3, 
                                           ScriptVar va4, 
                                           ScriptVar va5)
: _env(getJavaInterpreter())
, _jobj(_env->jenv())
, _jclass(_env->jenv())
{
  ScriptVarArray args(0); 
  if (va1.type == ScriptVar::UnknownType) {

  } else if (va2.type == ScriptVar::UnknownType) {
    args.ensureSize(1);
    args[0] = va1;
  } else if (va3.type == ScriptVar::UnknownType) {
    args.ensureSize(2);
    args[0] = va1;
    args[1] = va2;
  } else if (va4.type == ScriptVar::UnknownType) {
    args.ensureSize(3);
    args[0] = va1;
    args[1] = va2;
    args[2] = va3;
  } else if (va5.type == ScriptVar::UnknownType) {
    args.ensureSize(4);
    args[0] = va1;
    args[1] = va2;
    args[2] = va3;
    args[3] = va4;
  } else {
    args.ensureSize(5);
    args[0] = va1;
    args[1] = va2;
    args[2] = va3;
    args[3] = va4;
    args[4] = va5;
  }
  _jobj = getNewJObject(_env->jenv(), clsname, args);
 
}


ScriptVar 
JavaObject::invoke(IN(RString) methodname, ScriptVar va1, ScriptVar va2, ScriptVar va3, ScriptVar va4, ScriptVar va5)
{
  ScriptVarArray sa(0);
  if (va1.type == ScriptVar::UnknownType) {
    return invoke(methodname, sa);
  } else if (va2.type == ScriptVar::UnknownType) {
    sa.ensureSize(1);
    sa[0] = va1;
    return invoke(methodname, sa);
  } else if (va3.type == ScriptVar::UnknownType) {
    sa.ensureSize(2);
    sa[0] = va1;
    sa[1] = va2;
    return invoke(methodname, sa);
  } else if (va4.type == ScriptVar::UnknownType) {
    sa.ensureSize(3);
    sa[0] = va1;
    sa[1] = va2;
    sa[2] = va3;
    return invoke(methodname, sa);
  } else if (va5.type == ScriptVar::UnknownType) {
    sa.ensureSize(4);
    sa[0] = va1;
    sa[1] = va2;
    sa[2] = va3;
    sa[3] = va4;
    return invoke(methodname, sa);
  } else {
    sa.ensureSize(5);
    sa[0] = va1;
    sa[1] = va2;
    sa[2] = va3;
    sa[3] = va4;
    sa[4] = va5;
    return invoke(methodname, sa);
  }
}

ScriptVar 
JavaObject::invoke_static(IN(RString) classname, IN(RString) methodname, 
                                           ScriptVar va1, 
                                           ScriptVar va2, 
                                           ScriptVar va3, 
                                           ScriptVar va4, 
                                           ScriptVar va5)
{
  ScriptVarArray sa(0);
  if (va1.type == ScriptVar::UnknownType) {
    return invoke_static(getJavaInterpreter(), classname, methodname, sa);
  } else if (va2.type == ScriptVar::UnknownType) {
    sa.ensureSize(1);
    sa[0] = va1;
    return invoke_static(getJavaInterpreter(), classname, methodname, sa);
  } else if (va3.type == ScriptVar::UnknownType) {
    sa.ensureSize(2);
    sa[0] = va1;
    sa[1] = va2;
    return invoke_static(getJavaInterpreter(), classname, methodname, sa);
  } else if (va4.type == ScriptVar::UnknownType) {
    sa.ensureSize(3);
    sa[0] = va1;
    sa[1] = va2;
    sa[2] = va3;
    return invoke_static(getJavaInterpreter(), classname, methodname, sa);
  } else if (va5.type == ScriptVar::UnknownType) {
    sa.ensureSize(4);
    sa[0] = va1;
    sa[1] = va2;
    sa[2] = va3;
    sa[3] = va4;
    return invoke_static(getJavaInterpreter(), classname, methodname, sa);
  } else {
    sa.ensureSize(5);
    sa[0] = va1;
    sa[1] = va2;
    sa[2] = va3;
    sa[3] = va4;
    sa[4] = va5;
    return invoke_static(getJavaInterpreter(), classname, methodname, sa);
  }
}

//static 
ScriptVar 
JavaObject::invoke_static(IN(RJavaInterpreter) env, IN(RString) classname, IN(RString) methodname, ScriptVarArray& args)
{
  jobjectArray jargs; // this is a array of java/lang/Class
  jmethodID mid = 0;
  JNIEnv* jenv = env->jenv();
  jclass jcls = jenv->FindClass(classname->c_str());
  if (jcls == 0) {
    System::out->println("cannot find class: " + classname);
    // ### error  
    return ScriptVar();
  }
  ScriptVar::Type t = lookupReturnType(env, jcls, methodname, args, mid, jargs);
  
  if (mid == 0) {
    System::out->println("cannot find method: " + methodname);
    return ScriptVar(); // ### error
  }
  // transform arguemnets
  int argscount = args.size();
  if (argscount == 0)
    argscount = 1;
  jvalue* jvalues = new jvalue[argscount];

  if (acdk2java(jenv, jargs, args, jvalues) == false) {
    return false; //### error
  }
  ScriptVar erg;
  switch (t) {
  case ScriptVar::BoolType : 
    erg = jenv->CallStaticBooleanMethodA(jcls, mid, jvalues);
    break;
  case ScriptVar::CharType : 
    erg = jenv->CallStaticCharMethodA(jcls, mid, jvalues);
    break;
  case ScriptVar::ByteType : 
    erg = jenv->CallStaticByteMethodA(jcls, mid, jvalues);
    break;
  case ScriptVar::ShortType : 
    erg = jenv->CallStaticShortMethodA(jcls, mid, jvalues);
    break;
  case ScriptVar::IntType : 
    erg = (int)jenv->CallStaticIntMethodA(jcls, mid, jvalues);
    break;
  case ScriptVar::LongType : 
    erg = jenv->CallStaticLongMethodA(jcls, mid, jvalues);
    break;
  case ScriptVar::FloatType : 
    erg = jenv->CallStaticFloatMethodA(jcls, mid, jvalues);
    break;
  case ScriptVar::DoubleType : 
    erg = jenv->CallStaticDoubleMethodA(jcls, mid, jvalues);
    break;
  case ScriptVar::ObjectType : {
    jobject retobj = jenv->CallStaticObjectMethodA(jcls, mid, jvalues);
    checkExceptions(jenv);
    java2acdk(jenv, retobj, erg);
  }
  case ScriptVar::UnknownType : // ### at the moment is void
    jenv->CallStaticVoidMethodA(jcls, mid, jvalues);
    break;
  }
  delete[] jvalues;
  checkExceptions(jenv);
  return erg;
}
 

/*
ScriptVar 
JavaObject::peek(IN(RString) fieldname)
{
  return getMember(fieldname->c_str(), get, int flags, const ::acdk::lang::dmi::ClazzInfo* type_requested)


  JNIEnv *jenv = _env->jenv();
  RString jtypenam = lookupFieldSignature(jenv, getJObjectClass(), fieldname);
   
  jfieldID id	= jenv->GetFieldID(getJObjectClass(), fieldname->c_str(), jtypenam->c_str());
  char typec = jtypenam->charAt(0);
  switch (typec)
  {
  case 'Z':
    return jenv->GetBooleanField(_jobj, id);
  case 'C':
    return jenv->GetCharField(_jobj, id);
  case 'B':
    return jenv->GetByteField(_jobj, id);
  case 'S':
    return jenv->GetShortField(_jobj, id);
  case 'I':
    return (int)jenv->GetIntField(_jobj, id);
  case 'J':
    return jenv->GetLongField(_jobj, id);
  case 'F':
    return jenv->GetFloatField(_jobj, id);
  case 'D':
    return jenv->GetDoubleField(_jobj, id);
  case 'L':
    return java2acdk(jenv, jenv->GetObjectField(_jobj, id));
  default:
    // ### ex
    break;
  }
  return ScriptVar();
}
 */

//static 
ScriptVar 
JavaObject::peek_static(IN(RJavaInterpreter) env, IN(RString) classname, IN(RString) fieldname)
{
  JNIEnv *jenv = env->jenv();
  JClass jcls = JClass::findClass(jenv, classname);
  
  RString jtypenam = lookupFieldSignature(jenv, jcls, fieldname);
  jtypenam = javaType2Signature(jtypenam);
  char typec = jtypenam->charAt(0);

  JField field(jenv, jcls, fieldname->c_str(), jtypenam->c_str(), true);
  
  ScriptVar ret;
  switch (typec)
  {
  case 'Z':
    ret = field.getStaticBoolean(jcls);
    break;
  case 'C':
    ret = field.getStaticChar(jcls);
    break;
  case 'B':
    ret = field.getStaticByte(jcls);
    break;
  case 'S':
    ret = field.getStaticShort(jcls);
    break;
  case 'I':
    ret = field.getStaticInt(jcls);
    break;
  case 'J':
    ret = field.getStaticLong(jcls);
    break;
  case 'F':
    ret = field.getStaticFloat(jcls);
    break;
  case 'D':
    ret = field.getStaticDouble(jcls);
    break;
  case 'L':
  {
    jobject retobj = field.getStaticObject(jcls);
    java2acdk(jenv, retobj, ret);
    break;
  }
  default:
    // ###ex
    break;
  }
  return ret;
}


//static
void 
JavaObject::poke_static(IN(RJavaInterpreter) env, IN(RString) classname, IN(RString) fieldname, const ScriptVar& val)
{
  JNIEnv *jenv = env->jenv();
  JClass jcls = JClass::findClass(jenv, classname);
  
  RString jtypenam = lookupFieldSignature(jenv, jcls, fieldname);
  jtypenam = javaType2Signature(jtypenam);
  char typec = jtypenam->charAt(0);

  JField field(jenv, jcls, fieldname->c_str(), jtypenam->c_str(), true);
  switch (typec) 
  {
  case 'Z':
    field.setStaticBoolean(jcls, val);
    break;
  case 'C':
    field.setStaticChar(jcls, val);
    break;
  case 'B':
    field.setStaticByte(jcls, val);
    break;
  case 'S':
    field.setStaticShort(jcls, val);
    break;
  case 'I':
    field.setStaticInt(jcls, val);
    break;
  case 'J':
    field.setStaticLong(jcls, val);
    break;
  case 'F':
    field.setStaticFloat(jcls, val);
    break;
  case 'D':
    field.setStaticDouble(jcls, val);
    break;
  case 'L':
    field.setStaticObject(jcls, acdk2jobject(jenv, val));
    break;
  default:
    // ### ex
    break;
  }
}


//virtual 
RString 
JavaObject::toString()
{
  if (isString(_env->jenv(), _jobj) == true) {
    return getString(_env->jenv(), (jstring)(jobject)_jobj);
  }
  ScriptVarArray args(0);
  ScriptVar tret = invoke("toString", args);
  return (RString)tret.getObjectVar();
}

jclass 
JavaObject::getJObjectClass() 
{ 
  if (_jclass == 0)
    _jclass = jenv()->GetObjectClass(_jobj); 
  return _jclass;
}

} //namespace java
} // namespace acdk


