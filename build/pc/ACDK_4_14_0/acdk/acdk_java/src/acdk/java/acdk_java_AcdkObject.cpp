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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/acdk_java_AcdkObject.cpp,v 1.12 2005/04/20 08:56:10 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>
#include "jniext.h"
#include "JavaObject.h"

#include "acdk_java_AcdkObject.h"
#include "acdk2java.h"

using namespace ::acdk::java;

#ifdef __cplusplus
extern "C" {
#endif
extern "C" JNIEXPORT 


inline
void initAcdk(JNIEnv* env, jclass cls)
{
  static bool initialized = false;
  if (initialized == true)
    return;

  initialized = true;
  System::getSystem();
}

//::acdk::lang::dmi::AcdkStdWeakTypeDmiClient jTypeConverter(true, false);

JNIEXPORT jint JNICALL Java_acdk_java_AcdkObject_newAcdkObject
  (JNIEnv* env, jclass cls, jstring clsname, jobjectArray args)
{
  try {
    initAcdk(env, cls);

    JString classname(env, clsname);
    JObjectArray arguments(env, args);
    ScriptVarArray sargs(arguments.size());
    java2acdk(env, arguments, sargs);
    RObject obj = (RObject)Object::New(classname.toString(), sargs);
    if (obj != Nil)
      obj->addRef();
    return (int)obj.impl();
  } catch (RThrowable ex) {
    mapException(env, ex);
    return 0;
  }
}
/*
jobject JNICALL Java_acdk_java_AcdkObject_New
  (JNIEnv *env, jclass cls, jstring clsname, jobjectArray args)
{
  try {
    initAcdk(env, cls);

    JString classname(env, clsname);
    JObjectArray arguments(env, args);
    ScriptVarArray sargs(arguments.size());
    java2acdk(env, arguments, sargs);
    RObject obj = Object::New(classname.toString(), sargs);
    return createNewAcdkObject(env, cls, obj);

  } catch (RThrowable ex) {
    mapException(env, ex);
    return 0;
  }
  return 0;
}
*/

/*
 * Class:     acdk_java_AcdkObject
 * Method:    peek
 * Signature: (Ljava/lang/String;)Ljava/lang/Object;
 */
JNIEXPORT jobject JNICALL Java_acdk_java_AcdkObject_peek
  (JNIEnv* env, jobject jobj, jstring mname)
{
  try {
    JString membername(env, mname);
    Object* obj = getObjectHandle(env, jobj);
    ScriptVar erg = obj->peek(membername.toString());
    return acdk2jobject(env, erg);
  } catch (RThrowable ex) {
    mapException(env, ex);
    return 0;
  }
}

/*
 * Class:     acdk_java_AcdkObject
 * Method:    poke
 * Signature: (Ljava/lang/String;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL Java_acdk_java_AcdkObject_poke
  (JNIEnv* env, jobject jobj, jstring mname, jobject v)
{
  try {
    //initAcdk();
    JString membername(env, mname);
    Object* obj = getObjectHandle(env, jobj);
    ScriptVar sv;
    java2acdk(env, v, sv);
    obj->poke(membername.toString(), sv);
  } catch (RThrowable ex) {
    mapException(env, ex);
  }
}

/*
 * Class:     acdk_java_AcdkObject
 * Method:    invoke
 * Signature: (Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/Object;
 */
JNIEXPORT jobject JNICALL Java_acdk_java_AcdkObject_invoke
  (JNIEnv* env, jobject jobj, jstring jmethodn, jobjectArray jargs)
{
  try {
    //initAcdk();
    JObjectArray arguments(env, jargs);
    ScriptVarArray sargs(arguments.size());
    java2acdk(env, arguments, sargs);
    Object* obj = getObjectHandle(env, jobj);
    JString methodname(env, jmethodn);
    if (obj == 0)
    {
      // ### ex
    }
    
    ScriptVar sv = obj->invokeMethod(methodname.toString(), sargs, JavaObject::gJavaDmiClient, Nil);
    return acdk2jobject(env, sv);
  } catch (RThrowable ex) {
    mapException(env, ex);
    return 0;
  }
}

/*
 * Class:     acdk_java_AcdkObject
 * Method:    peek_static
 * Signature: (Ljava/lang/String;Ljava/lang/String;)Ljava/lang/Object;
 */
JNIEXPORT jobject JNICALL Java_acdk_java_AcdkObject_peek_1static
  (JNIEnv *env, jclass jcls, jstring clsname, jstring mname)
{
  try {
    //initAcdk();
    JString classname(env, clsname);
    JString membername(env, mname);
    ScriptVar sv = Object::peek_static(classname.toString(), membername.toString());
    return acdk2jobject(env, sv);
  } catch (RThrowable ex) {
    mapException(env, ex);
    return 0;
  }
}

/*
 * Class:     acdk_java_AcdkObject
 * Method:    poke_static
 * Signature: (Ljava/lang/String;Ljava/lang/String;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL Java_acdk_java_AcdkObject_poke_1static
  (JNIEnv* env, jclass jcls, jstring clsname, jstring mname, jobject v)
{
  try {
    initAcdk(env, jcls);
    JString classname(env, clsname);
    JString membername(env, mname);
    ScriptVar sv;
    java2acdk(env, v, sv);
    Object::poke_static(classname.toString(), membername.toString(), sv);
  } catch (RThrowable ex) {
    mapException(env, ex);
  }
}


/*
 * Class:     acdk_java_AcdkObject
 * Method:    invoke_static
 * Signature: (Ljava/lang/String;Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/Object;
 */
JNIEXPORT jobject JNICALL Java_acdk_java_AcdkObject_invoke_1static
  (JNIEnv* env, jclass jcls, jstring clsname, jstring mname, jobjectArray jargs)
{
  try {
    initAcdk(env, jcls);
    JString classname(env, clsname);
    JString methodname(env, mname);
    JObjectArray arguments(env, jargs);
    ScriptVarArray sargs(arguments.size());
    java2acdk(env, arguments, sargs);
    ScriptVar erg = Object::invokeStaticMethod(classname.toString(), methodname.toString(), sargs, JavaObject::gJavaDmiClient, Nil);
    return acdk2jobject(env, erg);
  } catch (RThrowable ex) {
    mapException(env, ex);
    return 0;
  }
}

JNIEXPORT void JNICALL 
Java_acdk_java_AcdkObject_dispose(JNIEnv* env, jobject jobj)
{
  Object* obj = getObjectHandle(env, jobj);
  if (obj == 0)
    return;
  
  setObjectHandle(env, jobj, Nil);
  obj->releaseRef();
}


#ifdef __cplusplus
}
#endif

