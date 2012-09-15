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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/acdk2java.h,v 1.11 2005/04/18 20:40:35 kommer Exp $

#ifndef acdk_java_acdk2java_h
#define acdk_java_acdk2java_h

#include "jniext.h"

namespace acdk {
namespace java {

using namespace acdk::lang::dmi;

void java2acdk(JNIEnv *env, jobject jobj, ::acdk::lang::dmi::ScriptVar& sa);
void java2acdk(JNIEnv *env, JObjectArray& arguments, ::acdk::lang::dmi::ScriptVarArray& sargs);
jobject acdk2jobject(JNIEnv *env, const ::acdk::lang::dmi::ScriptVar& sa);
RObject jobject2aobject(JNIEnv *env, jobject jobj);
bool acdk2java(JNIEnv *jenv, jclass jcls, ScriptVar& marg, jvalue& jarg);
bool acdk2java(JNIEnv *jenv, jobjectArray classarray, ScriptVarArray& margs, jvalue* jargs);
bool acdk2java(JNIEnv *env, ScriptVar& marg, jvalue& jarg);
bool acdk2java(JNIEnv *env, ScriptVarArray& margs, jvalue* jargs);

jclass getAcdkObjectClass(JNIEnv *env);

jobject createNewAcdkObject(JNIEnv* env, jclass jcls, IN(RObject) obj);
void setObjectHandle(JNIEnv* env, jobject jobj, IN(RObject) obj);
Object* getObjectHandle(JNIEnv* env, jobject obj);

#if 0

class JavaDmiClient
: public ::acdk::lang::dmi::AcdkDmiClient
{
public:

  /**
   this method now returns the difference beetween the types.
     0 for an exact match, -1 for incompatible types, various
     differences from 1 to 7 for compatible upcasts, from 257 to
    263 for maybe compatible downcasts.
   > 300 for intepreted casts ( read int out of string convert to string)
  */
  virtual int typeDistance(const ::acdk::lang::dmi::ScriptVar& arg, const ::acdk::lang::dmi::ClazzInfo* toType);
  /**
    @see int typeDistance(const ScriptVar& arg, const ClazzInfo* toType);
  */
  virtual int typeDistance(const ::acdk::lang::dmi::ClazzInfo* fromType, const ::acdk::lang::dmi::ClazzInfo* toType);

  /**
    After checked with typeDistance() use this method to do the cast.
    This method may changes the type of value.

    @param value the value to cast. On succes the type value may be changed by this method
    @param type information of the target type, implemented by the target member/method argument
  */
  virtual void castTo(::acdk::lang::dmi::ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType);
};

#endif //0

} // namespace java 
} // namespace acdk

#endif //acdk_java_acdk2java_h

