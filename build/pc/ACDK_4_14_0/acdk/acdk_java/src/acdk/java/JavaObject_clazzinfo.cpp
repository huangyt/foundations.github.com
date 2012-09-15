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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/JavaObject_clazzinfo.cpp,v 1.12 2005/02/20 13:55:37 kommer Exp $
// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>



#include "JavaObject.h"
#include "JavaObject.h"
#include <acdk/io/ObjectReader.h> // for instantiation purposes

namespace acdk { 
namespace java { 

using namespace acdk::lang::dmi;

using namespace acdk::lang;
ClazzFieldInfo* _JavaObject_fields[] = 
{
  0
};




ClazzSuperInfo _JavaObject_super_acdk__lang__Object =
{
  MiPublic | MiSuperInfo,
  0, // attribute 
  acdk::lang::Object::clazzInfo()
};

ClazzSuperInfo* _JavaObject_interfaces[] =
{
  &_JavaObject_super_acdk__lang__Object,
  0
};

ClazzMethodArgInfo* JavaObject_methods_GetClass__L_acdk_lang_RClass__args[] = 
{
  0
};

ClazzInfo* JavaObject_methods_GetClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};

ClazzMethodInfo JavaObject_method_GetClass__L_acdk_lang_RClass_ = 
{
  MiPublic | MiStatic | MiMethodInfo ,// class flags, like static, Constructor
  0, 
  "GetClass", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::RClass::clazzInfo(), // return type
  0, // altname
  -1, // hashCode
  JavaObject_methods_GetClass__L_acdk_lang_RClass__args, // return the arguments
  0, // args num
  JavaObject_methods_GetClass__L_acdk_lang_RClass__exceptions, // the declared exceptions
  0 // address of method currently not supported
};

ClazzMethodArgInfo* JavaObject_methods_getClass__L_acdk_lang_RClass__args[] = 
{
  0
};

ClazzInfo* JavaObject_methods_getClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};

ClazzMethodInfo JavaObject_method_getClass__L_acdk_lang_RClass_ = 
{
  MiPublic | MiMiVirtual | MiMethodInfo,// class flags, like static, Constructor
  0, 
  "getClass", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::RClass::clazzInfo(), // return type
  0, // altname
  -1, // hashCode
  JavaObject_methods_getClass__L_acdk_lang_RClass__args, // return the arguments
  0, //args num
  JavaObject_methods_getClass__L_acdk_lang_RClass__exceptions, // the declared exceptions
  0 // address of method currently not supported
};

ClazzMethodArgInfo* JavaObject_methods_toString__LRString__args[] = 
{
  0
};

ClazzInfo* JavaObject_methods_toString__LRString__exceptions[] =
{
  0
};

ClazzMethodInfo JavaObject_method_toString__LRString_ = 
{
  MiPublic | MiMethodInfo,// class flags, like static, Constructor
  0, 
  "toString", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextScopeSibling
  RString::clazzInfo(), // return type
  0, // altname
  -1, // hashCode
  JavaObject_methods_toString__LRString__args, // return the arguments
  0, // args num
  JavaObject_methods_toString__LRString__exceptions, // the declared exceptions
  0 // address of method currently not supported
};

ClazzMethodInfo* _JavaObject_methods[] = 
{
  &JavaObject_method_GetClass__L_acdk_lang_RClass_,
  &JavaObject_method_getClass__L_acdk_lang_RClass_,
  &JavaObject_method_toString__LRString_,
  0
};

ClazzInfo JavaObject::_clazzInfo =
{
  MiCiWeakBind | MiResolved | MiClazzInfo, // clazz-flags
  0, // attributeRes
  "JavaObject", // name of class
  -1, // hashCode
  "acdk/java", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &JavaObject::_clazzInfo,
  0, // _firstChild
  _JavaObject_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  _JavaObject_fields, // pointer to Array of fields
  0, // count of Fields
  _JavaObject_methods, // pointer to Array of fields
  0, // count of Fields
  0, // create-function for cloning/serializing
  &JavaObject::create_array, // create-function for cloning/serializing arrays
  &JavaObject::create_array_array, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  JLONG_CONSTANT(0), // jlong serialVersionUID; for serialization
  JavaObject::_invoke_dynamic,
  JavaObject::_invoke_static, // dispatch
 0, // count off all collectable members in this class
  0, // user defined info
  0 // ClazzInfo* _next; 
};
static RegisterClazzInfo _register_JavaObject(JavaObject::clazzInfo());


} // namespace acdk
} // namespace java


