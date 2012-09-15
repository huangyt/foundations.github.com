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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/CorObject_clazzInfo.cpp,v 1.11 2005/02/20 13:56:34 kommer Exp $

#include "CorObject.h"
#include <acdk/lang/ParamsMismatchException.h>

namespace acdkx {
namespace orb {

using namespace ::acdk::lang::dmi;

using ::org::omg::CORBA::portable::CallerWrite;
using ::org::omg::CORBA::portable::CalleeRead;
using ::org::omg::CORBA::portable::CalleeWrite;
using ::org::omg::CORBA::portable::CallerRead;


::acdk::lang::dmi::ClazzMethodArgInfo* CorObject_methods_GetClass__L_acdk_lang_RClass__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* CorObject_methods_GetClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};


::acdk::lang::dmi::ClazzMethodInfo CorObject_method_GetClass__L_acdk_lang_RClass_ = 
{
  MiStatic | MiPublic | MiMethodInfo,// class flags, like static, Constructor
  0,
  "GetClass", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::RClass::clazzInfo(), // return type
  0, // altname
  -1, // hashCode
  CorObject_methods_GetClass__L_acdk_lang_RClass__args, // return the arguments
  0, // arg num
  CorObject_methods_GetClass__L_acdk_lang_RClass__exceptions, // the declared exceptions
  0 // address of method currently not supported
};


::acdk::lang::dmi::ClazzMethodArgInfo* CorObject_methods_getClass__L_acdk_lang_RClass__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* CorObject_methods_getClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo CorObject_methods_New_LRString__LRCorObject__arg_classname = 
{
  MiMethodArgInfo | MiAiIn,
  0,
  "classname",
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  RString::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo* CorObject_methods_New_LRString__LRCorObject__args[] = 
{
  &CorObject_methods_New_LRString__LRCorObject__arg_classname,
  0
};

::acdk::lang::dmi::ClazzInfo* CorObject_methods_New_LRString__LRCorObject__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo CorObject_method_New_LRString__LRCorObject_ = 
{
  MiStatic | MiPublic | MiMethodInfo,// class flags, like static, Constructor
  0,
  "New", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  RCorObject::clazzInfo(), // return type
  0, // altname
  -1, // hashCode
  CorObject_methods_New_LRString__LRCorObject__args, // return the arguments
  0, // arg num
  CorObject_methods_New_LRString__LRCorObject__exceptions, // the declared exceptions
  0 // address of method currently not supported
};

::acdk::lang::dmi::ClazzMethodArgInfo CorObject_methods_CorObject_LRString__LRCorObject__arg_classname = 
{
  MiMethodArgInfo | MiAiIn,
  0,
  "classname",
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  RString::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo* CorObject_methods_CorObject_LRString__LRCorObject__args[] = 
{
  &CorObject_methods_CorObject_LRString__LRCorObject__arg_classname,
  0
};

::acdk::lang::dmi::ClazzInfo* CorObject_methods_CorObject_LRString__LRCorObject__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo CorObject_method_CorObject_LRString__LRCorObject_ = 
{
  MiPublic | MiStatic | MiMiConstructor | MiMethodInfo,// class flags, like static, Constructor
  0,
  "CorObject", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  CorObject::clazzInfo(), // return type
  0, // altname
  -1, // hashCode
  CorObject_methods_CorObject_LRString__LRCorObject__args, // return the arguments
  0, // arg num
  CorObject_methods_CorObject_LRString__LRCorObject__exceptions, // the declared exceptions
  0 // address of method currently not supported
};


::acdk::lang::dmi::ClazzMethodInfo* _CorObject_methods[] = 
{
  &CorObject_method_CorObject_LRString__LRCorObject_,
  &CorObject_method_GetClass__L_acdk_lang_RClass_,
  &CorObject_method_GetClass__L_acdk_lang_RClass_,
  &CorObject_method_New_LRString__LRCorObject_,
  0
};

::acdk::lang::dmi::ClazzFieldInfo* _CorObject_fields[] = 
{
  0
};

::acdk::lang::dmi::ClazzSuperInfo _CorObject_super___acdk__lang__Object =
{
  MiSuperInfo | MiPublic,
  0,
  ::acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _CorObject_interfaces[] =
{
  &_CorObject_super___acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo CorObject::_clazzInfo =
{
  MiPublic | MiCiWeakBind | MiClazzInfo, // clazz-flags
  0,
  "CorObject", // name of class
  -1, // hashCode
  "acdkx/com", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  &CorObject::_clazzInfo,
  0, // _firstChild
  _CorObject_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  _CorObject_fields, // pointer to Array of fields
  0, // count of Fields
  _CorObject_methods, // pointer to Array of fields
  0, // count of Fields
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  JLONG_CONSTANT(0), // jlong serialVersionUID; for serialization
  0,// &CorObject::standardDispatch, // dynamic_dispatch
  0, //&CorObject::StandardDispatch, // static_dispatch
  0, // count off all collectable members in this class
  0, // user defined info
  0 // ClazzInfo* _next; 
};
static ::acdk::lang::dmi::RegisterClazzInfo _register_CorObject(CorObject::clazzInfo());


} // namespace orb 
} // namespace acdkx 


