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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ObjectArrayImpl.cpp,v 1.17 2005/02/05 10:44:56 kommer Exp $


#if 0 // dead code

#include <acdk.h>

#include "ObjectArrayImpl.h"

#ifndef ACDK_NOMETAINFO

/*
ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_length =
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "length", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_get =
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "get", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_getref =
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "get", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};



ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_set =
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "set", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_ensureCapacity = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "ensureCapacity", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_resize =
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "resize", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_append =
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "append", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_insert =
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "insert", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_remove =
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "remove", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};
*/

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo ObjectArray_fields__length =
{
  2,
  0, // attributeRes
  "_length", // label
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(),
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo ObjectArray_fields__data =
{
  2,
  0, // attributeRes
  "_data", // label
  ::acdk::lang::Object::clazzInfo(),
  (void*)0 // address of field
};
    

::acdk::lang::dmi::ClazzFieldInfo* _ObjectArrayImpl_fields[] = 
{
  &ObjectArray_fields__length,
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_ObjectArrayImpl_I_LRObjectArrayImpl__arg_count = 
{
  0,
  0, // attributeRes
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
  "count"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_ObjectArrayImpl_I_LRObjectArrayImpl__args[] = 
{
  &ObjectArrayImpl_methods_ObjectArrayImpl_I_LRObjectArrayImpl__arg_count,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_ObjectArrayImpl_I_LRObjectArrayImpl_ = 
{
  269484033,// class flags, like static, Constructor
  0, // attributeRes
  "ObjectArrayImpl", // name of method
  0, //altname
  ObjectArray::clazzInfo(), // return type
  ObjectArrayImpl_methods_ObjectArrayImpl_I_LRObjectArrayImpl__args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzSuperInfo _ObjectArrayImpl_super___acdk__lang__Object =
{
  3,
  0, // attributeRes
  ::acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _ObjectArrayImpl_interfaces[] =
{
  &_ObjectArrayImpl_super___acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_get_I_Lacdk_lang_RObject__arg_idx = 
{
  0,
  0, // attributeRes
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
  "idx"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_get_I_Lacdk_lang_RObject__args[] = 
{
  &ObjectArrayImpl_methods_get_I_Lacdk_lang_RObject__arg_idx,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_get_I_Lacdk_lang_RObject_ = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "get", // name of method
  0, //altname
  acdk::lang::RObject::clazzInfo(), // return type
  ObjectArrayImpl_methods_get_I_Lacdk_lang_RObject__args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_getref_I_Lacdk_lang_RObject__arg_idx = 
{
  0,
  0, // attributeRes
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
  "idx"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_getref_I_Lacdk_lang_RObject__args[] = 
{
  &ObjectArrayImpl_methods_getref_I_Lacdk_lang_RObject__arg_idx,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_getref_I_Lacdk_lang_RObject_ = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "getref", // name of method
  0, //altname
  acdk::lang::RObject::clazzInfo(), // return type
  ObjectArrayImpl_methods_getref_I_Lacdk_lang_RObject__args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_remove_I_Lacdk_lang_RObject__arg_idx = 
{
  0,
  0, // attributeRes
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
  "idx"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_remove_I_Lacdk_lang_RObject__args[] = 
{
  &ObjectArrayImpl_methods_remove_I_Lacdk_lang_RObject__arg_idx,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_remove_I_Lacdk_lang_RObject_ = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "remove", // name of method
  0, //altname
  acdk::lang::RObject::clazzInfo(), // return type
  ObjectArrayImpl_methods_remove_I_Lacdk_lang_RObject__args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_set_IL_acdk_lang_RObject__V_arg_idx = 
{
  0,
  0, // attributeRes
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
  "idx"
};

::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_set_IL_acdk_lang_RObject__V_arg_obj = 
{
  0,
  0, // attributeRes
  ::acdk::lang::RObject::clazzInfo() ,
  "obj"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_set_IL_acdk_lang_RObject__V_args[] = 
{
  &ObjectArrayImpl_methods_set_IL_acdk_lang_RObject__V_arg_idx,
  &ObjectArrayImpl_methods_set_IL_acdk_lang_RObject__V_arg_obj,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_set_IL_acdk_lang_RObject__V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "set", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  ObjectArrayImpl_methods_set_IL_acdk_lang_RObject__V_args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_insert_IL_acdk_lang_RObject__V_arg_idx = 
{
  0,
  0, // attributeRes
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
  "idx"
};

::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_insert_IL_acdk_lang_RObject__V_arg_obj = 
{
  0,
  0, // attributeRes
  ::acdk::lang::RObject::clazzInfo() ,
  "obj"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_insert_IL_acdk_lang_RObject__V_args[] = 
{
  &ObjectArrayImpl_methods_insert_IL_acdk_lang_RObject__V_arg_idx,
  &ObjectArrayImpl_methods_insert_IL_acdk_lang_RObject__V_arg_obj,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_insert_IL_acdk_lang_RObject__V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "insert", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  ObjectArrayImpl_methods_insert_IL_acdk_lang_RObject__V_args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_length__I_args[] = 
{
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_length__I = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "length", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(), // return type
  ObjectArrayImpl_methods_length__I_args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_ensureCapacity_I_V_arg_newSize = 
{
  0,
  0, // attributeRes
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
  "newSize"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_ensureCapacity_I_V_args[] = 
{
  &ObjectArrayImpl_methods_ensureCapacity_I_V_arg_newSize,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_ensureCapacity_I_V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "ensureCapacity", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  ObjectArrayImpl_methods_ensureCapacity_I_V_args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_resize_I_V_arg_newSize = 
{
  0,
  0, // attributeRes
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
  "newSize"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_resize_I_V_args[] = 
{
  &ObjectArrayImpl_methods_resize_I_V_arg_newSize,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_resize_I_V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "resize", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  ObjectArrayImpl_methods_resize_I_V_args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_append_L_acdk_lang_RObject__V_arg_obj = 
{
  0,
  0, // attributeRes
  ::acdk::lang::RObject::clazzInfo() ,
  "obj"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_append_L_acdk_lang_RObject__V_args[] = 
{
  &ObjectArrayImpl_methods_append_L_acdk_lang_RObject__V_arg_obj,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_append_L_acdk_lang_RObject__V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "append", // name of method
  0, //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  ObjectArrayImpl_methods_append_L_acdk_lang_RObject__V_args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo ObjectArrayImpl_methods_append_L_acdk_lang_RObjectArray__V_arg_array = 
{
  0,
  0, // attributeRes
  ::acdk::lang::RObjectArray::clazzInfo() ,
  "array"
};

::acdk::lang::dmi::ClazzMethodArgInfo* ObjectArrayImpl_methods_append_L_acdk_lang_RObjectArray__V_args[] = 
{
  &ObjectArrayImpl_methods_append_L_acdk_lang_RObjectArray__V_arg_array,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo ObjectArrayImpl_method_append_L_acdk_lang_RObjectArray__V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "append", // name of method
  "append2", //altname
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  ObjectArrayImpl_methods_append_L_acdk_lang_RObjectArray__V_args, // return the arguments
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodInfo* _ObjectArrayImpl_methods[] = 
{
  &ObjectArrayImpl_method_ObjectArrayImpl_I_LRObjectArrayImpl_,
  &ObjectArrayImpl_method_get_I_Lacdk_lang_RObject_,
  &ObjectArrayImpl_method_getref_I_Lacdk_lang_RObject_,
  &ObjectArrayImpl_method_set_IL_acdk_lang_RObject__V,
  &ObjectArrayImpl_method_insert_IL_acdk_lang_RObject__V,
  &ObjectArrayImpl_method_length__I,
  &ObjectArrayImpl_method_ensureCapacity_I_V,
  &ObjectArrayImpl_method_resize_I_V,
  &ObjectArrayImpl_method_append_L_acdk_lang_RObject__V,
  &ObjectArrayImpl_method_append_L_acdk_lang_RObjectArray__V,
  &ObjectArrayImpl_method_remove_I_Lacdk_lang_RObject_,
  0
};



dmi::ClazzInfo ObjectArray_clazzInfo =
{
  reflect::MiMcKnownType | dmi::MiCiArray, // clazz-flags, // clazz-flags
  0, // attributeRes
  "[", // name of class
  "acdk/lang", // the namespace
  _ObjectArrayImpl_interfaces, // ClazzSuperInfo** interfaces;
  (sizeof(_ObjectArrayImpl_interfaces) / sizeof(_ObjectArrayImpl_interfaces[0])) - 1,
  _ObjectArrayImpl_fields, // ClazzFieldInfo** fields;
  (sizeof(_ObjectArrayImpl_fields) / sizeof(_ObjectArrayImpl_fields[0])) - 1,
  _ObjectArrayImpl_methods, // ClazzMethodInfo** methods; 
  (sizeof(_ObjectArrayImpl_methods) / sizeof(_ObjectArrayImpl_methods[0])) - 1,
  ObjectArray::create_instance, // ObjectCreator creator; create-function for cloning/serializing
  0, // create Arrays of this ObjectArray
  0, // create ArrayArrays of this ObjectArray
  0, // Class* thisClass; 
  0, // jlong serialVersionUID
  ObjectArray::StandardDispatch,
  0, // void *userInfo; slot for user type
  0, // UnitInfo* unitInfo
  0, // ClazzInfo* _next; 
  0 // ClazzInfo* _unitNext 
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_ObjectArray_clazzInfo(&ObjectArray_clazzInfo);

#endif //ACDK_NOMETAINFO
#endif // 0 dead code

