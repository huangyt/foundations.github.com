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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/BasicArray.cpp,v 1.26 2005/04/17 11:20:10 kommer Exp $



#include <acdk.h>

#include "BasicArray.h"
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/io/ObjectReader.h>


#ifndef ACDK_NOMETAINFO


EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, length);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, get);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, operator_bo_bc);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, getref);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, set);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, ensureCapacity);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, resize);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, append);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, insert);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, remove);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, findFirst);
EXPORT_ASCLITERAL(ACDK_CORE_PUBLIC, findLast);


const ::acdk::lang::dmi::ClazzMethodInfo* 
BasicArray_StandardDispatchFunction(IN(acdk::lang::RString) fname, 
                                    ::acdk::lang::dmi::ScriptVar& ret, 
                                    ::acdk::lang::dmi::ScriptVarArray& args, 
                                    ::acdk::lang::dmi::DmiClient& dc,
                                    IN(::acdk::lang::RStringArray) namedArgs,
                                    int flags,
                                    const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                    const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  if ((flags & dmi::MiIvConstructor) == 0)
  {
    if (dmi::MiIvNoThrowIfNotFound & flags)
      return 0;
    THROW1(NoSuchMethodException, "Only constructor as static methods are defined in BasicArray");
  }
  int size = 0;
  if (args.size() > 0)
  {
    size = args[0].getIntVar();
  }
  if (fname->equals("boolArray") == true || fname->equals("[bool") == true)
    ret = new boolArray(size);
  else if (fname->equals("charArray") == true || fname->equals("[char") == true)
    ret = new charArray(size);
  else if (fname->equals("uccharArray") == true || fname->equals("[ucchar") == true)
    ret = new uccharArray(size);
  else if (fname->equals("byteArray") == true || fname->equals("[byte") == true)
    ret = new byteArray(size);
  else if (fname->equals("shortArray") == true || fname->equals("[short") == true)
    ret = new shortArray(size);
  else if (fname->equals("intArray") == true || fname->equals("[int") == true)
    ret = new intArray(size);
  else if (fname->equals("jlongArray") == true || fname->equals("[jlong") == true)
    ret = new longArray(size);
  else if (fname->equals("floatArray") == true || fname->equals("[float") == true)
    ret = new floatArray(size);
  else if (fname->equals("doubleArray") == true || fname->equals("[double") == true)
    ret = new doubleArray(size);
  else
  {
    if (dmi::MiIvNoThrowIfNotFound & flags)
      return 0;
    THROW1(NoSuchMethodException, "No such static method found: " + fname);
  }
  return (const ::acdk::lang::dmi::ClazzMethodInfo*)1;
}

const ::acdk::lang::dmi::ClazzMethodInfo* BasicArray_DynamicDispatchFunction(
                                                        ::acdk::lang::Object* This,
                                                         IN(acdk::lang::RString) fname,
                                                         ::acdk::lang::dmi::ScriptVar& ret,
                                                         ::acdk::lang::dmi::ScriptVarArray& args,
                                                         ::acdk::lang::dmi::DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                                         const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  return Object::clazzInfo()->loadFullClazzInfo()->dynamic_dispatch(This, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
}

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields__length = 
{
  2,
  0, // attributeRes
  "_length", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(),
  0,
  (void*)0 // address of field
};


ACDK_CORE_PUBLIC  ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_bool_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getBoolClazz(),
  0,
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_char_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
   ::acdk::lang::dmi::ClazzInfo::getCharClazz(),
   0,
  (void*)0 // address of field
};
ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_ucchar_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
   ::acdk::lang::dmi::ClazzInfo::getUcCharClazz(),
   0,
  (void*)0 // address of field
};


ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_byte_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
   ::acdk::lang::dmi::ClazzInfo::getByteClazz(),
   0,
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_short_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getShortClazz(),
  0,
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_int_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(),
  0,
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_jlong_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getLongClazz(),
  0,
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_float_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getFloatClazz(),
  0,
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_double_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz(),
  0,
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo BasicArray_fields_unknown_data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  0,
  0, 
  0 // address of field
};

::acdk::lang::dmi::ClazzFieldInfo* _BasicArray_fields[] = 
{
  &BasicArray_fields__length,
  0
};

::acdk::lang::dmi::ClazzSuperInfo _BasicArray_super___acdk__lang__Object =
{
  3,
  0, // attributeRes
  ::acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _BasicArray_interfaces[] =
{
  &_BasicArray_super___acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_BasicArray_I_LRBasicArray__arg_count = 
{
  0,
  0, // attributeRes
  "count",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() 
  
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_methods_BasicArray_I_LRBasicArray__args[] = 
{
  &BasicArray_methods_BasicArray_I_LRBasicArray__arg_count,
  0
};

extern ::acdk::lang::dmi::ClazzInfo BasicArray_clazzInfo;

::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_BasicArray_I_LRBasicArray_ = 
{
  269484033,// class flags, like static, Constructor
  0, // attributeRes
  "BasicArray", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  &BasicArray_clazzInfo, // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_BasicArray_I_LRBasicArray__args, // return the arguments
  0, // argument count
  0, // exceptions
  0, 
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_get_I_D_arg_idx = 
{
  0,
  0, // attributeRes
  "idx",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
   ::acdk::lang::dmi::ClazzInfo::getIntClazz() ,
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_methods_get_I_D_args[] = 
{
  &BasicArray_methods_get_I_D_arg_idx,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_get_I_D = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "get", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_get_I_D_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_getref_I_D = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "getref", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_get_I_D_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_operator_bo_bc_I_D = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "operator_bo_bc", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz(), // return type
  "operator_bo", //altname
  -1, // nameHashCode
  BasicArray_methods_get_I_D_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_methods_length_I_args[] = 
{
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_length_I = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "length", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_length_I_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_ensureCapacity_I_V_arg_newSize = 
{
  0,
  0, // attributeRes
  "newSize",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() 
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_methods_ensureCapacity_I_V_args[] = 
{
  &BasicArray_methods_ensureCapacity_I_V_arg_newSize,
  0
};

ACDK_CORE_PUBLIC  ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_ensureCapacity_I_V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "ensureCapacity", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_ensureCapacity_I_V_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_resize_I_V_arg_newSize = 
{
  0,
  0, // attributeRes
  "newSize",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() 
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_methods_resize_I_V_args[] = 
{
  &BasicArray_methods_resize_I_V_arg_newSize,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_resize_I_V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "resize", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_resize_I_V_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};

::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_append_D_V_arg_val = 
{
  0,
  0, // attributeRes
  "val",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz() 
  
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_methods_append_D_V_args[] = 
{
  &BasicArray_methods_append_D_V_arg_val,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_append_D_V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "append", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_append_D_V_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_insert_I_D_V_arg_idx = 
{
  0,
  0, // attributeRes
  "idx",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() 
};

::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_insert_I_D_V_arg_val = 
{
  0,
  0, // attributeRes
  "val",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz() 
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_methods_insert_I_D_V_args[] = 
{
  &BasicArray_methods_insert_I_D_V_arg_idx,
  &BasicArray_methods_insert_I_D_V_arg_val,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_methods_insert_I_D_V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "insert", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_insert_I_D_V_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_set_I_D_V_arg_idx = 
{
  0,
  0, // attributeRes
  "idx",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz() 
};

::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_methods_set_I_D_V_arg_val = 
{
  0,
  0, // attributeRes
  "val",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz() 
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_methods_set_I_D_V_args[] = 
{
  &BasicArray_methods_set_I_D_V_arg_idx,
  &BasicArray_methods_set_I_D_V_arg_val,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_set_I_D_V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "get", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_methods_set_I_D_V_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_method_remove_I_V_args_idx = 
{
  0,
  0, // attributeRes
  "idx",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz()
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_method_remove_I_V_args[] = 
{
  &BasicArray_method_remove_I_V_args_idx,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_remove_I_V = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "remove", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getUnknownBasicClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_method_remove_I_V_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_method_findFirst_D_I_args_val = 
{
  0,
  0, // attributeRes
  "val",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz() 
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_method_findFirst_D_I_args[] = 
{
  &BasicArray_method_findFirst_D_I_args_val,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_findFirst_D_I = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "findFirst", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_method_findFirst_D_I_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};



::acdk::lang::dmi::ClazzMethodArgInfo BasicArray_method_findLast_D_I_args_val = 
{
  0,
  0, // attributeRes
  "val",
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getDoubleClazz() 
};

::acdk::lang::dmi::ClazzMethodArgInfo* BasicArray_method_findLast_D_I_args[] = 
{
  &BasicArray_method_findLast_D_I_args_val,
  0
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzMethodInfo BasicArray_method_findLast_D_I = 
{
  1048577,// class flags, like static, Constructor
  0, // attributeRes
  "findLast", // name of method
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(), // return type
  0, //altname
  -1, // nameHashCode
  BasicArray_method_findLast_D_I_args, // return the arguments
  0, // argument count
  0,
  0, // exceptions
  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc
};


::acdk::lang::dmi::ClazzMethodInfo* _BasicArray_methods[] = 
{
  &BasicArray_method_BasicArray_I_LRBasicArray_,
  &BasicArray_method_get_I_D,
  &BasicArray_method_getref_I_D,
  &BasicArray_method_set_I_D_V,
  &BasicArray_method_length_I,
  &BasicArray_method_ensureCapacity_I_V,
  &BasicArray_method_resize_I_V,
  &BasicArray_method_append_D_V,
  &BasicArray_method_remove_I_V,
  &BasicArray_methods_insert_I_D_V,
  &BasicArray_method_findFirst_D_I,
  &BasicArray_method_findLast_D_I,
  0
};



::acdk::lang::dmi::ClazzInfo BasicArray_clazzInfo =
{
  dmi::MiClazzInfo | dmi::MiCiArray | dmi::MiResolved, // clazz-flags
  0, // attributeRes
  "[", // name of class
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  &BasicArray_clazzInfo,
  0, //_firstChild
  _BasicArray_interfaces, // pointer to Array of ClazzInfo references
  0,
  _BasicArray_fields, //pointer to Array of fields
  0, // 
  _BasicArray_methods, // pointer to Array of fields
  0, // 
  0, // create-function for cloning/serializing
  0, // create-function for Array of BasicArrays
  0, // create-function for ArrayArray of BasicArrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  BasicArray_DynamicDispatchFunction, 
  BasicArray_StandardDispatchFunction,
  0, // user defined info
  0, // UnitInfo* unitInfo
  0 // ClazzInfo* _next; 
};
static ::acdk::lang::dmi::RegisterClazzInfo _register_BasicArray(&BasicArray_clazzInfo);

#endif // ACDK_NOMETAINFO

//template class BasicArray<char>;
template class BasicArray<short>;
template class BasicArray<int>;
template class BasicArray<jlong>;
template class BasicArray<float>;
template class BasicArray<double>;

//template class RBasicArray<char>;
template class RBasicArray<short>;
template class RBasicArray<int>;
template class RBasicArray<jlong>;
template class RBasicArray<float>;
template class RBasicArray<double>;

// force template instantiate, will never be called
static void foo()
{
  ::acdk::io::RObjectReader rin;
  {
    charArray* array = 0;
    acdk::io::readBasicArray(array, rin);
  }
  {
    boolArray* array = 0;
    acdk::io::readBasicArray(array, rin);
  }
  {
    byteArray* array = 0;
    acdk::io::readBasicArray(array, rin);
  }
  {
    intArray* array  = 0;
    acdk::io::readBasicArray(array, rin);
  }
  {
    longArray* array  = 0;
    acdk::io::readBasicArray(array, rin);
  }
  {
    floatArray* array  = 0;
    acdk::io::readBasicArray(array, rin);
  }
  {
    doubleArray* array  = 0;
    acdk::io::readBasicArray(array, rin);
  }
}

/* not needed
void
_Basic_Array_read_object(::acdk::lang::RClass cls, ::acdk::lang::RObject This, ::acdk::io::RObjectReader in)
{
  int size = in->readInt();
  if (cls == Boolean::getTYPE()) {
    RbooleanArray arr = (RbooleanArray)This;
    arr->resize(size);
    for (int i = 0; i < size; i++) 
      arr->set(i, in->readBoolean());
  } else if (cls == Character::getTYPE()) {
    RcharArray arr = (RcharArray)This;
    arr->resize(size);
    for (int i = 0; i < size; i++) 
      arr->set(i, in->readChar());
  } else if (cls == Byte::getTYPE()) {
    RbyteArray arr = (RbyteArray)This;
    arr->resize(size);
    for (int i = 0; i < size; i++) 
      arr->set(i, (byte)in->readChar());
  } else if (cls == Short::getTYPE()) {
    RshortArray arr = (RshortArray)This;
    arr->resize(size);
    for (int i = 0; i < size; i++) 
      arr->set(i, in->readShort());
  } else if (cls == Integer::getTYPE()) {
    RintArray arr = (RintArray)This;
    arr->resize(size);
    for (int i = 0; i < size; i++) 
      arr->set(i, in->readInt());
  } else if (cls == Long::getTYPE()) {
    RlongArray arr = (RlongArray)This;
    arr->resize(size);
    for (int i = 0; i < size; i++) 
      arr->set(i, in->readLong());
  } else if (cls == Float::getTYPE()) {
    RfloatArray arr = (RfloatArray)This;
    arr->resize(size);
    for (int i = 0; i < size; i++) 
      arr->set(i, in->readFloat());
  } else if (cls == Double::getTYPE()) {
    RdoubleArray arr = (RdoubleArray)This;
    arr->resize(size);
    for (int i = 0; i < size; i++) 
      arr->set(i, in->readDouble());
  }
}
*/
