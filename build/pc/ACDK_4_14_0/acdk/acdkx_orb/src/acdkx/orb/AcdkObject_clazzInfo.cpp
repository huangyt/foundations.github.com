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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AcdkObject_clazzInfo.cpp,v 1.12 2005/03/14 17:59:13 kommer Exp $

#include "AcdkObject.h"
#include "CorObject.h"
#include <acdk/lang/DmiTypeConversionException.h>
#include <org/omg/CORBA/TypeCode.h>
#include <acdk/lang/ParamsMismatchException.h>

namespace acdkx {
namespace orb {

using namespace acdk::lang::dmi;

char* _AcdkObject_Skel__ids[] = 
{
  "IDL:acdkx/orb/AcdkObject:1.0",
  0
};

::acdkx::orb::SkelInfoClassesStruct __AcdkObject_Skel_Info = 
{ 
  AcdkObject::clazzInfo(), 
  AcdkObject::create_instance, 
  0 
};

static::acdkx::orb::RegisterSkelInfoClass _register_AcdkObject_Skel(&__AcdkObject_Skel_Info);



::acdk::lang::dmi::ClazzFieldInfo AcdkObject_fields__acdkObject = 
{
  MiPrivate | MiFieldInfo,
  0, //AttributesRes
  "_acdkObject", // name
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
   ::acdk::lang::RObject::clazzInfo(),
   0, // accessor
  (void*)0 // address of field
};

::acdk::lang::dmi::ClazzFieldInfo* _AcdkObject_fields[] = 
{
  &AcdkObject_fields__acdkObject,
  0
};


//virtual
::acdk::lang::RClass
AcdkObject::getClass()
{
  ::acdk::lang::RClass tclass = GetClass();
  return tclass;
}

//static
::acdk::lang::RClass
AcdkObject::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
::acdk::lang::dmi::SysFields
AcdkObject::getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz)
{
  ::acdk::lang::dmi::SysFields _sys_fields = ACDK_FQ_SUPER_QUALIFIER(::acdkx::orb::, ServerDelegate)::getInternalFields(0, clazz);
  _sys_fields.push_back(::acdk::lang::dmi::SysField(& AcdkObject_fields__acdkObject, _acdkObject._ref_this())); // ::acdk::lang::RObject _acdkObject 
  return getImplFields(_sys_fields);
}

//virtual
void
AcdkObject::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(::acdkx::orb::, ServerDelegate)::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)_acdkObject._ref_this()); // ::acdk::lang::RObject _acdkObject 
}

::acdk::lang::dmi::ClazzSuperInfo _AcdkObject_super___acdkx__orb__ServerDelegate =
{
  MiPublic | MiSuperInfo,
  0, //AttributesRes
  ::acdkx::orb::ServerDelegate::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo _AcdkObject_super___org__omg__CORBA__portable__InvokeHandler =
{
  MiPublic | MiCiInterface | MiSuperInfo,
  0, //AttributesRes
  ::org::omg::CORBA::portable::InvokeHandler::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo _AcdkObject_super___acdkx_orb_AcdkObjectInterface =
{
  MiPublic | MiCiInterface | MiSuperInfo,
  0, //AttributesRes
  ::acdkx::orb::AcdkObjectInterface::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _AcdkObject_interfaces[] =
{
  &_AcdkObject_super___acdkx__orb__ServerDelegate,
  &_AcdkObject_super___org__omg__CORBA__portable__InvokeHandler,
  &_AcdkObject_super___acdkx_orb_AcdkObjectInterface,
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo AcdkObject_methods_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject__arg_localObject = 
{
  MiAiIn | MiMethodArgInfo, 
  0, //AttributesRes
  "localObject",
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::RObject::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo* AcdkObject_methods_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject__args[] = 
{
  &AcdkObject_methods_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject__arg_localObject,
  0
};

::acdk::lang::dmi::ClazzInfo* AcdkObject_methods_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo AcdkObject_method_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject_ = 
{
  MiPublic | MiMiConstructor | MiMethodInfo,// class flags, like static, Constructor
  0, //AttributesRes
  "AcdkObject", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  AcdkObject::clazzInfo(), // return type
  0, // alternative name of method
  -1, // hashCode
  AcdkObject_methods_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject__args, // return the arguments
  0, // args
  AcdkObject_methods_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject__exceptions // the declared exceptions
};

::acdk::lang::dmi::ClazzMethodArgInfo* AcdkObject_methods_GetClass__L_acdk_lang_RClass__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* AcdkObject_methods_GetClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo AcdkObject_method_GetClass__L_acdk_lang_RClass_ = 
{
  MiPublic | MiStatic | MiMethodInfo,// class flags, like static, Constructor
  0, //AttributesRes
  "GetClass", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::RClass::clazzInfo(), // return type
  0, // alternative name of method
  -1, // hashCode
  AcdkObject_methods_GetClass__L_acdk_lang_RClass__args, // return the arguments
  0, // arg num
  AcdkObject_methods_GetClass__L_acdk_lang_RClass__exceptions // the declared exceptions
};

::acdk::lang::dmi::ClazzMethodArgInfo* AcdkObject_methods_getClass__L_acdk_lang_RClass__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* AcdkObject_methods_getClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo AcdkObject_method_getClass__L_acdk_lang_RClass_ = 
{
  MiPublic | MiMiVirtual | MiMethodInfo,// class flags, like static, Constructor
  0, //AttributesRes
  "getClass", // name of method
  -1, // hashCode
  "",
  0, // _scopeParent
  0, // _nextSibling
  ::acdk::lang::RClass::clazzInfo(), // return type
  0, // alternative name of method
  -1, // hashCode
  AcdkObject_methods_getClass__L_acdk_lang_RClass__args, // return the arguments
  0, // arg num
  AcdkObject_methods_getClass__L_acdk_lang_RClass__exceptions // the declared exceptions

};

::acdk::lang::dmi::ClazzMethodInfo* _AcdkObject_methods[] = 
{
  &AcdkObject_method_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject_,
  &AcdkObject_method_GetClass__L_acdk_lang_RClass_,
  &AcdkObject_method_getClass__L_acdk_lang_RClass_,
  0
};

::acdk::lang::dmi::ClazzInfo AcdkObject::_clazzInfo =
{
  0, // clazz-flags
  0, //AttributesRes
  "AcdkObject", // name of class
  -1, // hashCode
  "acdkx/orb", // the namespace
  acdkx_orb_unitInfo.getMetaInfo(), // parent
  0, // _nextSibling
  &AcdkObject::_clazzInfo,
  0, // _firstChild
  _AcdkObject_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  _AcdkObject_fields, // pointer to Array of fields
  0, // count of Fields
  _AcdkObject_methods, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  &AcdkObject::create_array, // create-function for cloning/serializing arrays
  &AcdkObject::create_array_array, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  JLONG_CONSTANT(0), // jlong serialVersionUID; for serialization
  0, //&AcdkObject::standardDispatch, // static_dispatch
  &AcdkObject::StandardDispatch, // static_dispatch
  0, // count off all collectable members in this class
  0, // user defined info
  0 // next ClazzInfo in chain
 
};
static ::acdk::lang::dmi::RegisterClazzInfo _register_AcdkObject(AcdkObject::clazzInfo());


//static
::acdk::lang::RObject
AcdkObject::create_array(int length)
{
  return new ObjectArrayImpl<RAcdkObject>(length);
}

//static
::acdk::lang::RObject
AcdkObject::create_array_array(int firstLength, int secondLength)
{
  return Nil;//not implemented yet
}
//virtual
const ::acdk::lang::dmi::ClazzMethodInfo* 
AcdkObject::standardDispatch( IN(RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, 
                             ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, 
                             const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                             const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
 if (fname->equals("New") == true)
 {
   return StandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
 }
  if (methinf == 0)
    methinf = ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags);
  
  if (&AcdkObject_method_getClass__L_acdk_lang_RClass_ == methinf) 
  {
    ret = (::acdk::lang::RObject)getClass();
    return methinf;
  }
  if (methinf->dispatch != 0)
  {
    return methinf->dispatch(this, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  }
  return 0;
}

//static
const ::acdk::lang::dmi::ClazzMethodInfo* 
AcdkObject::StandardDispatch(IN(RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, 
                             ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, 
                             const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                             const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  /*
   if (strcmp(fname, "New") == 0)
  {
    if (args.size() < 1)
      THROW1(ParamsMismatchException, "AcdkObject::New needs name of class as first parameters");  
    RString name = args[0].getStringVar();
    ScriptVarArray sargs(args.size() - 1);
    for (int i = 1; i < args.size(); ++i)
      sargs[i - 1] = args[i];
    ret = New(name, sargs, namedArgs, dc); 
    return (const ::acdk::lang::dmi::ClazzMethodInfo* )1;
  }*/
  if (methinf == 0)
    methinf = ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags);
  if (methinf->dispatch != 0)
  {
    return methinf->dispatch(0, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  }

  if (&AcdkObject_method_AcdkObject_INL_acdk_lang_RObject__LRAcdkObject_ == methinf) {
    ret = (::acdk::lang::RObject)new AcdkObject((::acdk::lang::RObject)args[0].getObjectVar());
    return methinf;
  }
  if (&AcdkObject_method_GetClass__L_acdk_lang_RClass_ == methinf) {
    ret = (::acdk::lang::RObject)GetClass();
    return methinf;
  }
  return 0;
  /*
  if ((methinf = ACDK_FQ_SUPER_QUALIFIER(::acdkx::orb::, ServerDelegate)::StandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf)) != 0)
    return methinf;
  if ((methinf = ACDK_FQ_SUPER_QUALIFIER(::org::omg::CORBA::portable::, InvokeHandler)::StandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf)) != 0)
    return methinf;
  return 0;
  */
}




} // namespace orb 
} // namespace acdkx 


