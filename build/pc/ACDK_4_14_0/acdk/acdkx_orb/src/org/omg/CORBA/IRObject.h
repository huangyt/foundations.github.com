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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/IRObject.h,v 1.9 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_IRObject_h
#define org_omg_CORBA_IRObject_h

#include "CORBA.h"
#include <org/omg/CORBA/portable/InvokeHandler.h>
#include <acdkx/orb/CorObject.h>

namespace org {
namespace omg {
namespace CORBA {

enum DefinitionKind
{
  dk_none, 
  dk_all,
  dk_Attribute, 
  dk_Constant, 
  dk_Exception, 
  dk_Interface,
  dk_Module, 
  dk_Operation, 
  dk_Typedef,
  dk_Alias, 
  dk_Struct, 
  dk_Union, 
  dk_Enum,
  dk_Primitive, 
  dk_String, 
  dk_Sequence, 
  dk_Array,
  dk_Repository,
  dk_Wstring, 
  dk_Fixed,
  dk_Value, 
  dk_ValueBox, 
  dk_ValueMember,
  dk_Native,
  dk_AbstractInterface,
  dk_LocalInterface
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, DefinitionKind);

ACDK_DECL_CLASS(IRObject);


// was already deaktived ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbDispatchAttribute())
class ACDKX_ORB_PUBLIC IRObject
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(IRObject)
public:
  DefinitionKind def_kind; // only used for declaration
  virtual ~IRObject() {}
  DefinitionKind get_def_kind() { const acdk::lang::dmi::ClazzInfo* ci = 0; return (DefinitionKind)(int)getDmiTarget(ci)->invoke("_get_def_kind"); }
  virtual void destroy () { const acdk::lang::dmi::ClazzInfo* ci = 0; getDmiTarget(ci)->invoke("destroy"); }
};



ACDK_DECL_CORPROXY(IRObject, ACDKX_ORB_PUBLIC);


/*
ACDK_DECL_CLASS(IRObjectProxy);

class ACDKX_ORB_PUBLIC IRObjectProxy
: extends ::acdkx::orb::CorObject
, implements IRObject
{
public:
  IRObjectProxy(IN(::acdkx::orb::RCorObject) obj)
    : CorObject(obj->objectKey(), obj->orb())
  {
    setRemoteClazz(IRObject::clazzInfo());
  }
};
*/
} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_IRObject_h
