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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/Contained.h,v 1.7 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_Contained_h
#define org_omg_CORBA_Contained_h

#include "IRObject.h"
#include "Any.h"

namespace org {
namespace omg {
namespace CORBA {

enum DefinitionKind;
ACDK_DECL_CLASS(Container);
ACDK_DECL_CLASS(Repository);


ACDK_DECL_CLASS(Description);

ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbSetRepId("org/omg/CORBA/Contained/Description"))

class ACDKX_ORB_PUBLIC Description 
: implements ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(Description)
public:
  DefinitionKind kind;
  RAny value;
  /// needed for serialization
  Description() 
  {
  }
  Description(DefinitionKind kind_,  IN(RAny) value_) 
  : kind(kind_)
  , value(value_)
  {
  }
};

ACDK_DECL_CLASS(Contained);


ACDK_CLASSATTRIBUTE(acdk.tools.mc.InvokeForwardAttribute)
class ACDKX_ORB_PUBLIC Contained
: implements ::org::omg::CORBA::IRObject
{
  ACDK_WITH_METAINFO(Contained)
public:
  RString id;
  RString version;
  
  ACDK_FIELDATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_readonly"))
  RContainer defined_in;

  ACDK_FIELDATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_readonly"))
  RString absolute_name;

  ACDK_FIELDATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_readonly"))
  RRepository containing_repository;

  virtual RDescription describe();
  virtual void move(IN(RContainer) new_container, IN(RString) new_name, IN(RString) new_version);
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_Contained_h

