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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/InterfaceDef.h,v 1.6 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_InterfaceDef_h
#define org_omg_CORBA_InterfaceDef_h

#include "IDLType.h"
#include "OperationDef.h"
#include "AttributeDef.h"

namespace org {
namespace omg {
namespace CORBA {

enum AttributeMode;
enum OperationMode;

ACDK_DECL_CLASS(FullInterfaceDescription);

ACDK_CLASSATTRIBUTE(acdkx.orb.mc.OrbSetRepId("org/omg/CORBA/InterfaceDef/FullInterfaceDescription"))
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
class ACDKX_ORB_PUBLIC FullInterfaceDescription 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(FullInterfaceDescription)
public:
  RString name;
  RString id;
  RString defined_in;
  RString version;
  RParameterDescriptionArray operations;
  RAttributeDescriptionArray attributes;
  RStringArray base_interfaces;
  RTypeCode type;
  FullInterfaceDescription() {}
};


ACDK_DECL_CLASS(InterfaceDescription);
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
class ACDKX_ORB_PUBLIC InterfaceDescription 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(InterfaceDescription)
public:
  RString name;
  RString id;
  RString defined_in;
  RString version;
  RStringArray base_interfaces;
  InterfaceDescription() {}
};

ACDK_DECL_CLASS(InterfaceDef);

ACDK_CLASSATTRIBUTE(acdk.tools.mc.InvokeForwardAttribute)
class ACDKX_ORB_PUBLIC InterfaceDef
: implements ::org::omg::CORBA::Contained
, implements ::org::omg::CORBA::Container
, implements ::org::omg::CORBA::IDLType
{
  ACDK_WITH_METAINFO(InterfaceDef)
public:
  RInterfaceDefArray base_interfaces;
  virtual bool is_a (IN(RString) interface_id);
  virtual RFullInterfaceDescription describe_interface();

  virtual RAttributeDef create_attribute(IN(RString) id, IN(RString) name, IN(RString) version,
                                         IN(RIDLType) type, AttributeMode mode);
  virtual ROperationDef create_operation(IN(RString) id, IN(RString) name, IN(RString) version,
                                         IN(RIDLType) result, OperationMode mode,
                                         IN(ROperationDescriptionArray) params, IN(RExceptionDescriptionArray) exceptions,
                                         IN(RStringArray) contexts);
};





} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_InterfaceDef_h
