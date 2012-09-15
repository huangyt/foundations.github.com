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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/AttributeDef.h,v 1.7 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_AttributeDef_h
#define org_omg_CORBA_AttributeDef_h

#include "IDLType.h"

namespace org {
namespace omg {
namespace CORBA {

enum AttributeMode 
{
  ATTR_NORMAL, 
  ATTR_READONLY
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, AttributeMode);

ACDK_DECL_CLASS(AttributeDef);

class ACDKX_ORB_PUBLIC AttributeDef
: implements ::org::omg::CORBA::IDLType
{
  ACDK_WITH_METAINFO(AttributeDef)
public:
  //readonly attribute 
  RTypeCode type;
  RIDLType type_def;
  AttributeMode mode;
};


ACDK_DECL_CLASS(AttributeDescription);
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
class ACDKX_ORB_PUBLIC AttributeDescription 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(AttributeDescription)
public:
  RString name;
  RString id;
  RString defined_in;
  RString version;
  RTypeCode type;
  AttributeMode mode;
  AttributeDescription() { }
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_AttributeDef_h
