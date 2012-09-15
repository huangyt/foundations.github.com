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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/OperationDef.h,v 1.7 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_OperationDef_h
#define org_omg_CORBA_OperationDef_h

#include "IDLType.h"
#include "Contained.h"
#include "ExceptionDef.h"

namespace org {
namespace omg {
namespace CORBA {

enum OperationMode 
{
  OP_NORMAL, 
  OP_ONEWAY
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, OperationMode);

enum ParameterMode 
{
  PARAM_IN, 
  PARAM_OUT, 
  PARAM_INOUT
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, ParameterMode);

ACDK_DECL_CLASS(ParameterDescription);
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
class ACDKX_ORB_PUBLIC ParameterDescription 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(ParameterDescription)
public:
  RString name;
  RTypeCode type;
  RIDLType type_def;
  ParameterMode mode;
  ParameterDescription() {}
};


ACDK_DECL_CLASS(OperationDef);

class ACDKX_ORB_PUBLIC OperationDef
: implements ::org::omg::CORBA::Contained
{
  ACDK_WITH_METAINFO(OperationDef)
public:
  //readonly attribute 
  RTypeCode result;
  RIDLType result_def;
  RParameterDescriptionArray params;
  OperationMode mode;
  RStringArray contexts;
  RExceptionDescriptionArray exceptions;
};


ACDK_DECL_CLASS(OperationDescription);
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("acdkx_orb_StructType"))
class ACDKX_ORB_PUBLIC OperationDescription 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(OperationDescription)
public:
  RString name;
  RString id;
  RString defined_in;
  RString version;
  RTypeCode result;
  OperationMode mode;
  RStringArray contexts;
  RParameterDescriptionArray parameters;
  RExceptionDescriptionArray exceptions;
  OperationDescription() {}
};



} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_OperationDef_h
