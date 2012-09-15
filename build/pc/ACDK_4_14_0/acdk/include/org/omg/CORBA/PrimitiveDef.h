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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/PrimitiveDef.h,v 1.7 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_PrimitiveDef_h
#define org_omg_CORBA_PrimitiveDef_h

#include "IDLType.h"

namespace org {
namespace omg {
namespace CORBA {

enum PrimitiveKind 
{
  pk_null, 
  pk_void, 
  pk_short, 
  pk_long, 
  pk_ushort, 
  pk_ulong,
  pk_float, 
  pk_double, 
  pk_boolean, 
  pk_char, 
  pk_octet,
  pk_any, 
  pk_TypeCode, 
  pk_Principal, 
  pk_string, 
  pk_objref,
  pk_longlong, 
  pk_ulonglong, 
  pk_longdouble, 
  pk_wchar, 
  pk_wstring
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, PrimitiveKind);

ACDK_DECL_CLASS(PrimitiveDef);

class ACDKX_ORB_PUBLIC PrimitiveDef
: implements ::org::omg::CORBA::IDLType
{
  ACDK_WITH_METAINFO(PrimitiveDef)
public:
  //readonly 
  PrimitiveKind kind;
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_PrimitiveDef_h
