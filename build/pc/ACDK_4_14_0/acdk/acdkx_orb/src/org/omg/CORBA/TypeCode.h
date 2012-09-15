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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/TypeCode.h,v 1.9 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_TypeCode_h
#define org_omg_CORBA_TypeCode_h

#include "CORBA.h"


namespace org {
namespace omg {
namespace CORBA {


/**
  CORBA types
*/
enum TCKind 
{
  tk_null = 0,
  tk_void,
  tk_short,
  tk_long,
  tk_ushort,
  tk_ulong,
  tk_float,
  tk_double,
  tk_boolean,
  tk_char,
  tk_octet,
  tk_any,
  tk_TypeCode,
  tk_Principal,
  tk_objref,
  tk_struct,
  tk_union,
  tk_enum,
  tk_string,
  tk_sequence,
  tk_array,
  tk_alias,
  tk_except,
  tk_longlong,
  tk_ulonglong,
  tk_longdouble,
  tk_wchar,
  tk_wstring,
  tk_fixed,
  tk_value,
  tk_value_box,
  tk_native,
  tk_abstract_interface
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, TCKind);


ACDK_DECL_CLASS(TypeCode);

class ACDKX_ORB_PUBLIC TypeCode
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(TypeCode)
private:
  TCKind _type;
public:
  TypeCode(TCKind typ)
    : ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object())
    , _type(typ)
  {
  }
  TCKind kind() { return _type; }
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_TypeCode_h
