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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/Repository.h,v 1.6 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_Repository_h
#define org_omg_CORBA_Repository_h

#include "Container.h"
#include "TypeCode.h"
#include "PrimitiveDef.h"
#include "StringDef.h"

namespace org {
namespace omg {
namespace CORBA {

enum DefinitionKind;
enum PrimitiveKind;


ACDK_DECL_CLASS(Repository);

ACDK_CLASSATTRIBUTE(acdk.tools.mc.InvokeForwardAttribute)
class ACDKX_ORB_PUBLIC Repository
: implements ::org::omg::CORBA::Container
{
  ACDK_WITH_METAINFO(Repository)
public:
  RContained lookup_id(IN(RString) search_id);
  RTypeCode get_canonical_typecode(IN(RTypeCode) tc);
  RPrimitiveDef get_primitive(PrimitiveKind kind);
  
  // write interface
  RStringDef create_string(int bound);
  //WstringDef create_wstring(in unsigned long bound);
  //SequenceDef create_sequence (in unsigned long bound, in IDLType element_type);
  //ArrayDef create_array (in unsigned long length,in IDLType element_type);
  // FixedDef create_fixed(in unsigned short digits, in short scale );
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_Repository_h
