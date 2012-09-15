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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/IDLType.h,v 1.6 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_IDLType_h
#define org_omg_CORBA_IDLType_h

#include "IRObject.h"
#include "TypeCode.h"

namespace org {
namespace omg {
namespace CORBA {

enum DefinitionKind;



ACDK_DECL_CLASS(IDLType);

class ACDKX_ORB_PUBLIC IDLType
: implements ::org::omg::CORBA::IRObject
{
  ACDK_WITH_METAINFO(IDLType)
public:
  RTypeCode type;
};

} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_IDLType_h
