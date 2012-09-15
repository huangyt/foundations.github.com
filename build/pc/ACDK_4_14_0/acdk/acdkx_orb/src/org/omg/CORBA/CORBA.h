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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/CORBA.h,v 1.9 2005/04/10 12:52:41 kommer Exp $

#ifndef org_omg_CORBA_CORBA_h
#define org_omg_CORBA_CORBA_h

#include <acdkx/orb/Config.h>
#include <acdk.h>

ACDK_DECL_UNIT(org_omg_CORBA)

namespace org {
/**
  classes originally defined by the Open Management Group (OMG)
*/
namespace omg {
/** 
  Interfaces of Corba
  */
namespace CORBA {

}
}
}


#define octet byte
#define sequence ::acdk::lang::sys::core_vector


#endif //org_omg_CORBA_CORBA_h
