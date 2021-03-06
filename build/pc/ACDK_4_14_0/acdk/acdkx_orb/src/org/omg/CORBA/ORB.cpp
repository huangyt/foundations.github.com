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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/ORB.cpp,v 1.8 2005/02/05 10:45:40 kommer Exp $


#include "ORB.h"
#include <acdkx/orb/AORB.h>

namespace org {
namespace omg {
namespace CORBA {

//static 
RORB 
ORB::init()
{
  return ::acdkx::orb::AORB::getORB();
}

//static 
RORB 
ORB::init(IN(RStringArray) args, IN(acdk::util::RProperties) props/* = Nil*/)
{
  ::acdkx::orb::RAORB aorb = &::acdkx::orb::AORB::getAORB();
  aorb->init(args, props);
  return &aorb;

}

} // namespace CORBA
} // namespace omg
} // namespace org 


