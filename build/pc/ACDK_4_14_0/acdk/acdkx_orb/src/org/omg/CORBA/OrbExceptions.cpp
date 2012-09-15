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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/OrbExceptions.cpp,v 1.8 2005/02/05 10:45:40 kommer Exp $


#include "OrbExceptions.h"

namespace org {
namespace omg {
namespace CORBA {

//virtual 
RString 
SystemException::toString() 
{
  RString superString = RuntimeException::toString();
  RString completedString = "";
  RString minorString = "";
  
  if (_minor != 0) {
    minorString = RString(" minor code: ") + _minor;
  }
  
  switch (_completed) {
    case COMPLETED_YES:
      completedString = "  completed: Yes";
      break;
    case COMPLETED_NO:
      completedString = "  completed: No";
      break;
    case COMPLETED_MAYBE:
      // no break
    default:
      completedString = "  completed: Maybe";
      break;
    }
  return superString + minorString + completedString;
} 



} // namespace CORBA
} // namespace omg
} // namespace org 


