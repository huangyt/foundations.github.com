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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/util/Source.h,v 1.4 2005/02/05 10:44:51 kommer Exp $


#ifndef acdk_aci_util_Source_h
#define acdk_aci_util_Source_h

#include "CodeLocation.h"

namespace acdk {
namespace aci {
namespace util {



ACDK_DECL_INTERFACE(Source);
/**
  General representation of Source
*/
class ACDK_ACI_PUBLIC Source
      ACDK_INTERFACEBASE
{
public:
  /**
    return the name of the source, for example file name
  */
  virtual RString getSourceName() = 0;
  /**
    return the source code from the location
    return complete text if location is Nil
  */
  virtual RString getSourceFromLocation(IN(RCodeLocation) location) = 0;
};



} // util
} // aci
} // acdk


#endif //acdk_aci_util_Source_h

