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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Enumeration.h,v 1.10 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_Enumeration_h
#define acdk_util_Enumeration_h

#include <acdk.h>

namespace acdk {
namespace util {

using namespace acdk::lang;


ACDK_DECL_INTERFACE(Enumeration);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/

class ACDK_CORE_PUBLIC Enumeration
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Enumeration)
public :
  virtual bool hasMoreElements() = 0;
  /** for Iterator compatibility */
   bool hasNext() { return hasMoreElements(); }
  virtual RObject nextElement() = 0;
  /** the const version */
  //const RObject nextElement() { return const_cast<Enumeration*>(this)->nextElement(); }          
  /** for Iterator compatibility */
  RObject next() { return nextElement(); }
  /** for Iterator compatibility */
  //const RObject next() { return const_cast<Enumeration*>(this)->next(); }
};


} // util
} // acdk

#endif //acdk_util_Enumeration_h

