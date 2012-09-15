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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Comparable.h,v 1.13 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Comparable_h
#define acdk_lang_Comparable_h

#include <acdk.h>
#include <acdk/lang/InterfaceBase.h>

namespace acdk {
namespace lang {


ACDK_DECL_INTERFACE(Comparable);

/**
  Interface for comparing 2 objects.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.13 $
  @date $Date: 2005/04/09 19:26:48 $
*/  
class ACDK_CORE_PUBLIC Comparable 
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Comparable)
public:
  virtual int compareTo(IN(RObject) o) = 0 ;
};





}
}

#endif //acdk_lang_Comparable_h

