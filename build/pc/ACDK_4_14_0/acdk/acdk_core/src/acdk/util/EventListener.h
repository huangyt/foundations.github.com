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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/EventListener.h,v 1.3 2005/02/05 10:45:05 kommer Exp $

#ifndef acdk_util_EventListener_h
#define acdk_util_EventListener_h


namespace acdk {
namespace util {


ACDK_DECL_INTERFACE(EventListener);

/**
  API: Java
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:05 $
*/

class ACDK_CORE_PUBLIC EventListener 
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(EventListener)
public:
};


} // util
} // acdk

#endif //acdk_util_EventListener_h
