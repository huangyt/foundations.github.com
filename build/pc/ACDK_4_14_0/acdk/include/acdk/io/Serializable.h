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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/Serializable.h,v 1.10 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_Serializable_h
#define acdk_io_Serializable_h

#ifndef acdk_h
#include <acdk.h>
#endif

namespace acdk {
namespace io {


ACDK_DECL_INTERFACE(Serializable);

/**
  Tag interface to declare a class as serializable.
  API: ACDK<br/>
  See also: gw_ref[acdk_hb_mi_serialization].
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:45 $
  
*/
class ACDK_CORE_PUBLIC Serializable 
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Serializable)
public:
  
};


} // io
} // acdk

#endif //acdk_io_Serializable_h

