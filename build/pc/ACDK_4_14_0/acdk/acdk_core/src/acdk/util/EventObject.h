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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/EventObject.h,v 1.4 2005/02/05 10:45:05 kommer Exp $

#ifndef acdk_util_EventObject_h
#define acdk_util_EventObject_h

#include <acdk.h>

namespace acdk {
namespace util {


ACDK_DECL_CLASS(EventObject);

/**
  API: Java
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:05 $
*/

class ACDK_CORE_PUBLIC EventObject 
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(EventObject)
protected:
  RString _source;
public:
  EventObject(IN(RObject) source)
  : _source(source)
  {
  }
  virtual RObject getSource() { return &_source; }
  RString toString() 
  { 
    return getClass()->getName() + "[source=" + _source + "]";
  }
};


} // util
} // acdk

#endif //acdk_util_EventObject_h

