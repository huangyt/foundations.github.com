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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Iterator.h,v 1.11 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_Iterator_h
#define acdk_util_Iterator_h

#ifndef acdk_h
#include <acdk.h>
#endif

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_INTERFACE(Iterator);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC Iterator
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Iterator)
public :
  virtual bool hasNext() = 0;

  virtual RObject next() = 0;
  //const RObject next() { return const_cast<Iterator*>(this)->next(); }

  /***
    API: Extension, may not supported by some Containers 
    returns current Element, whithout forward to next element
  */
  virtual RObject element() = 0;

  //const RObject element() { return const_cast<Iterator*>(this)->element(); }
  /** 
    remove current element from the iterator. 
    May not support by all Iterator
  */
  virtual void remove() = 0;
  /** for java.util.Enumeration - compatibility */
  bool hasMoreElements()
  {
    return hasNext();
  }
  /** for java.util.Enumeration - compatibility */
  RObject nextElement() 
  {
    return next();
  }
};

} // util
} // acdk

#endif //acdk_util_Iterator_h

