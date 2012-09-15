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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/DoubleIterator.h,v 1.10 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_DoubleIterator_h
#define acdk_util_DoubleIterator_h

#include "Iterator.h"

namespace acdk {
namespace util {

using namespace ::acdk::lang;

ACDK_DECL_CLASS(DoubleIterator);

/** 
  Joins 2 Iterator to single one.  At first, all elements in iterator1 will listed. 
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/

class ACDK_CORE_PUBLIC  DoubleIterator
: extends acdk::lang::Object,
  implements Iterator
{
  ACDK_WITH_METAINFO(DoubleIterator)
private:
  RIterator _it1;
  RIterator _it2;
  mutable bool _in1;
  mutable bool _checkinin1;
public :
  DoubleIterator(IN(RIterator) it1, IN(RIterator) it2)
  : Object(),
    _it1(it1),
    _it2(it2),
    _in1(true),
    _checkinin1(false)
  {
  }
  foreign virtual bool hasNext() 
  {
    if (_in1 == true) {
      if (_checkinin1 == true)
        return true;
      if (_it1->hasNext() == true) {
        _checkinin1 = true;
        return true;
      } else
        _in1 = false;
    }
    return _it2->hasNext();
  }

  foreign virtual RObject next()
  {
    if (_in1 == true) {
      if (_checkinin1) {
        _checkinin1 = false;
        return _it1->next();
      } else if (_it1->hasNext()) {
        return _it1->next();
      } else {
        _in1 = false;
      }
    }
    return _it2->next();
  }
  foreign virtual RObject element() 
  {
    if (_in1 == true)
      return _it1->element();
    else
      return _it2->element();
  }

  foreign virtual void remove()
  {
    if (_in1 == true)
      _it1->remove();
    else
      _it2->remove();
  }
};

} // namespace util 
} //namespace acdk 


#endif //acdk_util_DoubleIterator_h

