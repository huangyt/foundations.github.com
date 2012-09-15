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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/BasicMapEntry.h,v 1.12 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_BasicMapEntry_h
#define acdk_util_BasicMapEntry_h

#include "Map.h"
#include <acdk/io/Serializable.h>

namespace acdk {
namespace util {

using namespace acdk::lang;


ACDK_DECL_CLASS(BasicMapEntry);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC BasicMapEntry
: extends acdk::lang::Object
, implements MapEntry
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(BasicMapEntry)
public:
  RObject _key;
  RObject _value;
public:
  BasicMapEntry(IN(RObject) newKey, IN(RObject) newValue) 
  :  _key(newKey),
    _value(newValue)
  {
  }
  foreign virtual bool equals(IN(RObject) o) 
  { 
    if (instanceof(o, MapEntry) == false)
      return false;
    RMapEntry other = RMapEntry(o);
    if (other->getKey() == Nil) {
      if (getKey() != Nil)
        return false;
    } else if (other->getKey()->equals(getKey()) == false)
      return false;
    if (other->getValue() == Nil) {
      if (getValue() != Nil)
        return false;
    } else if (other->getValue()->equals(getValue()) == false)
      return false;
    return true;
  }
  foreign virtual RObject getKey() { return _key; }
  foreign virtual RObject getValue() { return _value; }
  foreign virtual int hashCode() 
  { 
    RObject key = getKey();
    RObject value = getValue();
    return ((key == Nil) ? 0 : key->hashCode()) ^ ((value == Nil) ? 0 : value->hashCode());
  }
  foreign virtual RObject setValue(IN(RObject) newVal)
  {
    RObject ret = getValue();
    _value = newVal;
    return ret;
  }
};


} // util
} // acdk

#endif // acdk_util_BasicMapEntry_h

