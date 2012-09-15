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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TBasicMapEntry.h,v 1.9 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TBasicMapEntry_h
#define acdk_util_TBasicMapEntry_h

#include "TMap.h"

namespace acdk {
namespace util {

using namespace acdk::lang;




/**
  @internal
  Used by TTreeSet.

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/

template <class K, class V>
class TBasicMapEntry
: extends acdk::lang::Object,
  implements TMapEntry<K, V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TBasicMapEntry<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TMapEntry<RKeyType, RValueType> MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

public:
  RKeyType _key;
  RValueType _value;
public:
  TBasicMapEntry(IN(RKeyType) newKey, IN(RValueType) newValue) 
  : Object(),
    _key(newKey),
    _value(newValue)
  {
  }

  foreign virtual bool equals(IN(RMapEntryType) other) 
  { 
    if (acdk_equals(other->getKey(), getKey()) == false)
        return false;
    if (acdk_equals(other->getValue(), getValue()) == false)
      return false;
    return true;
  }
  foreign virtual bool equals(IN(RObject) other)
  {
    if (instanceof(other, MapEntryType) == false)
      return false;
    return equals(RMapEntryType(other)); 
  }
  foreign virtual RKeyType getKey() { return _key; }
  foreign virtual RValueType getValue() { return _value; }
  foreign virtual int hashCode() 
  { 
    RKeyType key = getKey();
    RValueType value = getValue();
    return acdk_hashCode(key) ^ acdk_hashCode(value);
  }
  RString toString()
  {
    acdk::lang::StringBuffer sb;
    sb.append(acdk_toString(_key));
    sb.append("=");
    sb.append(acdk_toString(_value));
    return sb.toString();
  }
  foreign virtual RValueType setValue(INP(RValueType) newVal)
  {
    RValueType ret = getValue();
    _value = newVal;
    return ret;
  }
};


} // util
} // acdk

#endif // acdk_util_TBasicMapEntry_h

