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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Hashtable.h,v 1.12 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_Hashtable_h
#define acdk_util_Hashtable_h

#include <acdk.h>

#include "Dictionary.h"
#include "Map.h"
#include "HashMap.h"

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;


ACDK_DECL_CLASS(Hashtable);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/

class ACDK_CORE_PUBLIC Hashtable
: extends acdk::util::Dictionary,
  implements acdk::util::Map,
  implements acdk::io::Serializable,
  implements acdk::lang::Cloneable

{
  ACDK_WITH_METAINFO(Hashtable)
public:
  Hashtable();

  Hashtable(int initialCapacity, float loadFactor);

  Hashtable(int initialCapacity);
  
  Hashtable(IN(RMap) t);
  virtual ~Hashtable();

  foreign virtual int size() 
  {
    SYNCTHIS();
    return _table->size(); 
  }

  foreign virtual bool isEmpty() 
  {
    SYNCTHIS();
    return _table->isEmpty(); 
  }

  foreign virtual void clear() 
  {
    SYNCTHIS();
    _table->clear();
  }
  foreign virtual RObject clone() { return clone(allocator()); }
  foreign virtual RObject clone(sys::Allocator* alc) 
  {
    SYNCTHIS();
    return _table->clone();
  }

  foreign virtual RSet keySet() 
  {
    SYNCTHIS();
    return _table->keySet();
  }

  foreign virtual RSet entrySet() 
  {
    SYNCTHIS();
    return _table->entrySet();
  }
  foreign virtual RCollection values() 
  {
    SYNCTHIS();
    return _table->values();
  }

  foreign virtual bool containsKey(IN(RObject) key) 
  {
    SYNCTHIS();
    return _table->containsKey(key);
  }

  foreign virtual bool containsValue(IN(RObject) value) 
  {
    SYNCTHIS();
    return _table->containsValue(value);
  }

  foreign virtual RObject get(IN(RObject) key) 
  {
    SYNCTHIS();
    return _table->get(key);
  }

  foreign virtual RObject put(IN(RObject) key, IN(RObject) value) 
  {
    SYNCTHIS();
    return _table->put(key, value);
  }

  foreign virtual void putAll(IN(RMap) t) 
  {
    SYNCTHIS();
    _table->putAll(t);
  }

  foreign virtual RObject remove(IN(RObject) key) 
  {
    SYNCTHIS();
    return _table->remove(key);
  }
  
  int capacity() 
  {
    SYNCTHIS();
    return _table->capacity() ;
  }
  
  float loadFactor() 
  {
    SYNCTHIS();
    return _table->loadFactor();
  }

  

  foreign bool equals(IN(RObject) obj);

  foreign int hashCode(); 

  REnumeration keys();
  REnumeration elements();
protected:
  RHashMap _table;
private:

};


} // Util
} //acdk

#endif //acdk_util_Hashtable_h

