// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Parts of this class are ported from the of GNU Classpath project 
//  (http://www.gnu.org/software/classpath/classpath.html)
//   with following copyright statement:
// Copyright (c) 1998 by Stuart Ballard (stuart.ballard@mcmail.com),
//                       Geoff Berry (gcb@cs.duke.edu)
// Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Library General Public License as published
// by the Free Software Foundation, version 2. (see COPYING.LIB)
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library General Public License for more details.
//
// You should have received a copy of the GNU Library General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
// END


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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractMap.h,v 1.16 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_util_AbstractMap_h
#define acdk_util_AbstractMap_h

#include "Set.h"
#include "Map.h"
#include "AbstractSet.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(AbstractMap);


/**
  API: Java<br/>
  @author of the original classpath implementation: Stuart Ballard (stuart.ballard@mcmail.com), Geoff Berry (gcb@cs.duke.edu)
          Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/
class ACDK_CORE_PUBLIC AbstractMap
: extends acdk::lang::Object,
  implements Map
{
  ACDK_WITH_METAINFO(AbstractMap)
public:
  AbstractMap()
  {
  }
  virtual void clear();
  virtual bool containsKey(IN(RObject) key);
  virtual bool containsValue(IN(RObject) value);
  virtual RSet entrySet() = 0;
  virtual bool equals(IN(RObject) o);
  virtual RObject get(IN(RObject) key);
  virtual int hashCode();
  virtual bool isEmpty();
  virtual RSet keySet();
  virtual RObject put(IN(RObject) key, IN(RObject) value);
  virtual void putAll(IN(RMap) m);
  virtual RObject remove(IN(RObject) key);
  virtual int size();
  virtual RString toString();
  virtual RCollection values();

  friend class AbstractMapCachedKeySetIterator;
};


ACDK_DECL_CLASS(AbstractMapCachedKeySet);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC AbstractMapCachedKeySet
:  extends AbstractSet
{
  ACDK_WITH_METAINFO(AbstractMapCachedKeySet)
private:
  RAbstractMap _map;
public:
  AbstractMapCachedKeySet(IN(RAbstractMap) map)
  : AbstractSet(),
    _map(map)
  {
  }
  virtual bool isEmpty() { return size() == 0; }

  virtual int size() { return _map->size(); }
  virtual RIterator iterator();

  friend class AbstractMapCachedKeySetIterator;
};



ACDK_DECL_CLASS(AbstractMapCachedKeySetIterator);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC AbstractMapCachedKeySetIterator
: extends acdk::lang::Object,
  public Iterator
{
  ACDK_WITH_METAINFO(AbstractMapCachedKeySetIterator)
private:
  RIterator _it;
public:
  AbstractMapCachedKeySetIterator(IN(RAbstractMap) map)
  : Object(),
    Iterator(),
    _it(map->entrySet()->iterator())
  {
  }
  virtual bool hasNext() 
  {
    return _it->hasNext(); 
  }
  
  virtual RObject next() 
  {
    RMapEntry me;
    me = (RMapEntry)_it->next();
    return me->getKey(); 
  }
  virtual RObject element() 
  {
    return RMapEntry(_it->element())->getKey(); 
  }
  virtual void remove() { _it->remove(); }
};


ACDK_DECL_CLASS(AbstractMapCachedValuesCollection);

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC AbstractMapCachedValuesCollection
: extends AbstractCollection
{
  ACDK_WITH_METAINFO(AbstractMapCachedValuesCollection)
private:
  RAbstractMap _map;
public:
  RAbstractMap theMap() { return _map; }
  AbstractMapCachedValuesCollection(RAbstractMap map)
  : AbstractCollection(),
    _map(map)
  {
  }
  virtual int size() 
  {
    return _map->size(); 
  }
  virtual RSet entrySet() 
  {
    return _map->entrySet(); 
  }
  virtual RIterator iterator();
// collection
  bool equals(IN(RObject) o) 
  { 
    if (instanceof(o, AbstractMapCachedValuesCollection) == false)
      return false;
    return _map->equals((RObject)RAbstractMapCachedValuesCollection(o)->theMap());
  }
  int hashCode() { return _map->hashCode(); }
  
  /*
  bool add(RObject obj) { return _map->add(obj); }
  bool addAll(RCollection col) { return _map->addAll(col); }
  void clear() { _map->clear(); }
  bool isEmpty() { return _map->isEmpty(); }
  bool contains(RObject o) { return _map->contains(o); }
  bool containsAll(RCollection c) { return _map->containsAll(c); }
  bool remove(RObject o) { return _map->remove(o); }
  bool removeAll(RCollection c) { return _map->removeAll(c); }
  bool retainAll(RCollection c) { return _map->retainAll(c); }
  RObjectArray toArray() { return _map->toArray(); }
  RObjectArray toArray(RObjectArray arr) { _map->toArray(arr); }
  */
};


ACDK_DECL_CLASS(AbstractMapCachedValuesCollectionIterator);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/09 19:26:56 $
  
*/

class ACDK_CORE_PUBLIC AbstractMapCachedValuesCollectionIterator
: extends  acdk::lang::Object,
  implements Iterator
{
  ACDK_WITH_METAINFO(AbstractMapCachedValuesCollectionIterator)
private:
  RIterator _it;
public:
  AbstractMapCachedValuesCollectionIterator(IN(RAbstractMap) map)
  : _it(map->entrySet()->iterator())
  {
  }
  virtual bool hasNext() 
  {
    return _it->hasNext(); 
  }
  virtual RObject next() 
  {
    return RMapEntry(_it->next())->getValue();
  }
  virtual RObject element() 
  {
    return RMapEntry(_it->element())->getValue(); 
  }
  virtual void remove() 
  {
    _it->remove(); 
  }
};


} // util
} // acdk

#endif //acdk_util_AbstractMap_h


