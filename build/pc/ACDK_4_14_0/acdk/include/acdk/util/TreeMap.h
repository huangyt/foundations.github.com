// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Parts of this class are ported from the of GNU Classpath project 
//  (http://www.gnu.org/software/classpath/classpath.html)
//   with following copyright statement:

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
// Copyright (C) 1998, 1999, 2001, 2002, 2003, 2004
//                Free Software Foundation, Inc.
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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TreeMap.h,v 1.22 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_TreeMap_h
#define acdk_util_TreeMap_h

#include "Map.h"
#include "AbstractMap.h"
#include "AbstractSet.h"

#include "SortedMap.h"
#include "Comparator.h"
#include "BasicMapEntry.h"
#include "ConcurrentModificationException.h"
#include "AbstractCollection.h"

#include "ConcurrentModificationException.h"
#include <acdk/lang/UnsupportedOperationException.h>

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

namespace acdk {
namespace util {

using namespace acdk::lang;

ACDK_DECL_CLASS(RedBlackNode);

enum NodeType 
{
  RedNodeType = -1,
  BlackNodeType = 1
};

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC RedBlackNode 
: extends BasicMapEntry 
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(RedBlackNode)
public:
 
  NodeType _color;
  RRedBlackNode _left;
  RRedBlackNode _right;
  RRedBlackNode _parent;

  RedBlackNode(IN(RObject) key, IN(RObject) value)
  : BasicMapEntry(key, value),
    _color(BlackNodeType),
    _left(nilNode()),
    _right(nilNode()),
    _parent(nilNode())
  {
  }
  static RRedBlackNode _nilNode;
  static RRedBlackNode nilNode();

  void _clear()
  {
    RRedBlackNode nn = nilNode();
    if (_left != nn && _left != Nil && _left.iptr() != this)
      _left->_clear();
    if (_right != nn && _right != Nil && _right.iptr() != this)
      _right->_clear();
    _left = Nil;
    _right = Nil;
    _parent = Nil;
  }
};

/**
  Specialized Version for the NilNode

*/
class ACDK_CORE_PUBLIC NilRedBlackNode
: extends RedBlackNode
{
public:
  NilRedBlackNode();
  ~NilRedBlackNode();
};



/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
enum MapEntryTyp 
{
  Entries = 0,
  Keys = 1,
  Values = 2
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, MapEntryTyp);


ACDK_DECL_CLASS(TreeMap);
/**
  Implements a standard sortable map using black/red trees

  This class is partly ported from the GNU Classpath implementation
  @author Original GNU Classpath implementation: Jon Zeppieri
          // Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
  @author ACDK: Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC TreeMap 
: extends AbstractMap,
  implements SortedMap,
  implements acdk::lang::Cloneable,
  implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(TreeMap)
private:
  RRedBlackNode _root;
  int _size;
  RComparator _comparator;
  int _modCount;
public:
  static RObject create_instance() { return new TreeMap(RMap(Nil)); }
  TreeMap(IN(RComparator) comp = Nil);
  TreeMap(IN(RMap) map);
  TreeMap(IN(RSortedMap) sortedMap);

  virtual void clear();
  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc);
  
  virtual RComparator comparator()
  {
    return _comparator;
  }

  virtual bool containsKey(IN(RObject) key)
  {
    return (treeSearch(_root, _comparator, key) != nilNode());
  }

  virtual bool containsValue(IN(RObject) val);
  
  virtual RSet entrySet();
  virtual RObject firstKey();
  virtual RObject get(IN(RObject) key);
  virtual RSortedMap headMap(IN(RObject) key);
  virtual RSet keySet();
  virtual RObject lastKey();
  virtual RObject put(IN(RObject) key, IN(RObject) val);
  virtual void putAll(IN(RMap) map);
  virtual RObject remove(IN(RObject) key);
  virtual int size()
  {
    return _size;
  }
  virtual RSortedMap subMap(IN(RObject) from, IN(RObject) to);
  virtual RSortedMap tailMap(IN(RObject) from);
  virtual RCollection values();
  
  virtual bool equals(IN(RObject) other)
  {
    return AbstractMap::equals(other);
  }
  virtual int hashCode()
  {
    return AbstractMap::hashCode();
  }
  virtual bool isEmpty()
  {
    return AbstractMap::isEmpty();
  }
protected:
  void putAllLinear(IN(RMapEntryArray) entries);
private:
  foreign static RRedBlackNode buildTree(IN(RMapEntryArray) entries, int height,  bool completed, int currentTier,  int start, int end,
                                         acdk::lang::sys::Allocator* alloc);
  static int compare(IN(RComparator) comp, IN(RObject) o1, IN(RObject) o2)
  {
    if (comp == Nil)
      return RComparable(o1)->compareTo(o2);
    return comp->compare(o1, o2);
  }

  static bool keyInMinRange(IN(RComparator) comp, IN(RObject) key, IN(RObject) minKey)
  {
    return ((minKey == Nil) ||  (compare(comp, minKey, key) <= 0));
  }

  static bool keyInMaxRange(IN(RComparator) comp, IN(RObject) key, IN(RObject) maxKey)
  {
    return ((maxKey == Nil) ||  (compare(comp, maxKey, key) > 0));
  }
  static bool keyInClosedMaxRange(IN(RComparator) comp, IN(RObject) key, IN(RObject) maxKey)
  {
    return ((maxKey == Nil) ||  (compare(comp, maxKey, key) >= 0));
  }
  static bool keyInRange(IN(RComparator) comp, IN(RObject) key, IN(RObject) minKey, IN(RObject) maxKey)
  {
    return (keyInMinRange(comp, key, minKey) && keyInMaxRange(comp, key, maxKey));
  }

  static bool keyInClosedRange(IN(RComparator) comp, IN(RObject) key, IN(RObject) minKey, IN(RObject) maxKey)
  {
    return (keyInMinRange(comp, key, minKey) && keyInClosedMaxRange(comp, key, maxKey));
  }
  static RRedBlackNode treeSearch(IN(RRedBlackNode) root, IN(RComparator) comp, IN(RObject) key);
  static RRedBlackNode treeMin(IN(RRedBlackNode) root);
  static RRedBlackNode treeMinConstrained(IN(RRedBlackNode) root, IN(RComparator) comp, IN(RObject) minKey, IN(RObject) maxKey);
  static RRedBlackNode treeMax(IN(RRedBlackNode) root);
  static RRedBlackNode treeMaxConstrained(IN(RRedBlackNode) root, IN(RComparator) comp, IN(RObject) minKey, IN(RObject) maxKey);

  static RRedBlackNode lowerBound(IN(RRedBlackNode) root, IN(RComparator) comp, IN(RObject) minKey, IN(RObject) maxKey)
  {
    return ((minKey != Nil) ? treeMinConstrained(root, comp, minKey, maxKey) : treeMin(root));
  }

  static RRedBlackNode upperBound(RRedBlackNode root, RComparator comp, RObject minKey, RObject maxKey)
  {
    return ((maxKey != Nil) ? treeMaxConstrained(root, comp, minKey, maxKey) : nilNode());
  }

  static RRedBlackNode treeSuccessor(IN(RRedBlackNode) nod);
  static RRedBlackNode treePredecessor(IN(RRedBlackNode) nod);
  foreign static RRedBlackNode treeInsert(IN(RTreeMap) tree,  IN(RComparator) comp,  IN(RRedBlackNode) newNode, acdk::lang::sys::Allocator* alloc);
  static void leftRotate(IN(RTreeMap) tree, IN(RRedBlackNode) nod);
  static void rightRotate(IN(RTreeMap) tree, IN(RRedBlackNode) nod);
  
  static RRedBlackNode rbInsert(IN(RTreeMap) tree,  IN(RComparator) comp, IN(RRedBlackNode) nod);
  
  static RRedBlackNode rbDelete(IN(RTreeMap) tree, IN(RRedBlackNode) nod, acdk::lang::sys::Allocator* alloc);
  static void rbDeleteFixup(IN(RTreeMap) tree, IN(RRedBlackNode) nod);

  static RRedBlackNode nilNode()
  {
    return RedBlackNode::nilNode();
  }
  friend class TreeMapIterator;
  
  friend class SubTreeMap;
  friend class TreeMapSetIterator;
  friend class TreeSet;
};


ACDK_DECL_CLASS(TreeMapIterator);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC TreeMapIterator
: extends acdk::lang::Object,
  implements Iterator
{
  ACDK_WITH_METAINFO(TreeMapIterator)
private:
  RTreeMap _map;
  RRedBlackNode _first;
  RRedBlackNode _last;
  RRedBlackNode _prev;
  MapEntryTyp _type;
  int _knownMods;
public:
  TreeMapIterator(IN(RTreeMap) map, IN(MapEntryTyp) type);
  virtual bool hasNext()
  {
    _checkMod();
    return (_first != TreeMap::nilNode());
  }
  virtual RObject element();
  virtual RObject next();
  
  virtual void remove();
private:
  void _checkMod()
  {
    if (_knownMods < _map->_modCount)
      THROW0(ConcurrentModificationException);
    }

};

ACDK_DECL_CLASS(TreeMapCollection);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC TreeMapCollection 
: extends AbstractCollection
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(TreeMapCollection)
private:
  RSortedMap _map;
public:
  TreeMapCollection(IN(RSortedMap) map)
  : AbstractCollection(),
    _map(map)
  {
  }
  virtual bool add(IN(RObject) object)
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual bool addAll(IN(RCollection) coll) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual void clear()
  {
    _map->clear();
  }
  virtual bool contains(IN(RObject) object)
  {
    return _map->containsValue(object);
  }
  virtual bool isEmpty()
  {
    return _map->isEmpty();
  }
  virtual int size()
  {
    return _map->size();
  }
  virtual RIterator iterator();
  virtual bool equals(IN(RObject) obj)
  {
    return _map->equals(obj);
  }
  virtual int hashCode()
  {
    return _map->hashCode();
  }
};

  
ACDK_DECL_CLASS(TreeMapSet);
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC TreeMapSet 
: extends AbstractSet
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(TreeMapSet)
private:
  RSortedMap _map;
  RTreeMap _treeMap;
  MapEntryTyp _type;
public:
  TreeMapSet(IN(RSortedMap) sortedmap, IN(RTreeMap) treeMap, IN(MapEntryTyp) type)
  : AbstractSet(),
    _map(sortedmap),
    _treeMap(treeMap),
    _type(type)
  {
  }
  virtual bool add(IN(RObject) object) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual bool addAll(IN(RCollection) coll) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual void clear()
  {
    _map->clear();
  }
  virtual bool contains(IN(RObject) object);
  virtual bool isEmpty()
  {
    return _map->isEmpty();
  }
  virtual bool remove(IN(RObject) object);
  virtual int size()
  {
    return _map->size();
  }
  virtual RIterator iterator();
};


ACDK_DECL_CLASS(TreeMapSetIterator);  

class ACDK_CORE_PUBLIC TreeMapSetIterator
: extends acdk::lang::Object
, implements Iterator
{
  ACDK_WITH_METAINFO(TreeMapSetIterator)
private:
  RSortedMap _map;
  RTreeMap _treeMap;
  RRedBlackNode _first;
  RRedBlackNode _last;
  RRedBlackNode _prev;
  MapEntryTyp _type;
  int _knownMods;
public:
  TreeMapSetIterator(IN(RSortedMap) map, IN(RTreeMap) treemap, MapEntryTyp type);

  virtual bool hasNext()
  {
    _checkMod();
    return (_first != TreeMap::nilNode());
  }
  virtual RObject element();
  virtual RObject next();
  
  virtual void remove();
private:
  void _checkMod()
  {
    if (_knownMods < _treeMap->_modCount)
      THROW0(ConcurrentModificationException);
    }

};



ACDK_DECL_CLASS(SubTreeMap);  
/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC SubTreeMap
: extends AbstractMap
, implements SortedMap
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(SubTreeMap)
private:
  RTreeMap _map;
  RObject _minKey;
  RObject _maxKey;
public:
  SubTreeMap(IN(RTreeMap) map, IN(RObject) minKey, IN(RObject) maxKey)
  : _map(map),
    _minKey(minKey),
    _maxKey(maxKey)
  {
  }
  virtual void clear();
  virtual bool containsKey(IN(RObject) key);
  virtual bool containsValue(IN(RObject) val);
  virtual RObject get(IN(RObject) key);
  virtual RObject put(IN(RObject) key, IN(RObject) val);
  virtual void putAll(IN(RMap) map);
  virtual RObject remove(IN(RObject) key);
  virtual int size();
  virtual RSet entrySet();
  virtual RSet keySet()
  {
    return new TreeMapSet(RSortedMap(this), _map, Keys);
  }
  virtual RCollection values()
  {
    // probbly doesn't work
    return new TreeMapCollection(RSortedMap(this));
  }
  virtual RComparator comparator()
  {
    return _map->_comparator;
  }
  virtual RObject firstKey();
  virtual RObject lastKey();
  virtual RSortedMap subMap(IN(RObject) from, IN(RObject) to);
  virtual RSortedMap headMap(IN(RObject) to);
  virtual RSortedMap tailMap(IN(RObject) from);
  
  virtual bool equals(IN(RObject) other)
  {
    return AbstractMap::equals(other);
  }
  virtual int hashCode()
  {
    return AbstractMap::hashCode();
  }
  virtual bool isEmpty()
  {
    return AbstractMap::isEmpty();
  }
  RTreeMap parentTreeMap() { return _map; }
};


} // util
} // acdk

#endif //acdk_util_TreeMap_h

