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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TTreeMap.h,v 1.26 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_TTreeMap_h
#define acdk_util_TTreeMap_h

#include "TMap.h"
#include "TAbstractMap.h"
#include "TAbstractSet.h"

#include "TSortedMap.h"
#include "TComparator.h"
#include "TBasicMapEntry.h"
#include "ConcurrentModificationException.h"
#include "TAbstractCollection.h"

#include "ConcurrentModificationException.h"
#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/IllegalStateException.h>

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/NullPointerException.h>

namespace acdk {
namespace util {




/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class  TRedBlackNode 
: public TBasicMapEntry<K, V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TRedBlackNode<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TBasicMapEntry<RKeyType, RValueType> SuperType;

  enum NodeType 
  {
    Red = -1,
    Black = 1
  };
  NodeType _color;
  RThisType _left;
  RThisType _right;
  RThisType _parent;

  TRedBlackNode(IN(RKeyType) key, IN(RValueType) value)
  : SuperType(key, value)
  , _color(Black)
  , _left(nilNode())
  , _right(nilNode())
  , _parent(nilNode())
  {
  }
  void _clear()
  {
    RThisType nn = nilNode();
    if (_left != nn && _left != Nil && _left.iptr() != this)
      _left->_clear();
    if (_right != nn && _right != Nil && _right.iptr() != this)
      _right->_clear();
    _left = Nil;
    _right = Nil;
    _parent = Nil;
  }
  static RThisType nilNode()
//#if defined(__BORLANDC__) && defined(ACDK_OS_LINUX)
#if __BORLANDC__ > 0x0551
{
  static RefHolder<TNilRedBlackNode<K, V> > _nilNode;
  static bool initialiseNilNode = false;
  if (_nilNode == Nil && initialiseNilNode == false) 
  {
    initialiseNilNode = true;
    _nilNode = new TNilRedBlackNode<K, V>();
    System::registerStaticReference(_nilNode);
    initialiseNilNode = false;
  }
  return &_nilNode;
}  
#else
  ;
#endif
};

/**
  Specialized Version for the NilNode

*/
template <class K, class V>
class TNilRedBlackNode
: extends TRedBlackNode<K, V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TRedBlackNode<RKeyType, RValueType> SuperType;
  typedef TNilRedBlackNode<RKeyType, RValueType> ThisType;
  TNilRedBlackNode()
    : SuperType(Nil, Nil)
  {
    ACDK_SAFE_CONSTRUCTOR();
    ThisType::_left = this;
    //_releaseRef();
    ThisType::_right = this;
    //_releaseRef();
    ThisType::_parent = this;
    //_releaseRef();
  }
  ~TNilRedBlackNode()
  {
    Object::_setObjectRefFlag(true, Object::IsWeakRef);
  }
};

//#if !(defined(__BORLANDC__) && defined(ACDK_OS_LINUX))
#if __BORLANDC__ <= 0x0551
template <class K, class V>
typename TRedBlackNode<K, V>::RThisType
TRedBlackNode<K, V>::nilNode()
{
  static RefHolder<TNilRedBlackNode<K, V> > _nilNode;
  static bool initialiseNilNode = false;
  if (_nilNode == Nil && initialiseNilNode == false) 
  {
    initialiseNilNode = true;
    _nilNode = new TNilRedBlackNode<K, V>();
    System::registerStaticReference(_nilNode);
    initialiseNilNode = false;
  }
  return &_nilNode;
}
#endif

template <class K, class V> class TTreeMapKeyIterator;
template <class K, class V> class TTreeMapValueIterator;
template <class K, class V> class TTreeMapEntryIterator;


template <class K, class V> class TTreeMapKeySet;
template <class K, class V> class TTreeMapEntrySet;

template <class K, class V> class TTreeMapSetKeyIterator;
template <class K, class V> class TTreeMapSetEntryIterator;

template <class K, class V> class TSubTreeMap;
template <class K, class V> class TSortedMapValueCollection;
template <class K, class V> class TTreeMap;

#define ACDK_DECL_TREEMAP(Type1, RType1, Type2, RType2) \
  ACDK_DECL_SORTEDMAP(Type1, RType1, Type2, RType2); \
  ACDK_DECL_SET(Type1, RType1); \
  typedef ::acdk::util::TTreeMap<RType1, RType2> Type1##To##Type2##TreeMap; \
  typedef Type1##To##Type2##TreeMap::RefType R##Type1##To##Type2##TreeMap

/**
  A TreeMap organized as Red-Black tree.
  
  Requirements to the Object types to store:
  - implements compareTo(INP(T) other);
  @see @ref tmap
  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/TreeMap.html
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class TTreeMap 
: extends TAbstractMap<K, V>
, implements TSortedMap<K, V>
, implements acdk::lang::Cloneable
, implements acdk::io::Serializable
{
  DECL_ACDK_DEFAULT_METACLASS(SuperType);
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TTreeMap<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TAbstractMap<RKeyType, RValueType> SuperType;

  typedef ThisType TreeMapType;
  typedef RThisType RTreeMapType;

  typedef TRedBlackNode<RKeyType, RValueType> RedBlackNodeType;
  typedef typename RedBlackNodeType::RefType RRedBlackNodeType;
  
  typedef typename SuperType::MapEntrySetType MapEntrySetType;
  typedef typename MapEntrySetType::RefType RMapEntrySetType;

  typedef TMap<RKeyType, RValueType> MapType;
  typedef typename MapType::RefType RMapType;
  
  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;

  typedef typename MapType::MapEntryType MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;
  
  typedef typename SuperType::KeySetType KeySetType;
  typedef typename KeySetType::RefType RKeySetType;

  typedef TTreeMapKeySet<RKeyType, RValueType> TreeMapKeySetType;
  
  
  typedef TSubTreeMap<RKeyType, RValueType> SubTreeMapType;
  
  typedef typename MapType::ValueCollectionType ValueCollectionType;
  typedef typename ValueCollectionType::RefType RValueCollectionType;
  
  typedef typename SortedMapType::ComparatorType ComparatorType;
  typedef typename ComparatorType::RefType RComparatorType;
  
friends_private:
  RRedBlackNodeType _root;
  int _size;
  RComparatorType _comparator;
  int _modCount;
public:
  /*
    ???? vc overload bug?
  
  TTreeMap(IN(RComparatorType) comp = Nil)
  : _root(nilNode())
  , _size(0)
  , _comparator(comp)
  , _modCount(0)
  {
  }
  TTreeMap(IN(RSortedMapType) sortedMap)
  : _root(nilNode())
  , _size(0)
  , _comparator(sortedMap->comparator())
  , _modCount(0)
  {
  
    RObjectArrayImpl<RMapEntryType> entries = new ObjectArrayImpl<RMapEntryType>(sortedMap->size());
    typename SortedMapType::MapEntrySetType::RIterator it = sortedMap->entrySet()->iterator();
    int i = 0;
    while (it->hasNext() == true) 
    {
      RMapEntryType tmapentry = it->next();
      entries[i++] = tmapentry;
    }
    _size = i;
    putAllLinear(entries);
  }
  */
  TTreeMap(IN(RMapType) map = Nil, IN(RComparatorType) comp = Nil)
  : _root(nilNode())
  , _size(0)
  , _comparator(Nil)
  , _modCount(0)
  {
    if (map != Nil)
      putAll(map);
  }
  ~TTreeMap()
  {
    clear();
  }
  
  virtual void clear()
  {
    RRedBlackNodeType nn = RedBlackNodeType::nilNode();
    if (_root != nn && _root != Nil)
      _root->_clear();

    _root = nn;
    _size = 0;
    _modCount++;
  }
  virtual RObject clone() { return clone(Object::allocator()); }
  virtual RObject clone(sys::Allocator* alc)
  {
    return new (alc) ThisType(this);
  }
  
  virtual RComparatorType comparator()
  {
    return _comparator;
  }

  virtual bool containsKey(IN(RKeyType) key)
  {
    return (treeSearch(_root, _comparator, key) != nilNode());
  }

  virtual bool containsValue(IN(RValueType) val)
  {
    RRedBlackNodeType nod = treeMin(_root); 
    while (nod != nilNode()) 
    {
      RValueType curVal = nod->getValue();
      if (acdk_equals(val, curVal) == true)
        return true;
      nod = treeSuccessor(nod);
    }
    return false;
  }
  
  virtual RMapEntrySetType entrySet();
  /**
    @todo relay on NullPointerException
  */
  virtual RKeyType firstKey()
  {
    try {
      return treeMin(_root)->getKey();
#if __GNUC__ == 3 && __GNUC_MINOR__ == 3
    } catch (RThrowable) {
#else
    } catch (RNullPointerException) {
#endif
      THROW1(NoSuchElementException, "TreeMap is empty");
    }
    return RKeyType();
  }
  virtual RValueType get(IN(RKeyType) key)
  {
    RRedBlackNodeType nod = treeSearch(_root, _comparator, key);
    return (nod != nilNode()) ? nod->getValue() : RValueType(Nil);
  }
  virtual RSortedMapType headMap(IN(RKeyType) key)
  {
    if (keyInClosedMaxRange(_comparator, key, Nil))
      return new SubTreeMapType(this, Nil, key);
    else
      THROW0_FQ(acdk::lang::, IllegalArgumentException);
    return RSortedMapType();
  }
  virtual RKeySetType keySet()
  {
    return new TreeMapKeySetType(this, this);
  }
  virtual RKeyType lastKey()
  {
    try {
     return treeMax(_root)->getKey();
#if __GNUC__ == 3 && __GNUC_MINOR__ == 3// && __GNUC_PATHLEVEL__ == 1
    } catch (RThrowable) {
#else
    } catch (RNullPointerException) {
#endif
      THROW1(NoSuchElementException, "TreeMap is empty");
    }
    return RKeyType();
  }
  virtual RValueType put(IN(RKeyType) key, IN(RValueType) val)
  {
     RRedBlackNodeType entry = rbInsert(this, _comparator, new RedBlackNodeType(key, val));
    if (entry == nilNode())
      _size++;
    _modCount++;
    return ((entry == nilNode()) ? RValueType(Nil) : entry->getValue());
  }
  virtual void putAll(IN(RMapType) map)
  {
    typename MapType::MapEntrySetType::RIteratorType it = map->entrySet()->iterator();
    RMapEntryType entry;
    while (it->hasNext()) 
    {
      entry = it->next();
      put(entry->getKey(), entry->getValue());
    }
  }
  virtual RValueType remove(IN(RKeyType) key)
  {
    RRedBlackNodeType result = treeSearch(_root, _comparator, key);
    if (result == nilNode()) 
      return Nil;
    result = rbDelete(this, result);
    _size--;
    _modCount++;
    return result->getValue();
  }
  virtual int size()
  {
    return _size;
  }
  virtual RSortedMapType subMap(IN(RKeyType) from, IN(RKeyType) to)
  {
     if (compare(_comparator, from, to) < 0)
      return new SubTreeMapType(this, from, to);
    THROW0(IllegalArgumentException);
    return Nil;
  }
  virtual RSortedMapType tailMap(IN(RKeyType) from)
  {
    if (keyInMinRange(_comparator, from, Nil))
      return new SubTreeMapType(this, from, Nil);
    THROW0(IllegalArgumentException);
    return Nil;
  }
  virtual RValueCollectionType values()
  {
     return new TSortedMapValueCollection<RKeyType, RValueType>(this);
  }
  
  virtual bool equals(IN(RObject) other)
  {
    return SuperType::equals(other);
  }
  virtual int hashCode()
  {
    return SuperType::hashCode();
  }
  virtual bool isEmpty()
  {
    return SuperType::isEmpty();
  }
friends_private:
  void putAllLinear(IN(RObjectArrayImpl<RMapEntryType>) entries)
  {
     double dHeight = Math::pow((double) entries->length(), (double) 0.5);
    int height = (int) dHeight;
    bool completed = (dHeight == ((double) height));
    _root = buildTree(entries, height, completed, 0, 0, entries->length());
  }
friends_private:
  static RRedBlackNodeType buildTree(IN(RObjectArrayImpl<RMapEntryType>) entries, int height,  bool completed, 
                                     int currentTier,  int start, int end,
                                     acdk::lang::sys::Allocator* alloc)
  {
    if (start == end) 
      return nilNode();
    int rootIndex = (end + start) / 2;
    RMapEntryType tentry = entries[rootIndex];
    RRedBlackNodeType newTree = new RedBlackNodeType(tentry->getKey(), tentry->getValue());
    newTree->_left = buildTree(entries, height, completed,  (currentTier + 1), start, rootIndex);
    newTree->_right = buildTree(entries, height, completed, (currentTier + 1),  (rootIndex + 1), end);
    if ((completed == false) &&  ((height % 2) == 1) && (currentTier >= (height - 2)))
      newTree->_color = (currentTier == (height - 1)) ? RedBlackNodeType::Black : RedBlackNodeType::Black;
    else
      newTree->_color = ((currentTier % 2) == 1) ? RedBlackNodeType::Black : RedBlackNodeType::Black;
    if (newTree->_left != nilNode())
      newTree->_left->_parent = newTree;
    if (newTree->_right != nilNode())
      newTree->_right->_parent = newTree;
    return newTree;
  }
  static int compare(IN(RComparatorType) comp, IN(RKeyType) o1, IN(RKeyType) o2)
  {
    if (comp == Nil)
      return RComparable(&o1)->compareTo(&o2);
    return comp->compare(o1, o2);
  }

  static bool keyInMinRange(IN(RComparatorType) comp, IN(RKeyType) key, IN(RKeyType) minKey)
  {
    return ((minKey == Nil) ||  (compare(comp, minKey, key) <= 0));
  }

  static bool keyInMaxRange(IN(RComparatorType) comp, IN(RKeyType) key, IN(RKeyType) maxKey)
  {
    return ((maxKey == Nil) ||  (compare(comp, maxKey, key) > 0));
  }
  static bool keyInClosedMaxRange(IN(RComparatorType) comp, IN(RKeyType) key, IN(RKeyType) maxKey)
  {
    return ((maxKey == Nil) ||  (compare(comp, maxKey, key) >= 0));
  }
  static bool keyInRange(IN(RComparatorType) comp, IN(RKeyType) key, IN(RKeyType) minKey, IN(RKeyType) maxKey)
  {
    return (keyInMinRange(comp, key, minKey) && keyInMaxRange(comp, key, maxKey));
  }

  static bool keyInClosedRange(IN(RComparatorType) comp, IN(RKeyType) key, IN(RKeyType) minKey, IN(RKeyType) maxKey)
  {
    return (keyInMinRange(comp, key, minKey) && keyInClosedMaxRange(comp, key, maxKey));
  }
  static RRedBlackNodeType treeSearch(IN(RRedBlackNodeType) rootNode, IN(RComparatorType) comp, IN(RKeyType) key)
  {
    int res;
    RRedBlackNodeType root = rootNode;
    while (root != nilNode()) 
    {
      res = compare(comp, key, root->getKey());
      if (res == 0)
        return root;
      if (res < 0)
        root = root->_left;
      else
        root = root->_right;
    }
    return root;
  }
  static RRedBlackNodeType treeMin(IN(RRedBlackNodeType) rootNode)
  {
    RRedBlackNodeType root = rootNode;
    while (root->_left != nilNode())
      root = root->_left;
    return root;
  }
  static RRedBlackNodeType treeMinConstrained(IN(RRedBlackNodeType) rootNode, IN(RComparatorType) comp, IN(RKeyType) minKey, IN(RKeyType) maxKey)
  {
    int dif;
    RRedBlackNodeType root = rootNode;
    RRedBlackNodeType curNode = nilNode();
    do {
      curNode = root;
      dif = compare(comp, minKey, curNode->getKey());
      if (dif == 0)
        return root;
      root = (dif < 0) ? curNode->_left : curNode->_right;
    } while (root != nilNode());
    if (dif > 0)
      curNode = treeSuccessor(curNode);
    return curNode;
  }
  static RRedBlackNodeType treeMax(IN(RRedBlackNodeType) rootNode)
  {
    RRedBlackNodeType root = rootNode;
    while (root->_right != nilNode())
      root = root->_right;
    return root;
  }
  static RRedBlackNodeType treeMaxConstrained(IN(RRedBlackNodeType) rootNode, IN(RComparatorType) comp, IN(RKeyType) minKey, IN(RKeyType) maxKey)
  {
    int dif;
    RRedBlackNodeType curNode = nilNode();
    RRedBlackNodeType root = rootNode;
    do {
      curNode = root;
      dif = compare(comp, maxKey, curNode->getKey());
      if (dif == 0)
        return root;
      root = (dif < 0) ? curNode->_left : curNode->_right;
    } while (root != nilNode());
    if (dif < 0)
      curNode = treePredecessor(curNode);
    return curNode;
  }

  static RRedBlackNodeType lowerBound(IN(RRedBlackNodeType) root, IN(RComparatorType) comp, IN(RKeyType) minKey, IN(RKeyType) maxKey)
  {
    return ((minKey != Nil) ? treeMinConstrained(root, comp, minKey, maxKey) : treeMin(root));
  }

  static RRedBlackNodeType upperBound(IN(RRedBlackNodeType) root, IN(RComparatorType) comp, IN(RKeyType) minKey, IN(RKeyType) maxKey)
  {
    return ((maxKey != Nil) ? treeMaxConstrained(root, comp, minKey, maxKey) : nilNode());
  }

  static RRedBlackNodeType treeSuccessor(IN(RRedBlackNodeType) nod)
  {
    if (nod->_right != nilNode())
      return treeMin(nod->_right);
    
    RRedBlackNodeType node = nod;
    
    RRedBlackNodeType parent = node->_parent;
    while ((parent != nilNode()) && (node == parent->_right)) 
    {
      node = parent;
      parent = parent->_parent;
    }
    return parent;
  }
  static RRedBlackNodeType treePredecessor(IN(RRedBlackNodeType) node)
  {
    if (node->_left != nilNode())
      return treeMax(node->_left);
    
    RRedBlackNodeType nod = node;
    RRedBlackNodeType parent = nod->_parent;
    while ((parent != nilNode()) && (nod == parent->_left)) 
    {
      nod = parent;
      parent = parent->_parent;
    }
    return parent;
  }
  static RRedBlackNodeType treeInsert(IN(RTreeMapType) tree,  IN(RComparatorType) comp,  IN(RRedBlackNodeType) newNode)
  {
    int res;
    RKeyType newKey = newNode->getKey();
    RRedBlackNodeType parent = nilNode();
    RRedBlackNodeType root = tree->_root;
    RRedBlackNodeType result = nilNode();
    
    while (root != nilNode()) 
    {
      parent = root;
      res = compare(comp, newKey, root->getKey());
      if (res == 0) {
        result = new RedBlackNodeType(root->getKey(), root->getValue());
        root->_key = newNode->_key;
        root->_value = newNode->_value;
        return result;
      } else {
        root = (res < 0) ? root->_left : root->_right;
      }
    }
    
    newNode->_parent = parent;
    if (parent == nilNode())
      tree->_root = newNode;
    else if (compare(comp, newKey, parent->getKey()) < 0)
      parent->_left = newNode;
    else
      parent->_right = newNode;
    return root;
  }
  static void leftRotate(IN(RTreeMapType) tree, IN(RRedBlackNodeType) nod)
  {
    RRedBlackNodeType child = nod->_left;
    nod->_left = child->_right;
    if (child->_right != nilNode())
      child->_right->_parent = nod;
    child->_parent = nod->_parent;
    if (nod->_parent == nilNode())
      tree->_root = child;
    else if (nod == nod->_parent->_right)
      nod->_parent->_right = child;
    else
      nod->_parent->_left = child;
    child->_right = nod;
    nod->_parent = child;
  }
  
  static void rightRotate(IN(RTreeMapType) tree, IN(RRedBlackNodeType) nod)
  {
    RRedBlackNodeType child = nod->_left;
    nod->_left = child->_right;
    if (child->_right != nilNode())
      child->_right->_parent = nod;
    child->_parent = nod->_parent;
    if (nod->_parent == nilNode())
      tree->_root = child;
    else if (nod == nod->_parent->_right)
      nod->_parent->_right = child;
    else
      nod->_parent->_left = child;
    child->_right = nod;
    nod->_parent = child;
  }
  
  static RRedBlackNodeType rbInsert(IN(RTreeMapType) tree,  IN(RComparatorType) comp, IN(RRedBlackNodeType) node)
  {
    RRedBlackNodeType uncle = nilNode();
    RRedBlackNodeType nod = node;
    RRedBlackNodeType result = treeInsert(tree, comp, nod);
    
    if (result == nilNode()) {
      nod->_color = RedBlackNodeType::Red;
      while ((nod != tree->_root) && (nod->_parent->_color == RedBlackNodeType::Red)) {
        if (nod->_parent == nod->_parent->_parent->_right) {
          uncle = nod->_parent->_parent->_right;
          if (uncle->_color == RedBlackNodeType::Red) {
            nod->_parent->_color = RedBlackNodeType::Black;
            uncle->_color = RedBlackNodeType::Black;
            nod->_parent->_parent->_color = RedBlackNodeType::Red;
            nod = nod->_parent->_parent;
          } else {
            if (nod == nod->_parent->_right) {
              nod = nod->_parent;
              leftRotate(tree, nod);
            }
            nod->_parent->_color = RedBlackNodeType::Black;
            nod->_parent->_parent->_color = RedBlackNodeType::Red;
            rightRotate(tree, nod->_parent->_parent);
          }
        } else {
          uncle = nod->_parent->_parent->_left;
          if (uncle->_color == RedBlackNodeType::Red) {
            nod->_parent->_color = RedBlackNodeType::Black;
            uncle->_color = RedBlackNodeType::Black;
            nod->_parent->_parent->_color = RedBlackNodeType::Red;
            nod = nod->_parent->_parent;
          } else {
            if (nod == nod->_parent->_left) {
              nod = nod->_parent;
              rightRotate(tree, nod);
            }
            nod->_parent->_color = RedBlackNodeType::Black;
            nod->_parent->_parent->_color = RedBlackNodeType::Red;
            leftRotate(tree, nod->_parent->_parent);
          }
        }
      }
    }
    tree->_root->_color = RedBlackNodeType::Black;
    return result;
  }
  
  static RRedBlackNodeType rbDelete(IN(RTreeMapType) tree, IN(RRedBlackNodeType) nod)
  {
    RRedBlackNodeType splice = nilNode();
    RRedBlackNodeType child = nilNode();
    RRedBlackNodeType sentinelParent = nilNode();
    RRedBlackNodeType result = nod;
    
    splice = (((nod->_left == nilNode()) || (nod->_right == nilNode())) ? nod : treeSuccessor(nod));
    child = (splice->_left != nilNode()) ? splice->_left : splice->_right;
    
    child->_parent = splice->_parent;
    
    if (splice->_parent == nilNode())
      tree->_root = child;
    else if (splice == splice->_parent->_left)
      splice->_parent->_left = child;
    else
      splice->_parent->_right = child;
    
    if (splice != nod) {
      result = new RedBlackNodeType(nod->getKey(), nod->getValue());
      nod->_key = splice->_key;
      nod->_value = splice->_value;
    }
    
    if (splice->_color == RedBlackNodeType::Black)
      rbDeleteFixup(tree, child);
    
    return result;
  }
  static void rbDeleteFixup(IN(RTreeMapType) tree, IN(RRedBlackNodeType) node)
  {
    RRedBlackNodeType sibling = nilNode();
    RRedBlackNodeType nod = node;
    while ((nod != tree->_root) && (nod->_color == RedBlackNodeType::Black)) {
      if (nod == nod->_parent->_left) {
        sibling = nod->_parent->_right;
        if (sibling->_color == RedBlackNodeType::Red) {
          sibling->_color = RedBlackNodeType::Black;
          nod->_parent->_color = RedBlackNodeType::Red;
          leftRotate(tree, nod->_parent);
          sibling = nod->_parent->_right;
        }
        if ((sibling->_left->_color == RedBlackNodeType::Black) && 
          (sibling->_right->_color == RedBlackNodeType::Black)) {
          sibling->_color = RedBlackNodeType::Red;
          nod = nod->_parent;
        } else {
          if (sibling->_right->_color == RedBlackNodeType::Black) {
            sibling->_left->_color = RedBlackNodeType::Black;
            sibling->_color = RedBlackNodeType::Red;
            rightRotate(tree, sibling);
            sibling = nod->_parent->_right;
          }
          sibling->_color = nod->_parent->_color;
          nod->_parent->_color = RedBlackNodeType::Black;
          sibling->_right->_color = RedBlackNodeType::Black;
          leftRotate(tree, nod->_parent);
          nod = tree->_root;
        }
      } else {
        sibling = nod->_parent->_left;
        if (sibling->_color == RedBlackNodeType::Red) {
          sibling->_color = RedBlackNodeType::Black;
          nod->_parent->_color = RedBlackNodeType::Red;
          rightRotate(tree, nod->_parent);
          sibling = nod->_parent->_left;
        }
        if ((sibling->_right->_color == RedBlackNodeType::Black) && 
          (sibling->_left->_color == RedBlackNodeType::Black)) {
          sibling->_color = RedBlackNodeType::Red;
          nod = nod->_parent;
        } else {
          if (sibling->_left->_color == RedBlackNodeType::Black) {
            sibling->_right->_color = RedBlackNodeType::Black;
            sibling->_color = RedBlackNodeType::Red;
            leftRotate(tree, sibling);
            sibling = nod->_parent->_left;
          }
          sibling->_color = nod->_parent->_color;
          nod->_parent->_color = RedBlackNodeType::Black;
          sibling->_left->_color = RedBlackNodeType::Black;
          rightRotate(tree, nod->_parent);
          nod = tree->_root;
        }
      }
    }
    nod->_color = RedBlackNodeType::Black;
  }

  static RRedBlackNodeType nilNode()
  {
    return RedBlackNodeType::nilNode();
  }
  /*
  friend class TTreeMapIterator;
  
  friend class SubTTreeMap;
  friend class TTreeMapSetIterator;
  friend class TreeSet;
  */
};

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TTreeMapIteratorBase
: public acdk::lang::Object
{
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TIterator<RKeyType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

  typedef TTreeMapKeyIterator<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  
  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;
  
  typedef typename TreeMapType::RedBlackNodeType RedBlackNodeType;
  typedef typename RedBlackNodeType::RefType RRedBlackNodeType;
  
private:

  RTreeMapType _map;
  RRedBlackNodeType _first;
  RRedBlackNodeType _last;
  RRedBlackNodeType _prev;
  int _knownMods;
public:
  TTreeMapIteratorBase(IN(RTreeMapType) map)
  :  _map(map)
  , _first()
  , _last()
  , _prev(TreeMapType::nilNode())
  , _knownMods(map->_modCount)
  {
    if (_map->isEmpty()) {
      _first = RedBlackNodeType::nilNode();
    } else {
      _first = TreeMapType::treeSearch(_map->_root, _map->_comparator, _map->firstKey());
      _last = TreeMapType::treeSearch(_map->_root, _map->_comparator, _map->lastKey());
    }
  }

  virtual bool _hasNext()
  {
    _checkMod();
    return (_first != TreeMapType::nilNode());
  }
  virtual RRedBlackNodeType _element()
  {
    THROW0(UnsupportedOperationException);
    return Nil;
  }
  virtual RRedBlackNodeType _next()
  {
    _checkMod();
    RRedBlackNodeType result = _first;
    if (result == RedBlackNodeType::nilNode())
      THROW0(NoSuchElementException);
    if (result == _last)
      _first = RedBlackNodeType::nilNode();
    else
      _first = TreeMapType::treeSuccessor(_first);
    _prev = result;
    return result;
  }
  virtual void _remove()
  {
    _checkMod();
    if (_prev == RedBlackNodeType::nilNode()) 
      THROW0(IllegalStateException);
    RKeyType key = _prev->getKey();
    if (_map->containsKey(key)) {
      _map->remove(key);
      _knownMods++;
    }
    _prev = RedBlackNodeType::nilNode();
  }
  void _checkMod()
  {
    if (_knownMods < _map->_modCount)
      THROW0(ConcurrentModificationException);
  }
};



/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TTreeMapKeyIterator
: extends TTreeMapIteratorBase<K, V>
, implements TIterator<K>
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  typedef TTreeMapKeyIterator<K, V> ThisType;
  typedef TTreeMapIteratorBase<K, V> SuperType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;
 

  typedef TIterator<RKeyType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

public:
  TTreeMapKeyIterator(IN(RTreeMapType) map)
  :  SuperType(map)
  {
  }
  virtual bool hasNext() { return ThisType::_hasNext(); }
  virtual RKeyType element() { return ThisType::_element()->getKey(); }
  virtual RKeyType next() { return ThisType::_next()->getKey(); }
  virtual void remove() { ThisType::_remove(); }
};


/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TTreeMapValueIterator
: extends TTreeMapIteratorBase<K, V>
, implements TIterator<V>
{
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TTreeMapValueIterator<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TTreeMapIteratorBase<K, V> SuperType;
  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;

  typedef TIterator<RValueType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

public:
  TTreeMapValueIterator(IN(RTreeMapType) map)
  : SuperType(map)
  {
  }
  virtual bool hasNext() { return ThisType::_hasNext(); }
  virtual RValueType element() { return ThisType::_element()->getValue(); }
  virtual RValueType next() { return ThisType::_next()->getValue(); }
  virtual void remove() { ThisType::_remove(); }
};



/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TTreeMapEntryIterator
: extends TTreeMapIteratorBase<K, V>
, implements TIterator<InterfaceHolder< TMapEntry<K, V> > >
{
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TTreeMapIteratorBase<K, V> SuperType;

  typedef TTreeMapEntryIterator<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;

  typedef TRedBlackNode<K, V> MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;

  TTreeMapEntryIterator(IN(RTreeMapType) map)
  : SuperType(map)
  {
  }
  virtual bool hasNext() { return ThisType::_hasNext(); }
  virtual RMapEntryType element() { return ThisType::_element(); }
  virtual RMapEntryType next() { return ThisType::_next(); }
  virtual void remove() { ThisType::_remove(); }
};





/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TSortedMapValueCollection 
: public TAbstractCollection<V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TSortedMapValueCollection<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TCollection<RValueType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;

  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;
  
  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;

  typedef TSubTreeMap<RKeyType, RValueType> SubTreeMapType;
  typedef typename SubTreeMapType::RefType RSubTreeMapType;
  
  typedef typename CollectionType::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

private:
  RSortedMapType _map;
public:
  TSortedMapValueCollection(IN(RSortedMapType) map)
  : _map(map)
  {
  }
  virtual bool add(IN(RValueType) object)
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual bool addAll(IN(RCollectionType) coll) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual void clear()
  {
    _map->clear();
  }
  virtual bool contains(IN(RValueType) object)
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
  virtual RIteratorType iterator();
  virtual bool equals(IN(RObject) obj)
  {
    return _map->equals(obj);
  }
  virtual int hashCode()
  {
    return _map->hashCode();
  }
};

  

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TTreeMapKeySet 
: public TAbstractSet<K>
{
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TTreeMapKeySet<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TAbstractSet<K> SuperType;

  typedef TCollection<K> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  typedef TSet<RKeyType> SetType;
  typedef typename SetType::RefType RSetType;

  typedef typename SuperType::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;

  
private:
  RSortedMapType _map;
  RTreeMapType _treeMap;
  //MapEntryTyp _type;
public:
  TTreeMapKeySet(IN(RSortedMapType) sortedmap, IN(RTreeMapType) treeMap)
  : _map(sortedmap)
  , _treeMap(treeMap)
  {
  }
  virtual bool add(IN(RKeyType) object) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual bool addAll(IN(RCollectionType) coll) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual void clear()
  {
    _map->clear();
  }
  virtual bool contains(IN(RKeyType) object)
  {
    return _map->containsKey(object);
  }
  virtual bool isEmpty()
  {
    return _map->isEmpty();
  }
  virtual bool remove(IN(RKeyType) object)
  {
    return (_map->remove(object) != Nil);
  }
  virtual int size()
  {
    return _map->size();
  }
  virtual RIteratorType iterator()
  {
    return new TTreeMapSetKeyIterator<RKeyType, RValueType>(_map, _treeMap); 
  }
};

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TTreeMapValueSet 
: public TAbstractSet<V>
{
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TTreeMapKeySet<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TCollection<V> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  typedef TSet<V> SetType;
  typedef typename SetType::RefType RSetType;
  
  typedef typename SetType::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;

  
private:
  RSortedMapType _map;
  RTreeMapType _treeMap;
  //MapEntryTyp _type;
public:
  TTreeMapValueSet(IN(RSortedMapType) sortedmap, IN(RTreeMapType) treeMap)
  : _map(sortedmap)
  , _treeMap(treeMap)
  {
  }
  virtual bool add(IN(RValueType) object) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual bool addAll(IN(RCollectionType) coll) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual void clear()
  {
    _map->clear();
  }
  virtual bool contains(IN(RValueType) object)
  {
    return false;
  }
  virtual bool isEmpty()
  {
    return _map->isEmpty();
  }
  virtual bool remove(IN(RValueType) object)
  {
    return false;
  }
  virtual int size()
  {
    return _map->size();
  }
  virtual RIteratorType iterator();
};

/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TTreeMapEntrySet 
: public TAbstractSet<InterfaceHolder< TMapEntry<K, V> > >
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TMapEntry<K, V> ElementType;
  typedef typename ElementType::RefType RElementType;
  
  typedef TTreeMapKeySet<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TAbstractSet<InterfaceHolder< TMapEntry<K, V> > > SuperType;
  
  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;
  
  typedef TCollection<RElementType> CollectionType;
  typedef typename CollectionType::RefType RCollectionType;
  
  typedef typename SuperType::IteratorType IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

private:
  RSortedMapType _map;
  RTreeMapType _treeMap;
public:
  TTreeMapEntrySet(IN(RSortedMapType) sortedmap, IN(RTreeMapType) treeMap)
  : _map(sortedmap)
  , _treeMap(treeMap)
  {
  }
  virtual bool add(IN(RElementType) object) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual bool addAll(IN(RCollectionType) coll) 
  {
    THROW0(UnsupportedOperationException);
    return false;
  }
  virtual void clear()
  {
    _map->clear();
  }
  virtual bool contains(IN(RElementType) entry)
  {
    RKeyType key = entry->getKey();
    if (_map->containsKey(key)) 
    {
      RValueType inputVal = entry->getValue();
      RValueType mapValue = _map->get(key);
      return ((inputVal == Nil)  ? (mapValue == Nil) : (inputVal->equals(mapValue)));
    }
    return false;
  }
  virtual bool isEmpty()
  {
    return _map->isEmpty();
  }
  virtual bool remove(IN(RElementType) object)
  {
    return _map->remove(object->getKey()) != Nil;
  }
  virtual int size()
  {
    return _map->size();
  }
  virtual RIteratorType iterator()
  {
    return new TTreeMapSetEntryIterator<RKeyType, RValueType>(_map, _treeMap); 
  }
};



/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class TTreeMapSetIteratorBase
: extends acdk::lang::Object
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TTreeMapSetIteratorBase<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;
  
  typedef typename TreeMapType::RedBlackNodeType RedBlackNodeType;
  typedef typename RedBlackNodeType::RefType RRedBlackNodeType;

  //typedef TMapEntry<RKeyType, RValueType> ElementType;
  typedef RedBlackNodeType ElementType;
  typedef typename ElementType::RefType RElementType;

private:
  RSortedMapType _map;
  RTreeMapType _treeMap;
  RRedBlackNodeType _first;
  RRedBlackNodeType _last;
  RRedBlackNodeType _prev;
  int _knownMods;
public:
  TTreeMapSetIteratorBase(IN(RSortedMapType) map, IN(RTreeMapType) treemap)
  : _map(map)
  , _treeMap(treemap)
  , _prev(TreeMapType::nilNode())
  , _knownMods(treemap->_modCount)
  {
    if (_map->isEmpty()) {
      _last = _first = RedBlackNodeType::nilNode();
    } else {
      _first = TreeMapType::treeSearch(_treeMap->_root, _treeMap->_comparator, _map->firstKey());
      _last = TreeMapType::treeSearch(_treeMap->_root, _treeMap->_comparator, _map->lastKey());
    }
  }

  bool _hasNext()
  {
    _checkMod();
    return (_first != TreeMapType::nilNode());
  }
  virtual RElementType _element()
  {
    THROW0(UnsupportedOperationException);
    return Nil;
  }
  virtual RElementType _next()
  {
    _checkMod();
    RRedBlackNodeType result = _first;
    
    if (result == RedBlackNodeType::nilNode())
      THROW0(NoSuchElementException);

    if (result == _last)
      _first = RedBlackNodeType::nilNode();
    else
      _first = TreeMapType::treeSuccessor(_first);
    _prev = result;
    return result;
  }
  
  virtual void _remove()
  {
    _checkMod();
    if (_prev == RedBlackNodeType::nilNode()) 
      THROW0(IllegalStateException);
    RKeyType key = _prev->getKey();
    if (_map->containsKey(key)) 
    {
      _map->remove(key);
      _knownMods++;
    }
    _prev = RedBlackNodeType::nilNode();
  }
  void _checkMod()
  {
    if (_knownMods < _treeMap->_modCount)
      THROW0(ConcurrentModificationException);
    }

};



/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class TTreeMapSetKeyIterator
: extends TTreeMapSetIteratorBase<K, V>
, implements TIterator<K>
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TTreeMapSetKeyIterator<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TTreeMapSetIteratorBase<K, V> SuperType;

  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;
  
  typedef typename TreeMapType::RedBlackNodeType RedBlackNodeType;
  typedef typename RedBlackNodeType::RefType RRedBlackNodeType;

  typedef RKeyType RElementType;
  
  typedef TIterator<RElementType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

public:
  TTreeMapSetKeyIterator(IN(RSortedMapType) map, IN(RTreeMapType) treemap)
  : SuperType(map, treemap)
  {
  }
  virtual bool hasNext()  { return ThisType::_hasNext(); }
  virtual RElementType element() { return ThisType::_element()->getKey(); }
  virtual RElementType next() { return ThisType::_next()->getKey(); }
  virtual void remove() { ThisType::_remove(); }
};



/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class TTreeMapSetValueIterator
: extends TTreeMapSetIteratorBase<K, V>
, implements TIterator<V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TTreeMapSetValueIterator<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TTreeMapSetIteratorBase<K, V> SuperType;
  
  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;
  
  typedef typename TreeMapType::RedBlackNodeType RedBlackNodeType;
  typedef typename RedBlackNodeType::RefType RRedBlackNodeType;

  typedef RValueType RElementType;
  
  typedef TIterator<RElementType> IteratorType;
  typedef typename IteratorType::RefType RIteratorType;

public:
  TTreeMapSetValueIterator(IN(RSortedMapType) map, IN(RTreeMapType) treemap)
  : SuperType(map, treemap)
  {
  }
 
  virtual bool hasNext() { return ThisType::_hasNext(); }
  virtual RElementType element() { return ThisType::_element()->getValue(); }
  virtual RElementType next() { return ThisType::_next()->getValue(); }
  virtual void remove() { ThisType::_remove(); }
};


/**
  @internal
  Used internally by THashMap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
*/
template <class K, class V>
class TTreeMapSetEntryIterator
: extends TTreeMapSetIteratorBase<K, V>
, implements TIterator<InterfaceHolder< TMapEntry<K, V> > >
{
public:
  typedef K RKeyType;
  typedef V RValueType;
  
  typedef TTreeMapSetEntryIterator<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;
  
  typedef TTreeMapSetIteratorBase<K, V> SuperType;
  
  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;

  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;
  
  typedef typename TreeMapType::RedBlackNodeType RedBlackNodeType;
  typedef typename RedBlackNodeType::RefType RRedBlackNodeType;
  
  typedef TMapEntry<RKeyType, RValueType> ElementType;
  typedef typename ElementType::RefType RElementType;


  TTreeMapSetEntryIterator(IN(RSortedMapType) map, IN(RTreeMapType) treemap)
  : SuperType(map, treemap)
  {
  }
  virtual bool hasNext() { return ThisType::_hasNext(); }
  virtual RElementType element() { return &ThisType::_element(); }
  virtual RElementType next() { return &ThisType::_next(); }
  virtual void remove() { ThisType::_remove(); }
};



/**
  API: Java<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
template <class K, class V>
class TSubTreeMap
: extends TAbstractMap<K, V>
, implements TSortedMap<K, V>
{
public:
  typedef K RKeyType;
  typedef V RValueType;

  typedef TSubTreeMap<RKeyType, RValueType> ThisType;
  typedef RefHolder<ThisType> RThisType;
  typedef RThisType RefType;

  typedef TAbstractMap<RKeyType, RValueType> AbstractMapType;
  typedef typename AbstractMapType::RefType RAbstractMapType;

  typedef ThisType SubTreeMapType;
  typedef typename SubTreeMapType::RefType RSubTreeMapType;

  typedef TMap<RKeyType, RValueType> MapType;
  typedef typename MapType::RefType RMapType;
  
  typedef typename MapType::MapEntryType MapEntryType;
  typedef typename MapEntryType::RefType RMapEntryType;
  
  typedef TSortedMap<RKeyType, RValueType> SortedMapType;
  typedef typename SortedMapType::RefType RSortedMapType;
  
  typedef typename MapType::MapEntrySetType MapEntrySetType;
  typedef typename MapEntrySetType::RefType RMapEntrySetType;

  typedef typename MapType::KeySetType KeySetType;
  typedef typename KeySetType::RefType RKeySetType;
  
  typedef TTreeMap<RKeyType, RValueType> TreeMapType;
  typedef typename TreeMapType::RefType RTreeMapType;

  typedef typename TreeMapType::ValueCollectionType ValueCollectionType;
  typedef typename ValueCollectionType::RefType RValueCollectionType;
  
  typedef TTreeMapEntrySet<RKeyType, RValueType> TreeMapEntrySetType;
  typedef typename TreeMapEntrySetType::RefType RTreeMapEntrySetType;

  typedef typename TreeMapType::RedBlackNodeType RedBlackNodeType;
  typedef typename RedBlackNodeType::RefType RRedBlackNodeType;
  
  typedef TComparator<RKeyType> ComparatorType;
  typedef typename ComparatorType::RefType RComparatorType;

private:
  RTreeMapType  _map;
  RKeyType _minKey;
  RKeyType _maxKey;
public:
  TSubTreeMap(IN(RTreeMapType) map, IN(RKeyType) minKey, IN(RKeyType) maxKey)
  : _map(map)
  , _minKey(minKey)
  , _maxKey(maxKey)
  {
  }
  virtual void clear()
  {
    RRedBlackNodeType minNode = TreeMapType::lowerBound(_map->_root, _map->_comparator, _minKey, _maxKey);
    RRedBlackNodeType maxNode = TreeMapType::upperBound(_map->_root, _map->_comparator, _minKey, _maxKey);
    RKeyType maxKey = maxNode->getKey();
    while ((minNode != RedBlackNodeType::nilNode()) &&  ((maxNode == RedBlackNodeType::nilNode()) ||  
      (TreeMapType::compare(_map->_comparator,  minNode->getKey(), maxKey) < 0))) 
    {
      _map->remove(minNode->getKey());
      minNode = TreeMapType::treeSuccessor(minNode);
    }
  }
  virtual bool containsKey(IN(RKeyType) key)
  {
    return  (TreeMapType::keyInRange(_map->_comparator, key, _minKey, _maxKey) && _map->containsKey(key)); 
  }
  virtual bool containsValue(IN(RValueType) val)
  {
    RValueType curVal;
    RRedBlackNodeType minNode = TreeMapType::lowerBound(_map->_root,  _map->_comparator, _minKey, _maxKey);
    RRedBlackNodeType maxNode = TreeMapType::upperBound(_map->_root,  _map->_comparator, _minKey, _maxKey);
    RKeyType maxKey = maxNode->getKey();
    while ((minNode != RedBlackNodeType::nilNode()) && 
          ((maxNode == RedBlackNodeType::nilNode()) || 
          (TreeMapType::compare(_map->_comparator,  minNode->getKey(), maxKey) < 0))) 
    {
      curVal = minNode->getValue();
      if (((val == Nil) && (curVal == Nil)) || val->equals(curVal))
        return true;
      minNode = TreeMapType::treeSuccessor(minNode);        
    }
    return false;
  }
  virtual RValueType get(IN(RKeyType) key)
  {
    if (TreeMapType::keyInRange(_map->_comparator, key, _minKey, _maxKey))
      return _map->get(key);
    return Nil;
  }
  virtual RValueType put(IN(RKeyType) key, IN(RValueType) val)
  {
    if (TreeMapType::keyInRange(_map->_comparator, key, _minKey, _maxKey))
      return _map->put(key, val);

    THROW0(IllegalArgumentException);
    return Nil;
  }
  virtual void putAll(IN(RMapType) map)
  {
    RMapEntryType entry;
    typename MapType::MapEntrySetType::RIteratorType it = map->entrySet()->iterator();
    while (it->hasNext()) 
    {
      entry = it->next();
      put(entry->getKey(), entry->getValue());
    }
  }

  virtual RValueType remove(IN(RKeyType) key)
  {
    if (TreeMapType::keyInRange(_map->_comparator, key, _minKey, _maxKey))
      return _map->remove(key);
    THROW0(IllegalArgumentException);
    return Nil;
  }
  virtual int size()
  {
    RRedBlackNodeType minNode = TreeMapType::lowerBound(_map->_root, _map->_comparator, _minKey, _maxKey);
    RRedBlackNodeType maxNode = TreeMapType::upperBound(_map->_root, _map->_comparator, _minKey, _maxKey);
    RKeyType maxKey = maxNode->getKey();
    int count = 0;
    while ((minNode != RedBlackNodeType::nilNode()) && 
      ((maxNode == RedBlackNodeType::nilNode()) || 
      (TreeMapType::compare(_map->_comparator,  minNode->getKey(), maxKey) < 0))) 
    {
      count++;
      minNode = TreeMapType::treeSuccessor(minNode);
    }
    return count;
  }
  virtual RMapEntrySetType entrySet()
  {
    RSubTreeMapType stm(this);
    RSortedMapType sm = &stm;
    return new TreeMapEntrySetType(sm, _map); 
  }
  virtual RKeySetType keySet()
  {
    return new TTreeMapKeySet<RKeyType, RValueType>(this, _map);
  }
  virtual RValueCollectionType values()
  {
    // probbly doesn't work
    return new TSortedMapValueCollection<RKeyType, RValueType>(this);
  }
  virtual RComparatorType comparator()
  {
    return _map->_comparator;
  }
  virtual RKeyType firstKey()
  {
    RRedBlackNodeType first = TreeMapType::lowerBound(_map->_root, _map->_comparator, _minKey, _maxKey);
    return (first != RedBlackNodeType::nilNode()) ? first->getKey() : RKeyType(Nil);
  }
  virtual RKeyType lastKey()
  {
    RRedBlackNodeType last = RedBlackNodeType::nilNode();
    if (_maxKey == Nil) {
      last = TreeMapType::treeMax(_map->_root);
      return (last != RedBlackNodeType::nilNode()) ? last->getKey() : RKeyType(Nil);
    } else {
      last = TreeMapType::treeMaxConstrained(_map->_root, _map->_comparator, _minKey, _maxKey);
      return (last != RedBlackNodeType::nilNode()) ? TreeMapType::treePredecessor(last)->getKey() : RKeyType(Nil);
    }
  }
  virtual RSortedMapType subMap(IN(RKeyType) from, IN(RKeyType) to)
  {
     if ((TreeMapType::compare(_map->_comparator, from, to) < 0) &&
          TreeMapType::keyInMinRange(_map->_comparator,  from, _minKey) &&
          TreeMapType::keyInClosedMaxRange(_map->_comparator, from, _maxKey) &&
          TreeMapType::keyInMinRange(_map->_comparator,  to, _minKey) &&
          TreeMapType::keyInClosedMaxRange(_map->_comparator, to, _maxKey))
      return new SubTreeMapType(_map, from, to);

    THROW0(IllegalArgumentException);
    return Nil;
  }
  virtual RSortedMapType headMap(IN(RKeyType) to)
  {
    if (TreeMapType::keyInMinRange(_map->_comparator, to, _minKey) &&
        TreeMapType::keyInClosedMaxRange(_map->_comparator, to, _maxKey))
      return new SubTreeMapType(_map, _minKey, to);
    THROW0(IllegalArgumentException);
    return Nil;
  }
  virtual RSortedMapType tailMap(IN(RKeyType) from)
  {
    if (TreeMapType::keyInMinRange(_map->_comparator, from, _minKey) &&
        TreeMapType::keyInClosedMaxRange(_map->_comparator, from, _maxKey))
      return new SubTreeMapType(_map, from, _maxKey);

    THROW0(IllegalArgumentException);
    return Nil;
  }
  
  virtual bool equals(IN(RObject) other)
  {
    return AbstractMapType::equals(other);
  }
  virtual int hashCode()
  {
    return AbstractMapType::hashCode();
  }
  virtual bool isEmpty()
  {
    return AbstractMapType::isEmpty();
  }
  RTreeMapType parentTreeMap() { return _map; }
};

template <class K, class V>
typename TTreeMap<K, V>::RMapEntrySetType 
TTreeMap<K, V>::entrySet()
{
  return new TTreeMapEntrySet<RKeyType, RValueType>(this, this);
}

template <class K, class V>
typename TSortedMapValueCollection<K, V>::RIteratorType 
TSortedMapValueCollection<K, V>::iterator()
{
  if (instanceof(_map, TreeMapType) == true)
    return new TTreeMapValueIterator<RKeyType, RValueType>(RTreeMapType(_map));
  else if (instanceof(_map, SubTreeMapType) == true) 
  {
    RSubTreeMapType sbm = (RSubTreeMapType)_map;
    return new TTreeMapSetValueIterator<RKeyType, RValueType>(_map, sbm->parentTreeMap());
  } else
    return Nil;
}

template <class K, class V>
typename TTreeMapValueSet<K, V>::RIteratorType 
TTreeMapValueSet<K, V>::iterator()
{
  return new TTreeMapSetValueIterator<K, V>(_map, _treeMap); 
}

} // util
} // acdk

#endif //acdk_util_TTreeMap_h

