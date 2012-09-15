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
// Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TreeMap.cpp,v 1.21 2005/03/08 12:45:46 kommer Exp $

#include <acdk.h>

#include "TreeMap.h"

#include <acdk/lang/System.h>
#include <acdk/lang/IllegalArgumentException.h>
#include <acdk/lang/IllegalStateException.h>
#include "NoSuchElementException.h"
#include <acdk/lang/UnsupportedOperationException.h>

#include <acdk/lang/Cloneable.h>
#include <acdk/io/Serializable.h>

namespace acdk {
namespace util {

NilRedBlackNode::NilRedBlackNode()
: RedBlackNode(Nil, Nil)
{
  ACDK_SAFE_CONSTRUCTOR();
  _left = this;
  releaseRef();
  _right = this;
  releaseRef();
  _parent = this;
  releaseRef();
}

NilRedBlackNode::~NilRedBlackNode()
{
  _setObjectRefFlag(true, IsWeakRef);
}

RRedBlackNode RedBlackNode::_nilNode;// = new RedBlackNode(null, null);

//static 
RRedBlackNode 
RedBlackNode::nilNode()
{
  static bool initialiseNilNode = false;
  if (_nilNode == Nil && initialiseNilNode == false) {
    initialiseNilNode = true;
    _nilNode = new NilRedBlackNode();
    System::registerStaticReference(_nilNode);
    initialiseNilNode = false;
  }
  return _nilNode;
}

TreeMap::TreeMap(IN(RComparator) comp)
: _root(nilNode()),
  _size(0),
  _comparator(comp),
  _modCount(0)
{
}
  
TreeMap::TreeMap(IN(RMap) map)
: _root(nilNode()),
  _size(0),
  _comparator(Nil),
  _modCount(0)
{
  if (map != Nil)
    putAll(map);
}


TreeMap::TreeMap(IN(RSortedMap) sortedMap)
: _root(nilNode()),
  _size(0),
  _comparator(sortedMap->comparator()),
  _modCount(0)
{
  RMapEntryArray entries = new (allocator()) MapEntryArray(sortedMap->size());
  RIterator it = sortedMap->entrySet()->iterator();
  int i = 0;
  while (it->hasNext() == true) {
    RMapEntry tmapentry = RMapEntry(it->next());
    entries[i++] = tmapentry;
  }
  _size = i;
  putAllLinear(entries);
}

//virtual 
void 
TreeMap::clear()
{
  RRedBlackNode nn = nilNode();
  
  if (_root != nn && _root != Nil)
    _root->_clear();

  _root = nn;
  _size = 0;
  _modCount++;
}

//virtual 
RObject 
TreeMap::clone(sys::Allocator* alc)
{
  return new (alc) TreeMap(RSortedMap(const_cast<TreeMap*>(this)));
}

//virtual 
bool 
TreeMap::containsValue(IN(RObject) val)
{
  RRedBlackNode nod = treeMin(_root); 
  RObject curVal;
  while (nod != nilNode()) {
    curVal = nod->getValue();
    if (((val == Nil) && (curVal == Nil)) || val->equals(curVal))
      return true;
    nod = treeSuccessor(nod);
  }
  return false;
}

//virtual 
RSet 
TreeMap::entrySet()
{
  return new TreeMapSet(this, this, Entries);
}


//virtual 
RObject 
TreeMap::firstKey()
{
  try {
    return treeMin(_root)->getKey();
  } catch (RNullPointerException ) {
    THROW1(NoSuchElementException, "TreeMap is empty");
  }
  return Nil;
}

//virtual 
RObject 
TreeMap::get(IN(RObject) key)
{
  RRedBlackNode nod = treeSearch(_root, _comparator, key);
  return (nod != nilNode()) ? nod->getValue() : RObject(Nil);
}


//virtual 
RSortedMap 
TreeMap::headMap(IN(RObject) key)
{
  if (keyInClosedMaxRange(_comparator, key, Nil))
    return new SubTreeMap(this, Nil, key);
  THROW0(IllegalArgumentException);
  return Nil;
}


//virtual 
RSet 
TreeMap::keySet()
{
  return new TreeMapSet(this, this, Keys);
}


//virtual 
RObject 
TreeMap::lastKey()
{
  try {
    return treeMax(_root)->getKey();
  } catch (RNullPointerException) {
    THROW1(NoSuchElementException, "TreeMap is empty");
  }
  return Nil;
}

//virtual 
RObject 
TreeMap::put(IN(RObject) key, IN(RObject) val)
{
  RRedBlackNode entry = rbInsert(this, _comparator, new (allocator()) RedBlackNode(key, val));
  if (entry == nilNode())
    _size++;
  _modCount++;
  return ((entry == nilNode()) ? RObject(Nil) : entry->getValue());
}

//virtual 
void 
TreeMap::putAll(IN(RMap) map)
{
  RIterator it = map->entrySet()->iterator();
  RMapEntry entry;
  while (it->hasNext()) {
    entry = RMapEntry(it->next());
    put(entry->getKey(), entry->getValue());
  }
}

//virtual 
RObject 
TreeMap::remove(IN(RObject) key)
{
  RRedBlackNode result = treeSearch(_root, _comparator, key);
  if (result == nilNode()) 
    return Nil;
  result = rbDelete(this, result, allocator());
  _size--;
  _modCount++;
  return result->getValue();
}

//virtual 
RSortedMap 
TreeMap::subMap(IN(RObject) from, IN(RObject) to)
{
  if (compare(_comparator, from, to) < 0)
      return new SubTreeMap(this, from, to);
  THROW0(IllegalArgumentException);
  return Nil;
}

//virtual 
RSortedMap 
TreeMap::tailMap(IN(RObject) from)
{
  if (keyInMinRange(_comparator, from, Nil))
      return new SubTreeMap(this, from, Nil);
  THROW0(IllegalArgumentException);
  return Nil;
}

//virtual 
RCollection 
TreeMap::values()
{
  return new TreeMapCollection(this);
}

void 
TreeMap::putAllLinear(IN(RMapEntryArray) entries)
{
  double dHeight = Math::pow((double) entries->length(), (double) 0.5);
  int height = (int) dHeight;
  bool completed = (dHeight == ((double) height));
  _root = buildTree(entries, height, completed, 0, 0, entries->length(), allocator());
}

//static 
RRedBlackNode 
TreeMap::buildTree(IN(RMapEntryArray) entries, int height,  bool completed, int currentTier,  int start, int end,
                   acdk::lang::sys::Allocator* alloc)
{
  
  
  if (start == end) 
    return nilNode();
  int rootIndex = (end + start) / 2;
  RMapEntry tentry = entries[rootIndex];
  RRedBlackNode newTree = new (alloc) RedBlackNode(tentry->getKey(), tentry->getValue());
  newTree->_left = buildTree(entries, height, completed,  (currentTier + 1), start, rootIndex, alloc);
  newTree->_right = buildTree(entries, height, completed, (currentTier + 1),  (rootIndex + 1), end, alloc);
  if ((completed == false) &&  ((height % 2) == 1) && (currentTier >= (height - 2)))
    newTree->_color = (currentTier == (height - 1)) ? BlackNodeType : BlackNodeType; // ### ????
  else
    newTree->_color = ((currentTier % 2) == 1) ? BlackNodeType : BlackNodeType; // ### ?????
  if (newTree->_left != nilNode())
    newTree->_left->_parent = newTree;
  if (newTree->_right != nilNode())
    newTree->_right->_parent = newTree;
  return newTree;
}

//static 
RRedBlackNode 
TreeMap::treeSearch(IN(RRedBlackNode) rootNode, IN(RComparator) comp, IN(RObject) key)
{
  int res;
  RRedBlackNode root = rootNode;
  while (root != nilNode()) {
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

//static 
RRedBlackNode 
TreeMap::treeMin(IN(RRedBlackNode) rootNode)
{
  
  RRedBlackNode root = rootNode;
  while (root->_left != nilNode())
    root = root->_left;
  return root;
}

//static 
RRedBlackNode 
TreeMap::treeMinConstrained(IN(RRedBlackNode) rootNode, IN(RComparator) comp, IN(RObject) minKey, IN(RObject) maxKey)
{
  int dif;
  RRedBlackNode root = rootNode;
  RRedBlackNode curNode = nilNode();
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

//static 
RRedBlackNode 
TreeMap::treeMax(IN(RRedBlackNode) rootNode)
{
  
  RRedBlackNode root = rootNode;
  while (root->_right != nilNode())
    root = root->_right;
  return root;
}

//static 
RRedBlackNode 
TreeMap::treeMaxConstrained(IN(RRedBlackNode) rootNode, IN(RComparator) comp, IN(RObject) minKey, IN(RObject) maxKey)
{
  int dif;
  RRedBlackNode curNode = nilNode();
  RRedBlackNode root = rootNode;
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


//static 
RRedBlackNode 
TreeMap::treeSuccessor(IN(RRedBlackNode) nod)
{
  
  if (nod->_right != nilNode())
    return treeMin(nod->_right);

  RRedBlackNode node = nod;

  RRedBlackNode parent = node->_parent;
  while ((parent != nilNode()) && (node == parent->_right)) {
    node = parent;
    parent = parent->_parent;
  }
  return parent;
}

//static 
RRedBlackNode 
TreeMap::treePredecessor(IN(RRedBlackNode) node)
{
  if (node->_left != nilNode())
    return treeMax(node->_left);

  RRedBlackNode nod = node;
  RRedBlackNode parent = nod->_parent;
  while ((parent != nilNode()) && (nod == parent->_left)) {
    nod = parent;
    parent = parent->_parent;
  }
  return parent;
}

//static 
RRedBlackNode 
TreeMap::treeInsert(IN(RTreeMap) tree, IN(RComparator) comp, IN(RRedBlackNode) newNode, acdk::lang::sys::Allocator* alloc)
{
  int res;
  RObject newKey = newNode->getKey();
  RRedBlackNode parent = nilNode();
  RRedBlackNode root = tree->_root;
  RRedBlackNode result = nilNode();

  while (root != nilNode()) {
    parent = root;
    res = compare(comp, newKey, root->getKey());
    if (res == 0) {
      result = new (alloc) RedBlackNode(root->getKey(), root->getValue());
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


//static 
void 
TreeMap::leftRotate(IN(RTreeMap) tree, IN(RRedBlackNode) nod)
{
  RRedBlackNode child = nod->_left;
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

//static 
void 
TreeMap::rightRotate(IN(RTreeMap) tree, IN(RRedBlackNode) nod)
{
  RRedBlackNode child = nod->_left;
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

//static 
RRedBlackNode 
TreeMap::rbInsert(IN(RTreeMap) tree,  IN(RComparator) comp, IN(RRedBlackNode) node)
{
  RRedBlackNode uncle = nilNode();
  RRedBlackNode nod = node;
  RRedBlackNode result = treeInsert(tree, comp, nod, tree->allocator());
  
  if (result == nilNode()) 
  {
    nod->_color = RedNodeType;
    while ((nod != tree->_root) && (nod->_parent->_color == RedNodeType)) 
    {
      if (nod->_parent == nod->_parent->_parent->_right) {
        uncle = nod->_parent->_parent->_right;
        if (uncle->_color == RedNodeType) {
          nod->_parent->_color = BlackNodeType;
          uncle->_color = BlackNodeType;
          nod->_parent->_parent->_color = RedNodeType;
          nod = nod->_parent->_parent;
        } else {
          if (nod == nod->_parent->_right) {
            nod = nod->_parent;
            leftRotate(tree, nod);
          }
          nod->_parent->_color = BlackNodeType;
          nod->_parent->_parent->_color = RedNodeType;
          rightRotate(tree, nod->_parent->_parent);
        }
      } else {
        uncle = nod->_parent->_parent->_left;
        if (uncle->_color == RedNodeType) {
          nod->_parent->_color = BlackNodeType;
          uncle->_color = BlackNodeType;
          nod->_parent->_parent->_color = RedNodeType;
          nod = nod->_parent->_parent;
        } else {
          if (nod == nod->_parent->_left) {
            nod = nod->_parent;
            rightRotate(tree, nod);
          }
          nod->_parent->_color = BlackNodeType;
          nod->_parent->_parent->_color = RedNodeType;
          leftRotate(tree, nod->_parent->_parent);
        }
      }
    }
  }
  tree->_root->_color = BlackNodeType;
  return result;
}


//static 
RRedBlackNode 
TreeMap::rbDelete(IN(RTreeMap) tree, IN(RRedBlackNode) nod, acdk::lang::sys::Allocator* alloc)
{
  RRedBlackNode splice = nilNode();
  RRedBlackNode child = nilNode();
  RRedBlackNode sentinelParent = nilNode();
  RRedBlackNode result = nod;
  
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
    result = new (alloc) RedBlackNode(nod->getKey(), nod->getValue());
    nod->_key = splice->_key;
    nod->_value = splice->_value;
  }
  
  if (splice->_color == BlackNodeType)
    rbDeleteFixup(tree, child);
  
  return result;
}

//static 
void 
TreeMap::rbDeleteFixup(IN(RTreeMap) tree, IN(RRedBlackNode) node)
{
  
  RRedBlackNode sibling = nilNode();
  RRedBlackNode nod = node;
  while ((nod != tree->_root) && (nod->_color == BlackNodeType)) {
    if (nod == nod->_parent->_left) {
      sibling = nod->_parent->_right;
      if (sibling->_color == RedNodeType) {
        sibling->_color = BlackNodeType;
        nod->_parent->_color = RedNodeType;
        leftRotate(tree, nod->_parent);
        sibling = nod->_parent->_right;
      }
      if ((sibling->_left->_color == BlackNodeType) && 
        (sibling->_right->_color == BlackNodeType)) {
        sibling->_color = RedNodeType;
        nod = nod->_parent;
      } else {
        if (sibling->_right->_color == BlackNodeType) {
          sibling->_left->_color = BlackNodeType;
          sibling->_color = RedNodeType;
          rightRotate(tree, sibling);
          sibling = nod->_parent->_right;
        }
        sibling->_color = nod->_parent->_color;
        nod->_parent->_color = BlackNodeType;
        sibling->_right->_color = BlackNodeType;
        leftRotate(tree, nod->_parent);
        nod = tree->_root;
      }
    } else {
      sibling = nod->_parent->_left;
      if (sibling->_color == RedNodeType) {
        sibling->_color = BlackNodeType;
        nod->_parent->_color = RedNodeType;
        rightRotate(tree, nod->_parent);
        sibling = nod->_parent->_left;
      }
      if ((sibling->_right->_color == BlackNodeType) && 
        (sibling->_left->_color == BlackNodeType)) {
        sibling->_color = RedNodeType;
        nod = nod->_parent;
      } else {
        if (sibling->_left->_color == BlackNodeType) {
          sibling->_right->_color = BlackNodeType;
          sibling->_color = RedNodeType;
          leftRotate(tree, sibling);
          sibling = nod->_parent->_left;
        }
        sibling->_color = nod->_parent->_color;
        nod->_parent->_color = BlackNodeType;
        sibling->_left->_color = BlackNodeType;
        rightRotate(tree, nod->_parent);
        nod = tree->_root;
      }
    }
  }
  nod->_color = BlackNodeType;
}


TreeMapIterator::TreeMapIterator(IN(RTreeMap) map, IN(MapEntryTyp) type)
: _map(map),
  _first(),
  _last(),
  _prev(TreeMap::nilNode()),
  _type(type),
  _knownMods(map->_modCount)
{
  if (_map->isEmpty()) {
    _first = RedBlackNode::nilNode();
  } else {
    _first = TreeMap::treeSearch(_map->_root, _map->_comparator, _map->firstKey());
    _last = TreeMap::treeSearch(_map->_root, _map->_comparator, _map->lastKey());
  }
}

//virtual 
RObject 
TreeMapIterator::next()
{
  _checkMod();
  RRedBlackNode result = _first;
  if (result == RedBlackNode::nilNode())
    THROW0(NoSuchElementException);
  if (result == _last)
    _first = RedBlackNode::nilNode();
  else
    _first = TreeMap::treeSuccessor(_first);
  _prev = result;
  if (_type == Keys) 
    return result->getKey();
  if (_type == Values)
    return result->getValue();
  return (RObject)result;
}

//virtual 
RObject 
TreeMapIterator::element() // ### @todo implement me
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
//virtual 
void 
TreeMapIterator::remove()
{
  _checkMod();
  if (_prev == RedBlackNode::nilNode()) 
    THROW0(IllegalStateException);
  RObject key = _prev->getKey();
  if (_map->containsKey(key)) {
    _map->remove(key);
    _knownMods++;
  }
  _prev = RedBlackNode::nilNode();
}
  

TreeMapSetIterator::TreeMapSetIterator(IN(RSortedMap) map, IN(RTreeMap) treemap, MapEntryTyp type)
: _map(map),
  _treeMap(treemap),
  _first(),
  _last(),
  _prev(TreeMap::nilNode()),
  _type(type),
  _knownMods(treemap->_modCount)
{
  if (_map->isEmpty()) {
    _last = _first = RedBlackNode::nilNode();
  } else {
    _first = TreeMap::treeSearch(_treeMap->_root, _treeMap->_comparator, _map->firstKey());
    _last = TreeMap::treeSearch(_treeMap->_root, _treeMap->_comparator, _map->lastKey());
  }
}

//virtual 
RObject 
TreeMapSetIterator::next()
{
  _checkMod();
  RRedBlackNode result = _first;
  if (result == RedBlackNode::nilNode())
    THROW0(NoSuchElementException);
  if (result == _last)
    _first = RedBlackNode::nilNode();
  else
    _first = TreeMap::treeSuccessor(_first);
  _prev = result;
  if (_type == Keys) 
    return result->getKey();
  if (_type == Values)
    return result->getValue();
  return (RObject)result;
}

//virtual 
RObject 
TreeMapSetIterator::element()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}
//virtual 
void 
TreeMapSetIterator::remove()
{
  _checkMod();
  if (_prev == RedBlackNode::nilNode()) 
    THROW0(IllegalStateException);
  RObject key = _prev->getKey();
  if (_map->containsKey(key)) {
    _map->remove(key);
    _knownMods++;
  }
  _prev = RedBlackNode::nilNode();
}
  

//virtual 
RIterator 
TreeMapCollection::iterator()
{
  if (instanceof(_map, TreeMap) == true)
    return new TreeMapIterator(RTreeMap(_map), Values);
  else if (instanceof(_map, SubTreeMap) == true) 
  {
    RSubTreeMap sbm = (RSubTreeMap)_map;
    return new TreeMapSetIterator(_map, sbm->parentTreeMap(), Values);
  } else
    return Nil;
}


//virtual 
bool 
TreeMapSet::contains(IN(RObject) object)
{
  if (_type == Keys) 
      return _map->containsKey(object);
  if (instanceof(object, MapEntry)) {
    RMapEntry entry = RMapEntry(object);
    RObject key = entry->getKey();
    if (_map->containsKey(key)) {
      RObject inputVal = entry->getValue();
      RObject mapValue = _map->get(key);
      return ((inputVal == Nil)  ? (mapValue == Nil) : (inputVal->equals(mapValue)));
    }
  }
  return false;
}

//virtual 
RIterator 
TreeMapSet::iterator()
{
  RTreeMapSetIterator tmi = new TreeMapSetIterator(_map, _treeMap, _type); 
  return (RIterator)tmi;
}


//virtual 
bool 
TreeMapSet::remove(IN(RObject) object)
{
  if (_type == Keys)
    return (_map->remove(object) != Nil);
  if (instanceof(object, MapEntry) == true)
    return _map->remove(RMapEntry(object)->getKey()) != Nil;
  return false;
}


//virtual 
void 
SubTreeMap::clear()
{
  RRedBlackNode minNode = TreeMap::lowerBound(_map->_root, _map->_comparator, _minKey, _maxKey);
  RRedBlackNode maxNode = TreeMap::upperBound(_map->_root, _map->_comparator, _minKey, _maxKey);
  RObject maxKey = maxNode->getKey();
  while ((minNode != RedBlackNode::nilNode()) &&  ((maxNode == RedBlackNode::nilNode()) ||  
          (TreeMap::compare(_map->_comparator,  minNode->getKey(), maxKey) < 0))) {
    _map->remove(minNode->getKey());
    minNode = TreeMap::treeSuccessor(minNode);
  }
}

//virtual 
bool 
SubTreeMap::containsKey(IN(RObject) key)
{
  return  (TreeMap::keyInRange(_map->_comparator, key, _minKey, _maxKey) && _map->containsKey(key)); 
}    
  
//virtual 
bool 
SubTreeMap::containsValue(IN(RObject) val)
{
  RObject curVal;
  RRedBlackNode minNode = TreeMap::lowerBound(_map->_root,  _map->_comparator, _minKey, _maxKey);
  RRedBlackNode maxNode = TreeMap::upperBound(_map->_root,  _map->_comparator, _minKey, _maxKey);
  RObject maxKey = maxNode->getKey();
  while ((minNode != RedBlackNode::nilNode()) && 
        ((maxNode == RedBlackNode::nilNode()) || 
        (TreeMap::compare(_map->_comparator, 
        minNode->getKey(), maxKey) < 0))) {
    curVal = minNode->getValue();
    if (((val == Nil) && (curVal == Nil)) || val->equals(curVal))
      return true;
    minNode = TreeMap::treeSuccessor(minNode);        
  }
  return false;
}

//virtual 
RSet 
SubTreeMap::entrySet()
{
  RSubTreeMap stm((SubTreeMap*) this);
  RSortedMap sm = (RSortedMap) stm;
  RTreeMapSet tms = new TreeMapSet(sm, _map, Entries); // RSortedMap sortedmap, RTreeMap treeMap, MapEntryTyp type)
  return (RSet) tms;
}

//virtual 
RObject 
SubTreeMap::get(IN(RObject) key)
{
  if (TreeMap::keyInRange(_map->_comparator, key, _minKey, _maxKey))
    return _map->get(key);
  return Nil;
}
  
//virtual 
RObject 
SubTreeMap::put(IN(RObject) key, IN(RObject) val)
{
  if (TreeMap::keyInRange(_map->_comparator, key, _minKey, _maxKey))
    return _map->put(key, val);
  THROW0(IllegalArgumentException);
  return Nil;
}
    
//virtual 
void 
SubTreeMap::putAll(IN(RMap) map)
{
  RMapEntry entry;
  RIterator it = map->entrySet()->iterator();
  while (it->hasNext()) {
    entry = RMapEntry(it->next());
    put(entry->getKey(), entry->getValue());
  }
}

//virtual 
RObject 
SubTreeMap::remove(IN(RObject) key)
{
  if (TreeMap::keyInRange(_map->_comparator, key, _minKey, _maxKey))
    return _map->remove(key);
  THROW0(IllegalArgumentException);
  return Nil;
}
  
//virtual 
int 
SubTreeMap::size()
{
  
  RRedBlackNode minNode = TreeMap::lowerBound(_map->_root, _map->_comparator, _minKey, _maxKey);
  RRedBlackNode maxNode = TreeMap::upperBound(_map->_root, _map->_comparator, _minKey, _maxKey);
  RObject maxKey = maxNode->getKey();
  int count = 0;
  while ((minNode != RedBlackNode::nilNode()) && 
        ((maxNode == RedBlackNode::nilNode()) || 
        (TreeMap::compare(_map->_comparator, 
        minNode->getKey(), maxKey) < 0))) {
    count++;
    minNode = TreeMap::treeSuccessor(minNode);
  }
  return count;
}

//virtual 
RObject 
SubTreeMap::firstKey()
{
  RRedBlackNode first = TreeMap::lowerBound(_map->_root, _map->_comparator, _minKey, _maxKey);
  return (first != RedBlackNode::nilNode()) ? first->getKey() : RObject(Nil);
}

//virtual 
RObject 
SubTreeMap::lastKey()
{
  RRedBlackNode last = RedBlackNode::nilNode();
  if (_maxKey == Nil) {
    last = TreeMap::treeMax(_map->_root);
    return (last != RedBlackNode::nilNode()) ? last->getKey() : RObject(Nil);
  } else {
    last = TreeMap::treeMaxConstrained(_map->_root,
      _map->_comparator,
      _minKey, _maxKey);
    return (last != RedBlackNode::nilNode()) ? TreeMap::treePredecessor(last)->getKey() : RObject(Nil);
  }
}


//virtual 
RSortedMap 
SubTreeMap::subMap(IN(RObject) from, IN(RObject) to)
{
  if ((TreeMap::compare(_map->_comparator, from, to) < 0) &&
      TreeMap::keyInMinRange(_map->_comparator,  from, _minKey) &&
      TreeMap::keyInClosedMaxRange(_map->_comparator, from, _maxKey) &&
      TreeMap::keyInMinRange(_map->_comparator,  to, _minKey) &&
      TreeMap::keyInClosedMaxRange(_map->_comparator, to, _maxKey))
    return new SubTreeMap(_map, from, to);
  THROW0(IllegalArgumentException);
  return Nil;
}
  
//virtual 
RSortedMap 
SubTreeMap::headMap(IN(RObject) to)
{
  if (TreeMap::keyInMinRange(_map->_comparator, to, _minKey) &&
      TreeMap::keyInClosedMaxRange(_map->_comparator, to, _maxKey))
    return new SubTreeMap(_map, _minKey, to);
  THROW0(IllegalArgumentException);
  return Nil;
}
  
//virtual 
RSortedMap 
SubTreeMap::tailMap(IN(RObject) from)
{
  if (TreeMap::keyInMinRange(_map->_comparator, from, _minKey) &&
      TreeMap::keyInClosedMaxRange(_map->_comparator, from, _maxKey))
    return new SubTreeMap(_map, from, _maxKey);

  THROW0(IllegalArgumentException);
  return Nil;
}

} // util
} // acdk
