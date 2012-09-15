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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/AllocatorInternals.h,v 1.6 2005/04/14 10:41:55 kommer Exp $

#ifndef acdk_lang_sys_AllocatorInternals_h
#define acdk_lang_sys_AllocatorInternals_h

#include <acdk.h>
#include <map>
namespace acdk {
namespace lang {
namespace sys {

typedef core_vector<Object*> ObjectPtrVector;
typedef core_vector<RObject*> ObjectRefPtrVector;

struct ReferedMapValue
{
  ObjectPtrVector refered;
  ObjectRefPtrVector hold;
  ReferedMapValue()
  {
  }
};

class ReferedMap
{
public:
  //typedef core_flathashmap<Object*, ReferedMapValue*> Map;
  typedef std::map<Object*, ReferedMapValue*> Object2ReferedMap;
  typedef Object2ReferedMap::iterator iterator;
  
  Object2ReferedMap _map;
public:
  
  static ReferedMap* _thisInstance;
  
  ReferedMap(int mapsize)
    : _map(/*mapsize*/)
  {
    _thisInstance = this;
  }
  ~ReferedMap()
  {
    _thisInstance = 0;
  }
  ObjectPtrVector* getRefered(Object* obj)
  {
    iterator it = _map.find(obj);
    if (it == _map.end())
      return 0;
    return &(*it).second->refered;
    /*
    ReferedMapValue** rm = _map.get(obj);
    if (rm == 0)
      return 0;
    return &(*rm)->refered;
    */
  }
  ObjectRefPtrVector* getHolded(Object* obj)
  {
    iterator it = _map.find(obj);
    if (it == _map.end())
      return 0;
    return &(*it).second->hold;
    /*
    ReferedMapValue** rm = _map.get(obj);
    if (rm == 0)
      return 0;
    return &(*rm)->hold;
    */
  }
  void addRefered(Object* obj, Object* f)
  {
    iterator it = _map.find(obj);
    if (it != _map.end()) {
      (*it).second->refered.push_back(f);
    } else {
      ReferedMapValue* v = new ReferedMapValue();
      v->refered.push_back(f);
      _map[obj] = v;

    }
  }
  void addHolder(RObject* r)
  {
    if (r->impl() == 0)
      return;
    iterator it = _map.find(r->impl());
    if (it != _map.end()) {
      (*it).second->hold.push_back(r);
    } else {
      ReferedMapValue* v = new ReferedMapValue();
      v->hold.push_back(r);
      _map[r->impl()] = v;
    }
  }
  iterator begin() { return _map.begin(); }
  iterator end() { return _map.end(); }
  iterator find(Object* o) { return _map.find(o); }
  void erase(iterator& it) 
  { 
    ReferedMapValue* rm = (*it).second;
    if (rm != 0) 
      delete rm;
    _map.erase(it); 

    /*
    ReferedMapValue* rm = (*it).second;
    if (rm != 0) 
      delete rm;
    _map.erase(it); 
    */
  }
  void eraseValue(RObject* field)
  {
    if (field == 0 || field->impl() == 0)
      return;
    ObjectRefPtrVector* hvec =  getHolded(field->impl());
    if (hvec == 0)
      return;
    for (int i = 0; i < hvec->size(); i++) {
      if (hvec->operator[](i) == field)
        hvec->operator[](i) = 0;
    }
  }
};

/**
  contains information about a allocated memory
*/
struct AllocatorElement
{
  union Mem {
    void* buffer;
    Object* obj;
  } mem;
  /// see AllocatedType
  int type;
  /**
    size of allocated element without internal 
    management size
  */
  int size;
  /**
    size of allocated element including internal 
    management size
  */
  int rawsize;
  AllocatorElement(void* ptr = 0, int typ = UnspecifiedMem, int siz = 0, int rawsiz = 0)
    : type(typ)
    , size(siz)
    , rawsize(rawsiz)
  {
    mem.buffer = ptr;
  }
  AllocatorElement(Object* obj, int siz = 0, int rawsiz = 0)
    : type(ObjectMem)
    , size(siz)
    , rawsize(rawsiz)
  {
    mem.obj = obj;
  }
};

foreign 
class AllocatorObjectsIterator
{
public:
  virtual ~AllocatorObjectsIterator() {}
  virtual void* getNext() = 0;
  virtual Object* getNextObject() = 0;
  virtual AllocatorElement getCurrentElement() = 0;
  virtual void reset() = 0;
};

/// do a mark sweep on objects.
bool doGc(Allocator* allocator, AllocatorObjectsIterator* iterator, bool recursiv);

/**
  mark all members of the objects
  unmarked objects are root objects
*/
void markMemberObjects(AllocatorObjectsIterator* iterator);

/// reset mark and visited flags
void unmarkAndUnvisitedAll(AllocatorObjectsIterator* iterator);

/// mark all members of this object
void markMembers(Object* obj);


void genericListObject(HeapFrame* frame, AllocatorObjectsIterator* iterator, ::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);


inline bool _valid(const Object* o)
{
  if (o == 0)
    return false;
  if (o->magic() != ::acdk::lang::_MagicObjectSig) 
    return false;
  return true;
}

} // namespace sys
} // namespace lang 
} // namespace acdk 




#endif //acdk_lang_AllocatorInternals_h

