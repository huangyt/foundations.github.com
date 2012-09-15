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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/LocalGcHeap.cpp,v 1.7 2005/03/07 13:47:28 kommer Exp $


#include "LocalGcHeap.h"
#include "../ref/NotifyObjectEvent.h"
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace lang {
namespace sys {

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)
# define DOUT(msg) sys::coreout << msg << sys::eofl
#else
# define DOUT(msg) do { } while(false)
#endif
namespace {

typedef core_vector<Object*> ObjectPtrVector;
typedef core_vector<RObject*> ObjectRefPtrVector;

struct ReferedMapValue
{
  ObjectPtrVector referes;
  ObjectPtrVector holdes;
  ObjectRefPtrVector holdedBy;
  bool collected;
  //int masterRefs;
  ReferedMapValue()
    : collected(false)
  {
  }
};

class ReferedMap
: extends acdk::lang::Object
, implements ::acdk::lang::ref::NotifyObjectEventListener 
{
public:
  //typedef core_flathashmap<Object*, ReferedMapValue*> Map;
  typedef std::map<Object*, ReferedMapValue*> Object2ReferedMap;
  typedef Object2ReferedMap::iterator iterator;
  
  Object2ReferedMap _map;
  Object* _object;
public:
  
  
  ReferedMap()
    : _map()
  {
  }
  virtual ~ReferedMap()
  {
    reset();
  }
  void reset()
  {
    iterator it = _map.begin();
    iterator end = _map.end();
    for (; it != end; ++it)
    {
      ReferedMapValue* rm = (*it).second;
      if (rm != 0) 
        delete rm;
    }
    _map.erase(_map.begin(), _map.end());
  }

  ReferedMapValue* get(Object* obj)
  {
    iterator it = _map.find(obj);
    if (it == _map.end())
      return 0;
    return it->second;
  }
  /**
    obj points to other objects
  */
  ObjectPtrVector* getRefered(Object* obj)
  {
    iterator it = _map.find(obj);
    if (it == _map.end())
      return 0;
    return &(*it).second->referes;
  }
  ObjectPtrVector* getOutRefs(Object* obj)
  {
    return getRefered(obj);
  }
  /**
    obj is holded by other objects
  */
  ObjectPtrVector* getHolded(Object* obj)
  {
    iterator it = _map.find(obj);
    if (it == _map.end())
      return 0;
    return &(*it).second->holdes;
  }
  ObjectPtrVector* getInRefs(Object* obj)
  {
    return getHolded(obj);
  }
  ReferedMapValue* getCreate(Object* o)
  {
    iterator it = _map.find(o);
    if (it != _map.end())
      return (*it).second;
    ReferedMapValue* v = new ReferedMapValue();
    _map[o] = v;
    return v;
  }
  /**
    obj points to f
  */
  void addRefered(Object* obj, Object* f)
  {
    ReferedMapValue* om = getCreate(obj);
    om->referes.push_back(f);
    om = getCreate(f);
    om->holdes.push_back(obj);
  }
  void addHolder(RObject* r)
  {
    if (r->impl() == 0)
      return;
    iterator it = _map.find(r->impl());
    if (it != _map.end()) {
      (*it).second->holdedBy.push_back(r);
    } else {
      ReferedMapValue* v = new ReferedMapValue();
      v->holdedBy.push_back(r);
      _map[r->impl()] = v;
    }
  }
  
  iterator begin() { return _map.begin(); }
  iterator end() { return _map.end(); }
  iterator find(Object* o) { return _map.find(o); }
  bool isCollected(Object* o)
  {
    iterator it = _map.find(o);
    if (it == _map.end())
      return false;
    return (*it).second->collected;
  }
  void setCollected(Object* o)
  {
    getCreate(o)->collected = true;
  }
  void resetCollected()
  {
    Object2ReferedMap::iterator it = _map.begin();
    Object2ReferedMap::iterator end = _map.end();
    for (; it != end; ++it)
      (*it).second->collected = false;
  }
  void erase(iterator& it) 
  { 
    ReferedMapValue* rm = (*it).second;
    if (rm != 0) 
      delete rm;
    _map.erase(it); 
  }
  void registerMapCallback(Object* o)
  {
    ref::NotifyObjectEvent::add(o, this);
    _object = o;
    /*
    iterator it = begin();
    iterator eit = end();
    for (; it != eit; ++it)
    {
      NotifyObjectEvent::add(it->first, this);
    }
    */
  }
  bool objectStillAlive()
  {
    return _object != 0;
  }
  void unregisterMapCallback(Object* o)
  {
    ref::NotifyObjectEvent::remove(o, this);
    _object = 0; 
  }

  void eraseFromSet(Object* o)
  {
    iterator it = find(o);
    if (it == end())
      return;
    erase(it);
    if (_object == o)
      _object = 0;
  }
  void getClusterRefCounts(int& refC, int& inRefC);
  void buildDeps(Object* o, bool withHoldedBy);
  void _buildDeps(Object* o, bool withHoldedBy);
  void dump();
  foreign virtual void notifyBeforeConstruction(Object* obj) {}
  foreign virtual void notifyWhileDestruction(Object* obj) {}
  foreign virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) { return false; }
  foreign virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) { return false; }
  foreign virtual bool notifyBeforeDestruction(Object* obj) { eraseFromSet(obj); return true; }
};

void 
ReferedMap::dump()
{
#if !defined(LOCAL_DEBUG)
  return;
#else
  ReferedMap::iterator it = begin();
  ReferedMap::iterator oend = end();
  DOUT("ReferedMap::dump: ");
  for (; it != oend; ++it)
  {
    Object* o = it->first;
    ReferedMapValue* v = it->second;
    DOUT((void*)o << "; rc=" << o->refCount() << "; " << o->getClazzInfo()->name);
    {
      ObjectPtrVector::iterator it = v->referes.begin();
      ObjectPtrVector::iterator iend = v->referes.end();
      for (; it != iend; ++it)
      {
        DOUT(" -> " << (void*)*it);
      }
    }
    {
      ObjectPtrVector::iterator it = v->holdes.begin();
      ObjectPtrVector::iterator iend = v->holdes.end();
      for (; it != iend; ++it)
      {
        DOUT(" <- " << (void*)*it);
      }
    }
    /*
    {
      ObjectRefPtrVector::iterator it = v->holdedBy.begin();
      ObjectRefPtrVector::iterator iend = v->holdedBy.end();
      for (; it != iend; ++it)
      {
        DOUT(" <= " << (void*)(*it)->impl());
      }
    }*/
  }
#endif // LOCAL_DEBUG
}

void
ReferedMap::getClusterRefCounts(int& refC, int& inRefC)
{
  refC = 0;
  inRefC = 0;
  //int masterRefC = 0;
  ReferedMap::iterator it = begin();
  ReferedMap::iterator oend = end();
  for (; it != oend; ++it)
  {
    Object* o = (*it).first;
    int rc = o->refCount();
    int hc = getHolded(o)->size();
    refC += o->refCount();
    inRefC += getHolded(o)->size();
    //masterRefC += getMasterRefs(o);
  }
  //return masterRefC;
}


void
ReferedMap::_buildDeps(Object* o, bool withHoldedBy)
{
  if (o == 0)
    return;
  FieldReferences fields;
  o->getCollectableFields(fields);
  setCollected(o);
  for (int i = 0; i < fields.size(); i++) 
  {
    Object* f = fields[i]->impl();
    if (f == 0)
      continue;
    if (withHoldedBy == true)
      addHolder(fields[i]);
    addRefered(o, f);
    if (isCollected(f) == false)
      _buildDeps(f, withHoldedBy);
  }
}

void
ReferedMap::buildDeps(Object* o, bool withHoldedBy)
{
  _buildDeps(o, withHoldedBy);
  resetCollected();
}

void markAsExternal(Object* obj, ReferedMapValue* rmv, ReferedMap& refMap);

void
markAsExternal(Object* obj, ReferedMap& refMap)
{
  if (obj->_isObjectRefFlag(ObjectBase::Marked) == true)
    return;
  ReferedMap::iterator it = refMap.find(obj);
  if (it == refMap.end())
  {
    obj->_setObjectRefFlag(true, ObjectBase::Marked);
    return;
  }
  markAsExternal(obj, it->second, refMap);
}

void
markAsExternal(Object* obj, ReferedMapValue* rmv, ReferedMap& refMap)
{
  if (obj->_isObjectRefFlag(ObjectBase::Marked) == true)
    return;
  obj->_setObjectRefFlag(true, ObjectBase::Marked);
  DOUT("GC: Mark Object: " << (void*)obj << ", " << obj->getClazzInfo()->name);
  
  ObjectPtrVector::iterator it = rmv->referes.begin();
  ObjectPtrVector::iterator end = rmv->referes.end();
  for (; it != end; ++it)
  {
    Object* f = *it;
    markAsExternal(f, refMap);
  }

  /*
  FieldReferences fields;
  obj->getCollectableFields(fields);
  for (int i = 0; i < fields.size(); i++) 
  {
    Object* f = fields[i]->impl();
    if (f != 0 && f->_isObjectRefFlag(ObjectBase::Marked) == false)
      markAsExternal(f);
  }
  */
}

inline bool isMarkedAsExternal(Object* o)
{
  return o->_isObjectRefFlag(ObjectBase::IsStaticRef) == true || 
        o->_isObjectRefFlag(ObjectBase::IsMemLocked) == true || 
        o->_isObjectRefFlag(ObjectBase::IsStackRef) == true ||
        o->_isObjectRefFlag(ObjectBase::Marked) == true;

}


bool
hasExternalReference(Object* obj, ReferedMapValue* rmv, ReferedMap& refmap)
{
  if (isMarkedAsExternal(obj) == true)
    return true;
  
  if (obj->_isObjectRefFlag(ObjectBase::Visited) == true)
    return isMarkedAsExternal(obj);
  
  obj->_setObjectRefFlag(true, ObjectBase::Visited);
  
  int refC = obj->refCount();
  int inRefC = rmv->holdedBy.size();
  if (refC > inRefC)
  {
    markAsExternal(obj, rmv, refmap);
    return true;
  }
  return false;
}



void
mark(ReferedMap& refMap)
{
  ReferedMap::iterator it = refMap.begin();
  ReferedMap::iterator end = refMap.end();
  for (; it != end; ++it)
  {
    Object* o = it->first;
    o->_setObjectRefFlag(false, ObjectBase::Marked);
    o->_setObjectRefFlag(false, ObjectBase::Visited);
  }
  it = refMap.begin();
  for (; it != end; ++it)
  {
    Object* o = it->first;
    if (isMarkedAsExternal(o) == true)
    {
      markAsExternal(o, (*it).second, refMap);
    }
  }
  it = refMap.begin();
  for (; it != end; ++it) 
  {
    Object* robj = (*it).first;
    if (robj->magic() != _MagicObjectSig) 
     continue;

    if (hasExternalReference(robj, (*it).second, refMap) == true) 
    {
      markAsExternal(robj, (*it).second, refMap);
    }
  }
}

bool 
sweep(Object* obj, ReferedMap& refmap)
{
  if (obj->_isObjectRefFlag(ObjectBase::Marked) == true) 
    return false;
  try {
    obj->lockMem(true);
    obj->_call_finalize();
    obj->lockMem(false);
  } catch (RThrowable ex) {
  }
  ObjectRefPtrVector& refvec = (refmap.find(obj))->second->holdedBy;
  bool restart = false;
  for (int i = 0; i < refvec.size(); i++) 
  {
    RObject* ref = refvec[i];
    if (ref == 0 || ref->impl() == 0)
      continue;
    if (ref->impl() != obj)
      DOUT(" OOPS " << (void*)ref->impl() << " != " << obj);
    if (obj->refCount() == 1) 
    {
      restart = true;  
    }
    DOUT("GC: Free Ref: " << (void*)ref << "; Object: " << (void*)ref->impl() << ", " << ref->impl()->getClazzInfo()->name);
    *(ref) = Nil;
    if (restart == false && refmap.objectStillAlive() == false)
      restart = true;
    if (restart == true) 
    {
      //?? refmap.erase(it);
      DOUT("[GC] restart");
      return true;
    }
    //wasmodified = true;
  }
  return true;
    
  // dead code
  bool wasmodified = true;
  while (wasmodified == true) 
  {
    wasmodified = false;
    ReferedMap::iterator it = refmap.begin();
    ReferedMap::iterator end = refmap.end();
    
    for (; it != end; ++it) 
    {
      Object* obj = (*it).first;
      if (obj == 0)
        continue;
      if (obj->magic() != _MagicObjectSig) 
          continue;
      try {
        obj->testIntegrity();
      } 
      catch (...) 
      {
        ACDK_NLOG("acdk.lang.gc", Error, "Object is insane: " + Integer::toHexString((int)(void*)obj));
        return false;
      }
      if (obj->_isObjectRefFlag(ObjectBase::Marked) == false) 
      {
        ObjectRefPtrVector& refvec = (*it).second->holdedBy;
        bool restart = false;
        for (int i = 0; i < refvec.size(); i++) 
        {
          RObject* ref = refvec[i];
          if (ref == 0 || ref->impl() == 0)
            continue;
          if (ref->impl()->magic() != _MagicObjectSig) 
            continue;
          if (ref->impl() != obj)
            DOUT(" OOPS " << (void*)ref->impl() << " != " << obj);
          if (obj->refCount() == 1) 
            restart = true;  
          DOUT("GC: Free Ref: " << (void*)ref << "; Object: " << (void*)ref->impl() << ", " << ref->impl()->getClazzInfo()->name);
          *(ref) = Nil;
          if (restart == false && refmap.find(obj) == refmap.end())
            restart = true;
          if (restart == true) 
          {
            //?? refmap.erase(it);
            DOUT("[GC] restart");
            return true;
          }
          wasmodified = true;
        }
      }
    }
  }
  return wasmodified;
}

bool 
doGc(Object* obj, ReferedMap& refMap)
{
  obj->_setObjectRefFlag(false, ObjectBase::ObjectHasLocalGc); // to avoid cyclic gc

  bool gced = false;
  refMap.registerMapCallback(obj);
  bool restarted = false;
  bool erg = false;
restart:

  ReferedMap::iterator oit = refMap.find(obj);
  if (refMap.objectStillAlive() == false)
  {
    goto endGc;
  }

  refMap.dump();
  if (restarted == true)
  {
    refMap.reset();
    refMap.buildDeps(obj, true);
  }
  mark(refMap);
  erg = sweep(obj, refMap);
  gced |= erg;
  if (erg == true)
  {
    restarted = true;
    goto restart;
  }
endGc:
  refMap.unregisterMapCallback(obj);
  if (oit != refMap.end())
    obj->_setObjectRefFlag(true, ObjectBase::ObjectHasLocalGc); // enable avoid cyclic gc
  return gced;
  /*
  ObjectRefPtrVector::iterator it = oit->second->holdedBy.begin();
  ObjectRefPtrVector::iterator end = oit->second->holdedBy.end();

  for (; it != end; ++it)
  {
    if ((*it)->impl() != 0)
    {
      *(*it) = Nil;
      ++releasedCount;
      goto restart;
    }
  }
  refMap.unregisterMapCallback(obj);
  return releasedCount;
  */
}


} // anon namespace

void 
LocalGcHeap::addObject(Object* o, RObject* referedFrom)
{
  if (_objects.find(o) != _objects.end())
    return;

  _objects[o] = SharedOwnedValue(referedFrom);
  ref::NotifyObjectEvent::add(o, this);
}

void 
LocalGcHeap::eraseFromSet(Object* o)
{
  ObjectMap::iterator it = _objects.find(o);
  if (it == _objects.end())
    return;
  _objects.erase(it);
  ref::NotifyObjectEvent::remove(o, this);
}

Allocator* 
LocalGcHeap::allocator() 
{ 
  return ObjectHeap::allocator(); 
}

void 
LocalGcHeap::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  if (flags & ListRootsOnly)
    return;
  ObjectMap::iterator it = _objects.begin();
  ObjectMap::iterator oend = _objects.end();
  bool gced = false;
  for (; it != oend; ++it)
  {
    if (listener->listedAllocated(0, it->first, ObjectMem, 0) == false)
      return;
  }
}



bool 
LocalGcHeap::gcObject(Object* obj, RObject* referedFrom)
{
  DOUT("LocalGcHeap::gcObject: " << (void*)obj);
  if (core_system::inMain() == false)
    return false;
  ReferedMap refMap;
  refMap.buildDeps(obj, true);
  /*int inRefC;
  int refC;
  refMap.getClusterRefCounts(refC, inRefC);
  if (inRefC < refC)
    return false;
    */
  return doGc(obj, refMap);
}

bool 
LocalGcHeap::gc(bool recursiv)
{
  bool gced = false;
  if (core_system::inMain() == false)
    return false;
  
restart:
  ObjectMap::iterator it = _objects.begin();
  ObjectMap::iterator oend = _objects.end();
  
  for (; it != oend; ++it)
  {
    bool erg = gcObject(it->first, it->second.referedBy);
    gced |= erg;
    if (erg == true)
      goto restart;
  }
  return gced;
}




} // namespace sys
} // namespace lang 
} // namespace acdk 


