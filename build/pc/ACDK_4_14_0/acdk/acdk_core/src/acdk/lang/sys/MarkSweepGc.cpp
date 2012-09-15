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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/MarkSweepGc.cpp,v 1.11 2005/05/02 23:05:45 kommer Exp $


#include <acdk.h>
#include <acdk/util/logging/Log.h>
#include <acdk/lang/Integer.h>

#include "Allocator.h"
#include "AllocatorInternals.h"

namespace acdk {
namespace lang {
namespace sys {

//#define DEBUG_PA
#define DOUTST(msgstr) sys::coreout << msgstr

#ifdef DEBUG_PA
#define DOUT(msgstr) sys::coreout << msgstr
#else //DEBUG_PA
#define DOUT(msgstr) do { } while(false)
#endif //DEBUG_PA

inline bool hasColletableFields(Object* o)
{
  return true; // because DmiObject always return 0
  /*
  if (o->_isObjectRefFlag(ObjectBase::ObjectHasLocalGc) == true)
    return true;
  int colfields = o->getClazzInfo()->getCollectableFieldsCount();
  return colfields > 0;
  */
}


void
markAsExternal(Object* obj)
{
  //DOUT("MARK: " << (void*) obj << sys::eofl);
  obj->_setObjectRefFlag(true, ObjectBase::Marked);
  obj->_setObjectRefFlag(true, ObjectBase::Visited);
  DOUT("GC: Mark Object: " << (void*)obj << ", " << obj->getClazzInfo()->name << sys::eofl);

  if (hasColletableFields(obj) == false)
    return;

  FieldReferences fields;
  obj->getCollectableFields(fields);
  for (int i = 0; i < fields.size(); i++) 
  {
    Object* f = fields[i]->impl();
    if (f != 0 && f->_isObjectRefFlag(ObjectBase::Marked) == false)
      markAsExternal(f);
  }
}


bool
hasExternalReference(Object* findobj, Object* obj, ReferedMap& refmap)
{
  if (obj->_isObjectRefFlag(ObjectBase::Visited) == true)
    return obj->_isObjectRefFlag(ObjectBase::Marked);
  //obj->_setObjectRefFlag(true, ObjectBase::Visited);
  if (obj->_isObjectRefFlag(ObjectBase::Marked) == true)
    return true;
  ReferedMap::iterator rmend = refmap.end();
  ReferedMap::iterator rmit = refmap.find(obj);
  if (rmit == rmend)
    return false;
  obj->_setObjectRefFlag(true, ObjectBase::Visited);
  while (rmit != rmend) 
  {
    if ((*rmit).first == 0)
      break;
    if ((*rmit).first == obj) {
      ReferedMapValue* ovecptr = (*rmit).second;
      if (ovecptr == 0) {
        ++rmit;
        continue;
      }
      ObjectPtrVector& ovec = ovecptr->refered;
      for (int i = 0; i < ovec.size(); i++) {
        Object* mobj = ovec[i];
        if (mobj == obj) { // ??
          //probably nonsense: ++rmit;
          continue;
        }
        if (refmap.getRefered(mobj) == 0) {
          markAsExternal(mobj);
          //obj->_setObjectRefFlag(false, ObjectBase::Visited);
          return true;
        }
        if (hasExternalReference(findobj, mobj, refmap) == true) {
          markAsExternal(obj);
          //obj->_setObjectRefFlag(false, ObjectBase::Visited);
          return true;
        }
      }
    }
    ++rmit;
  }
  //obj->_setObjectRefFlag(false, ObjectBase::Visited);
  return false;
}




bool
doGc(Allocator* allocator, AllocatorObjectsIterator* iterator, bool recursiv)
{
  
  int before_gc_elements;
  int before_gc_memory;
  {
    const AllocatorInfo& ainf = allocator->getAllocatorInfo();
    before_gc_elements = ainf.memTypes[ObjectMem].count;
    before_gc_memory = ainf.memTypes[ObjectMem].size;
  }
  
//#if 0
  //int before_gc_elements = allocator->getElementCount(UnspecifiedMem);
  //int before_gc_memory = allocator->getAllocatedCount(UnspecifiedMem);
  Object* o = 0;
  int objectcount = 0;
  while ((o = iterator->getNextObject()) != 0)
  {
    //o = iterator->getCurrentElement().mem.obj;
    o->_setObjectRefFlag(false, ObjectBase::Marked);
    o->_setObjectRefFlag(false, ObjectBase::Visited);
    if (o->_isObjectRefFlag(ObjectBase::IsStaticRef) == true || 
        o->_isObjectRefFlag(ObjectBase::IsMemLocked) == true || 
        o->_isObjectRefFlag(ObjectBase::IsStackRef) == true //|| 
        //o->_isObjectRefFlag(ObjectBase::ObjectHasLocalGc) == true
        )
    {
      markAsExternal(o);
    }
    /*
    if (_lastAddress < o)
      _lastAddress = o;
    if (_firstAddress > o)
      _firstAddress = o;
    */
    ++objectcount;
  }
  iterator->reset();

  ReferedMap refmap((1 + objectcount) * 3);
  //HolderMap holdermap(mapsize);

  FieldReferences fields;
  // building backward ref
  while ((o = iterator->getNextObject()) != 0)
  {
    o = iterator->getCurrentElement().mem.obj;
    if (hasColletableFields(o) == false) 
      continue;
    
    fields.clear();
    o->getCollectableFields(fields);
    for (int i = 0; i < fields.size(); i++) 
    {
      if (*fields[i] != Nil) 
      {
              //DOUT(fields[i]->impl() << " refered by " <<  o << sys::eofl);
        refmap.addRefered(fields[i]->impl(), o);
        refmap.addHolder(fields[i]);
          //holdermap.add(fields[i]);
      }
    }
  }
  DOUT("[GC] marking" << sys::eofl);
  {
    ReferedMap::iterator it = refmap.begin();
    ReferedMap::iterator end = refmap.end();
    for (; it != end; ++it) 
    {
      Object* robj = (*it).first;
      if (robj->magic() != _MagicObjectSig) 
       continue;

      if (hasExternalReference(robj, robj, refmap) == true) 
      {
        //DOUT("MARK: " << (void*)robj);
        robj->_setObjectRefFlag(true, ObjectBase::Marked);
      }
      
    }
  }
//#endif

   DOUT("[GC] sweeping" << sys::eofl);
RestartSweep:
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
      //sys::coreout << "o:" << (void*)obj << sys::eofl;
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
        ObjectRefPtrVector& refvec = (*it).second->hold;
        bool restart = false;
        for (int i = 0; i < refvec.size(); i++) 
        {
          RObject* ref = refvec[i];
          if (ref == 0 || ref->impl() == 0)
            continue;
          if (ref->impl()->magic() != _MagicObjectSig) 
            continue;
          if (ref->impl() != obj)
            DOUT(" OOPS " << (void*)ref->impl() << " != " << obj << sys::eofl);
          if (obj->refCount() == 1) 
            restart = true;  
          DOUT("GC: Free Ref: " << (void*)ref << "; Object: " << (void*)ref->impl() << ", " << ref->impl()->getClazzInfo()->name << sys::eofl);
          *(ref) = Nil;
          if (restart == true) 
          {
            //?? refmap.erase(it);
            DOUT("[GC] restart" << sys::eofl);
            goto RestartSweep;
          }
          wasmodified = true;
        }
      }
    }
  }
  /*
  int after_gc_elements = _allocator->getElementCount(UnspecifiedMem);
  int after_gc_memory = _allocator->getAllocatedCount(UnspecifiedMem);
  */
  const AllocatorInfo& ainf = allocator->getAllocatorInfo();
  int after_gc_elements = ainf.memTypes[ObjectMem].count;
  int after_gc_memory = ainf.memTypes[ObjectMem].size;

  if (after_gc_elements == before_gc_elements)
    ACDK_NLOG("acdk.lang.gc", SysDebug, "GC released no objects");
  else 
    ACDK_NLOGP("acdk.lang.gc", SysDebug, "GC Released objects", LOG_NPV(Elements, before_gc_elements - after_gc_elements)
                                                            << LOG_NPV(Bytes, before_gc_memory - after_gc_memory));

  DOUT("After Sweep" << sys::eofl);
  //_allocator->dumpStatistics(false);
  //_allocator->_heap = 0;
  return after_gc_elements != before_gc_elements;
  return false;
}

void unmarkAndUnvisitedAll(AllocatorObjectsIterator* iterator)
{
  Object* o = 0;
  while ((o = iterator->getNextObject()) != 0)
  {
    o->_setObjectRefFlag(false, ObjectBase::Marked);
    o->_setObjectRefFlag(false, ObjectBase::Visited);
  }
}

void markSubStrings(String* s)
{
  if (s->storageType() != SubSST)
    return;
  s->_getSubstrBase()->_setObjectRefFlag(true, ObjectBase::Marked);
}



void markMemberObjects(AllocatorObjectsIterator* iterator)
{
  unmarkAndUnvisitedAll(iterator);
  iterator->reset();
  Object* o = 0;
  while ((o = iterator->getNextObject()) != 0)
  {
    markMembers(o);
  }
  iterator->reset();
  while ((o = iterator->getNextObject()) != 0)
  {
    String* s = dynamic_cast<String*>(o);
    if (s != 0)
      markSubStrings(s);
    if (o->isStaticRef() == true)
    {
      o->_setObjectRefFlag(true, ObjectBase::Marked);
    }
  }
}

void markMembers(Object* obj)
{
  //if (obj->_isObjectRefFlag(ObjectBase::Visited) == true)
  //  return;
  if (hasColletableFields(obj) == false)
    return;
  FieldReferences fields;
  obj->getCollectableFields(fields);
  for (int i = 0; i < fields.size(); i++) 
  {
    Object* f = fields[i]->impl();
    if (f != 0/* && f->_isObjectRefFlag(ObjectBase::Marked) == false*/)
    {
      if (f->_isObjectRefFlag(ObjectBase::Marked) == false)
      {
        f->_setObjectRefFlag(true, ObjectBase::Marked);
        markMembers(f);
        //f->_setObjectRefFlag(true, ObjectBase::Visited);
      }
  
    }
  }
}


void listGcAble(HeapFrame* frame, AllocatorObjectsIterator* iterator, ::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
    Object* o = 0;
  int objectcount = 0;
  while ((o = iterator->getNextObject()) != 0)
  {
    o->_setObjectRefFlag(false, ObjectBase::Marked);
    o->_setObjectRefFlag(false, ObjectBase::Visited);
    if (o->_isObjectRefFlag(ObjectBase::IsStaticRef) == true || 
        o->_isObjectRefFlag(ObjectBase::IsMemLocked) == true || 
        o->_isObjectRefFlag(ObjectBase::IsStackRef) == true //|| 
        //o->_isObjectRefFlag(ObjectBase::ObjectHasLocalGc) == true
        )
    {
      markAsExternal(o);
    }
    ++objectcount;
  }
  iterator->reset();
  ReferedMap refmap((1 + objectcount) * 3);
  
  //HolderMap holdermap(mapsize);
  FieldReferences fields;
  
  // building backward ref
  while ((o = iterator->getNextObject()) != 0)
  {
    o = iterator->getCurrentElement().mem.obj;
    if (hasColletableFields(o) == false) 
      continue;
    
    fields.clear();
    o->getCollectableFields(fields);
    for (int i = 0; i < fields.size(); i++) 
    {
      if (*fields[i] != Nil) 
      {
              //DOUT(fields[i]->impl() << " refered by " <<  o << sys::eofl);
        refmap.addRefered(fields[i]->impl(), o);
        refmap.addHolder(fields[i]);
          //holdermap.add(fields[i]);
      }
    }
  }
  //DOUT("[GC] marking" << sys::eofl);
  {
    ReferedMap::iterator it = refmap.begin();
    ReferedMap::iterator end = refmap.end();
    for (; it != end; ++it) 
    {
      Object* robj = (*it).first;
      if (robj->magic() != _MagicObjectSig) 
       continue;

      if (hasExternalReference(robj, robj, refmap) == true) 
      {
        //DOUT("MARK: " << (void*)robj);
        robj->_setObjectRefFlag(true, ObjectBase::Marked);
      }
      
    }
  }
  ReferedMap::iterator it = refmap.begin();
  ReferedMap::iterator end = refmap.end();
    
  for (; it != end; ++it) 
  {
    Object* obj = (*it).first;
    if (obj == 0)
      continue;
    if (obj->magic() != _MagicObjectSig) 
      continue;
    //sys::coreout << "o:" << (void*)obj << sys::eofl;
    try {
      obj->testIntegrity();
    } 
    catch (...) 
    {
      ACDK_NLOG("acdk.lang.gc", Error, "Object is insane: " + Integer::toHexString((int)(void*)obj));
      return;
    }
    if (obj->_isObjectRefFlag(ObjectBase::Marked) == false) 
    {
      ObjectRefPtrVector& refvec = (*it).second->hold;
      bool restart = false;
      for (int i = 0; i < refvec.size(); i++) 
      {
        RObject* ref = refvec[i];
        if (ref == 0 || ref->impl() == 0)
          continue;
        if (ref->impl()->magic() != _MagicObjectSig) 
          continue;
        //DOUT("GC: Free Ref: " << (void*)ref << "; Object: " << (void*)ref->impl() << ", " << ref->impl()->getClazzInfo()->name << sys::eofl);
        if (listener->listedAllocated(frame, (*ref).impl(), ObjectMem, 0) == false)
          return;
      }
    }
  }
}

void genericListObject(HeapFrame* frame, AllocatorObjectsIterator* iterator, ::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  
  Object* o = 0;
  if (flags & ListRootsOnly)
  {
    markMemberObjects(iterator);
    
  }
  if (flags & ListGcAble)
  {
    listGcAble(frame, iterator, listener, flags);
    return;
  }
  iterator->reset();
  while (iterator->getNext() != 0)
  {
    AllocatorElement el = iterator->getCurrentElement();
    if (el.type == ObjectMem)
      o = el.mem.obj;
    else
      o = 0;
    if (flags & ListRootsOnly)
    {
      if (o != 0 && o->_isObjectRefFlag(ObjectBase::Marked) == false)
      {
        if (listener->listedAllocated(frame, el.mem.buffer, (AllocatedType)(AllocatedTypeMask & el.type), el.size) == false)
          return;
      }
    }
    else if (flags & ListGcAble)
    {
      if (o != 0 && o->_isObjectRefFlag(ObjectBase::Marked) == false)
      {
        if (listener->listedAllocated(frame, el.mem.buffer, (AllocatedType)(AllocatedTypeMask & el.type), el.size) == false)
          return;
      }
    }
    else
    {
      if (listener->listedAllocated(frame, el.mem.buffer, (AllocatedType)(AllocatedTypeMask & el.type), el.size) == false)
        return;
    }
  }
}



} // namespace sys
} // namespace lang 
} // namespace acdk 


