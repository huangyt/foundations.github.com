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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/PagedHeap.cpp,v 1.28 2005/03/07 13:47:28 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Error.h>
#include  "PagedHeap.h"
#include <acdk/lang/ref/NotifyObjectEvent.h>
#include <acdk/util/logging/Log.h>
#include <acdk/lang/Integer.h>
#include "../Error.h"
#include <acdk/lang/sys/core_hashmap.h>
#include "BitmapPagedAllocator.h"
#include <map>
#include "core_prim.h"
#include "core_value_scope.h"
#include "AllocatorInternals.h"

static void* firstaddress = 0;

//#define DEBUG_PA
#define DOUTST(msgstr) sys::coreout << msgstr

#ifdef DEBUG_PA
#define DOUT(msgstr) sys::coreout << msgstr
#else //DEBUG_PA
#define DOUT(msgstr) do { } while(false)
#endif //DEBUG_PA

int hash(void* ptr, int mod)
{
  if (firstaddress == 0)
    firstaddress = ptr;
  
  const char* cptr = (const char*)ptr;
  int erg = 0;
  for (int i = 3; i >= 0; i--) {
    erg = (64 * erg + cptr[i]) % mod;
  }
  return erg;
#ifdef ACDK_BIGENDIAN
  return int(ptr);
#else
  return int(ptr) << 8; 
#endif
  /*//return (int)ptr;
  const char* cptr = (const char*)ptr;
  erg = cptr[1] + (cptr[0] >> 
  int erg = cptr[1];
  erg = erg * 31 + cptr[0];
  erg = erg * 31 + cptr[3];
  return erg * 31 + cptr[2];
  */
}

int hash(const acdk::lang::RObject* ref, int tablesize) 
{ 
  return hash((void*)ref, tablesize); 
}

static const acdk::lang::Object* _lastAddress = 0;
static const acdk::lang::Object* _firstAddress = 0;
static float _addressFactor = 1.0;
int hash(const acdk::lang::Object* ref, int tablesize) 
{ 
  //return hash((void*)ref, tablesize);
  int offset = int(ref) - int(_firstAddress);
  int erg = (int)((float)offset * _addressFactor);
  if (erg > tablesize)
    return erg % tablesize;
  return erg;
  //return hash((void*)ref, tablesize); 
}


namespace acdk {
namespace lang {
namespace sys {


PagedHeap::PagedHeap(int pagesize/* = 1024*/, RHeapFrame top/* = 0*/, int flags, const char* name/* = ""*/) 
: HeapFrame(top, flags |  HeapHasRC | HeapHasGC | HeapTraceObjects, name)
, _allocator(new PagedAllocator(pagesize))
{
}



#define CHECK_PagedAllocatorPage(ap) if (ap == 0) break; if (ap->_magic != (short)PagedAllocatorPage::Magic) THROW1(Error, "Page is corrupt")
#define CHECK_PagedAllocatorPageSlot(ps) if (ps == 0) break; if (ps->_magic != (short)PagedAllocatorPageSlot::Magic) THROW1(Error, "PageSlot is corrupt")
#define CHECK_Object(o) if (o->magic() != _MagicObjectSig)  THROW1(Error, "*** Oops invalide Object Reference found")

/**
  for given Object* o the corresponding Object* p, which has o as member 
*/



ReferedMap* ReferedMap::_thisInstance = 0;

/** 
  A multimap, which holds for each Object o the RObject, which holds o 
class HolderMap
{
  core_flathashmap<Object*, ObjectRefPtrVector*> _map;

public:
  static HolderMap* _thisInstance;
  typedef core_flathashmap<Object*, ObjectRefPtrVector*>::iterator iterator;

  HolderMap(int mapsize)
  : _map(mapsize, false)
  {
    _thisInstance = this;
  }
  ~HolderMap()
  {
    _thisInstance = 0;
  }
  void add(RObject* reference)
  {
    add(reference->impl(), reference);
  }
  void add(Object* obj, RObject* reference)
  {
    ObjectRefPtrVector** orgvec = _map.get(obj);
    if (orgvec != 0) {
      (*orgvec)->push_back(reference);
    } else {
      ObjectRefPtrVector* _vec = new ObjectRefPtrVector(3);
      _vec->push_back(reference);
      _map.put(obj, _vec);
    }    
  }
  iterator begin() { return _map.begin(); }
  iterator end() { return _map.end(); }

  iterator find(Object* obj) { return _map.find(obj); }
  void erase(const iterator& it) { _map.erase(it); }
  void eraseValue(RObject* ref)
  {
    if (ref->impl() == 0 || _map.find(ref->impl()) == _map.end())
      return;
    iterator it = begin();
    iterator et = end();
    if (it != et && it.value() != 0) {
      ObjectRefPtrVector& orgvec = *it.value();
      for (int i = 0; i < orgvec.size(); i++) {
        if (orgvec[i] == ref)
          orgvec[i] = 0;
      }
    }
  }
  void dumpRef();
};

HolderMap* HolderMap::_thisInstance = 0;
*/





class PagedHeap_ObjectDestroyListener
: public acdk::lang::Object,
  implements acdk::lang::ref::NotifyObjectEventListener
{
  ReferedMap& _refmap;
public:
  PagedHeap_ObjectDestroyListener(ReferedMap& refmap)
  : _refmap(refmap)
  {
  }
  virtual void notifyBeforeConstruction(Object* obj) { }
  virtual bool notifyBeforeDestruction(Object* obj) 
  {
    ReferedMap::iterator it = _refmap.find(obj);
    if (it != _refmap.end())
      _refmap.erase(it);
    return true;
  }
  virtual void notifyWhileDestruction(Object* obj) { }
  virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) { return false; }
  virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, AllocatedType type) { return false; }
};

class PagedAllocatorObjectsIterator
: public AllocatorObjectsIterator
{
  PagedAllocator* _allocator;
  PagedAllocatorPage* _page;
  PagedAllocatorPageSlot* _slot;
  PagedAllocatorPageSlot* _currentSlot;
  bool _hasNext;
  /// see ListObjectsFlags
  int _searchFlags;
public:
  PagedAllocatorObjectsIterator(PagedAllocator* pagedAllocator, int searchFlags = ObjectMem)
  : _allocator(pagedAllocator)
  , _page(_allocator->_first)
  , _slot(_page->_first)
  , _currentSlot(0)
  , _hasNext(true)
  , _searchFlags(searchFlags)
  {
  }
  
  virtual void* getNext()
  {
   return seekNext();
  }
  virtual Object* getNextObject()
  {
    if (_searchFlags & ObjectMem)
    {
      if (seekNext() == 0)
       return 0;
      return _currentSlot->_getObject();
    }
    core_value_scope<int> sf(_searchFlags,  (_searchFlags & ~AllocatedTypeMask) | ObjectMem);
     if (seekNext() == 0)
       return 0;
    return _slot->_getObject();
  }
  void reset()
  {
    _page = _allocator->_first;
    _slot = _page->_first;
    _hasNext = true;
  }
  void _nextSlot()
  {
    bool checkNextPage = false;
    if (_slot->_next == _slot)
      checkNextPage = true;
    _slot = _slot->_next;
    if (checkNextPage == true || _slot == _page->_first || _slot == 0)
    {
      _page = _page->_next;
      if (_page == _allocator->_first || _page == 0)
      {
        _hasNext = false;
      }
      else
        _slot = _page->_first;
    }
  }

  void* seekNext()
  {
    if (_hasNext == false)
      return 0;
    do {
      if (_page == 0) 
        break; 
      if (_slot == 0) 
          break; 
      int sf = _searchFlags & AllocatedTypeMask;
      if (sf == 0 || (sf & _slot->_flags))
      {
        if (_slot->_flags & ObjectMem) 
        {
          Object* o = _slot->_getObject();
          DOUT("O: " << (void*)o << sys::eofl);
          if (o->magic() == _MagicObjectSig) 
          {
            _currentSlot = _slot;
            _nextSlot();
            return o;
          }
        }
        else
        {
          _currentSlot = _slot;
          _nextSlot();
          return _currentSlot->mem();
        }
      }
      _nextSlot();
    } while (_hasNext == true);
    //_hasNext = false;
    //_currentSlot = 0;
    return 0;
  }
  AllocatorElement getCurrentElement()
  {
    if (_currentSlot == 0)
      return AllocatorElement();
    if (_currentSlot->_flags & ObjectMem)
      return AllocatorElement(_currentSlot->_getObject(), _currentSlot->_size, _currentSlot->_size + ALIGNEDSIZEOF(PagedAllocatorPageSlot));

    return AllocatorElement(_currentSlot->mem(), _currentSlot->_flags, _currentSlot->_size, _currentSlot->_size + ALIGNEDSIZEOF(PagedAllocatorPageSlot));
  }
};



//virtual 
bool 
PagedHeap::gc(bool recursiv/* = true */)
{
  PAGELOCK();
  PagedAllocatorObjectsIterator pait(_allocator, ObjectMem);
  _allocator->_heap = this;
  bool bret =  doGc(_allocator, &pait, recursiv);
  _allocator->_heap = 0;
  return bret;
}


//virtual 
bool 
PagedHeap::onDestroy(Object* obj)
{
  if (ReferedMap::_thisInstance == 0) 
    return true;
  //DOUT("GC: ~obj: " << (void*) obj << sys::eofl);
  ReferedMap::iterator it = ReferedMap::_thisInstance->find(obj);
  if (it != ReferedMap::_thisInstance->end())
    ReferedMap::_thisInstance->erase(it);
  else
    DOUT("~obj not found: " << (void*) obj << sys::eofl);
  return true;
  /** really needed??
  int colfields = const_cast<Object*>(obj)->getClazzInfo()->_collectableFieldsCount();
  if (colfields == 0) 
    return true;
  
  FieldReferences fields;
  const_cast<Object*>(obj)->getCollectableFields(fields);
  for (int i = 0; i < fields.size(); i++) {
    if (*fields[i] != Nil) {
      ReferedMap::_thisInstance->eraseValue(fields[i]);
    }
  }
  return true;
  */
}






//virtual 
void 
PagedHeap::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  if (flags & ListRootsOnly)
    flags |= ObjectMem;
  PAGELOCK();
  
  PagedAllocatorObjectsIterator pait(_allocator, flags);
  genericListObject(this, &pait, listener, flags);
  /*
  Object* o = 0;
  if (flags & ListRootsOnly)
  {
    PAGELOCK();
    markMemberObjects(&pait);
  }
  pait.reset();
  while (pait.getNext() != 0)
  {
    AllocatorElement el = pait.getCurrentElement();
    if (el.type == ObjectMem)
      o = el.mem.obj;
    else
      o = 0;
    if ((flags & ListRootsOnly) == 0 ||  (o != 0 && o->_isObjectRefFlag(ObjectBase::Marked) == false))
    {
      if (listener->listedAllocated(this, el.mem.buffer, (AllocatedType)(AllocatedTypeMask & el.type), el.size) == false)
        return;
    }
  }
  */
  if ((flags & ListObjectsRecursive) == 0)
    return;
  if (_top == 0)
    return;
  _top->listObjects(listener, flags);

}



} // namespace sys
} // namespace lang 
} // namespace acdk 


