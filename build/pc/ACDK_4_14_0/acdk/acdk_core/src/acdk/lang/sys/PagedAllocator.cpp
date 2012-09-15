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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/PagedAllocator.cpp,v 1.36 2005/04/28 15:00:05 kommer Exp $


#include <acdk.h>

#include "PagedAllocator.h"
#include "PagedHeap.h"
#include <acdk/lang/sys/core_vector.h>
#include <acdk/lang/sys/core_hashmap.h>
#include <acdk/lang/OutOfMemoryError.h>

#include <acdk/lang/ref/NotifyObjectEvent.h>

#include "core_memtrace.h"


namespace acdk {
namespace lang {
namespace sys {

using namespace acdk::lang;

// don't memset in debug mode
#define ACDK_NOMEMINITIALIZE

//#define DEBUG_ALLOC

#ifdef DEBUG_ALLOC

#define DOUT(m) \
sys::coreout << m << sys::eofl

#else
#define DOUT(m)
#endif

#undef THROW_INTERNAL
#define THROW_INTERNAL(msg) \
do {\
sys::coreout << msg << sys::eofl; \
char* nptr = 0; \
  *nptr = 0; \
} while (false)

PageMutex& _pageLock()
{
  static PageMutex _pageLock;
  return _pageLock;
}

PagedAllocator::PagedAllocator(int size, int bufferedPages)
: AbstractAllocator(ManualGcAllocatorType | SupportListObjectAllocatorType, core_string("PagedAllocator", false))
, _standardPageSize(aligned(size))
, _first(0)
, _last(0)
, _reservedPages(0)
, _bufferedPages(0, bufferedPages, 0)
, _maxBufferedPages(bufferedPages)
, _heap(0)
{
  DOUT("PagedAllocator::PagedAllocator(" << (void*)this << ")");
  _first = _last = _allocatePage(_standardPageSize);
  _first->_prev = _first;
  _first->_next = _first;
  //_maxBufferedPages = 0;
//  _bufferedPages.ensureCapacity(_maxBufferedPages);
}

PagedAllocator::~PagedAllocator()
{
  DOUT("PagedAllocator::~PagedAllocator(" << (void*)this << ")");
  doGc(true, true);

}



RawAllocator* _rawAllocator();




bool
PagedAllocator::doGc(bool threadStorage, bool force)
{
  if (force == false)
    return false;
  bool ret = false;
  while (_bufferedPages.empty() == false)
  {
    PagedAllocatorPage* page = _bufferedPages.back();
    _bufferedPages.pop_back();
    os_dealloc(page->_size, page);
    ret = true;
  }
  if (ret == true)
    return true;
  {
    PAGEUNLOCK();
    return ObjectHeap::gc(true);
  }
}





void*
PagedAllocator::allocate(int size, short type)
{
  PAGELOCK();
  return _allocate(size, type);
}

void*
PagedAllocator::_allocate(int size, short type)
{
  DOUT("PagedAllocator::_allocate(" << (void*)this  << "; size=" << size << "; type=" << type << ")");
  size = aligned(size);
  if (_last->_hasCapacity(size + PagedAllocatorPageSlot::getPagedAllocatorPageSlotSize()) == true)
    return _last->_allocate(size, type);

  int npagesize = size + ALIGNEDSIZEOF(PagedAllocatorPage) + PagedAllocatorPageSlot::getPagedAllocatorPageSlotSize();
  if (npagesize < _standardPageSize)
    npagesize = _standardPageSize;
  PagedAllocatorPage* np = _allocatePage(npagesize);
  _last = np;
  return _last->_allocate(size, type);


}

//virtual
void*
PagedAllocator::allocate(size_t size, AllocatedType at)
{
  PAGELOCK();
  addRef();
  ++_allocInfo.memTypes[at].count;
  _allocInfo.memTypes[at].size += size;
  void *ret = _allocate(aligned(size) + getHeaderSize(), at);
  MemChunkHeader* ch = getHeaderFromChunk(ret);
  ch->allocator = this;
  ch->chunkSize = aligned(size) + getHeaderSize();
  void* retptr = getObjectFromChunk(ret);
  initObjectPtr(retptr, at);
  return core_memtrace::OnAlloc(retptr, size, 3, at);
}



PagedAllocatorPage*
PagedAllocator::_allocatePage2(int size)
{
  DOUT("PagedAllocator::_allocPage2(" << (void*)this << "): new page: " << size);
  for (core_stack<PagedAllocatorPage*>::iterator it = _bufferedPages.begin();
       it != _bufferedPages.end();
       ++it)
  {
    if ((*it)->_size >= size)
    {
      PagedAllocatorPage* pap = *it;
      int psize = pap->_size;
      _bufferedPages.erase(it);
      DOUT("reuse page: " << (void*) pap << ": " << psize << "; req: " << size);
      return new (pap) PagedAllocatorPage(this, psize, 0, _last, _first);
    }
  }
  return new (os_alloc(size)) PagedAllocatorPage(this, size, 0, _last, _first);
}

PagedAllocatorPage*
PagedAllocator::_allocatePage(int size)
{
  if (size == 0)
    size = _standardPageSize;
  return _allocatePage2(aligned(size));
}


void
PagedAllocator::_deallocatePage2(PagedAllocatorPage* page)
{
  DOUT("PagedAllocator::_deallocPage2(" << (void*)this << ")");

  if (_bufferedPages.size() >= _maxBufferedPages)
  {
    PagedAllocatorPage* page_to_free = page;
    for (core_stack<PagedAllocatorPage*>::iterator it = _bufferedPages.begin();
       it != _bufferedPages.end();
       ++it)
    {
      if ((*it)->_size < page_to_free->_size)
      {
        PagedAllocatorPage* sw = *it;
        *it = page_to_free;
        page_to_free = sw;
      }
    }
    os_dealloc(page_to_free->_size, page_to_free);
    return;
  }
  DOUT("PagedAllocator::_deallocPage2(" << (void*)this << "): buffer page: " << (void*)page << ": " << page->_size);
  _bufferedPages.push_back(page);
}

void
PagedAllocator::_deallocatePage(PagedAllocatorPage* page)
{
  if (page->_magic != (short)PagedAllocatorPage::Magic)
    THROW_INTERNAL("Corrupt Allocation state");
  page->_first = 0;
  page->_last = 0;

  page->_prev->_next = page->_next;
  page->_next->_prev = page->_prev;
  if (page == _first && page == _last) // leave at least 1 Page
    return;
  if (_last == page)
    _last = page->_prev;
  if (_first == page)
    _first = page->_next;

  _deallocatePage2(page);
}


//virtual
void
PagedAllocator::deallocate(void* ptr, AllocatedType at)
{
  PAGELOCK();
  DOUT("PagedAllocator::deallocate(" << (void*)this << ", " << ptr << ")");
  core_memtrace::OnFree(ptr, 4);
  --_allocInfo.memTypes[at].count;

  //char* o = (char*)ptr;
  if (_heap != 0 && at == ObjectMem)
    _heap->onDestroy((Object*)ptr);
  MemChunkHeader* ch = getHeaderFromObject(ptr);
  ptr = getChunkFromObject(ptr);

  PagedAllocatorPageSlot* ps = (PagedAllocatorPageSlot*) ((char*)ptr - ALIGNEDSIZEOF(PagedAllocatorPageSlot));
  if (ps->_magic != (short)PagedAllocatorPageSlot::Magic)
  {
    THROW_INTERNAL("PagedAllocator::deallocate() PagedAllocatorPageSlot is currupt");
    return ;
  }
  _allocInfo.memTypes[at].size -= ps->getUserMemSize();
  ps->_deallocate();
  releaseRef();
}

void*
PagedAllocator::_allocateObject(int size)
{
  return _allocate(size, ObjectMem);
}



void
PagedAllocator::_deallocateObject(void* ptr)
{
  PagedAllocatorPageSlot* ps = (PagedAllocatorPageSlot*) ((char*)ptr - ALIGNEDSIZEOF(PagedAllocatorPageSlot));
  if (ps->_magic != (short)PagedAllocatorPageSlot::Magic) {
    THROW_INTERNAL("PagedAllocator PagedAllocatorPageSlot is corrupt");
  }
  ps->_deallocate();

}


PagedAllocatorPageSlot::PagedAllocatorPageSlot(PagedAllocatorPage* page, int size, PagedAllocatorPageSlotFlags flags,
                                               PagedAllocatorPageSlot* prev,  PagedAllocatorPageSlot* next)
: _magic((short)Magic)
, _flags(flags)
, _size(size + getPagedAllocatorPageSlotSize())
, _startPage(page)
, _prev(prev)
, _next(next)
{
  if (_prev != 0)
    _prev->_next = this;
  if (_next != 0)
    _next->_prev = this;

#if defined(ACDK_DEBUG) && !defined(ACDK_NOMEMINITIALIZE)
  char* nml = getNoMenLand();
  Object* obj = const_cast<PagedAllocatorPageSlot*>(this)->_getObject();
  int ums = getUserMemSize();
  memset(nml, _noMenLandMask, _noMenLandSize);
    //checkNoMenLand();
#endif
}


void
PagedAllocatorPageSlot::checkNoMenLand() const
{
  char* ptr = getNoMenLand();
  Object* obj = const_cast<PagedAllocatorPageSlot*>(this)->_getObject();

  for (int i = 0; i < _noMenLandSize; ++i)
  {
    if (ptr[i] != _noMenLandMask)
    {
      sys::coreout << "bounds read PAPS: Page: " << (void*)this << ", size: " << _size
                <<  "; Object: " << (void*)obj << ", size: " << PagedAllocatorPageSlot::getUserMemSize() << sys::eofl;
      return;
      //THROW_INTERNAL("PagedAllocator PagedAllocatorPageSlot was corrupted by user object (bounds read)");
    }
  }
  /*sys::coreout << "ok Page: " << (void*)this << ", size: " << _size
                <<  "; Object: " << (void*)obj << ", size: " << PagedAllocatorPageSlot::getUserMemSize() << sys::eofl;*/
}

void
PagedAllocatorPageSlot::_deallocate()
{
#if defined(ACDK_DEBUG) && !defined(ACDK_NOMEMINITIALIZE)
  checkNoMenLand();
#endif
  //if (isObject() == true)
  //  sys::coreout << "PA-[" << (void*)getObject() << "]"  << sys::eofl;

  if (_next == 0 || _prev == this) {
    _deallocatePage();
    return;
  }
  _startPage->_deallocateSlot(this);
  _next->_prev = _prev;
  _prev->_next = _next;
  _magic = 0xdd;
#if defined(ACDK_DEBUG) && !defined(ACDK_NOMEMINITIALIZE)
  memset(((char*)this) +  ALIGNEDSIZEOF(PagedAllocatorPageSlot*), 0xdd, _size - ALIGNEDSIZEOF(PagedAllocatorPageSlot*));
#endif
  //memset(this, 0, _size);

}


//virtual
void*
PagedAllocator::raw_allocate(size_t size, AllocatedType type)
{
  THROW1(Error, "Misuse of PagedAllocator: raw_allocate() should never been called");
  return 0;
}

//virtual
void
PagedAllocator::raw_deallocate(size_t size, void* ptr, AllocatedType type)
{
  THROW1(Error, "Misuse of PagedAllocator: raw_allocate() should never been called");
}

sys::core_output& operator<<(sys::core_output& os, Object* obj)
{
  //os << "Object=[" << (void*)obj << "]; RefCount=[" << obj->refCount() << "]; ";
  os << "CLASS=[" << obj->getClazzInfo()->name << "]; ";
  return os;
}


void printindent(int level)
{
  while (level--)
    sys::coreout << "\t";
}


void
dumpMembers(Object* so, Object* o, int ident, int level)
{

  //int colfields = o->getClazzInfo()->getCollectableFieldsCount();
  //if (colfields == 0)
  //  return;
  FieldReferences fields;
  o->getCollectableFields(fields);
  for (int i = 0; i < fields.size(); i++)
  {
    if (fields[i]->impl() != 0)
    {
      Object* fo = fields[i]->impl();
      printindent(ident);
      if (so == o)
      {
        sys::coreout << "SelfRef: " << fo << sys::eofl;
        return;
      }
      else
        sys::coreout << fo << sys::eofl;
      if (level - 1 > 0)
        dumpMembers(so, fo, ident + 1, level - 1);
    }
  }
}

void
PagedAllocator::dumpStatistics(bool withDetails, int level, bool onlyumarked)
{
  PAGELOCK();
  PagedAllocatorPage* np = _first;
  int pagecount = 0;
  int slotcount = 0;
  int objcount = 0;
  int rawcount = 0;
  int allocatedMem = 0;
  do
  {
    if (np == 0)
      break;
    if (np->_magic != (short)PagedAllocatorPage::Magic)
    {
      sys::coreout << "*** Opps, invalide PagedAllocatorPage at " << (void*)np << sys::eofl;
      THROW_INTERNAL("Memory Error");
      break;
    }
    PagedAllocatorPageSlot* ps = np->_first;
    do
    {
      if (ps == 0)
        break;
      if (ps->_magic != (short)PagedAllocatorPageSlot::Magic)
      {
        sys::coreout << "*** Opps, invalide PagedAllocatorPageSlot at " << (void*)ps << sys::eofl;
        THROW_INTERNAL("Memory Error");
        break;
      }
      if (ps->_flags & ObjectMem)
      {
        ++objcount;
        Object* o = ps->_getObject();
        if (o->magic() != _MagicObjectSig)
        {
          ps = ps->_next;
          continue;
        }
        else
        {
          if (withDetails == true)
          {
            if (onlyumarked == false || o->_isObjectRefFlag(ObjectBase::Marked) == false)
            {
              sys::coreout << "P["  << np << "] S[" << ps << "] O[" << ps->mem()
                << "] size[" << ps->_size << "]; [" << o << "];"
                << "FM[" << o->_isObjectRefFlag(ObjectBase::Marked) << "]; FV[" << o->_isObjectRefFlag(ObjectBase::Visited) << "]"
                << sys::eofl;
              if (level > 0)
                dumpMembers(0, o, 1, level);
            }

          }
        }
      }
      else
      {
        ++rawcount;
        if (withDetails == true)
        {
          sys::coreout << "P["  << np << "] S[" << ps << "] R[" << ps->mem()
                << "] size[" << ps->_size << "]" << sys::eofl;
        }
      }

      ++slotcount;
      ps = ps->_next;

    } while (ps != np->_first);

    ++pagecount;
    allocatedMem += np->_size;
    np = np->_next;
  } while (np != _first);
#if 0
  sys::coreout << "PAGES=[" << pagecount << "]; ALLOC=[" << allocatedMem << "]; SLOTS=["
        << slotcount << "]; OBJ=[" << objcount << "]; RAW=[" << rawcount << "];" << sys::eofl;
#endif

}


int
PagedAllocator::getElementCount(AllocatedType type) const
{
  PagedAllocatorPage* np = _first;
  int elementcount = 0;
  do {
    if (np == 0)
      break;
    if (np->_magic != (short)PagedAllocatorPage::Magic) {
      sys::coreout << "*** Opps, invalide PagedAllocatorPage at " << (void*)np << sys::eofl;
      THROW_INTERNAL("Memory Error");
    }
    PagedAllocatorPageSlot* ps = np->_first;
    do {
      if (ps == 0)
        break;
      if (ps->_magic != (short)PagedAllocatorPageSlot::Magic)
      {
        sys::coreout << "*** Opps, invalide PagedAllocatorPageSlot at " << (void*)ps << sys::eofl;
        THROW_INTERNAL("Memory Error");
      }
      if ((type == UnspecifiedMem) || (type & ps->_flags))
        ++elementcount;
      ps = ps->_next;
    } while (ps != np->_first);
    np = np->_next;
  } while (np != _first);
  return elementcount;
}


int
PagedAllocator::getAllocatedCount(AllocatedType type) const
{
  PAGELOCK();
  PagedAllocatorPage* np = _first;
  int elementcount = 0;
  do {
    if (np == 0)
      break;
    if (np->_magic != (short)PagedAllocatorPage::Magic) {
      sys::coreout << "*** Opps, invalide PagedAllocatorPage at " << (void*)np << sys::eofl;
      THROW_INTERNAL("Memory Error");
    }
    PagedAllocatorPageSlot* ps = np->_first;
    do {
      if (ps == 0)
        break;
      if (ps->_magic != (short)PagedAllocatorPageSlot::Magic) {
        sys::coreout << "*** Opps, invalide PagedAllocatorPageSlot at " << (void*)ps << sys::eofl;
        THROW_INTERNAL("Memory Error");
      }
      if ((type == UnspecifiedMem) || (type & ps->_flags))
        elementcount += ps->_size;
      ps = ps->_next;
    } while (ps != np->_first);
    np = np->_next;
  } while (np != _first);
  return elementcount;
}

//virtual
void
PagedAllocator::listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
{
  PagedAllocatorPage* np = _first;
  int elementcount = 0;
  do {
    if (np == 0)
      break;
    if (np->_magic != (short)PagedAllocatorPage::Magic) {
      sys::coreout << "*** Opps, invalide PagedAllocatorPage at " << (void*)np << sys::eofl;
      THROW_INTERNAL("Memory Error");
    }
    PagedAllocatorPageSlot* ps = np->_first;
    do {
      if (ps == 0)
        break;
      if (ps->_magic != (short)PagedAllocatorPageSlot::Magic) {
        sys::coreout << "*** Opps, invalide PagedAllocatorPageSlot at " << (void*)ps << sys::eofl;
        THROW_INTERNAL("Memory Error");
      }

      if (listener->listedAllocated(0, ps->mem(), AllocatedType(ps->_flags), ps->_size) == false)
        return;

      ps = ps->_next;
    } while (ps != np->_first);
    np = np->_next;
  } while (np != _first);
}


} // namespace sys
} // namespace lang
} // namespace acdk


