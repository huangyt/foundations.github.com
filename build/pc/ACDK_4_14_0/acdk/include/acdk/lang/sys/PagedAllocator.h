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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/PagedAllocator.h,v 1.23 2005/03/14 12:13:44 kommer Exp $

#ifndef acdk_lang_sys_PagedAllocator_h
#define acdk_lang_sys_PagedAllocator_h

#include "core_mutex.h"
#include "core_fastmutex.h"
#include "core_sys_static_mutex.h"

#include "ObjectLockPool.h"
#include "SysRefHolder.h"
#include "../Thread.h"

namespace acdk {
namespace lang {
namespace sys {

  /*
enum PagedAllocatorMemType
{
  MemType_Undefined = 0, 
  MemType_Page = 1,
  MemType_Raw = 2,
  MemType_Object = 4,
  MemType_BasicType = 6
};
*/
class PagedAllocatorPage;
class PagedAllocator;

typedef short PagedAllocatorPageSlotFlags;
#if defined(ACDK_OS_WIN32)
typedef core_fastmutex PageMutex;
#else
typedef core_mutex PageMutex;
#endif

PageMutex& _pageLock();
//#define PAGELOCK() TLockGuard<PageMutex> lockthis(_pageLock())
#define PAGELOCK() SysStaticMutexLockGuard<PageMutex> lockthis(Thread::isSingleThreaded() == false, _pageLock())

//#define PAGEUNLOCK() core_unlock_guard<PageMutex> unlockthis(_pageLock())
#define PAGEUNLOCK() SysStaticMutexUnLockGuard<PageMutex> unlockthis(Thread::isSingleThreaded() == false, _pageLock())

// In diesem Makro wird eine Speicherschutzverletzung provoziert, um
// ein Debugging via Backtrace zu ermoeglichen.
#define THROW_INTERNAL(msg) \
 do { \
  sys::coreout << "Internal Error: " << msg << "[" << __FILE__ << ":" << __LINE__ << "]" << sys::eofl; \
  char *forceSegmentationFaultPtr = 0; \
  *forceSegmentationFaultPtr = 0; \
} while (false)


const int _noMenLandSize = 8;
const char _noMenLandMask = 0xee;
  

foreign class ACDK_CORE_PUBLIC PagedAllocatorPageSlot
{
public:
  enum {
    Magic = 0xfedc
  };
  short _magic;
  short _flags;
  // brutto size 
  int _size;
  PagedAllocatorPage* _startPage;
  PagedAllocatorPageSlot* _prev;
  PagedAllocatorPageSlot* _next;
  PagedAllocatorPageSlot(PagedAllocatorPage* page, int size, PagedAllocatorPageSlotFlags flags, 
                         PagedAllocatorPageSlot* prev = 0,  PagedAllocatorPageSlot* next = 0);
  inline void* _allocate() 
  { 
    return (char*)this + ALIGNEDSIZEOF(PagedAllocatorPageSlot); 
  }
  void _deallocatePage();
  void _deallocate();
  inline void* mem()
  {
    return ((char*)this) + ALIGNEDSIZEOF(PagedAllocatorPageSlot);
  }
  inline int getUserMemSize() const;

  inline acdk::lang::Object* _getObject()
  {
    return (Object*)Allocator::getObjectFromChunk(mem());
  } 
  inline bool _isObject() const { return _flags & ObjectMem; }
  inline void _checkMagic(bool checkall = true)
  {
    if (_magic != (short)Magic) 
      THROW_INTERNAL("PagedAllocatorPageSlot is insane");

    if (checkall == false)
      return;
    /*if (_next != 0 && _next != this)
      _next->_checkMagic();
    if (_prev != 0 && _prev != this)
      _prev->_checkMagic();
      */
  }
  /** return the netto size overhead of one slot including no mans land */
  inline static int getPagedAllocatorPageSlotSize()
  {
#if defined(ACDK_DEBUG)
    return ALIGNEDSIZEOF(PagedAllocatorPageSlot) + _noMenLandSize;
#else
    return ALIGNEDSIZEOF(PagedAllocatorPageSlot);
#endif
  }
  inline char* getNoMenLand() const
  {
    return ((char*)this) + _size - _noMenLandSize;
  }
  void checkNoMenLand() const;
};


foreign  class ACDK_CORE_PUBLIC  PagedAllocatorPage
{
public:
  enum {
    Magic = 0xdcba
  };
  short _magic;
  enum Flags
  {
    /** Standard page with page slots */
    Standard = 0,
    Raw = 1
  };
  short _flags;
  PagedAllocator* _allocator;
  /* brutto size of this page */
  int _size;
  int _sync;
  PagedAllocatorPage* _prev;
  PagedAllocatorPage* _next;
  PagedAllocatorPageSlot* _first;
  PagedAllocatorPageSlot* _last;
  
  PagedAllocatorPage(PagedAllocator* allocator, int size, short flags, PagedAllocatorPage* prev = 0, PagedAllocatorPage* next = 0)
  : _magic((short)Magic),
    _flags(flags),
    _allocator(allocator),
    _size(size),
    _prev(prev),
    _next(next),
    _first(0),
    _last(0)
  {
    if (_prev != 0)
      _prev->_next = this;
    if (_next != 0)
      _next->_prev = this;
  }
  int getFreeCapacity()
  {
    PAGELOCK();
    return _getFreeCapacity();
  }
  int _getFreeCapacity()
  {
    if (_last == 0)
      return _size - ALIGNEDSIZEOF(PagedAllocatorPage);
    char *topptr = (char*)this + _size;
    char *ltop = (char*)_last + _last->_size;
    int freec = topptr - ltop;
    return freec;
  }
  bool hasCapacity(int size)
  {
    return getFreeCapacity() > size;
  }
  bool _hasCapacity(int size)
  {
    return _getFreeCapacity() > size;
  }
  void* allocate(int size, short flags)
  {
    PAGELOCK();
    return _allocate(size, flags);
  }
  void* _allocate(int size, short flags)
  {
    _checkMagic();
    if (_last == 0) {
      PagedAllocatorPageSlot* slot = new ((void*)((char*)this + ALIGNEDSIZEOF(PagedAllocatorPage))) PagedAllocatorPageSlot(this, size, flags, _last, _first);
      _last = _first = slot;
      return slot->_allocate();
    }
    _last->_checkMagic();
    PagedAllocatorPageSlot* slot = new ((char*)_last + _last->_size) PagedAllocatorPageSlot(this, size, flags, _last, _first);
    _last = slot;
    return slot->_allocate();
  }
  void _deallocatePage();
  
  void _deallocateSlot(PagedAllocatorPageSlot* ps)
  {
    if (ps == _first)
      _first = _first->_next;
    if (ps == _last)
      _last = _last->_prev;
  }
  void _checkMagic() 
  {  
    if (_magic != (short)Magic)
      THROW_INTERNAL("PagedAllocatorPage is corrupt");
    /*if (_prev != 0 && _prev != this)
      _prev->_checkMagic();
    if (_next != 0 && _next != this)
      _next->_checkMagic();
      */
    if (_first != 0)
      _first->_checkMagic();
    if (_last != 0)
      _last->_checkMagic();
  }
};

inline 
void 
PagedAllocatorPageSlot::_deallocatePage()
{
  PagedAllocatorPage* sp = _startPage;
  //memset(this, 0xd, _size);
  sp->_deallocatePage();
  
}

class PagedHeap;

ACDK_DECL_SYS_CLASS(PagedAllocator);

class PagedAllocatorObjectsIterator;

class ACDK_CORE_PUBLIC  PagedAllocator
:  public AbstractAllocator
{
protected:
  friend class PagedHeap;
  int _standardPageSize;
  PagedAllocatorPage* _first;
  PagedAllocatorPage* _last;
  int _reservedPages;
  core_vector<PagedAllocatorPage*> _bufferedPages;
  int _maxBufferedPages;
  /**
    if _heap != 0, Allocator should inform
    on changes (destroyObject) */
  PagedHeap* _heap;
public: 
  PagedAllocator(int size, int bufferedPages = 100);
  virtual ~PagedAllocator();
// Allocator
  virtual void* allocate(size_t size, AllocatedType at = RawMem);
  virtual void deallocate(void* ptr, AllocatedType at = RawMem);
  virtual void* raw_allocate(size_t size, AllocatedType type);
  virtual void raw_deallocate(size_t size, void* ptr, AllocatedType type);
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
  
  PagedAllocatorPage* _allocatePage(int size = 0);
  
  void _deallocatePage(PagedAllocatorPage* page);
  
  void* allocate(int size, short type);
  
  
  /** 
    dump statistics of Alloctor on cout 
    @arg withDetails dumps with details on heap memory objects
    @arg memberlevel if memory object is Object, dump information about member recursivly * memberlevel
  */
  void dumpStatistics(bool withDetails = false, int memberlevel = 0, bool onlyumarked = false);
  
  /** 
      count of elements in the pages 
  */
  int getElementCount(AllocatedType type) const;
  /**
     return number of bytes allocated by given type
  */
  int getAllocatedCount(AllocatedType type) const;
  /**
    try to free memory.
    @return true if any memory was freed
    @param force if true free also internal cached elements
           for PageAllocator if not true always return false
  */
  bool doGc(bool threadStorage = true, bool force = false);
  friend class PagedAllocatorObjectsIterator;
protected:
  void* _allocate(int size, short type);
  void* _allocateObject(int size);
  void _deallocateObject(void* ptr);
  PagedAllocatorPage* _allocatePage2(int size);
  void _deallocatePage2(PagedAllocatorPage* page);
  

};

inline 
int 
PagedAllocatorPageSlot::getUserMemSize() const
{
  int overhead = Allocator::getHeaderSize() + getPagedAllocatorPageSlotSize();
  return _size - overhead;
}

inline
void 
PagedAllocatorPage::_deallocatePage()
{
  _allocator->_deallocatePage(this);  
}

} // namespace sys
} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_Allocator_h

