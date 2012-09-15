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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/BitmapPagedAllocator.h,v 1.13 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_BitmapPagedAllocator_h
#define acdk_lang_sys_BitmapPagedAllocator_h

#include "core_mutex.h"
#include "core_fastmutex.h"
#include "ObjectLockPool.h"
#include "SysRefHolder.h"

namespace acdk {
namespace lang {
namespace sys {


  
ACDK_DECL_SYS_CLASS(BitmapPagedAllocator);

/*
Sizes of BitmapPage
32, 64, 128, 256, 512, 1024, 2048
*/

class BitmapPagedAllocator;

template <int SlotSize>
class ACDK_CORE_PUBLIC BitmapPage
{
  /** next page */
  
  BitmapPage<SlotSize>* _next;
  BitmapPagedAllocator* _allocator;
  int _bitmap;
  int _slotSize;
  char _data[32][SlotSize + sizeof(BitmapPage<SlotSize>*) /*+ sizeof(int)*/];
  //BitmapPageSlot<SlotSize> _slots[32];
  /*
    char data[]
    int idx;
    BitmapPage*   
  */
public:  
  BitmapPage(BitmapPagedAllocator* allocator, BitmapPage<SlotSize>* prev)
  : _next(0),
    _allocator(allocator),
    _slotSize(SlotSize)
  {
    if (prev)
      prev->_next = this;
    memset((void*)&_bitmap, 0, sizeof(_bitmap));
    memset(_data, 0, sizeof(_data));
    for (int i = 0; i < 32; i++) 
    {
      char* ptr = (char*)&_data[i][0];
      *((BitmapPage<SlotSize>**)ptr) = this;
    }
    //BitmapPage<SlotSize>** mp = (BitmapPage<SlotSize>**)&_data[1][0];
  }
  char* getFreeSlot()
  {
    int idx = getFreeIndex();
    if (idx == -1) 
    {
      if (_next == 0)
        _next = new BitmapPage<SlotSize>(_allocator, this);
      return _next->getFreeSlot();
    }
    int sls = SlotSize;
    // debuggin
    //BitmapPage<SlotSize>** mp = (BitmapPage<SlotSize>**)&_data[idx][0];
    return (char*)_data[idx] + ALIGNEDSIZEOF(BitmapPage<SlotSize>**);// + ALIGNEDSIZEOF(int);
  }
  int getFreeIndex()
  {
    if (_bitmap == -1) 
      return -1;
    int tbm = _bitmap;
    int offset = 0;
    unsigned short serg;
    if ((unsigned short)_bitmap != 0xFFFF) 
      serg = (unsigned short)_bitmap;
    else {
      serg = _bitmap >> 16;
      offset += 16;
    }
    unsigned char berg;
    if ((unsigned char)serg != 0xFF) 
      berg = (unsigned char)serg;
    else {
      berg = serg >> 8;
      offset += 8;
    }
    int nmask = 1;
    for (int i = 0; i < 8; i++) 
    {
      if (~berg & nmask) {
        offset += i;
        int mask = 1;
        mask <<= offset;
        _bitmap |= mask;
        return offset;
      }
      nmask <<= 1;
    }
    return -1;
  }
  bool releaseSlot(char* ptr)
  {
    int diff = (ptr - sizeof(BitmapPage<SlotSize>*) - (char*)&_data[0][0]);
    int idx = diff / (_slotSize + sizeof(BitmapPage<SlotSize>*));
    int nmask = 1;
    nmask <<= idx;
    _bitmap &= ~nmask;
    return _bitmap == 0;
  }
};

class ACDK_CORE_PUBLIC RawPage
{
private:
  BitmapPagedAllocator* _allocator;
  RawPage* _next;
  RawPage* _prev;
  char* _data;
public:
  RawPage(BitmapPagedAllocator* allocator, int size, RawPage* prev)
  : _allocator(allocator),
    _next(0),  
    _prev(prev),
    _data(0)
  {
    if (_prev)
      _prev->_next = this;
    _data = new char[size];
  }
  ~RawPage()
  {
    if (_data != 0)
      delete _data;
    _data = 0;
  }
  void flush()
  {
    if (_next != 0)
      _next->flush();
    delete this;
  }
  char* data() { return _data; }
};

foreign
class ACDK_CORE_PUBLIC  BitmapPagedAllocator
:  public AbstractAllocator
{
protected:
  
  BitmapPage<32>* _page32;
  BitmapPage<64>* _page64;
  BitmapPage<128>* _page128;
  BitmapPage<512>* _page512;
  BitmapPage<1024>* _page1024;
  BitmapPage<2048>* _page2048;
  RawPage* _rawPagesFirst;
  RawPage* _rawPagesLast;
public: 
  BitmapPagedAllocator()
    : AbstractAllocator(StandardAllocatorType, "BitmapPagedAllocator"),
      _rawPagesFirst(0),
      _rawPagesLast(0)
  {
    _page32 = new BitmapPage<32>(this, 0);
    _page64 = new BitmapPage<64>(this, 0);
    _page128 = new BitmapPage<128>(this, 0);
    _page512 = new BitmapPage<512>(this, 0);
    _page1024 = new BitmapPage<1024>(this, 0);
    _page2048 = new BitmapPage<2048>(this, 0);
  }
  virtual ~BitmapPagedAllocator();
// Allocator
  virtual void* allocate(size_t size, AllocatedType at = RawMem)
  {
    if (size < 512) {
      if (size < 64) {
        if (size < 32)
          return _page32->getFreeSlot();
        else
          return _page64->getFreeSlot();
      } else {
        if (size < 128)
          return _page128->getFreeSlot();
        else
          return _page512->getFreeSlot();
      }
    } else {
      if (size < 2048) {
        if (size < 1024)
          return _page1024->getFreeSlot();
        else
          return _page2048->getFreeSlot();
      } else {
        if (_rawPagesFirst == 0) {
          _rawPagesFirst = new RawPage(this, size, 0);
          _rawPagesLast = _rawPagesFirst;  
        } else {
          _rawPagesLast = new RawPage(this, size, _rawPagesLast);
        }
        return _rawPagesLast->data();
      }
    }
  }
  virtual void deallocate(void* ptr, AllocatedType at = RawMem)
  {
    void *realptr = ((char*)ptr) - ALIGNEDSIZEOF(::acdk::lang::sys::BitmapPage<1>**);
    ::acdk::lang::sys::BitmapPage<1>** bm = (::acdk::lang::sys::BitmapPage<1>**)realptr;
    (*bm)->releaseSlot((char*)ptr);
  }
  //virtual void* allocateObject(size_t size);
  //virtual void deallocateObject(void* ptr);

  virtual void* raw_allocate(size_t size, AllocatedType type)
  {
    return allocate(size);
  }
  virtual void raw_deallocate(size_t size, void* ptr, AllocatedType type)
  {
    deallocate(ptr);
  }
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
  
  
  
};


class ACDK_CORE_PUBLIC BitmapPagedHeap
:  public HeapFrame 
{
protected:
  RBitmapPagedAllocator _allocator;  
public:
  
  BitmapPagedHeap(int pagesize = 1024, RHeapFrame top = 0, int flags = HeapIsThread, const char* name = "");
  virtual bool onDestroy(Object* obj); 
  virtual bool gc(bool recursiv = true);
  virtual Allocator* allocator() { return _allocator.getImpl(); }
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);
};


} // namespace sys
} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_BitmapPagedAllocator_h

