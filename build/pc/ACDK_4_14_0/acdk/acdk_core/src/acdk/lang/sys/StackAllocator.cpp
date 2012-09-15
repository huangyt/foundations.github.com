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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/StackAllocator.cpp,v 1.16 2005/03/07 13:47:28 kommer Exp $


#include <acdk.h>
#include "StackAllocator.h"
#include "core_specific.h"
#include "assert.h"
#include <acdk/lang/ref/NotifyObjectEvent.h>

namespace acdk {
namespace lang {
namespace sys {

#ifdef LOCAL_DEBUG
#define DOUT(msg) \
  do { sys::coreout << msg << sys::eofl; } while (false)
#else
#define DOUT(msg) 
#endif

/**
  stack -> ObjectSlot[]
  slot[] -> _prevObject=0 _nextObject=&_prevObject[+1] [data] 
            _prevObject=&_prevObject[-1] _nextObject [data] _prevObject=_prevObject[-1] _nextObject=0
*/
struct Stack
{
  enum 
  {
    EntryOverhead = sizeof(char*)  + sizeof(int)
  };
  struct ObjectSlot
  {
    ObjectSlot* prev;
    ObjectSlot* next;
    int type;
    char* data() { return ((char*)this + sizeof(ObjectSlot)); }
    int data_size() { return next == 0 ? 0 : ((char*)next - (char*)this) - sizeof(ObjectSlot); }
    Object* object() { return (Object*)Allocator::getObjectFromChunk(((char*)this + sizeof(ObjectSlot))); }
  };
  char* _data;
  ObjectSlot* _offset;// points to ObjectSlot
  int _size;
  Stack* _prev;
  int _standardPageSize;
  Stack(int size, int stdPageSize, Stack* prev = 0)
  : _data(0)
  , _offset(0)
  , _size(size)
  , _prev(prev)
  , _standardPageSize(stdPageSize)
  {
    _data = ((char*)this) + sizeof(Stack);
    _offset = (ObjectSlot*)_data;
    _offset->prev  = 0;
    _offset->next  = 0;
  }
  ~Stack()
  {
    //operator delete(_data);
  }
  char* sp() { return (char*)_offset; }
  Stack* alloc(int size, AllocatedType type, void*& mem)
  {
    size = aligned(size);
    
    if (_offset->data() + size  + sizeof(ObjectSlot) < _data + _size) 
    {
      ObjectSlot* slot = _offset;
      slot->next = (ObjectSlot*)((char*)_offset + sizeof(ObjectSlot) + size);
      slot->next->prev = slot;
      slot->next->next = 0;
      slot->type = (int)type;
      mem = slot->data();
      _offset = slot->next;
      
      return this;
    }
    int overhead = sizeof(Stack) + sizeof(ObjectSlot) * 2;
    int newsize = _standardPageSize < size + overhead ? size + overhead : _standardPageSize;
    void* new_mem = new char[newsize];
    Stack* ret = new (new_mem) Stack(newsize - sizeof(Stack), _standardPageSize, this);
    return ret->alloc(size, type, mem);
  }
  void free(void *ptr)
  {
    if (ptr > _data && ptr < (void*)_offset)
    {
      ObjectSlot* slot = (ObjectSlot*)((char*)ptr - sizeof(ObjectSlot));
      slot->type = 0;
      //slot = (ObjectSlot*)((char*)ptr);
      return;
    }
    if (_prev != 0)
      _prev->free(ptr);
  }
  void callObjectDestructors(void* ptr)
  {
    ObjectSlot* slot = _offset;
    while (slot != 0 && slot->data() > ptr)
    {
      if (AllocatedType(slot->type) == ObjectMem)
      {
        Object* obj = slot->object();
        if (obj->magic() == _MagicObjectSig)
        {
          DOUT("StackAlloc free: " << (void*)slot->object() << "; size=" << slot->data_size() << "; type=" << (int)slot->type);
          slot->object()->~Object();
        }
      }
      slot = slot->prev;
    }
  }
  Stack* resetStack(void *ptr, bool callDestructors = true)
  {
    if (_offset == 0 || ptr < _data) 
    {
      Stack* prevstack = _prev;
      if (prevstack != 0)
      {
        if (callDestructors == true)
          callObjectDestructors(_data);
        delete this;
        return prevstack->resetStack(ptr, callDestructors);
      }
      return this;
    }
    if (callDestructors == true)
      callObjectDestructors(ptr);
    _offset = (ObjectSlot*)ptr;
    _offset->next = 0;
    return this;
  }
  bool listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
  {
    ObjectSlot* slot = _offset->prev;
    while (slot != 0)
    {
      if (listener->listedAllocated(0, slot->object(), AllocatedType(slot->type), slot->data_size()) == false)
        return false;
      slot = slot->prev;
    }
    return true;
  }
};

class StackAllocatorImpl
: public AbstractAllocator
{
public:
  Stack* _root;
  Stack* _current;
  int _standardPageSize;
  StackAllocatorImpl()
  : AbstractAllocator(StackObjectAllocatorType | NoSyncAllocatorType | NoRefCountAllocatorType, "StackAllocator")
  , _root(0)
  , _current(0)
  , _standardPageSize(StandardAllocPageSize)
  {
    int nsize = _standardPageSize + sizeof(Stack);
    void* p = new char[nsize];
    _current = _root = new (p) Stack(_standardPageSize, _standardPageSize);
  }
  ~StackAllocatorImpl()
  {
    Stack* n = _current;
    while (n != 0)
    {
      Stack* ns = n->_prev;
      char* tc = (char*)n;
      delete[] tc;
      n = ns;
    }
  }
  void* curTop() { return _current->sp(); }
  void* alloc(int size, AllocatedType type)
  {
    void* mem;
    _current = _current->alloc(size, type, mem);
    DOUT("StackAlloc: " << getObjectFromChunk(mem) << "; size=" << size << "; type=" << (int)type);
    return mem;
  }
  void free(void* ptr)
  {
    //_current = _current->free(ptr);
  }
  void resetStack(void* ptr, bool callDestructors = true)
  {
    _current = _current->resetStack(ptr);
  }
  bool isEmpty() const
  {
    return _root == _current 
      && _root->_offset == 0;
  }
  virtual void lock()  { }
  virtual void unlock() { }
  
  virtual void* raw_allocate(size_t size, AllocatedType type) { return alloc(size, RawMem); }
  virtual void raw_deallocate(size_t size, void* ptr, AllocatedType type) 
  { 
    _current->free(ptr);
    /* free(ptr); */ 
  }
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags)
  {
    Stack* s = _current;
    while (s != 0)
    {
      if (s->listObjects(listener, flags) == false)
        return;
      s = s->_prev;
    }
  }
};
  


static specific<StackAllocatorImpl> _tlsStack;

StackAllocatorImpl* getStack()
{
  
  return &_tlsStack.get();
  /*
  static StackAllocatorImpl* _globalStack = 0;
  if (_globalStack == 0)
    _globalStack = new StackAllocatorImpl();
  return _globalStack;*/
}

::acdk::lang::sys::Allocator* 
#if !defined(_MSC_VER)
ACDK_CORE_PUBLIC 
#endif
getStackAllocator()
{
  return getStack();
}


} // namespace sys
} // namespace lang
} // namespace acdk


StackAllocScope::StackAllocScope(int reseverve)
: _allocator(acdk::lang::sys::getStack())
, _savedTop(_allocator->curTop())
{
  _allocator->_standardPageSize = reseverve;
}

StackAllocScope::~StackAllocScope()
{
  _allocator->resetStack(_savedTop);
}
StackAllocScope::operator ::acdk::lang::sys::Allocator*() 
{ 
  return _allocator; 
}


