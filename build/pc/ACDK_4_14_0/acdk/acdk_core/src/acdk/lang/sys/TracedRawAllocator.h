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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/TracedRawAllocator.h,v 1.15 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_TracedRawAllocator_h
#define acdk_lang_sys_TracedRawAllocator_h

#include "Allocator.h"
#include "AllocatorInternals.h"

#include <map>

namespace acdk {
namespace lang {
namespace sys {


using namespace acdk::lang;

ACDK_DECL_SYS_CLASS(TracedRawAllocator);

foreign class ACDK_CORE_PUBLIC TracedRawAllocatorInfo
{
public:
  unsigned int flags : 3; // is AllocatedType
  unsigned int size : 29; 
  TracedRawAllocatorInfo(AllocatedType at, int s)
    : flags(at)
    , size(s)
  {
  }
};

class ACDK_CORE_PUBLIC TracedRawAllocator
: public AbstractAllocator
{
protected:
public:
  typedef std::map<void*, TracedRawAllocatorInfo> ObjectsSet;
  ObjectsSet _heap;

  TracedRawAllocator()
   : AbstractAllocator(SupportListObjectAllocatorType, core_string("TracedRawAllocator", false))
  {
  }
  virtual void* raw_allocate(size_t size, AllocatedType type) { return os_alloc(size); }
  virtual void raw_deallocate(size_t size, void* ptr, AllocatedType type) { os_dealloc(size, ptr); }

  virtual void* allocate(size_t size, AllocatedType at = RawMem);
  virtual void deallocate(void* ptr, AllocatedType at = RawMem);
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags);

};


foreign 
class TracedRawAllocatorIterator
: public AllocatorObjectsIterator
{
public:
  TracedRawAllocator::ObjectsSet::iterator _begin;
  TracedRawAllocator::ObjectsSet::iterator _end;
  TracedRawAllocator::ObjectsSet::iterator _it;
  TracedRawAllocator::ObjectsSet::iterator _curIt;
  TracedRawAllocatorIterator(TracedRawAllocator* allocator)
    : _begin(allocator->_heap.begin())
    , _end(allocator->_heap.end())
    , _it(allocator->_heap.begin())
  {
  }
  virtual void* getNext() 
  {
    if (_it == _end)
      return 0;
    _curIt = _it;
    ++_it;
    return (*_curIt).first;
  }
  virtual Object* getNextObject()
  {
    while (_it != _end)
    {
      if ((*_it).second.flags & ObjectMem)
      {
        _curIt = _it;
        ++_it;

        return (Object*)(*_curIt).first;
      }
      ++_it;
    }
    return 0;
  }
  virtual AllocatorElement getCurrentElement()
  {
    return AllocatorElement((*_curIt).first, (*_curIt).second.flags, (*_curIt).second.size, (*_curIt).second.size + sizeof(*_curIt));
  }
  virtual void reset()
  {
    _it = _begin;
  }
};

} // namespace sys 
} // namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_TracedRawAllocator_h
