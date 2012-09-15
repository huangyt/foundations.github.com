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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/HeapFrame.h,v 1.17 2005/03/07 14:02:12 kommer Exp $

#ifndef acdk_lang_sys_HeapFrame_h
#define acdk_lang_sys_HeapFrame_h

#include "../../Config.h"

#include "core_string.h"


namespace acdk {
  namespace lang {
    namespace ref {
      class NotifyObjectEventListener;
    }
  }
}



namespace acdk {
namespace lang {
namespace sys {


using namespace acdk::lang;

enum HeapInfo
{
  HeapIsUnknown     = 0,
  HeapIsGlobal    =  0x00000001,
  HeapIsStatic    =  0x00000002,
  HeapIsThread    =  0x00000004,
  HeapTraceObjects = 0x00000010,
  HeapHasRC        = 0x00000020,
  HeapHasGC        = 0x00000040,
  HeapIsConsGC     = 0x00000100
  
};

enum AllocatedType
{
  UnspecifiedMem = 0x0,
  RawMem = 0x1,
  ObjectMem = 0x2,
  InternalMem = 0x3,
  /** contains RObject[] */
  ObjectRefArrayMem = 0x4,
  
  /** CharBuffer, contains no pointer to other objects */
  DumpBufferMem = 0x5,
  /** don't garbage this memory */
  NoGcMem =       0x6,
  MaxAllocatedType = 0x5
};

/**
  flags which be used in HeapFrame::listObjects
*/
enum ListObjectsFlags
{
  /* 
    If a bit is in AllocatedTypeMask only list
    AllocatedType
  */
  AllocatedTypeMask     = 0x0F,
  /// list objects from all heaps
  ListObjectsRecursive  = 0x10,
  /// list only root elements
  ListRootsOnly =         0x20,
  /// list objects, which would be gc
  ListGcAble            = 0x40
};

ACDK_DECL_SYS_CLASS(HeapFrame);

/** 
  HeapFrame declares just an abstrakt interface
  Please refer to class ObjectHeap  for more detailed information
*/
class ACDK_CORE_PUBLIC HeapFrame 
: public SysObject
{
protected:
  RHeapFrame _top;
  core_string _name;
  int _flags;
  /// is really an acdk::lang::ThreadId
  int _threadId;
public:
  HeapFrame(RHeapFrame top = 0, int flags = HeapIsThread, const char* name = "");
  
  virtual ~HeapFrame();
  /**
    manually remove this HeapFrame object.

    return true, if this will be deleted
  */
  virtual bool dispose() { return false; }
  /**
    return the current Allocator for this HeapFrame 
  */
  virtual Allocator* allocator() = 0; //{ return StandardHeapObjectAllocator::getAllocator(); }

  virtual bool gc(bool recursiv = true) = 0;
  /**
    uses NotifyObjectEventListener::listedAllocated
    @param flags combination of ListObjectsFlags
  */
  virtual void listObjects(::acdk::lang::ref::NotifyObjectEventListener* listener, int flags) = 0;
   
  
  RHeapFrame up() { return _top; }
  const RHeapFrame up() const { return _top; }
  void setUpFrame(RHeapFrame up) { _top = Nil; }
  int flags() { return _flags; }
  const char* name() const { return _name.c_str(); }
  /** is really an acdk::lang::ThreadId */
  int threadId() { return _threadId; }
// implementation defined
  virtual bool _setInFieldToNil(const Object* obj, bool recursiv);

  
  friend class ObjectHeap;
  
};
  
} // sys
} // lang
} // acdk

#endif //acdk_lang_sys_HeapFrame_h

