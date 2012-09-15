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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_memtrace.h,v 1.2 2005/04/28 15:00:05 kommer Exp $


#ifndef acdk_lang_sys_core_memtrace_h
#define acdk_lang_sys_core_memtrace_h

#include <acdk.h>
#include "core_vector.h"
#include "core_smartptr.h"


namespace acdk {
namespace lang {
namespace sys {

#define MAX_MEMTRACE_STACK 10

struct core_tracemem
{
  bool freed;
  AllocatedType type;
  size_t size;
  void* ptr;
  
  void* allocBackTrace[MAX_MEMTRACE_STACK];
  void* freeBackTrace[MAX_MEMTRACE_STACK];
  core_tracemem(void* p, size_t s, AllocatedType t)
    : freed(false)
    , type(t)
    , size(s)
    , ptr(p)
  {
    memset(allocBackTrace, 0, sizeof(allocBackTrace));
    memset(freeBackTrace, 0, sizeof(allocBackTrace));
  }
};

/*
class TraceReport
  : 
{
  jlong _ptr;
  RObject _obj;
  int _size;
  RStackFrameArray _allocated;
  RStackFrame _freed;
  foreign TraceReport(void* ptr, IN(RObject) obj, int size, int type, IN(RStackFrameArray) allocated, IN(RStackFrame) freed)
    : _ptr((jlong)ptr)
    , _obj(obj)
    , _size(size)
    ...
  {
  }
};
*/

/**
  used by core_memtrace to report objects
*/
enum MemTraceReportFlags
{
  /**
    report from this core_memtrace and parent
  */
  MTRFRecursive     = 0x0001,
  /**
    if core_tracemem has same backtrace merge them
  */
  MTRFMergeSameBt   = 0x0002,
  /**
    report only root objects
  */
  MTRFRootsOnly     = 0x0004,
  /**
    report only not freed objects
  */
  MTRFUnfreed       = 0x0008,
  /**
    ignore object held by references
    Only works in combination with MTRFRootsOnly
  */
  MTRFNoStatics     = 0x0010,
};

class ACDK_CORE_PUBLIC core_memtrace
{
public:
  typedef core_vector<core_tracemem> TraceMemContainerType;
  TraceMemContainerType _traces;
  typedef core_vector<core_tracemem*> TraceMemPtrContainerType;
  core_mutex _mutex;
  core_memtrace* previousMemTrace;
  bool _removeOnFree;
  static core_memtrace* activeMemTrace;
  struct MergedTrace
  {
    int count;
    TraceMemPtrContainerType _elements;
    MergedTrace()
      : count(0)
    {
    }
    MergedTrace(core_tracemem* tm)
      : count(1)
    {
      _elements.push_back(tm);
    }
  };
  typedef core_vector<MergedTrace> MergedTraces;
  core_memtrace(bool memLeakOnly = true)
    : previousMemTrace(activeMemTrace)
    , _removeOnFree(memLeakOnly)
  {
    activeMemTrace = this;
  }
  ~core_memtrace()
  {
    activeMemTrace = previousMemTrace;
  }
  void onAlloc(void* ptr, size_t size, int btOffset, AllocatedType type);
  void onFree(void* ptr, int btOffset);
  inline static void* OnAlloc(void* ptr, size_t size, int btOffset, AllocatedType type)
  {
    if (activeMemTrace != 0)
      activeMemTrace->onAlloc(ptr, size, btOffset + 1, type);
    return ptr;
  }
  inline static void* OnFree(void* ptr, int btOffset)
  {
    if (activeMemTrace != 0)
      activeMemTrace->onFree(ptr, btOffset + 1);
    return ptr;
  }
  void flushFreed();
  /**
    @reportFlags combination of enum ReportFlags
  */
  void reportUnfreed(int reportFlags = MTRFUnfreed);
  
  void report(core_tracemem& tm);
  void reportPointer(void* ptr, bool firstOnly = true, bool recursive = true);
  int getAllocatedCount(bool recursive = false);
protected:
  /**
    @param collected collection of all core_tracemem
    @param flags combination of enum ReportFlags
  */
  void _getElements(TraceMemPtrContainerType& collected, int flags);
  void _reportUnfreed(TraceMemPtrContainerType& collected, int reportFlags);
  void _eliminateSameBt(TraceMemPtrContainerType& collected, MergedTraces& merged);
  bool _checkSameBt(core_tracemem& tm, MergedTraces& merged);
  void _filterRoot(TraceMemPtrContainerType& collected, int reportFlags);
  void _reportMerged(MergedTraces& merged, int reportFlags);
};


} // namespace sys
} // namespace lang
} // namespace acdk



#endif //acdk_lang_sys_core_memtrace_h
