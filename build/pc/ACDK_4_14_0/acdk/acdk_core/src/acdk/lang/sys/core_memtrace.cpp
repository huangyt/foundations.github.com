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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_memtrace.cpp,v 1.3 2005/05/02 23:08:00 kommer Exp $


#include "core_memtrace.h"
#include "BackTrace.h"
#include "../Integer.h"
#include "../System.h"

namespace acdk {
namespace lang {
namespace sys {

core_memtrace* core_memtrace::activeMemTrace = 0;

struct DeactMemTraceScope
{
  core_memtrace* mt;
  DeactMemTraceScope()
    : mt(core_memtrace::activeMemTrace)
  {
    core_memtrace::activeMemTrace = 0;
  }
  ~DeactMemTraceScope()
  {
    core_memtrace::activeMemTrace = mt;
  }
};

void 
core_memtrace::onAlloc(void* ptr, size_t size, int btOffset, AllocatedType type)
{
  core_lock_guard<core_mutex> _lock(_mutex);
  DeactMemTraceScope _deact;
  core_tracemem tm(ptr, size, type);
  BackTrace::fillBackTrace(btOffset + 2, tm.allocBackTrace, MAX_MEMTRACE_STACK);
  _traces.push_back(tm);
}

void 
core_memtrace::onFree(void* ptr, int btOffset)
{
  core_lock_guard<core_mutex> _lock(_mutex);
  DeactMemTraceScope _deact;
  TraceMemContainerType::iterator it = _traces.end() - 1;
  TraceMemContainerType::iterator start = _traces.begin();
  bool found = false;
  for (; it >= start; --it)
  {
    if (it->ptr == ptr)
    {
      found = true;
      break;
    }
  }
  if (found == true)
  {
    if (_removeOnFree == true)
    {
      _traces.erase(it);
      return;
    }
    BackTrace::fillBackTrace(btOffset + 2, it->freeBackTrace, MAX_MEMTRACE_STACK);
    it->freed = true;
  }
  else if (previousMemTrace != 0)
    previousMemTrace->onFree(ptr, btOffset + 1);
}

void 
core_memtrace::flushFreed()
{
  core_lock_guard<core_mutex> _lock(_mutex);
  DeactMemTraceScope _deact;
restart:
  TraceMemContainerType::iterator it = _traces.begin();
  TraceMemContainerType::iterator end = _traces.end();
  for (; it != end; ++it)
  {
    if (it->freed == true)
    {
      TraceMemContainerType::iterator sit = it;
      for (; it != end; ++it)
      {
        if (it->freed == false)
          break;
      }
      _traces.erase(sit, it);
      goto restart;
    }
  }
}

void reportFrame(IN(RStackFrame) st, StringBuffer& sb)
{
  if (st->hasFileAndLine() == true)
  {
    sb <<  "  " << st->getFileName() << "(" << st->getFileLineNo() << "):\n    ";
  }
  sb << "  " << st->getFunctionSignature() << "\n";
}

void 
core_memtrace::report(core_tracemem& tm)
{
  DeactMemTraceScope _deact;
  {
  StringBuffer sb;
  sb << "pointer: 0x" << Integer::toHexString((int)tm.ptr) << "; size=" << (int)tm.size << "; memtype=" << tm.type;
  if (tm.type == ObjectMem)
  {
    Object* obj = reinterpret_cast<Object*>(tm.ptr);
    obj->lockMem(true);
    sb << "; Class: " << obj->getClass()->getName();
    sb << "; toString: " << obj->toString();
    obj->lockMem(false);
  }
  sb << ";\nallocated at:\n";
  for (int i = 0; i < MAX_MEMTRACE_STACK; ++i)
  {
    if (tm.allocBackTrace[i] == 0)
      break;
    RStackFrame st = BackTrace::getStackFrame(tm.allocBackTrace[i]);
    reportFrame(st, sb);
    
  }
  if (tm.freed == true)
  {
    sb << "freed at:\n";
    for (int i = 0; i < MAX_MEMTRACE_STACK; ++i)
    {
      if (tm.freeBackTrace[i] == 0)
        break;
      RStackFrame st = BackTrace::getStackFrame(tm.freeBackTrace[i]);
      reportFrame(st, sb);
    }
  }
  System::out->println(sb.toString());
  }
}

void 
core_memtrace::_getElements(TraceMemPtrContainerType& collected, int flags)
{
  TraceMemContainerType::iterator it = _traces.begin();
  TraceMemContainerType::iterator end = _traces.end();
  for (; it != end; ++it)
  {
    if ((flags & MTRFUnfreed) == MTRFUnfreed && it->freed == true)
      continue;
    collected.push_back(it);
  }
  if (flags & MTRFRecursive && previousMemTrace != 0)
    previousMemTrace->_getElements(collected, flags);
}

bool equalTrace(core_tracemem& f, core_tracemem& s)
{
  for (int i = 0; i < MAX_MEMTRACE_STACK; ++i)
  {
    if (f.allocBackTrace[i] != s.allocBackTrace[i])
      return false;
  }
  return true;
}

bool 
core_memtrace::_checkSameBt(core_tracemem& tm, MergedTraces& merged)
{
  MergedTraces::iterator it = merged.begin();
  MergedTraces::iterator end = merged.end();
  for (; it != end; ++it)
  {
    if (equalTrace(tm, **(it->_elements.begin())) == true)
    {
      ++it->count;
      return true;
    }
  }
  merged.push_back(MergedTrace(&tm));
  return false;
}

void 
core_memtrace::_eliminateSameBt(TraceMemPtrContainerType& collected, MergedTraces& merged)
{
  TraceMemPtrContainerType::iterator it =  collected.begin();
  TraceMemPtrContainerType::iterator end =  collected.end();
  for (; it != end; ++it)
  {
    _checkSameBt(**it, merged);
  }
}

void 
core_memtrace::_reportUnfreed(TraceMemPtrContainerType& collected, int reportFlags)
{
  TraceMemPtrContainerType::iterator it = collected.begin();
  TraceMemPtrContainerType::iterator end = collected.end();
  for (; it != end; ++it)
  {
    report(**it);
  }
}

void 
core_memtrace::_reportMerged(MergedTraces& merged, int reportFlags)
{
  MergedTraces::iterator it = merged.begin();
  MergedTraces::iterator end = merged.end();
  for (; it != end; ++it)
  {
    StringBuffer sb;
    sb << "Allocated " << it->count << " objects:\n";
    System::out->println(sb.toString());
    report(**it->_elements.begin());
  }
}

bool isInRoots(IN(RObjectArray) ra, void* ptr)
{
  ObjectArray::array_iterator it = ra->begin();
  ObjectArray::array_iterator end = ra->end();
  for (; it != end; ++it)
  {
    if (it->impl() == ptr)
      return true;
  }
  return false;
}



void 
core_memtrace::_filterRoot(TraceMemPtrContainerType& collected, int reportFlags)
{
  RObjectArray ra = System::getRootObjects((reportFlags & MTRFNoStatics) == 0);
  TraceMemPtrContainerType target;
  TraceMemPtrContainerType::iterator it = collected.begin();
  TraceMemPtrContainerType::iterator end = collected.end();
  for (; it != end; ++it)
  {
    if (isInRoots(ra, (*it)->ptr) == true)
      target.push_back(*it);
  }
  collected = target;
}

void 
core_memtrace::reportUnfreed(int reportFlags)
{
  core_lock_guard<core_mutex> _lock(_mutex);
  DeactMemTraceScope _deact;
  {
    TraceMemPtrContainerType collected;
    _getElements(collected, reportFlags);
    if (MTRFRootsOnly & reportFlags)
      _filterRoot(collected, reportFlags);
    
    if (MTRFMergeSameBt & reportFlags)
    {
      MergedTraces merged;
      _eliminateSameBt(collected, merged);
      _reportMerged(merged, reportFlags);
    }
    else
    {
      _reportUnfreed(collected, reportFlags);
    }
  }
  /*
  TraceMemContainerType::iterator it = _traces.begin();
  TraceMemContainerType::iterator end = _traces.end();
  for (; it != end; ++it)
  {
     if (it->freed == true)
       continue;
     report(*it);
  }
  }
  if (reportFlags & MTRFRecursive && previousMemTrace != 0)
    previousMemTrace->reportUnfreed(recursive);
  */
}

void 
core_memtrace::reportPointer(void* ptr, bool firstOnly, bool recursive)
{
  core_lock_guard<core_mutex> _lock(_mutex);
  DeactMemTraceScope _deact;
  {
  TraceMemContainerType::iterator it = _traces.end() - 1;
  TraceMemContainerType::iterator start = _traces.begin();
  bool found = false;
  for (; it >= start; --it)
  {
     if (it->ptr == ptr)
     {
       report(*it);
       if (firstOnly == true)
         return;
       found = true;
     }
    }
  }
  if (recursive == true && previousMemTrace != 0)
    previousMemTrace->reportPointer(ptr, firstOnly, recursive);
}

int 
core_memtrace::getAllocatedCount(bool recursive)
{
  core_lock_guard<core_mutex> _lock(_mutex);
  int count = 0;
  TraceMemContainerType::iterator it = _traces.begin();
  TraceMemContainerType::iterator end = _traces.end();
  for (; it != end; ++it)
  {
    if (it->freed == false)
      ++count;
  }
  if (recursive == true && previousMemTrace != 0)
    return count + previousMemTrace->getAllocatedCount(recursive);
  return count;
}


} // namespace sys
} // namespace lang
} // namespace acdk



