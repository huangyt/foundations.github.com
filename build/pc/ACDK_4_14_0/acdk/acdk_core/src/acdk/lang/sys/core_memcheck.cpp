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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_memcheck.cpp,v 1.11 2005/03/07 13:47:28 kommer Exp $


#include <acdk.h>
#include "ObjectHeap.h"

#include "core_memcheck.h"
#include <acdk/lang/ref/NotifyObjectEvent.h>

namespace acdk {
namespace lang {
namespace sys {




class ListObjects
: extends ::acdk::lang::Object
, implements ::acdk::lang::ref::NotifyObjectEventListener
{
  virtual void notifyBeforeConstruction(Object* obj) {}
  virtual bool notifyBeforeDestruction(Object* obj) { return true; }
  virtual void notifyWhileDestruction(Object* obj) {}
  virtual bool listHeaps(IN(::acdk::lang::sys::RHeapFrame) theheap) { return false; }
  virtual bool listedAllocated(IN(::acdk::lang::sys::RHeapFrame) theheap, void* obj, ::acdk::lang::sys::AllocatedType type, int size) 
  {
    switch (type)
    {
    case UnspecifiedMem: 
      sys::coreout << "Unspec Mem[" << size  << "]: " << obj << sys::eofl;
      break;
    case RawMem : 
      sys::coreout << "RawMem[" << size  << "]: " << obj << sys::eofl;
      break;
    case ObjectMem : 
    {
      RString s = ((Object*)obj)->toString()->convert(CCAscii);
      const char* c = s->c_str();
      if (c == 0)
        c = "";
      sys::coreout << "ObjectMem[" << size  << "]: " << obj << ": "  << ((Object*)obj)->getClass()->toString()->c_str() << ": " << c << sys::eofl;
      break;
    }
    case InternalMem :
      sys::coreout << "InternalMem[" << size  << "]: " << obj << sys::eofl;
      break;
    case ObjectRefArrayMem:
      sys::coreout << "ObjectRefArrayMem[" << size  << "]: " << obj << sys::eofl;
      break;
    case DumpBufferMem:
    {
      char* buffer = ::new char[size + 1];
      memcpy(buffer, obj, size);
      buffer[size] = 0;
      sys::coreout << "DumpBufferMem[" << size  << "]: " << buffer << sys::eofl;
      ::delete[] buffer;
      break;
    }
    case NoGcMem:
      sys::coreout << "NoGcMem[" << size  << "]: " << obj << sys::eofl;
      break;
    default : 
      sys::coreout << "<error> Mem[" << size  << "]: " << obj << sys::eofl;
      break;
    }
    return true;
  }
};


core_memcheck::core_memcheck(const char* name, bool newFrame, bool useNativeDebug)
: _name(name)
, _useNativeDebug(useNativeDebug)
, _useNewFrame(newFrame)
{
#if defined(_MSC_VER) && defined(_DEBUG)
  _CrtMemCheckpoint(&_checkpoint);
#endif
  if (_useNewFrame == true)
    ObjectHeap::pushFrame();
}
core_memcheck::~core_memcheck()
{
  if (_useNativeDebug == true)
    reportNativeDiff();
  reportHeapDiff();
  if (_useNewFrame == true)
    ObjectHeap::popFrame();
}

void 
core_memcheck::reportHeapDiff()
{
  ListObjects listo;
  acdk::lang::sys::RHeapFrame hf = acdk::lang::sys::ObjectHeap::topHeapFrame();
  if (_useNewFrame == true)
    ObjectHeap::pushFrame();
  hf->listObjects(&listo, 0);
  if (_useNewFrame == true)
    ObjectHeap::popFrame();
}
 


void 
core_memcheck::reportNativeDiff()
{
#if defined(_MSC_VER) && defined(_DEBUG)
  _CrtMemState checkpoint;
  _CrtMemCheckpoint(&checkpoint);
  _CrtMemState diff;
  _CrtMemDifference(&diff, &_checkpoint, &checkpoint);
  if (diff.lSizes[0] == 0 && diff.lSizes[1] == 0  && diff.lSizes[2] == 0  &&
      diff.lSizes[3] == 0  && diff.lSizes[4] == 0)
    return;
  _RPT1( _CRT_WARN, "\n\n%s:\n>>>>>>>>>>>>>>>>>>>>>>>>\n", _name);
  _CrtMemDumpAllObjectsSince(&_checkpoint);
  _RPT1( _CRT_WARN, "<<<<<<<<<<<<<<<<<<<<<<<<<<\n%s\n\n", _name);
#endif //defined(_MSC_VER)
}


} // namespace sys 
} // namespace lang 
} // namespace acdk 


