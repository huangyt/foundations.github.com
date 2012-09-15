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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/BoehmGC.cpp,v 1.13 2005/03/07 14:02:12 kommer Exp $

#include "BoehmGC.h"
#include "core_sharedlib.h"

extern "C" void warn_proc(char *msg, unsigned long arg)
{
  sys::coreout << "BoehmGC Warn: (" << (const char*)arg << "): " << (int)msg << sys::eofl;
}

namespace acdk {
namespace lang {
namespace sys {

//static 
BoehmGC::Functions& 
BoehmGC::functions()
{
  static BoehmGC::Functions _functions;
  return _functions;
}

struct InitBoehm
{
  InitBoehm()
  {
    BoehmGC::init();
  }
  ~InitBoehm()
  {
  }
};
#if defined(ACDK_OS_WIN32)
//static InitBoehm _initBoehm;
#endif




bool
BoehmGC::Functions::init()
{
  if (_inited == true)
    return true;
  //ObjectHeap::pushFrame(ObjectHeap::RC_Heap);
  core_sharedlib shlib("gc");
  if (shlib.loaded() == false)
  {
    _failedInited = true;
    sys::coreout << "cannot load gc library" << sys::eofl;
    return false;
  }
  bool debugversion = true;
  GC_init = (GC_init_t)shlib.locateFunction("GC_init") ;
  GC_gcollect = (GC_gcollect_t)shlib.locateFunction("GC_gcollect");
  GC_malloc_atomic = (GC_malloc_atomic_t)shlib.locateFunction(debugversion ? "GC_debug_malloc_atomic" : "GC_malloc_atomic");
  GC_malloc = (GC_malloc_t)shlib.locateFunction(debugversion ? "GC_debug_malloc" : "GC_malloc");
  GC_malloc_uncollectable = (GC_malloc_uncollectable_t)shlib.locateFunction(debugversion ? "GC_debug_malloc_uncollectable" : "GC_malloc_uncollectable");  
  GC_free = (GC_free_t)shlib.locateFunction(debugversion ? "GC_debug_free" : "GC_free");
  GC_register_finalizer_ignore_self = (GC_register_finalizer_ignore_self_t)shlib.locateFunction(debugversion ? "GC_debug_register_finalizer_ignore_self" : "GC_register_finalizer_ignore_self");
  GC_base = (GC_base_t)shlib.locateFunction("GC_base");
  GC_get_heap_size = (GC_get_heap_size_t)shlib.locateFunction("GC_get_heap_size");
  GC_set_max_heap_size = (GC_set_max_heap_size_t)shlib.locateFunction("GC_set_max_heap_size");
  GC_add_roots = (GC_add_roots_t)shlib.locateFunction("GC_add_roots");
  GC_set_warn_proc = (GC_set_warn_proc_t)shlib.locateFunction("GC_set_warn_proc");

  if (GC_init)
    GC_init();
  _inited = true;
  GC_set_warn_proc(warn_proc);
  //ObjectHeap::popFrame();
  return true;
}


} // namespace sys
} // namespace lang 
} // namespace acdk 




