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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/BoehmGC.h,v 1.8 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_BohemGC_h
#define acdk_lang_sys_BohemGC_h

#include <acdk.h>


namespace acdk {
namespace lang {
namespace sys {

typedef void (*GC_finalization_proc)(void* obj, void* client_data);
typedef void (*GC_warn_proc) (char *msg, unsigned long arg);

class BoehmGC
{
  struct Functions
  {
    bool _inited;
    bool _failedInited;
    typedef void (*GC_init_t)();
    GC_init_t GC_init;
    typedef void (*GC_gcollect_t)();
    GC_gcollect_t GC_gcollect;
    typedef void* (*GC_malloc_t)(size_t size_in_bytes);
    GC_malloc_t GC_malloc;
    typedef void* (*GC_malloc_atomic_t)(size_t size_in_bytes);
    GC_malloc_atomic_t GC_malloc_atomic;
    typedef void* (*GC_malloc_uncollectable_t)(size_t size_in_bytes);
    GC_malloc_uncollectable_t GC_malloc_uncollectable;
    
    typedef void (*GC_free_t)(void* object_addr);
    GC_free_t GC_free;
    typedef void (*GC_register_finalizer_ignore_self_t)(void* obj, GC_finalization_proc fn, void* cd, GC_finalization_proc *ofn, void** ocd);
    GC_register_finalizer_ignore_self_t GC_register_finalizer_ignore_self;
    typedef void* (*GC_base_t)(void *displaced_pointer);
    GC_base_t GC_base;

    typedef size_t (*GC_get_heap_size_t)(void);
    GC_get_heap_size_t GC_get_heap_size;
    typedef void (*GC_set_max_heap_size_t)(unsigned long n);
    GC_set_max_heap_size_t GC_set_max_heap_size;

    typedef void (*GC_add_roots_t) (char* low_address, char* high_address_plus_1);
    GC_add_roots_t GC_add_roots;
    
    typedef GC_warn_proc (*GC_set_warn_proc_t)(GC_warn_proc p);
    GC_set_warn_proc_t GC_set_warn_proc;

    Functions() 
    : _inited(false) 
    , _failedInited(false)
    , GC_init(0)
    , GC_gcollect(0)
    , GC_malloc(0)
    , GC_malloc_atomic(0)
    , GC_malloc_uncollectable(0)
    , GC_free(0)
    , GC_register_finalizer_ignore_self(0)
    , GC_base(0)
    , GC_get_heap_size(0)
    , GC_set_max_heap_size(0)
    , GC_add_roots(0)
    , GC_set_warn_proc(0)
    {
    }
    bool init();
  };
  static BoehmGC::Functions& functions();
  
public:
  inline static bool init()
  {
    if (functions()._inited == true)
      return true;
    if (functions()._failedInited == true)
      return false;
    return functions().init();
  }
  static void collect()
  {
    if (init() == false)
      return;
    functions().GC_gcollect();
  }
  static void* malloc(size_t size)
  {
    if (init() == false)
      return 0;
    return functions().GC_malloc(size);
  }
  static void* malloc_atomic(size_t size)
  {
    if (init() == false)
      return 0;
    return functions().GC_malloc_atomic(size);
  }
  static void* malloc_uncollectable(size_t size)
  {
    if (init() == false)
      return 0;
    return functions().GC_malloc_uncollectable(size);
  }
  static void free(void* ptr)
  {
     if (init() == false)
      return;
    functions().GC_free(ptr);
  }
  static void register_finalizer_ignore_self(void* obj, GC_finalization_proc fn, void* cd, GC_finalization_proc *ofn, void** ocd)
  {
    if (init() == false)
      return;
    functions().GC_register_finalizer_ignore_self(obj, fn, cd, ofn, ocd);
  }
  static void* base(void* ptr)
  {
    if (init() == false)
      return 0;
    return functions().GC_base(ptr);
  }
  static size_t get_heap_size()
  {
    if (init() == false)
      return 0;
    return functions().GC_get_heap_size();  
  }
  static void set_max_heap_size(long maxsize)
  {
    if (init() == false)
      return;
    functions().GC_set_max_heap_size(maxsize);
  }
  static void add_roots(char* low_address, char* high_address_plus_1)
  {
    if (init() == false)
      return;
    functions().GC_add_roots(low_address, high_address_plus_1);
  }
  static GC_warn_proc set_warn_proc(GC_warn_proc warnproc)
  {
    if (init() == false)
      return 0;
    return functions().GC_set_warn_proc(warnproc);
  }
};

} // namespace sys
} // namespace lang 
} // namespace acdk 


#endif //acdk_lang_BohemGC_h

