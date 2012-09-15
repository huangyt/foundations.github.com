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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_specific.h,v 1.12 2005/02/05 10:45:01 kommer Exp $

#ifndef acdk_lang_sys_core_specific_h
#define acdk_lang_sys_core_specific_h

#include "../../Config.h"
#include "core_thread_id.h"

namespace acdk {
namespace lang {
namespace sys {



/**
*/
class ACDK_CORE_PUBLIC core_specific
{
  int _value_index;
public:
 
  typedef void (*CleanUpFunc)(void* ptr);

  core_specific();
  virtual ~core_specific();
  void set(void *t);
  void* get();
  /**
    returns true if an initial value is set
  */
  bool isSet() { return get() != 0; }
  int get_key() const { return _value_index; }

  void register_cleanup(CleanUpFunc fnc);

  static void cleanup(void* ptr) { }

  /**
    the current thread going down. cleanup all specific data 
  */
  static void thread_cleanup();
  /**
    returns the values of this thread_specific in all threads.
    @note be care, this is not synchronized, and should only
          used for debugging.
  */
  void get_locals(core_vector<void*>& erg);
  static void get_thread_locals(core_vector<void*>& erg, core_thread_id tid);
  
  friend class core_tls_tables;
};


template <class T>
class specific
: protected core_specific
{
public:
  specific()
  : core_specific()
  {
    register_cleanup(cleanup);
  }
  specific(const T& intitval)
  : core_specific()
  {
    register_cleanup(cleanup);
    get() = intitval;
  }
  /**
    returns true if an initial value is set
  */
  bool isSet() { return core_specific::isSet(); }
  void reset()
  {
    void* ptr = core_specific::get();
    if (ptr == 0) 
      return;
    core_specific::set(0);
    T* t = reinterpret_cast<T*>(ptr);
    delete t;
  }
  
  T& get()
  {
    void* ptr = core_specific::get();
    if (ptr != 0) 
      return *reinterpret_cast<T*>(ptr);
    
    T* t = new T();
    core_specific::set(t);
    return *t;
  }
  specific<T>& operator=(const T& t)
  {
    get() = t;
    return *this;
  }

  static void cleanup(void* ptr)
  {
    T* t = reinterpret_cast<T*>(ptr);
    delete t;
  }
  T* operator->() { return &get(); }
  const T* operator->() const { return &get(); }
  operator T& () { return get(); }
  
};

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_specific_h

