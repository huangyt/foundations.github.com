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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_atomicop.h,v 1.20 2005/04/09 19:26:56 kommer Exp $

#ifndef acdk_lang_sys_core_atomicop_h
#define acdk_lang_sys_core_atomicop_h

#include "../../Config.h"
#include "core_threadsys.h"

#include "core_fastmutex.h"
#include "core_guard.h"

#if defined(ACDK_OS_LINUX) && !defined(ACDK_ATOMIC_USE_PTHREAD)
#  define ACDK_ATOMIC_COUNTER(_value) _value.counter
#else
#  define ACDK_ATOMIC_COUNTER(_value) _value
#endif

#if defined(_MSC_VER) && (_MSC_VER >= 1000)
extern "C"
{
   LONG  __cdecl _InterlockedIncrement(LONG volatile *Addend);
   LONG  __cdecl _InterlockedDecrement(LONG volatile *Addend);
   LONG  __cdecl _InterlockedCompareExchange(LPLONG volatile Dest, LONG Exchange, LONG Comp);
   LONG  __cdecl _InterlockedExchange(LPLONG volatile Target, LONG Value);
   LONG  __cdecl _InterlockedExchangeAdd(LPLONG volatile Addend, LONG Value);
}

#pragma intrinsic (_InterlockedCompareExchange)
#define InterlockedCompareExchange _InterlockedCompareExchange

#pragma intrinsic (_InterlockedExchange)
#define InterlockedExchange _InterlockedExchange 

#pragma intrinsic (_InterlockedExchangeAdd)
#define InterlockedExchangeAdd _InterlockedExchangeAdd

#pragma intrinsic (_InterlockedIncrement)
#define InterlockedIncrement _InterlockedIncrement

#pragma intrinsic (_InterlockedDecrement)
#define InterlockedDecrement _InterlockedDecrement

// Data to protect with the interlocked functions.
#endif // defined(_MSC_VER) && (_MSC_VER >= 1000)


namespace acdk {
namespace lang {
namespace sys {


/**
  This is the internal implementation of an atomic operation
*/
class ACDK_CORE_PUBLIC core_atomicop
{
#if defined(ACDK_OS_LINUX) && !defined(ACDK_ATOMIC_USE_PTHREAD)
  atomic_t _value;
#elif defined(ACDK_OS_BSD)
  volatile u_int _value;
#else
   long _value;
#endif
#if defined (ACDK_ATOMIC_USE_PTHREAD)
   core_fastmutex _mutex;
#endif 
public:
  /** runs on a single processor */
  static bool singleProcessor;
  /**
    constructs a new atomic op variable
  */
  core_atomicop(int val = 0)
#if defined(ACDK_OS_LINUX)
     : _value()
#else
   : _value(val)
#endif
   {
   }
   /** read a value */
   inline int read()
   {
     if (singleProcessor  == true)
		   return ACDK_ATOMIC_COUNTER(_value);
#if defined(ACDK_OS_LINUX) && !defined(ACDK_ATOMIC_USE_PTHREAD)
    return atomic_read(&_value);
#elif defined(ACDK_OS_BSD)
    return _value;
#elif defined (ACDK_ATOMIC_USE_PTHREAD)
	  if (singleProcessor  == true)
		  return _value;
    core_lock_guard<core_fastmutex> guard(_mutex);
    return _value;
#else
      return _value;
#endif
   }
   /**
    increments the value and return it
    */
   inline void increment()
   {
     if (singleProcessor  == true)
     {
       ++ACDK_ATOMIC_COUNTER(_value);
       return;
     }     
#if defined (ACDK_ATOMIC_USE_PTHREAD)
      core_lock_guard<core_fastmutex> guard(_mutex);
      ++_value;
#elif defined (WIN32_THREADS)
      InterlockedIncrement(&_value);
#elif defined(ACDK_OS_LINUX)
      atomic_inc(&_value);
#elif defined(ACDK_OS_BSD)
      atomic_add_int(&_value, 1);
#endif
   }
  /**
    decrement the value and return true if value is zero
  */
  inline bool decr_test_zero()
  {
    if (singleProcessor  == true)
      return --ACDK_ATOMIC_COUNTER(_value) == 0;
#if defined (ACDK_ATOMIC_USE_PTHREAD)
      core_lock_guard<core_fastmutex> guard(_mutex);
      bool erg = --_value == 0;
      return erg;
#elif defined (WIN32_THREADS)
      return InterlockedDecrement(&_value) == 0;
#elif defined(ACDK_OS_LINUX)
      return atomic_dec_and_test(&_value) != 0;
#elif defined(ACDK_OS_BSD)
      atomic_subtract_int(&_value, 1);
      return _value == 0;
#endif
  }
  /**
    decrement value
  */
   inline void decrement()
   {
     if (singleProcessor == true)
     {
       --ACDK_ATOMIC_COUNTER(_value);
       return;
     }
#if defined (ACDK_ATOMIC_USE_PTHREAD)
      core_lock_guard<core_fastmutex> guard(_mutex);
      --_value;
#elif defined (WIN32_THREADS)
      InterlockedDecrement(&_value);
#elif defined(ACDK_OS_LINUX)
      atomic_dec(&_value);
#elif defined(ACDK_OS_BSD)
      atomic_subtract_int(&_value, 1);
#endif
   }
  /* 
  not directly supported by Linux and not needed
   int swap(int other)
   {
#if defined (ACDK_ATOMIC_USE_PTHREAD)
     core_lock_guard<core_fastmutex> guard(_mutex);
      int ret = _value;
      _value = other;
      return ret;
#elif defined (WIN32_THREADS)
      return InterlockedExchange(_value), other);
#endif

   }
   int compare_swap(int first, int second)
   {
#if defined (POSIX_THREADS)
      core_lock_guard<core_fastmutex> guard(_mutex);
      int ret = _value;
		if (_value == first)
			_value = second;
		return ret;
#elif defined (WIN32_THREADS)
      return InterlockedCompareExchange(const_cast<long*>(&_value), first, second);
#endif
   }
  */
  /*
  static void increment(int& value)
  {
#if defined (ACDK_ATOMIC_USE_PTHREAD)
     core_lock_guard<core_fastmutex> guard(_mutex);
      ++_value;
#elif defined (WIN32_THREADS)
      InterlockedIncrement(&value);
#elif defined(ACDK_OS_LINUX)
      atomic_inc(&value);
#endif
  }
  static bool decr_test_zero(int& value)
  {
 #if defined (ACDK_ATOMIC_USE_PTHREAD)
      return --_value == 0;
#elif defined (WIN32_THREADS)
      return InterlockedDecrement(&_value) == 0;
#elif defined(ACDK_OS_LINUX)
      return atomic_dec_and_test(&_value) == TRUE;
#endif
  }
  */
};

#undef ACDK_ATOMIC_COUNTER

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_atomicop_h

