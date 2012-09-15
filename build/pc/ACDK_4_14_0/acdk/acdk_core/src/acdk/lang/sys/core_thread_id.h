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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_thread_id.h,v 1.5 2005/02/05 10:45:01 kommer Exp $

#ifndef acdk_lang_sys_core_thread_id_h
#define acdk_lang_sys_core_thread_id_h

#include "core_threadsys.h"

namespace acdk {
namespace lang {
namespace sys {

class ACDK_CORE_PUBLIC  core_thread_id
{
public:

#ifdef POSIX_THREADS
  pthread_t _id;
  core_thread_id(pthread_t thr = pthread_t(-1)) 
  : _id(thr) 
  { 
  }
  pthread_t tid() const { return _id; }
  bool is_current() const { return pthread_equal(tid(), pthread_self()); }
  static core_thread_id get_current()
  {
    return core_thread_id(pthread_self());
  }
#endif //POSIX_THREADS

#ifdef WIN32_THREADS
  DWORD _id;
  core_thread_id(DWORD thr = 0) 
  : _id(thr)
  {
  }
  DWORD tid() const { return _id; }
  bool is_current() const { return tid() == GetCurrentThreadId(); }
  static core_thread_id get_current()
  {
    return core_thread_id(GetCurrentThreadId());
  }
#endif //WIN32_THREADS
  bool operator==(const core_thread_id& other) const
  {
#ifdef POSIX_THREADS
    return pthread_equal(tid(), other.tid());
#endif //POSIX_THREADS
#ifdef WIN32_THREADS
    return tid() == other.tid();
#endif
  }
};

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_thread_id_h

