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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ThreadImpl.h,v 1.9 2005/04/09 19:26:51 kommer Exp $

#ifndef acdk_lang_ThreadImpl_h
#define acdk_lang_ThreadImpl_h
#if !defined(DOXYGENONLY)

#include <acdk/lang/sys/core_threadsys.h>

class ThreadID;
/** 
  API: ACDK Intern<br>
  @author Roger Rene Kommer
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:51 $
*/  

class ACDK_CORE_PUBLIC ThreadID 
{
public:
#ifdef POSIX_THREADS
  pthread_t id;
  ThreadID() : id(pthread_t(-1)) { }
  ThreadID(pthread_t thr) 
  : id(thr)
  {
  }
  ThreadID(const ThreadID& other)
  : id(other.id)
  {
  }
  pthread_t threadID() const { return id; }
  bool isThisThreadCurrent() const
  {
    return pthread_equal(threadID(), pthread_self());
  }
  static ThreadID getCurrentThreadID()
  {
    return ThreadID(pthread_self());
  }
  int getId() { return (int)id; }
#endif //POSIX_THREADS

#ifdef WIN32_THREADS
  DWORD id;
  ThreadID() : id(0) { }
  ThreadID(DWORD thr) 
  : id(thr)
  {
  }
  ThreadID(const ThreadID& other)
  : id(other.id)
  {
  }
  DWORD threadID() const { return id; }
  bool isThisThreadCurrent() const
  {
    return threadID() == GetCurrentThreadId();
  }
  static ThreadID getCurrentThreadID()
  {
    return ThreadID(GetCurrentThreadId());
  }
  int getId() { return (int)id; }
#endif //WIN32_THREADS
  bool operator==(const ThreadID& other) const
  {
#ifdef POSIX_THREADS
    return pthread_equal(threadID(), other.threadID());
#endif //POSIX_THREADS
#ifdef WIN32_THREADS
    return threadID() == other.threadID();
#endif
  }
#ifdef POSIX_THREADS
  bool operator==(const pthread_t& other) const
  {
    return pthread_equal(threadID(), other);
  }
#endif //POSIX_THREADS
#ifdef WIN32_THREADS
  bool operator==(const DWORD& other) const
  {
    return threadID() == other;
  }
#endif
  bool operator!=(const ThreadID& other) const
  {
    return operator==(other) ? false : true;
  }
  //acdk::lang::RString toString() const { return acdk::lang::String::valueOf(long(id)); }
};


#endif //!defined(DOXYGENONLY)
#endif //acdk_lang_ThreadImpl_h

