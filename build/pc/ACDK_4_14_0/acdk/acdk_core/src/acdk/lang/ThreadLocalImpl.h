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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ThreadLocalImpl.h,v 1.8 2005/04/09 19:26:51 kommer Exp $

#ifndef acdk_lang_ThreadLocalImpl_h
#define acdk_lang_ThreadLocalImpl_h
#if !defined(DOXYGENONLY)

#include "SystemError.h"
#include "sys/core_system.h"

namespace acdk {
namespace lang {

#ifdef WIN32_THREADS
typedef DWORD TlsKeyHandle_t;
#endif //WIN32_THREADS

#ifdef POSIX_THREADS
typedef pthread_key_t TlsKeyHandle_t;
#endif //POSIX_THREADS

/** 
  API: ACDK Intern<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/04/09 19:26:51 $
*/  
foreign
class ACDK_CORE_PUBLIC ThreadLocalKey
{
  TlsKeyHandle_t _key;
public:
  ThreadLocalKey() 
  {
#ifdef WIN32_THREADS
    ACDK_SYS_CALL0(_key = ::TlsAlloc, != 0xFFFFFFFF);
#endif //WIN32_THREADS
#ifdef POSIX_THREADS
    ACDK_SYS_CALL2(pthread_key_create, &_key, 0, == 0);
#endif //POSIX_THREADS
  }
  ~ThreadLocalKey() 
  {
#ifdef WIN32_THREADS
  ACDK_SYS_CALL1(TlsFree,  _key, == TRUE);
#endif //WIN32_THREADS
#ifdef POSIX_THREADS
  ACDK_SYS_CALL1(pthread_key_delete, _key, == 0);
#endif //POSIX_THREADS
  }
  TlsKeyHandle_t& key() { return _key; }
  TlsKeyHandle_t& operator()() { return _key; }

};

/** 
  API: ACDK Intern<br>
  @author Roger Rene Kommer
  @version $Revision: 1.8 $
  @date $Date: 2005/04/09 19:26:51 $
*/  

class ACDK_CORE_PUBLIC ThreadLocalImpl 
{
  ThreadLocalKey _key;
public:
  ThreadLocalImpl()
  {
  }
  void* get()
  {
#ifdef WIN32_THREADS
  return TlsGetValue(_key());
#endif //WIN32_THREADS
#ifdef POSIX_THREADS
  return pthread_getspecific(_key());
#endif //POSIX_THREADS
  }
  void set(void* obj)
  {
#ifdef WIN32_THREADS
    ACDK_SYS_CALL2(TlsSetValue, _key(), obj, == TRUE);
#endif //WIN32_THREADS
#ifdef POSIX_THREADS
    ACDK_SYS_CALL2(pthread_setspecific, _key(), obj, == 0);
#endif //POSIX_THREADS
  }
};

} // lang
} // acdk

#endif //!defined(DOXYGENONLY)
#endif //acdk_lang_ThreadLocalImpl_h

