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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_syslocal.h,v 1.5 2005/03/21 15:08:29 kommer Exp $

#ifndef acdk_lang_sys_core_syslocal_h
#define acdk_lang_sys_core_syslocal_h

#include "../../Config.h"
#include "core_threadsys.h"
#include "core_fastmutex.h"
#include "../System.h"

namespace acdk {
namespace lang {
namespace sys {

/*
    gcc on FreeBSD 5.3 does support TLS, but it does not run reliable
*/
#if defined(ACDK_OS_LINUX) || ACDK_OS_SOLARIS_VER >= 10

# define ACDK_OS_SUPPORT_TLS 1
#else

#endif


extern bool acdk_core_BoundStatic;

#if defined(ACDK_OS_WIN32) && defined(_MSC_VER)
# define DECLARE_STATIC_SPECIFIC(Type, Name) extern __declspec(thread) Type Name
# define DEFINE_STATIC_SPECIFIC(Type, Name) __declspec(thread) Type Name
# define ACDK_SUPPORT_STATIC_THREADLOCAL 1
#elif ACDK_CHECK_GCC_VERSION(3, 3) && defined(ACDK_OS_SUPPORT_TLS)
# define DECLARE_STATIC_SPECIFIC(Type, Name) extern __thread Type Name
#   define DEFINE_STATIC_SPECIFIC(Type, Name) __thread Type Name
#   define ACDK_SUPPORT_STATIC_THREADLOCAL 1
#else
#   define DEFINE_STATIC_SPECIFIC(Type, Name) Type Name // dummy entry
 // no support for static specific
#endif

/**
  the static_specific represents a thread local pointer to T
  T must have a public constructor with no arguments
  if called in main and ACDK_SUPPORT_STATIC_THREADLOCAL is defined
  it uses a the fast implementation of static TLS
  Otherwise it uses slower specific<T> implementation
  if called outside main it uses just a normal instance
*/
template <class T>
class static_specific
{
  bool _libLinked;
  T* _outMainPtr;
  T** _staticSpecific;
  specific<T> _dynSpecific;
public:
  static_specific(bool libLinked, T** staticSpecific)
    : _libLinked(libLinked)
    , _outMainPtr(0)
    , _staticSpecific(staticSpecific)
  {
  }
  T& get()
  {
    if (::acdk::lang::System::isInMain() == false)
    {
      if (_outMainPtr == 0)
        _outMainPtr = new T();
      return *_outMainPtr;
    }
#if defined(ACDK_SUPPORT_STATIC_THREADLOCAL)
    if (_libLinked == true)
    {
      if (*_staticSpecific == 0)
        *_staticSpecific = new T();
      return **_staticSpecific;
    }
#endif // ACDK_SUPPORT_STATIC_THREADLOCAL
    return _dynSpecific;
  }
};



} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_syslocal_h

















































