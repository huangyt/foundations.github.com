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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_threadsys.h,v 1.19 2005/02/21 20:49:23 kommer Exp $

#ifndef acdk_lang_sys_core_threadsys_h
#define acdk_lang_sys_core_threadsys_h

#include "../../Config.h"

#if defined (POSIX_THREADS)
# include <sys/types.h>
# include <pthread.h>
# include <semaphore.h>

#elif defined (WIN32_THREADS)

//# if !defined(__BORLANDC__)
#  ifndef _WIN32_WINNT
#    define _WIN32_WINNT 0x0500
#  endif
//# endif // !defined(__BORLANDC__)

#  if !defined(_WINDOWS_)
#    if defined(_AFXDLL) || defined(ACDK_USE_MFC)
#      include <afx.h>
#    else
#      include <windows.h>
#    endif
#  endif

#elif
# error no thread supported. POSIX_THREADS or WIN32_THREADS should be defined
#endif


#if defined(ACDK_OS_LINUX) && !defined(__sparc__) && !defined(_ARCH_PPC) && !defined(ACDK_OS_CYGWIN32) && !defined(__BORLANDC__)
#include "core_atomicop_linux.h" 

#elif defined (POSIX_THREADS)
// already included # include <pthread.h>
# if defined(ACDK_OS_BSD)
#  include <machine/atomic.h>
# else
#  define ACDK_ATOMIC_USE_PTHREAD
# endif
#elif defined (WIN32_THREADS)
//already included # include <windows.h>
#else
# error no thread or atomic supported. POSIX_THREADS or WIN32_THREADS should be defined
#endif

#ifdef ACDK_HAS_BOEHMGC
#include <gc.h>
#endif //ACDK_HAS_BOEHMGC
#endif //acdk_lang_sys_core_threadsys_h


