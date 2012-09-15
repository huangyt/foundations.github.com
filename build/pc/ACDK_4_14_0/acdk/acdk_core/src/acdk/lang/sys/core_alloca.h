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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_alloca.h,v 1.7 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_core_alloca_h
#define acdk_lang_sys_core_alloca_h

#include <stdio.h>

#if !defined(ACDK_OS_BSD) && !defined(ACDK_OS_DARWIN)
#include <malloc.h>
#else
# include <stdlib.h>

#endif //!defined(ACDK_OS_BSD)

# if defined(ACDK_OS_SOLARIS)
#   include <alloca.h> 
# endif

#if defined(ACDK_OS_MINGW)
#define core_alloca(size) (::new char[size])
#define core_freea(ptr) (::delete[] (ptr))

#else //defined(ACDK_OS_MINGW)

/**
  wrapper to alloca
  may uses new/delete if alloca is not available
*/
#define core_alloca(size) alloca(size)
#define core_freea(ptr) do {} while(false)

#endif

#endif //acdk_lang_sys_core_alloca_h

