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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_tick.h,v 1.15 2005/02/05 10:45:01 kommer Exp $

#ifndef acdk_lang_sys_core_tick_h
#define acdk_lang_sys_core_tick_h

#include "../../Config.h"
#include <time.h>
#ifndef CLOCKS_PER_SEC
# error CLOCKS_PER_SEC is undefined
#endif 

#if defined(ACDK_MINGW) && !defined(CLOCKS_PER_SEC)
# define CLOCKS_PER_SEC 1000
#endif 

#if defined(__BORLANDC__)
# undef CLOCKS_PER_SEC // becuase it is float
# define CLOCKS_PER_SEC 1000
#endif // defined(__BORLANC__)

#if !defined(ACDK_MINGW)
#if CLOCKS_PER_SEC == 0
# error CLOCKS _PER _SEC is 0
#endif
#if CLOCKS_PER_SEC / 1000 == 0
# define CLOCKS_PER_MILLISECONDS 1
#else
# define CLOCKS_PER_MILLISECONDS (CLOCKS_PER_SEC / 1000)
#endif
#else
# define CLOCKS_PER_MILLISECONDS 1 // ??? really
#endif

namespace acdk {
namespace lang {
namespace sys {

typedef clock_t tick_t;
class ACDK_CORE_PUBLIC core_tick
{
 public:
  enum {
    TicksPerSecond = CLOCKS_PER_SEC,
    TicksPerMillisecond = CLOCKS_PER_MILLISECONDS
  };
  static tick_t now() { return clock(); }
  static int millisecsDiff(tick_t prevtick, tick_t nexttick) 
  {
    return (nexttick - prevtick) / CLOCKS_PER_MILLISECONDS;
  }
  static int millisecsSince(tick_t ltick) 
  {
    return (now() - ltick) / CLOCKS_PER_MILLISECONDS;
  }
  
};


} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_tick_h

