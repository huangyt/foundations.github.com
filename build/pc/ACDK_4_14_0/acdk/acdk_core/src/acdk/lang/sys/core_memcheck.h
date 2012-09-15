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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_memcheck.h,v 1.10 2005/02/05 10:45:00 kommer Exp $


#ifndef acdk_lang_sys_core_memcheck_h
#define acdk_lang_sys_core_memcheck_h

#include <acdk.h>
#if defined(_MSC_VER)
#include "crtdbg.h"
#endif //defined(_MSC_VER)

namespace acdk {
namespace lang {
namespace sys {


/**
  Class to Check Memory usage in a scope
  @author Roger Rene Kommer
*/
class ACDK_CORE_PUBLIC core_memcheck
{
#if defined(_MSC_VER) && defined(_DEBUG)
  _CrtMemState _checkpoint;
#endif //defined(_MSC_VER)
  const char* _name;
  bool _useNativeDebug;
  bool _useNewFrame;
public:
  core_memcheck(const char* name = "MemLeak", bool newFrame = true, bool useNativeDebug = true);
  ~core_memcheck();
  void reportNativeDiff();
  void reportHeapDiff();
  static void setBreakAtObjectNo(int objno)
  {
#if  defined(_MSC_VER) && defined(_DEBUG)
    _CrtSetBreakAlloc(objno);
#endif //defined(_MSC_VER)
  }
};



} // namespace sys 
} // namespace lang 
} // namespace acdk 



#endif //acdk_lang_sys_core_memcheck_h
