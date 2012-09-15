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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/BackTrace.h,v 1.14 2005/04/28 08:49:07 kommer Exp $


#ifndef acdk_lang_sys_BackTrace_h
#define acdk_lang_sys_BackTrace_h


#include "core_vector.h"
#include "core_smartptr.h"
#if defined(ACDK_OS_WIN32)
struct _CONTEXT;
#endif

namespace acdk {
namespace lang {
namespace sys {


#ifdef ACDK_OS_WIN32
class core_winimage;

#endif



class ACDK_CORE_PUBLIC BackTrace
{
public:
  BackTrace(int ignoreTopFrames = 0);
#if defined(ACDK_OS_WIN32)
  BackTrace(_CONTEXT& ctx, int ignoreTopFrames);
#endif
  int getSize()  { return _program_counters.size(); }
  const core_vector<void*>&  program_counters() const { return _program_counters; }
  static RStackFrame getStackFrame(void* pc);
  ::acdk::lang::RStackFrameArray getStackFrames();
  void incIngnoreFrames()
  {
    ++_ignoreTopFrames;
  }
  /**
	  fill the program counters from offset up to max size
    @return number of filled pc's
  */
  static int fillBackTrace(int offset, void** pcBuff, int size);
#if defined(ACDK_OS_WIN32)
  void _save_pcs(_CONTEXT& ctx, bool replace = true);
#endif
  
private:
  void _save_pcs();

  core_vector<void*> _program_counters;
  int _ignoreTopFrames;
#ifdef ACDK_OS_WIN32
  static void init_winimage();
#if defined(__BORLANDC__) // make troubles in destruction
  static core_winimage* _winimage;
#else
  static core_scoped_ptr<core_winimage> _winimage;
#endif
#endif
};


} // namespace sys
} // namespace lang
} // namespace acdk



#endif //acdk_lang_sys_BackTrace_h
