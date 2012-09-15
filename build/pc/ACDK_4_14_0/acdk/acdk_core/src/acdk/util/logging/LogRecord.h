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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/LogRecord.h,v 1.18 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_util_logging_LogRecord_h
#define acdk_util_logging_LogRecord_h

#include <acdk.h>
#include <acdk/lang/Thread.h>
#include <acdk/lang/Process.h>
#include <acdk/util/SysDate.h>
#include <acdk/lang/dmi/ScriptVar.h>

#include "Level.h"
#include "NamedLogArgs.h"

namespace acdk {
namespace util {
namespace logging {

ACDK_DECL_CLASS(LogRecord);

/**
  represend one logging entry/message.
  Normally doesn't use this class in normal code, but the ACDK_NLOG() macros.
  @author Roger Rene Kommer
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC LogRecord
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(LogRecord)
public:
  int level;
  RString catName;
  jlong timeStamp;
  int processId;
  int threadId;
  RString message;
  RObjectArray parameters;
  RNamedParameterArray namedParameters;
  RThrowable ex;
  
  RString file;
  int line;

  LogRecord(int l, IN(RString) cat, IN(RString) msg)
  : Object()
  , level(l)
  , catName(cat)
  , timeStamp(SysDate().getTime())
  , processId(::acdk::lang::Process::getProcessId())
  , threadId(::acdk::lang::Thread::currentThreadId().getId())
  , message(msg)
  {
  }
  LogRecord(int l, IN(RString) cat, IN(RString) msg, const char* file, int lin)
  : Object()
  , level(l)
  , catName(cat)
  , timeStamp(SysDate().getTime())
  , processId(::acdk::lang::Process::getProcessId())
  , threadId(::acdk::lang::Thread::currentThreadId().getId())
  , message(msg)
  , file(new String(file))
  , line(lin)
  {
  }
  LogRecord(int l, IN(RString) cat, IN(RString) msg, IN(RNamedParameterArray) args, const char* file, int lin)
  : Object()
  , level(l)
  , catName(cat)
  , timeStamp(SysDate().getTime())
  , processId(::acdk::lang::Process::getProcessId())
  , threadId(::acdk::lang::Thread::currentThreadId().getId())
  , message(msg)
  , namedParameters(args)
  , file(new String(file))
  , line(lin)
  {
    /*
    namedParameters = new NamedParameterArray(args.size());
    for (int i = 0; i < args.size(); ++i)
    {
      namedParameters[i] = new NamedParameter(args[i]._name, *args[i]._val);
    }*/
  }
};


  

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_LogRecord_h
