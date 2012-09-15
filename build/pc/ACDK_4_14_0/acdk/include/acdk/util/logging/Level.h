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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/Level.h,v 1.16 2005/02/05 10:45:07 kommer Exp $

#ifndef acdk_util_logging_Level_h
#define acdk_util_logging_Level_h

#include <acdk.h>

namespace acdk {
namespace util {
namespace logging {



/**
  A LogRecord has a LogLevel to categorize its noisiness.
  From Debug, which should only be used for developer debug messessage
  to Fatal, which should used to notice fatal (non recovery) errors.
  The LogLevel below 0x0200 are reserved for special meaning
  @see LogManager::Threshold
  @see LogRecord
*/
enum LogLevel
{
  AllSys = 0x0000,
  /**
    All messages excluding SysDebug.
    This value is only used in LogManager::Threshold
    value to indicate, that all messages should be printed.
  */
  
  /**
    Used in connection with TransactionConsumer
    to start a new transaction
  */
  TransBegin    = 0x0F10,
  /**
    Used in connection with TransactionConsumer
    to commit a transaction
    In Buffered Consumer, this should flush the output stream
  */
  TransCommit   = 0x0F11,
  /**
    Used in connection with TransactionConsumer
    to rollback a transaction
  */
  TransRollback = 0x0F12,
  
  All       = 0x0100,
  /**
    Logs system internals
  */
  SysDebug  = 0x00F0,
  /**
    Messages which are used to debug a application by a developer.
    The short prefex is 'D'.
  */
  Debug     = 0x1000,
  Debug100  = 0x1100,
  Debug200  = 0x1200,
  Debug300  = 0x1300,
  Debug400  = 0x1400,
  /**
    Noisy application message. It should be used to help
    users to trace what the application is doing. Trace
    should only used to figure out error situations.
    The short prefex is 'T'.
  */
  Trace     = 0x2000,
  Trace100  = 0x2100,
  Trace500  = 0x2500,
  Trace700  = 0x2700,
  /**
    Standard trace messages, which provides
    standard processing of an application.
    The short prefex is 'I'.
  */
  Info      = 0x3000,
  Info100   = 0x3100,
  Info200   = 0x3200,
  Info300   = 0x3300,
  Info400   = 0x3400,
  Info500   = 0x3500,
  Info600   = 0x3600,
  /**
    A Note message is a important trace message, which
    provides important processing steps of an application
    The short prefex is 'N'.
  */
  Note      = 0x4000,
  /**
    A Warn is a warning, which may is also an warning
    The application should proceed after a warning, but the
    result of an application may be implete.
    The short prefex is 'W'.
  */
  Warn      = 0x5000,
  /**
    Error situtation are normally not handled (fixed) by an application.
    The application resumes with processing, but the results may
    be incomplete and wrong.
    The short prefex is 'E'.
  */
  Error     = 0x6000,
  /*
    Fatal errors should not be ignored. In most cases
    the application stops processing.
    The short prefex is 'F'.
  */
  Fatal     = 0x8000,
  /**
    None is only used LogManager::Threshold value to indicate
    that no message should be written at all.
  */
  None      = 0xFFFF
};

ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, LogLevel);


class ACDK_CORE_PUBLIC Level
: extends ::acdk::lang::Object
{
public:
  LogLevel _level;
    
  static const char* toCString(int level);
  static char toChar(int level);
  RString toString() { return new String(toCString(_level)); }
  /**
    Try to parse Level
    if Is Integer return value
    else if Is Enum return Enum Value
    else return NOne until throwExOnUnknown false
    else throw IllegalArgumentException
  */
  static int parseLevel(IN(RString) str, bool throwExOnUnknown = false);
};

} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_Level_h
