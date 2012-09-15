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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/Logger.h,v 1.16 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_util_logging_Logger_h
#define acdk_util_logging_Logger_h

#include "Level.h"
#include "LogConsumer.h"
#include "LogRecord.h"

namespace acdk {
namespace util {
namespace logging {


ACDK_DECL_CLASS(Logger);

/**
  represend one logical logger
  A logger can have a hierarchical name
  and 0 - n consumers
  @author Roger Rene Kommer
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC Logger
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(Logger)
protected:
  int _minLevel;
  RString _name;
  RLogger _parent;
  RLogConsumerArray _consumer;
  bool _inheritConsumer;
  
public:
  Logger(INP(RString) name);
  /**
    @param minLevel minimum level this logger should be log
  */
  Logger(INP(RString) name, int minLevel);
  
  ~Logger();
  void addConsumer(IN(RLogConsumer) cons);
  /**
    return false if level is greater than this logger level and logger has any consumer
  */
  bool doLog(int level) const
  {
    if (_minLevel > level || _consumer->length() == 0)
      return false;
    return true;
  }
  /**
    log a message
  */
  void log(int level, INP(RString) cat, INP(RString) text);
  /**
    log a message
    @param file must be a static string (for instance via macro __FILE__)
  */
  void log(int level, INP(RString) cat, INP(RString) text, const char* file, int line);
  void log(int level, INP(RString) cat, INP(RString) text, INP(RNamedParameterArray) args, const char* file, int line);
  void log(INP(RLogRecord) log);

  int getMinLevel() const { return _minLevel; }
  void setMinLevel(int level) { _minLevel = level; }
  RString getName() { return _name; }
  void setName(INP(RString) newname) { _name = newname; }
  /**
    Formats a message in compiler compatible format
  */
  static RString formatCompilerSourceMsg(IN(RString) msg, IN(RString) file, int line, int level = Error);
};


} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_Logger_h
