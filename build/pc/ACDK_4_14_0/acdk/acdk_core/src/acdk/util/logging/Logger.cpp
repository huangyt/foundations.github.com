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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/Logger.cpp,v 1.15 2005/02/21 16:57:11 kommer Exp $



#include "Logger.h"
#include "LogManager.h"

namespace acdk {
namespace util {
namespace logging {

Logger::Logger(IN(RString) name)
: Object()
, _minLevel(None)
, _name(name)
, _consumer(new LogConsumerArray(0))
, _inheritConsumer(false)
{
  ACDK_SAFE_CONSTRUCTOR();
  ASCLITERAL(Root);
  if (name != Nil && name->equals(lit_Root) == false)
    LogManager::registerLogger(this);
}

Logger::Logger(INP(RString) name, int minLevel)
: Object()
, _minLevel(minLevel)
, _name(name)
, _consumer(new LogConsumerArray(0))
, _inheritConsumer(false)
{
  ACDK_SAFE_CONSTRUCTOR();
  ASCLITERAL(Root);
  if (name != Nil && name->equals(lit_Root) == false)
    LogManager::registerLogger(this);
}

Logger::~Logger()
{
  //LogManager::deregisterLogger(this);
}

void 
Logger::addConsumer(INP(RLogConsumer) cons)
{
  if (cons == Nil)
    THROW1(Exception, "acdk::util::logging::Logger: Try to add Nil LogConsumer");
  int nml = cons->getMinLevel();
  if (nml < LogManager::MinLevel)
    LogManager::MinLevel = nml;
  if (nml < _minLevel)
    _minLevel = nml;
  _consumer->append(cons);
}

void 
Logger::log(int level, INP(RString) cat, INP(RString) text)
{
  log(new LogRecord(level, cat == Nil ? _name : cat, text));
}

void 
Logger::log(int level, INP(RString) cat, INP(RString) text, const char* file, int line)
{
  log(new LogRecord(level, cat == Nil ? _name : cat, text, file, line));
}

void 
Logger::log(int level, INP(RString) cat, INP(RString) text, INP(RNamedParameterArray) args, const char* file, int line)
{
  log(new LogRecord(level, cat == Nil ? _name : cat, text, args, file, line));
}

void 
Logger::log(INP(RLogRecord) logrecord)
{
  int logcsize = _consumer->length();
  for (int i = 0; i < logcsize; ++i)
  {
    RLogConsumer cons = _consumer[i];
    int level = logrecord->level;
    if (level >= cons->getMinLevel() && level <= cons->getMaxLevel())
      cons->publish(logrecord);
  }
}

//static 
RString 
Logger::formatCompilerSourceMsg(IN(RString) msg, IN(RString) file, int line, int level)
{
#if defined(_MSC_VER)
  return SBSTR(file << "(" << line << "): " << msg);
#else
  return SBSTR(file << "," << line << ": " << msg);
#endif
}

} // namespace logging
} // namespace util
} // namespace acdk


