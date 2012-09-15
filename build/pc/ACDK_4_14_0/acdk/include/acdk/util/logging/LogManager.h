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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/LogManager.h,v 1.19 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_util_logging_LogManager_h
#define acdk_util_logging_LogManager_h

#include "Level.h"
#include "Formatter.h"

namespace acdk {
namespace util {
namespace logging {


ACDK_DECL_CLASS(LogManager);
ACDK_DECL_CLASS(Logger);
ACDK_DECL_INTERFACE(LogConsumer);
ACDK_DECL_INTERFACE(Formatter);

/**
  central instance to manage logging.
  Normally this class has not to used directly in code.
  @author Roger Rene Kommer
  @ingroup acdklogging
*/
class ACDK_CORE_PUBLIC LogManager
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(LogManager)
public:
  /**
    returns the RootLogger
  */
  static INOUT(RLogger) getRootLogger();
  /**
    returns a Logger with no consumer
  */
  static INOUT(RLogger) getNullLogger();
  /** 
    returns Logger configured or registered
    if name has '.' the parent logger will be searched
    @code
      name = "acdk.aci.Parser"
      getLogger searches:
        acdk.aci.Parser return if found
        acdk.aci return if found
        acdk return if found
        return RootLogger
    @endcode
  */
  static RLogger getLogger(IN(RString) name);
  /**
    return existant Logger or create a new one
  */
  static RLogger getCreateLogger(IN(RString) name);

  static void registerLogger(IN(RLogger) logger);
  static void deregisterLogger(IN(RLogger) logger);

  static void init_configure(INOUT(RLogger) rootlogger);
  
  /**
    All Message below this level has no active Logger
    @see LogLevel
  */
  static int MinLevel;
  /**
    throw away all log message bolow this
    LogLevel
    @see LogLevel
  */
  static int Threshold;
  /**
    true if Threshold already forced by command line
    
  */
  static bool TresholdSet;
  inline static bool doLog(int level)
  {
    return level >= MinLevel && level >= Threshold;
  }
  static OUTP(RFormatter) getStandardFormatter();
  static void setStandardFormatter(INP(RFormatter) formatter);
  
  /**
    Parse the command line for Logging related options
    @param args command line options
    @return the args but without the logging options
  */
  static RStringArray parseCommandLine(IN(RStringArray) args);
};


} // namespace logging
} // namespace util
} // namespace acdk

#endif //acdk_util_logging_LogManager_h
