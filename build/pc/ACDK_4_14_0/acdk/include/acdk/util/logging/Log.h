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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/Log.h,v 1.13 2005/04/09 19:26:59 kommer Exp $

#ifndef acdk_util_logging_Log_h
#define acdk_util_logging_Log_h

#include <acdk.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Boolean.h>

#include "Level.h"
#include "LogConsumer.h"
#include "LogRecord.h"
#include "Logger.h"
#include "LogManager.h"

/**
  @defgroup acdklogging ACDK Logging classes/functions/macros
  @see gw_ref[acdk_tools_aunit_man]
*/

#if !defined(DOXYGENONLY)

template <class T>
inline
RObject toObject(const RefHolder<T>& t)
{
  return (RObject)&t;
}

template <class T>
inline
RObject toObject(const InterfaceHolder<T>& t)
{
  return (RObject)t;
}

inline 
RObject toObject(bool v) { return new acdk::lang::Boolean(v); }
inline 
RObject toObject(char v) { return new acdk::lang::Character(v); }
/*
inline 
RObject toObject(byte v) { return new acdk::lang::Byte(v); }
*/
inline 
RObject toObject(short v) { return new acdk::lang::Short(v); }
inline 
RObject toObject(int v) { return new acdk::lang::Integer(v); }
inline 
RObject toObject(jlong v) { return new acdk::lang::Long(v); }
inline 
RObject toObject(float v) { return new acdk::lang::Float(v); }
inline 
RObject toObject(double v) { return new acdk::lang::Double(v); }


inline RString classNameAsIdentifier(IN(RString) cn)
{
  return cn->replace('/', '.');
}
#endif //!defined(DOXYGENONLY)

/**
  Write a Log message to the root logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @ingroup acdklogging
*/
#define ACDK_LOG(level, msg) \
do { \
if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true)  \
{ \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getRootLogger())->doLog(::acdk::util::logging::level) == true) \
    { \
      ::acdk::lang::StringBuffer _sb_internal; \
       _sb_internal << msg; \
      logger->log(::acdk::util::logging::level, Nil, _sb_internal.toString(), __FILE__, __LINE__); \
    } \
} \
} while (false)


/**
  Write a Log message to the root logger
  @param level int Value of acdk::util::logging::Level
  @param msg String message
  @ingroup acdklogging
*/
#define ACDK_ILOG(level, msg) \
do { \
if (::acdk::util::logging::LogManager::doLog(level) == true)  \
{ \
   ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getRootLogger())->doLog(level) == true) \
    { \
      ::acdk::lang::StringBuffer _sb_internal; \
       _sb_internal << msg; \
      logger->log(level, Nil, _sb_internal.toString(), __FILE__, __LINE__); \
    } \
} \
} while (false)


/**
  Write a Log message with named parameters to the root logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @param namedargs NamedArgs
  @see NamedArgs
  @ingroup acdklogging
*/
#define ACDK_LOGP(level, msg, namedargs) \
do { \
if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true)  \
{ \
  ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getRootLogger())->doLog(::acdk::util::logging::level) == true) \
    { \
      ::acdk::lang::StringBuffer _sb_internal; \
      _sb_internal << msg; \
      ::acdk::util::logging::LogManager::getRootLogger()->log(::acdk::util::logging::level, Nil, _sb_internal.toString(), LOG_NPS(namedargs), __FILE__, __LINE__); \
    } \
} \
} while (false)

/**
  Write a Log message with named parameters to the root logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @param namedargs NamedArgs
  @see NamedArgs
  @ingroup acdklogging
*/
#define ACDK_ILOGP(level, msg, namedargs) \
do { \
if (::acdk::util::logging::LogManager::doLog(level) == true)  \
{ \
  ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(::level) == true) \
    { \
      ::acdk::lang::StringBuffer _sb_internal; \
      _sb_internal << msg; \
      ::acdk::util::logging::LogManager::getRootLogger()->log(level, Nil, _sb_internal.toString(), LOG_NPS(namedargs), __FILE__, __LINE__); \
    } \
} \
} while (false)


/**
  Write Log message to a named Logger
  @param name Name of the Logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @ingroup acdklogging
*/
#define ACDK_NLOG(name, level, msg) \
do { \
  if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true) { \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(::acdk::util::logging::level) == true) \
    { \
      ::acdk::lang::StringBuffer _sb_internal; \
      _sb_internal << msg; \
      logger->log(::acdk::util::logging::level, name, _sb_internal.toString(), __FILE__, __LINE__); \
    } \
  } \
} while (false)

/**
  Write Log message to to multiple Logger
  ACDK_MLOG(("a", "b"), Info, "asdf");
  @param names list of Name of the Logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @ingroup acdklogging
*/
#define ACDK_NxLOG(names, level, msg) \
do { \
  if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true) { \
      ::acdk::lang::StringBuffer _sb_internal; \
      _sb_internal << msg; \
      core_vector<const char*> _vec_internal = make_core_vector names; \
      ::acdk::util::logging::RLogger logger; \
      for (int _i_internal = 0; _i_internal < _vec_internal.size(); ++_i_internal) \
      { \
        if ((logger = ::acdk::util::logging::LogManager::getLogger(_vec_internal[_i_internal])))->doLog(::acdk::util::logging::level) == true) \
        { \
          logger->log(::acdk::util::logging::level, _vec_internal[_i_internal], _sb_internal.toString(), __FILE__, __LINE__); \
        } \
    } \
  } \
} while (false)

/**
  write log message in multiple logger
  @ingroup acdklogging
*/
#define ACDK_N2LOG(name1, name2, level, msg) ACDK_NxLOG(((const char*)name1, (const char*)name2), level, msg)
/**
  write log message in multiple logger
  @ingroup acdklogging
*/
#define ACDK_N3LOG(name1, name2, name3, level, msg) ACDK_NxLOG(((const char*)name1, (const char*)name2, (const char*)name3), level, msg)


/**
  Write Log message to a named Logger
  @param name Name of the Logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @ingroup acdklogging
*/
#define ACDK_INLOG(name, level, msg) \
do { \
  if (::acdk::util::logging::LogManager::doLog(level) == true) { \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(level) == true) \
    { \
      logger->log(level, name, msg, __FILE__, __LINE__); \
    } \
  } \
} while (false)

/**
  Write a Log message with named parameters to the root logger
  @param name Name of the Logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @param namedargs NamedArgs
  @see NamedArgs
  @ingroup acdklogging
*/
#define ACDK_NLOGP(name, level, msg, namedargs) \
do { \
  if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true) { \
    ::acdk::lang::StringBuffer _sb_internal; \
      _sb_internal << msg; \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(::acdk::util::logging::level) == true) \
    { \
      logger->log(::acdk::util::logging::level, name, msg, LOG_NPS(namedargs), __FILE__, __LINE__); \
    } \
  } \
} while (false)


/**
  Write Log message to to multiple Logger
  ACDK_MLOG(("a", "b"), Info, "asdf");
  @param names list of Name of the Logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @ingroup acdklogging
*/
#define ACDK_NxLOGP(names, level, msg, namedargs) \
do { \
  if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true) { \
      ::acdk::lang::StringBuffer _sb_internal; \
      _sb_internal << msg; \
      core_vector<const char*> _vec_internal = make_core_vector names; \
    ::acdk::util::logging::RLogger logger; \
      NamedLogArgs _np_internal  << stream; \
      for (int _i_internal = 0; _i_internal < _vec_internal.size(); ++_i_internal) \
    { \
        if ((logger = ::acdk::util::logging::LogManager::getLogger(_vec_internal[_i_internal]))->doLog(::acdk::util::logging::level) == true) \
        { \
          logger->log(::acdk::util::logging::level, _vec_internal[_i_internal], _sb_internal.toString(), _np_internal,  __FILE__, __LINE__); \
        } \
    } \
  } \
} while (false)

/**
  write log message in multiple logger
  @ingroup acdklogging
*/
#define ACDK_N2LOGP(name1, name2, level, msg, namedargs) ACDK_NxLOGP(((const char*)name1, (const char*)name2), level, msg, namedargs)
/**
  write log message in multiple logger
  @ingroup acdklogging
*/
#define ACDK_N3LOGP(name1, name2, name3, level, msg, namedargs) ACDK_NxLOGP(((const char*)name1, (const char*)name2, (const char*)name3), level, msg, namedargs)


/**
  Write a Log message with named parameters to the root logger
  @param name Name of the Logger
  @param level Value of acdk::util::logging::Level
  @param msg String message
  @param namedargs NamedArgs
  @see NamedArgs
  @ingroup acdklogging
*/
#define ACDK_INLOGP(name, level, msg, namedargs) \
do { \
  if (::acdk::util::logging::LogManager::doLog(level) == true) { \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(level) == true) \
    { \
      ::acdk::lang::StringBuffer _sb_internal; \
      _sb_internal << msg; \
      logger->log(level, name, _sb_internal.toString(), namedargs, __FILE__, __LINE__); \
    } \
  } \
} while (false)


/** 
  these are no longer supported
#define ACDK_NLOGF1(name, level, msg, Arg1) \
do { \
  if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true) { \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(::acdk::util::logging::level) == true) \
    { \
      logger->log(::acdk::util::logging::level, name, acdk::lang::String::sprintf(msg, Arg1), __FILE__, __LINE__); \
    } \
  } \
} while (false)

#define ACDK_NLOGF2(name, level, msg, Arg1, Arg2) \
do { \
  if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true) { \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(::acdk::util::logging::level) == true) \
    { \
      logger->log(::acdk::util::logging::level, name, acdk::lang::String::sprintf(msg, Arg1, Arg2), __FILE__, __LINE__); \
    } \
  } \
} while (false)

#define ACDK_NLOGF3(name, level, msg, Arg1, Arg2, Arg3) \
do { \
  if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true) { \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(::acdk::util::logging::level) == true) \
    { \
      logger->log(::acdk::util::logging::level, name, acdk::lang::String::sprintf(msg, Arg1, Arg2, Arg3), __FILE__, __LINE__); \
    } \
  } \
} while (false)

#define ACDK_NLOGF4(name, level, msg, Arg1, Arg2, Arg3, Arg4) \
do { \
  if (::acdk::util::logging::LogManager::doLog(::acdk::util::logging::level) == true) { \
    ::acdk::util::logging::RLogger logger; \
    if ((logger = ::acdk::util::logging::LogManager::getLogger(name))->doLog(::acdk::util::logging::level) == true) \
    { \
      logger->log(::acdk::util::logging::level, name, acdk::lang::String::sprintf(msg, Arg1, Arg2, Arg3, Arg4), __FILE__, __LINE__); \
    } \
  } \
} while (false)
*/

#endif //acdk_util_logging_Log_h
