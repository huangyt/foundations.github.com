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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/logging/LogManager.cpp,v 1.30 2005/05/02 23:07:20 kommer Exp $

#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include "LogManager.h"
#include "Logger.h"
#include "ConsoleConsumer.h"
#include "SimpleFormatter.h"
#include "Win32DbgConsumer.h"
#include "FileConsumer.h"
#include <acdk/util/THashMap.h>
#include <acdk/util/HashMap.h>
#include <acdk/lang/sys/core_system.h>

namespace acdk {
namespace util {
namespace logging {

  



  //static 
int LogManager::MinLevel = None;
int LogManager::Threshold = Warn;
bool LogManager::TresholdSet = false;


//#define USE_TYPED_HASHMAP
/*
#if ACDK_CHECK_GCC_VERSION(3, 0) && defined(USE_TYPED_HASHMAP)
# undef USE_TYPED_HASHMAP
#endif
*/

#if defined(USE_TYPED_HASHMAP)

typedef ::acdk::util::THashMap<RString, RLogger> StringToLoggerHashMap; 
typedef StringToLoggerHashMap::RefType RStringToLoggerHashMap;
ACDK_DECL_HASHMAP(String, RString, LogConsumer, RLogConsumer);

//static 
/// @internal
INOUTP(RStringToLogConsumerHashMap)
LogManager_getConsumerMap()
{
  ACDK_STATIC_INSTANCE0(StringToLogConsumerHashMap, consumer);
  return consumer;
}

/// @internal
//static 
INOUTP(RStringToLoggerHashMap)
LogManager_getLoggerMap()
{
  ACDK_STATIC_INSTANCE0(StringToLoggerHashMap, loggermap);
  return loggermap;
}
#else

INOUTP(RHashMap)
LogManager_getConsumerMap()
{
  ACDK_STATIC_INSTANCE0(HashMap, consumer);
  return consumer;
}


//static 
INOUTP(RHashMap)
LogManager_getLoggerMap()
{
  ACDK_STATIC_INSTANCE0(HashMap, loggermap);
  return loggermap;
}

#endif

//static 
INOUTP(RLogger)
LogManager::getRootLogger()
{

  ACDK_STATIC_INSTANCE2(Logger, const char*, "Root", int, None, rootLogger);
  if (rootLogger.isCreated())
    return rootLogger;
  if (sys::core_system::getState() == sys::AfterMain)
    return rootLogger;

  init_configure(rootLogger);
  return rootLogger;
}

//static 
INOUTP(RLogger) 
LogManager::getNullLogger()
{
  ACDK_STATIC_INSTANCE2(Logger, RString, RString("NullLogger"), int, None, nullLogger);
  return nullLogger;
}

namespace {

  /// @internal
OUTP(RFormatter) 
getStdFormater()
{
  static RFormatter format;
  return format;
}
} // anon namespace

//static 
OUTP(RFormatter) 
LogManager::getStandardFormatter()
{
  OUTP(RFormatter) frm = getStdFormater();
  if (frm == Nil)
    frm = new StdFormatter();
  return frm;
}

//static 
void 
LogManager::setStandardFormatter(INP(RFormatter) formatter)
{
  getStdFormater() = formatter;
}


//static 
void 
LogManager::init_configure(INOUT(RLogger) rootLogger)
{
  
  rootLogger = new Logger("Root", None);

  RProperties props = (RProperties)System::getProperties()->clone(); // clone it, because cfg may modify Property and make iterator invalid
  
  if (TresholdSet == false && props->getProperty("acdk.util.logging.threshold", Nil) != Nil)
    LogManager::Threshold = Level::parseLevel(props->getProperty("acdk.util.logging.threshold"));
  
  //acdk.util.logging.consumer.*
  RIterator it = props->propertyNames();
#if defined(USE_TYPED_HASHMAP)
  RStringToLogConsumerHashMap consumerMap = LogManager_getConsumerMap();
#else
  RHashMap consumerMap = LogManager_getConsumerMap();
#endif
  HashMap newConsumerMap;
  while (it->hasNext() == true)
  {
    RString n = (RString)it->next();
    if (n->startsWith("acdk.util.logging.consumer.") == true) 
    {
      RString consname = n->substr(strlen("acdk.util.logging.consumer."));
      RLogConsumer cons = (RLogConsumer)Class::forName(props->getProperty(n))->newInstance();
      cons->configure("acdk.util.logging.consumer_cfg." + consname, props);
#if defined(USE_TYPED_HASHMAP)
      consumerMap->put(consname, cons);
#else
      consumerMap->put((RObject)consname, (RObject)cons);
#endif
    }
  }
#if defined(USE_TYPED_HASHMAP)
  RStringToLoggerHashMap loggerMap = LogManager_getLoggerMap();
#else
  RHashMap loggerMap = LogManager_getLoggerMap();
#endif
  {
    RIterator it = props->propertyNames();
    while (it->hasNext() == true)
    {
      RObject o = it->next();
      if (instanceof(o, String) == false)
        continue;
      RString n = (RString)o;
      if (n->startsWith("acdk.util.logging.logger.") == false) 
        continue;
      RString loggername = n->substr(strlen("acdk.util.logging.logger."));
      int minlevel = Level::parseLevel(props->getProperty(n));
      RLogger logger = new Logger(loggername, minlevel);
      loggerMap->put(&loggername, &logger);
      if (MinLevel > minlevel)
        MinLevel = minlevel;
    }
  }  
  {
#if defined(USE_TYPED_HASHMAP)
    RStringIterator keys = loggerMap->keySet()->iterator();
#else
    RIterator keys = loggerMap->keySet()->iterator();
#endif
    while (keys->hasNext() == true)
    {
      RString k = (RString)keys->next();
      RLogger logger = (RLogger)loggerMap->get(&k);
      RString baskey = "acdk.util.logging.logger_cfg." + k + ".consumer";
      for (int i = 1; (k = props->getProperty(baskey + i, Nil)) != Nil; ++i)
      {
        RLogConsumer cons = (RLogConsumer)consumerMap->get(&k);
        if (cons != Nil)
          logger->addConsumer(cons);
      }
    }
  }
  RString rootname = "root";
  if (loggerMap->get(&rootname) != Nil)
    rootLogger =  (RLogger)loggerMap->get(&rootname);
  else
    rootLogger = new Logger("Root", None);
}

//static 
RLogger 
LogManager::getLogger(IN(RString) name)
{
  getRootLogger();
  RLogger logger = (RLogger)LogManager_getLoggerMap()->get(&name);
  if (logger != Nil)
    return logger;
  int idx = name->lastIndexOf('.');
  if (idx == -1)
    return getRootLogger();
  return getLogger(name->substr(0, idx));
}

//static 
RLogger 
LogManager::getCreateLogger(IN(RString) name)
{
  getRootLogger();
  RLogger logger = (RLogger)LogManager_getLoggerMap()->get(&name);
  if (logger != Nil)
    return logger;
  logger = new Logger(name);
  LogManager_getLoggerMap()->put(&logger->getName(), &logger);
  return logger;
}

//static 
void 
LogManager::registerLogger(IN(RLogger) logger)
{
  getRootLogger(); // force initialization
  LogManager_getLoggerMap()->put(&logger->getName(), &logger);
}

//static 
void 
LogManager::deregisterLogger(IN(RLogger) logger)
{
  LogManager_getLoggerMap()->remove(&logger->getName());
}

//static 
RStringArray 
LogManager::parseCommandLine(IN(RStringArray) args)
{
  RStringArray erg = new StringArray(0);
  RLogger log = getRootLogger();
  int loglevel = Threshold;
  for (int i = 0; i < args->length(); ++i)
  {
    RString s = args[i];
    /*if (s->startsWith("-log") == false)
    {
      erg->append(s);
    } 
    else
    {*/
    if (s->equals("-loglevel") == true)
    {
      ++i;
      
      int nll = Level::parseLevel(args[i]);
      if (nll < Threshold)
        Threshold = nll;
      loglevel = nll;
      TresholdSet = true;
    }
    else if (s->equals("-logcat") == true)
    {
      ++i;
      RString logCat = args[i];
      log = getCreateLogger(logCat);
    }
    else if (s->equals("-logto") == true)
    {
      ++i;
      RString target = args[i];
      
      if (target->equals("out") == true)
      {
        log->addConsumer(new ConsoleConsumer(new SimpleFormatter(), loglevel, None, false));
      }
      else if (target->equals("err") == true)
      {
        log->addConsumer(new ConsoleConsumer(new SimpleFormatter(), loglevel, None, true));
      }
      else if (target->equals("dbg") == true)
      {
        log->addConsumer(new Win32DbgConsumer());
      }
      else
      {
        log->addConsumer(new FileConsumer(target, new StdFormatter()));
      }
    }

    else
    {
      erg->append(s);
    }
    //}
  }
  return erg;
}

} // namespace logging
} // namespace util
} // namespace acdk


