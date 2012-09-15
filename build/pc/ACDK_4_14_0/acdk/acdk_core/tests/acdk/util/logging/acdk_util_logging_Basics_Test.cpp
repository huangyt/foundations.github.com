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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/logging/acdk_util_logging_Basics_Test.cpp,v 1.13 2005/02/05 10:45:09 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>

#include <acdk/util/Date.h>
#include <acdk/util/SysDate.h>
#include <acdk/util/GregorianCalendar.h>
#include <acdk/util/TimeZone.h>
#include <acdk/util/SimpleTimeZone.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/StdFormatter.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace util {
namespace logging {

using namespace ::acdk::lang;
using namespace ::acdk::util::logging;


  
BEGIN_DECLARE_TEST( Logging_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( parameters )
  DECLARE_TEST( hierarchical )
END_DECLARE_TEST( Logging_Test  )

BEGIN_DEFINE_TEST( Logging_Test )
  ADD_TEST( Logging_Test, standard ) 
  ADD_TEST( Logging_Test, parameters ) 
  ADD_TEST( Logging_Test, hierarchical ) 
  
END_DEFINE_TEST( Logging_Test )


//static RLogger mylogger = LogManager::getLogger("acdk.logger.test");
//static
RLogger createLogger()
{
  static RLogger log;
  if (log != Nil)
    return log;
  log = ::acdk::util::logging::LogManager::getCreateLogger("acdk.logger.test");
  log->addConsumer(new ::acdk::util::logging::ConsoleConsumer(new ::acdk::util::logging::StdFormatter()));
  return log;
}

void
Logging_Test::standard()
{
  ACDK_LOG(Fatal, "Bla");
  RLogger log = createLogger();
  log->log(::acdk::util::logging::Fatal, "", "Bla", __FILE__, __LINE__);
  ACDK_NLOG("acdk.logger.test", Fatal, "Blub");

}
void
Logging_Test::parameters()
{
  RString astr = new String("<string value>");
  RInteger intval = new Integer(42);
  RComparable comparable = &intval;
  createLogger();

  ACDK_NLOGP("acdk.logger.test", Error, "A Message with 1 parameters", 
                                    LOG_NPV(String, astr));
  ACDK_NLOGP("acdk.logger.test", Error, "A Message with 1 parameters", LOG_NPV(int, 42));
  ACDK_NLOGP("acdk.logger.test", Error, "A Message with 1 parameters", LOG_NPV(Comparable, comparable));
  ACDK_NLOGP("acdk.logger.test", Error, "A Message with 1 parameters", LOG_NPV(boolean, true));

  ACDK_NLOGP("acdk.logger.test", Error, "A Message with 2 parameters", 
                                      LOG_NPV(String, astr) <<  LOG_NPV(Integer, intval));
  ACDK_NLOGP("acdk.logger.test", Error, "A Message with 2 parameters", 
                                      LOG_NPV(String, astr) << LOG_NPV(Integer, intval));
}

void
Logging_Test::hierarchical()
{
  createLogger();
  ACDK_NLOG("acdk.logger.test", Error, "Should appear");
  ACDK_NLOG("acdk.logger.test.also", Error, "Should appear also");
  ACDK_NLOG("acdk.logger.test.also.to", Error, "Should appear also");
  ACDK_NLOG("acdk.logger", Error, "Should not appear");
  ACDK_NLOG("acdk.logger.other", Error, "Should not appear");
}

} // namespace logging
} // namespace util
} //namespace acdk 
} //namespace tests

