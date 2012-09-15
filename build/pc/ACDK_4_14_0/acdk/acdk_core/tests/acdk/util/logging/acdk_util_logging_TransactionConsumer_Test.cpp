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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/logging/acdk_util_logging_TransactionConsumer_Test.cpp,v 1.5 2005/02/05 10:45:09 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>

#include <acdk/util/Date.h>
#include <acdk/util/SysDate.h>
#include <acdk/util/GregorianCalendar.h>
#include <acdk/util/TimeZone.h>
#include <acdk/util/SimpleTimeZone.h>
#include <acdk/util/logging/Log.h>
#include <acdk/util/logging/StdFormatter.h>
#include <acdk/util/logging/TransactionConsumer.h>
#include <acdk/util/logging/ConsoleConsumer.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace util {
namespace logging {

using namespace ::acdk::lang;
using namespace ::acdk::util::logging;


  
BEGIN_DECLARE_TEST( TransactionConsumer_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( exception )
END_DECLARE_TEST( TransactionConsumer_Test  )

BEGIN_DEFINE_TEST( TransactionConsumer_Test )
  ADD_TEST( TransactionConsumer_Test, standard ) 
  ADD_TEST( TransactionConsumer_Test, exception ) 
  
END_DEFINE_TEST( TransactionConsumer_Test )


//static RLogger mylogger = LogManager::getLogger("acdk.logger.test");
//static
void
TransactionConsumer_Test::standard()
{
  RLogger log = ::acdk::util::logging::LogManager::getCreateLogger("acdk.logger.test.transact");
  RConsoleConsumer lc = new ConsoleConsumer();
  lc->setFormatter(new StdFormatter());
  RLogConsumer tlc = new TransactionConsumer(&lc, Warn);
  log->addConsumer(&tlc);
  ACDK_NLOG("acdk.logger.test.transact", Info, "Blub1");
  ACDK_NLOG("acdk.logger.test.transact", Info, "Blub2");
  ACDK_NLOG("acdk.logger.test.transact", TransCommit, "Blub2");
  {
    ACDK_NLOG("acdk.logger.test.transact", TransBegin, "Sub1");
    ACDK_NLOG("acdk.logger.test.transact", Info, "Blub2a");
    ACDK_NLOG("acdk.logger.test.transact", TransCommit, "Blub2b");
  
  }
  ACDK_NLOG("acdk.logger.test.transact", Info, "Blub3");
  ACDK_NLOG("acdk.logger.test.transact", Info, "Blub4");
  ACDK_NLOG("acdk.logger.test.transact", Error, "An error");
}

/**
  

*/

void
TransactionConsumer_Test::exception()
{
  RLogger log = ::acdk::util::logging::LogManager::getCreateLogger("acdk.logger.test.transact.1");
  RConsoleConsumer lc = new ConsoleConsumer();
  lc->setFormatter(new StdFormatter());
  RLogConsumer tlc = new TransactionConsumer(&lc, Warn);
  log->addConsumer(&tlc);

  ACDK_TRANSLOG_BEGIN("acdk.logger.test.transact.1", "Transact");
  ACDK_NLOG("acdk.logger.test.transact.1", Info, "Blub2a");
  ACDK_TRANSLOG_COMMIT();
  try {
    ACDK_TRANSLOG_BEGIN("acdk.logger.test.transact.1", "Transact 2");
    ACDK_NLOG("acdk.logger.test.transact.1", Info, "Blub3");
    ACDK_NLOG("acdk.logger.test.transact.1", Info, "Blub4");
    THROW1(Exception, "Just for Test");
    ACDK_TRANSLOG_COMMIT();
  } catch (RThrowable ) {
  }
}

} // namespace logging
} // namespace util
} //namespace acdk 
} //namespace tests

