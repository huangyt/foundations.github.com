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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/logging/acdk_util_logging_RollingFileAppender_Test.cpp,v 1.4 2005/02/05 10:45:09 kommer Exp $


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
#include <acdk/util/logging/RollingFileConsumer.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace util {
namespace logging {

using namespace ::acdk::lang;
using namespace ::acdk::util::logging;


  
BEGIN_DECLARE_TEST( RollingFileAppender_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( RollingFileAppender_Test  )

BEGIN_DEFINE_TEST( RollingFileAppender_Test )
  ADD_TEST( RollingFileAppender_Test, standard ) 
  
END_DEFINE_TEST( RollingFileAppender_Test )


//static RLogger mylogger = LogManager::getLogger("acdk.logger.test");
//static
void
RollingFileAppender_Test::standard()
{
  RLogger log = ::acdk::util::logging::LogManager::getCreateLogger("acdk.logger.test.rolling");
  RRollingFileConsumer rfc = new RollingFileConsumer("_LogTest", 4, 2);
  rfc->setFormatter(new StdFormatter());
  log->addConsumer(&rfc);
  
  for (int i = 0; i < 100; ++i)
  {
    ACDK_NLOG("acdk.logger.test.rolling", Fatal, "Blub");
  }

}

} // namespace logging
} // namespace util
} //namespace acdk 
} //namespace tests

