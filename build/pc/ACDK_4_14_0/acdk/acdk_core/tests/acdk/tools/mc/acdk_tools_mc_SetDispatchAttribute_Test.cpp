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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/tools/mc/acdk_tools_mc_SetDispatchAttribute_Test.cpp,v 1.6 2005/04/13 13:34:51 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>

#include <acdk/util/Date.h>
#include <acdk/util/SysDate.h>
#include <acdk/util/GregorianCalendar.h>
#include <acdk/util/TimeZone.h>
#include <acdk/util/SimpleTimeZone.h>

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/util/logging/Logger.h>
#include <acdk/util/logging/ConsoleConsumer.h>
#include <acdk/util/logging/SimpleFormatter.h>
#include <acdk/util/logging/LogManager.h>

#include <acdk/tools/mc/DispatchForwardAttributeTest.h>

namespace tests {
namespace acdk {
namespace tools {
namespace mc {



BEGIN_DECLARE_TEST( SetDispatchAttribute_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( SetDispatchAttribute_Test  )

BEGIN_DEFINE_TEST( SetDispatchAttribute_Test )
  ADD_TEST( SetDispatchAttribute_Test, standard ) 
END_DEFINE_TEST( SetDispatchAttribute_Test )


using namespace ::acdk::util::logging;
using namespace ::acdk::tools::mc;

void 
SetDispatchAttribute_Test::standard()
{
  RDispatchForwardAttributeTest d = new DispatchForwardAttributeTest(new String(""));
  RObject o = (RObject)d->peek("dummyvar");
  testAssert(o->equals(new Integer(42)) == true);
}


} // namespace mc
} // namespace tools
} //namespace acdk 
} //namespace tests

