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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/tools/mc/acdk_tools_mc_Misc_Test.cpp,v 1.5 2005/02/05 10:45:09 kommer Exp $


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

namespace tests {
namespace acdk {
namespace tools {
namespace mc {



BEGIN_DECLARE_TEST( Misc_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( Misc_Test  )

BEGIN_DEFINE_TEST( Misc_Test )
  ADD_TEST( Misc_Test, standard ) 
  
END_DEFINE_TEST( Misc_Test )


void
getAltMethodPrefix(StringBuffer& sb, int i)
{
   if (i < 10)
    sb.append(i);
  else if (i < 26 + 10)
    sb.append(char('a' + i - 10));
  else if (i < 2 * 26 + 10)
    sb.append(char('A' + i - (10 + 26)));
  else
  {
    int next = i / (2 * 26 + 10);
    getAltMethodPrefix(sb, next);
    int rest = i % (2 * 26 + 10);
    getAltMethodPrefix(sb, rest);
  }
}

void testPrint(int num)
{
  StringBuffer sb;
  getAltMethodPrefix(sb, num);
  System::out->println(sb.toString() + ": " + num);
}

void 
Misc_Test::standard()
{
  testPrint(0);
  testPrint(1);
  testPrint(9);
  testPrint(10);
  testPrint(11);
  testPrint(36);
  testPrint(37);
  testPrint(38);
  testPrint(126);
  testPrint((26 * 2) + 10 + 1);
}

} // namespace mc
} // namespace tools
} //namespace acdk 
} //namespace tests

