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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/acdk_util_Locale_Test.cpp,v 1.2 2005/03/19 09:05:42 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/util/Date.h>
#include <acdk/util/SysDate.h>
#include <acdk/util/GregorianCalendar.h>
#include <acdk/util/TimeZone.h>
#include <acdk/util/SimpleTimeZone.h>
#include <acdk/util/ResourceBundle.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace util {

using namespace ::acdk::lang;
using namespace ::acdk::util;


  
BEGIN_DECLARE_TEST( Locale_Test )
  DECLARE_TEST( standard )
  
END_DECLARE_TEST( Locale_Test  )

BEGIN_DEFINE_TEST( Locale_Test )
  ADD_TEST( Locale_Test, standard ) 
END_DEFINE_TEST( Locale_Test )


//static
void
Locale_Test::standard()
{
  RLocale loc = Locale::getDefault();
  System::out->println("Your system is running with following locale: " + loc->getDisplayName());
  
  RLocaleArray la = Locale::getAvailableLocals();
  for (int i = 0; i < la->length(); ++i)
  {
    RLocale locale = la[i];
    System::out->println("Locale: " + locale->toString());
    RResourceBundle resb =  ResourceBundle::getBundle("acdk.locale.LocaleInfo", locale);
    RString inf = resb->getString("infinity");
    System::out->println("infinity: " + inf + "; " + (int)inf->charAt(0));
    RStringArray month = resb->getStringArray("month");
    System::out->println("month: " + month->toString());
    if ((i % 10) == 0)
      ResourceBundle::flushResourceBundleCache();
  }

  
}

} // namespace util
} //namespace acdk 
} //namespace tests

