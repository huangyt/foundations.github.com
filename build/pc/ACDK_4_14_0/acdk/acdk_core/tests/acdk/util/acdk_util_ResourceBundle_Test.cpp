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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/acdk_util_ResourceBundle_Test.cpp,v 1.6 2005/04/19 10:28:42 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/PrintWriter.h>

#include <acdk/util/Properties.h>
#include <acdk/util/PropertiesListener.h>
#include <acdk/util/ResourceBundle.h>

#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace util {

  using namespace ::acdk::lang;
  using namespace ::acdk::io;
  using namespace ::acdk::util;

BEGIN_DECLARE_TEST( ResourceBundle_Test )
  DECLARE_TEST( properties )
  DECLARE_TEST( localeInfo )
END_DECLARE_TEST( ResourceBundle_Test  )

BEGIN_DEFINE_TEST( ResourceBundle_Test )
  ADD_TEST( ResourceBundle_Test, properties ) 
  ADD_TEST( ResourceBundle_Test, localeInfo ) 
  
END_DEFINE_TEST( ResourceBundle_Test )




void ResourceBundle_Test::properties()
{
  
  RString resbundleName = "tests.acdk.util.ResourceBundleTest";
  RResourceBundle bundle = ResourceBundle::getBundle(resbundleName, Locale::getUS());
  RString s = bundle->getString("FirstKey");
  testAssert(bundle->getString("FirstKey")->equals("First Value") == true);
  testAssert(bundle->getString("SecondKey")->equals("Second Value") == true);

  bundle = ResourceBundle::getBundle(resbundleName, Locale::getGERMAN());
  testAssert(bundle->getString("FirstKey")->equals("Erster Wert") == true);
  testAssert(bundle->getString("SecondKey")->equals("Zweiter Wert") == true);
}

void
ResourceBundle_Test::localeInfo()
{
  RResourceBundle bundle = ResourceBundle::getBundle("acdk/locale/LocaleInfo", Locale::getFRENCH());
}

} // namespace util
} // namespace acdk
} // namespace tests

