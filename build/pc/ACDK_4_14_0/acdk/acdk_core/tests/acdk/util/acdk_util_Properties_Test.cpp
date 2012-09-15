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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/acdk_util_Properties_Test.cpp,v 1.8 2005/04/06 10:03:51 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/io/FileWriter.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/io/File.h>

#include <acdk/util/Properties.h>
#include <acdk/util/PropertiesListener.h>

#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace util {

  using namespace ::acdk::lang;
  using namespace ::acdk::io;
  using namespace ::acdk::util;

BEGIN_DECLARE_TEST( Properties_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( propertyListener )
  DECLARE_TEST( parser )
  DECLARE_TEST( eval )
END_DECLARE_TEST( Properties_Test  )

BEGIN_DEFINE_TEST( Properties_Test )
  ADD_TEST( Properties_Test, standard ) 
  ADD_TEST( Properties_Test, propertyListener ) 
  ADD_TEST( Properties_Test, parser ) 
  ADD_TEST( Properties_Test, eval ) 
  
END_DEFINE_TEST( Properties_Test )


void Properties_Test::standard()
{

}

void Properties_Test::propertyListener()
{
  RString filename = "acdk_util_Properties_Test.cfg";
  ::acdk::util::RProperties props = new ::acdk::util::Properties();
  props->setProperty("AKey", "AValue");
  {
    ::acdk::io::FileWriter fout(filename);
    props->store(&fout, "Test for Properties");
  }
  RThread th = new PropertiesListener(filename, props);
  th->start();
  {
    ::acdk::util::RProperties props = new ::acdk::util::Properties();
    props->setProperty("BKey", "BValue");
    {
      ::acdk::io::FileWriter fout(filename);
      props->store(&fout, "Test for Properties");
    }
  }
  bool interactive = false; // ### for test purpose this should be true
  if (interactive == true)
  {
    System::out->println("Enter to continue...");
    System::in->readLine();
    RString v = props->getProperty("AKey", Nil);
    v = props->getProperty("BKey", Nil);
  }

}

void
Properties_Test::parser()
{
  RProperties props = new Properties();
  ::acdk::io::File f(System::getAcdkHome() + "/cfg/tests/acdk/util/Properties_Test.properties");
  props->load(f.getReader());
  RString v = props->getProperty("commentAsValue");
}

void
Properties_Test::eval()
{
  RString res;
  res = System::getProperties()->eval("$(ACDKHOME)/csf");
  res = System::getProperties()->eval("x$(ACDKHOME)/csf");
  
  {
    RProperties p = new Properties();
    p->setProperty("XNEST", "$(X) ");
    p->setProperty("X", "x");
    p->setProperty("Y", "abcdefg");
    p->setProperty("EMPTY", "");
    res = p->eval(" $(XNEST)", false);
    testAssert(res->equals(" $(X) ") == true);
    res = p->eval(" $(XNEST)", true);
    testAssert(res->equals(" x ") == true);
    res = p->eval("$(X)$(Y)", true);
    testAssert(res->equals("xabcdefg") == true);
    res = p->eval("a$(EMPTY)b", true);
    testAssert(res->equals("ab") == true);
    res = p->eval("a$(DOESNOTEXISTS)b", true);
    testAssert(res->equals("a$(DOESNOTEXISTS)b") == true);
    
  }
}

} // namespace util
} // namespace acdk
} // namespace tests

