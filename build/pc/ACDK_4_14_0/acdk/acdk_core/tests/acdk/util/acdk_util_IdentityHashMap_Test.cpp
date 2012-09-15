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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/acdk_util_IdentityHashMap_Test.cpp,v 1.3 2005/02/05 10:45:09 kommer Exp $


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
#include <acdk/util/IdentityHashMap.h>
#include <acdk/util/ResourceBundle.h>

#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace util {

  using namespace ::acdk::lang;
  using namespace ::acdk::io;
  using namespace ::acdk::util;

BEGIN_DECLARE_TEST( IdentityHashMap_Test )
  DECLARE_TEST( standard )
END_DECLARE_TEST( IdentityHashMap_Test  )

BEGIN_DEFINE_TEST( IdentityHashMap_Test )
  ADD_TEST( IdentityHashMap_Test, standard ) 
END_DEFINE_TEST( IdentityHashMap_Test )




void IdentityHashMap_Test::standard()
{
  RString f1 = "first";
  RString f2 = "first";
  RString v1 = "FirstVal1";
  RString v2 = "FirstVal1";
  {
    RIdentityHashMap map = new IdentityHashMap();
    map->put(&f1, &v1);
    map->put(&f2, &v2);
    testAssert(map->size() == 2);
    testAssert(map->get(&f1) == v1);
  }
  {
    RIdentityHashMap map = new IdentityHashMap();
    map->put(&f1->intern(), &v1);
    map->put(&f2->intern(), &v2);
    testAssert(map->size() == 1);
    testAssert(map->get(&f1->intern()) == v2);
  }
}


} // namespace util
} // namespace acdk
} // namespace tests

