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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/util/acdk_util_StringTokenizer_Test.cpp,v 1.12 2005/02/05 10:45:09 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>

#include <acdk/io/MemReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/util/StringTokenizer.h>

#include <acdk/util/Arrays.h>
#include <acdk/util/ArrayList.h>

#include <acdk/tools/aunit/core_test.h>
#include <acdk/tools/aunit/TestRunner.h>

namespace tests {
namespace acdk {
namespace util {

  using namespace ::acdk::lang;
  using namespace ::acdk::io;
  using namespace ::acdk::util;

BEGIN_DECLARE_TEST( StringTokenzier_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( countTokens )
  DECLARE_TEST( moreTokens )
  DECLARE_TEST( javaTokenizer )
  
  ::acdk::util::RCollection fill(::acdk::util::Iterator& it);
END_DECLARE_TEST( StringTokenzier_Test  )

BEGIN_DEFINE_TEST( StringTokenzier_Test )
  ADD_TEST( StringTokenzier_Test, standard ) 
  ADD_TEST( StringTokenzier_Test, countTokens ) 
  ADD_TEST( StringTokenzier_Test, moreTokens ) 
  ADD_TEST( StringTokenzier_Test, javaTokenizer ) 
END_DEFINE_TEST( StringTokenzier_Test )


::acdk::util::RCollection StringTokenzier_Test::fill(::acdk::util::Iterator& it)
{
  ::acdk::util::RArrayList al = new ::acdk::util::ArrayList(0);
  while (it.hasNext() == true) 
  {
    al->add(it.next());
  }
  return (::acdk::util::RCollection)al;
}

void StringTokenzier_Test::standard()
{
  RString text1 = "roger:rene:kommer";
  
  StringTokenizer st("roger:rene:kommer", ":", false);
  RCollection outtoken = fill(st);
  StringArray sa(3);
  sa[0] = "roger";
  sa[1] = "rene";
  sa[2] = "kommer";
  ::acdk::util::RCollection intokens 
    = (::acdk::util::RCollection)::acdk::util::ArraysImpl::asList((RObjectArray)&sa);
  testAssert(intokens->equals((RObject)outtoken) == true);
}

void StringTokenzier_Test::countTokens()
{
  RString text1 = "roger:rene:kommer";
  StringTokenizer st(text1, ":", false);
  testAssert(st.countTokens() == 3);
  st.nextToken();
  testAssert(st.countTokens() == 2);
  st.nextToken();
  testAssert(st.countTokens() == 1);
  st.nextToken();
  testAssert(st.countTokens() == 0);
}

void
StringTokenzier_Test::moreTokens()
{
  {
  RString text = "www//artefaktur//com//";
  StringTokenizer st(text, "//", false);
  testAssert(st.countTokens() == 3);
  }
  {
  RString text = "//www//artefaktur//com//";
  StringTokenizer st(text, "//", false);
  testAssert(st.countTokens() == 3);
  }
  {
  RString text = "//www//artefaktur//com//";
  StringTokenizer st(text, "//", true);
  testAssert(st.countTokens() == 7);
  }
  {
    RString text = "////";
    StringTokenizer st(text, "//", false);
    testAssert(st.countTokens() == 0);
  }
  {
    RString text = "////";
    StringTokenizer st(text, "//", true);
    testAssert(st.countTokens() == 2);
  }
}

void
StringTokenzier_Test::javaTokenizer()
{
  {
    RString text = "This is\t a\nTest";
    StringTokenizer st(text);
    testAssert(st.countTokens() == 4);
    while (st.hasNext() == true)
    {
      RString t = (RString)st.next();
    }
  }
}

} // namespace util
} // namespace acdk
} // namespace tests

