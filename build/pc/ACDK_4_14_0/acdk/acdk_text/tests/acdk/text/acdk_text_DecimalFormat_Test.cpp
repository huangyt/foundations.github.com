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
// $Header: /cvsroot/acdk/acdk/acdk_text/tests/acdk/text/acdk_text_DecimalFormat_Test.cpp,v 1.13 2005/04/19 13:49:49 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>
#include <acdk/text/DecimalFormatSymbols.h>
#include <acdk/text/DecimalFormat.h>
#include <acdk/text/FieldPosition.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace text {

using namespace ::acdk::lang;
using namespace ::acdk::text;


  
BEGIN_DECLARE_TEST( DecimalFormat_Test )
  DECLARE_TEST( standard )

END_DECLARE_TEST( DecimalFormat_Test  )

BEGIN_DEFINE_TEST( DecimalFormat_Test )
  ADD_TEST( DecimalFormat_Test, standard ) 

END_DEFINE_TEST( DecimalFormat_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);
USING_CLASS(::acdk::util::, Locale);


RString testDfs(jlong num, RString pattern)
{
  DecimalFormat df(new DecimalFormatSymbols(Locale::getUS()));
  df.applyPattern(pattern);
  StringBuffer sb;
  df.format(num, &sb, new FieldPosition(0));
  System::out->println(Long::toString(num) + "] + [" + pattern + "] -> [" + sb.toString() + "]");
  return sb.toString();
}

RString testDfs(double num, RString pattern)
{
  DecimalFormat df(new DecimalFormatSymbols(Locale::getUS()));
  df.applyPattern(pattern);
  StringBuffer sb;
  df.format(num, &sb, new FieldPosition(0));
  System::out->println(Double::toString(num) + "] + [" + pattern + "] -> [" + sb.toString() + "]");
  return sb.toString();
}




//static
void
DecimalFormat_Test::standard()
{
  testAssert(testDfs(jlong(1234), "###00.00")->equals("1234.00"));
  testAssert(testDfs(jlong(123456), "##,#00.00")->equals("123,456.00"));
  testAssert(testDfs(jlong(-123456), "##,#00.00")->equals("-123,456.00"));
#if !defined(__BORLANDC__) // #### test crashes on Borland C++
  System::out->println("Should be 123.45: " + testDfs(double(123.456), "###00.00"));
  testAssert(testDfs(double(123.456), "###00.00")->equals("123.45"));
  testAssert(testDfs(double(123.456), "###00.0000000")->equals("123.4560000"));
  // does not work testAssert(testDfs(Double::NaN, "###00.0000000")->equals("NAN"));
  testAssert(testDfs(-0.01234456, "%###,###.00")->equals("-%1.23"));
  testAssert(testDfs(-0.01234456, "###,###.00%")->equals("-1.23%"));
#endif // !defined(__BORLANDC__) 
  
}



} // namespace text
} //namespace acdk 
} //namespace tests

