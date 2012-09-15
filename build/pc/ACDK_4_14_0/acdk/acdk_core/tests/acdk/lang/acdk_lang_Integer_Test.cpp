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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Integer_Test.cpp,v 1.10 2005/02/05 10:45:08 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( Integer_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( toAndFromString )
  DECLARE_TEST( toHexString )
  DECLARE_TEST( toOctalString )
  DECLARE_TEST( toBinaryString )
  DECLARE_TEST( limits )
END_DECLARE_TEST( Integer_Test  )

BEGIN_DEFINE_TEST( Integer_Test )
  ADD_TEST( Integer_Test, standard ) 
  ADD_TEST( Integer_Test, toAndFromString ) 
  ADD_TEST( Integer_Test, toHexString ) 
  ADD_TEST( Integer_Test, toOctalString ) 
  ADD_TEST( Integer_Test, toBinaryString )
  ADD_TEST( Integer_Test, limits ) 
END_DEFINE_TEST( Integer_Test )

using namespace acdk::lang;


void 
Integer_Test::standard()
{
  Integer i(42);
  testAssert(i.intValue() == 42);
}

void 
Integer_Test::toAndFromString()
{
  RString str = RCS("42");
  int i = Integer::parseInt(str);
  testAssert(i == 42);
  i = Integer::parseInt(str, 16);
  testAssert(i == 66);
  i = Integer::parseInt(str, 8);
  testAssert(i == 34);
  i = Integer::parseInt(RCS("100010"), 2);
  testAssert(i == 34);

  RInteger integer = Integer::valueOf(RCS("0x42"));
  testAssert(integer->intValue() == 66);
  integer = Integer::valueOf(RCS("042"));
  testAssert(integer->intValue() == 34);
  
  testAssert(Integer::toString(34, 8)->equals("42"));
  testAssert(Integer::toString(66, 16)->equals("42"));

}

void
Integer_Test::toHexString()
{
  RString str = Integer::toHexString(11259375);
  testAssert(str->equals("abcdef") == true);

  str = Integer::toHexString(-11259375);
  testAssert(str->equals("100abcdef") == true);
}


void
Integer_Test::toOctalString()
{
  RString str = Integer::toOctalString(342391);
  testAssert(str->equals("1234567") == true);

  str = Integer::toOctalString(-342391);
  testAssert(str->equals("40001234567") == true);
}



void
Integer_Test::toBinaryString()
{
  RString str = Integer::toBinaryString(123456789);
  testAssert(str->equals("111010110111100110100010101") == true);

  str = Integer::toBinaryString(-123456789);
  testAssert(str->equals("100000111010110111100110100010101") == true);
}


void 
Integer_Test::limits()
{

  /* don't work on all machines, because NaN does not work ==
   testAssert(Integer(Integer::NaN).IntegerValue() == Integer::NaN);
  */

 
}


} // namespace lang 
} //namespace acdk 
} //namespace tests 



