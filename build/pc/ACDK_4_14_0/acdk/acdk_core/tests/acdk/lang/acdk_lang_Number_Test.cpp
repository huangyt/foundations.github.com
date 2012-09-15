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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Number_Test.cpp,v 1.9 2005/04/25 22:03:51 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>

#include <acdk/lang/Character.h>



namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( Number_Test )
  DECLARE_TEST( parseInts )
  DECLARE_TEST( parseFloats )
  DECLARE_TEST( parseDoubles )
  DECLARE_TEST( parseNumbers )
END_DECLARE_TEST( Number_Test  )

BEGIN_DEFINE_TEST( Number_Test )
  ADD_TEST( Number_Test, parseInts ) 
  ADD_TEST( Number_Test, parseFloats ) 
  ADD_TEST( Number_Test, parseDoubles ) 
  
  ADD_TEST( Number_Test, parseNumbers ) 
  
END_DEFINE_TEST( Number_Test )

using namespace acdk::lang;





void 
Number_Test::parseInts()
{
  RString s = "1234";
  bool tryOnly = true;
  char typeChar = 0;
  int ignoreTrailing = 0;
  testAssert(Number::decodeIntegerNumber("1234", tryOnly, typeChar, ignoreTrailing) == 1234);
  testAssert(Number::decodeIntegerNumber("-1234", tryOnly, typeChar, ignoreTrailing) == -1234);

  testAssert(Number::decodeIntegerNumber("-1234s", tryOnly, typeChar, ignoreTrailing) == -1234);
  testAssert(typeChar == 's');
  typeChar = 0;
  testAssert(Number::decodeIntegerNumber("-1234i", tryOnly, typeChar, ignoreTrailing) == -1234);
  testAssert(typeChar == 'i');
  typeChar = 0;
  
  testAssert(Number::decodeIntegerNumber("-1234l", tryOnly, typeChar, ignoreTrailing) == -1234);
  testAssert(typeChar == 'l');
  typeChar = 0;
  
}

void 
Number_Test::parseFloats()
{
  RString s = "1234";
  bool tryOnly = true;
  char typeChar = 0;
  int ignoreTrailing = 0;
  testAssert(Number::parseFloatNumber("1234", tryOnly, typeChar, ignoreTrailing) == 1234);
  testAssert(Number::parseFloatNumber("-1234", tryOnly, typeChar, ignoreTrailing) == -1234);
  testAssert(Number::parseFloatNumber("-1234.32", tryOnly, typeChar, ignoreTrailing) == -1234.32);
  testAssert(Number::parseFloatNumber("-1234.32e10", tryOnly, typeChar, ignoreTrailing) == -1234.32e10);
  testAssert(Number::parseFloatNumber("-1234.32e-10", tryOnly, typeChar, ignoreTrailing) == -1234.32e-10);
  
  

  Number::parseFloatNumber("fdsa", tryOnly, typeChar, ignoreTrailing);
  testAssert(tryOnly == false);
  tryOnly = true;
  typeChar = 0;
  
}

void
Number_Test::parseDoubles()
{
   RString s = "1234";
  bool tryOnly = true;
  char typeChar = 0;
  int ignoreTrailing = 0;
  double d = 42.0523;
  double derg = Number::parseFloatNumber("42.0523", tryOnly, typeChar, ignoreTrailing);
  testAssert(Number::parseFloatNumber("42.0523", tryOnly, typeChar, ignoreTrailing) == d);
}

void 
Number_Test::parseNumbers()
{

  RString s = "1234";
  bool tryOnly = true;
  char typeChar = 0;
  RNumber numerg;
  numerg = Number::decodeToNumber("12s");
  numerg = Number::decodeToNumber("-+i", true);
  testAssert(numerg == Nil);
  numerg = Number::decodeToNumber("-", true);
  testAssert(numerg == Nil);

  numerg = Number::decodeToNumber("-12.42");

  numerg = Number::decodeToNumber("12");
  testAssert(numerg->byteValue() == 12 && instanceof(numerg, Byte) == true);
  numerg = Number::decodeToNumber("12s");
  testAssert(numerg->byteValue() == 12 && instanceof(numerg, Short) == true);
  numerg = Number::decodeToNumber("12L");
  testAssert(numerg->longValue() == 12 && instanceof(numerg, Long) == true);
  numerg = Number::decodeToNumber("-12");
  testAssert(numerg->shortValue() == -12 && instanceof(numerg, Short) == true);
  numerg = Number::decodeToNumber("0x12s");
  testAssert(numerg->shortValue() == 0x12 && instanceof(numerg, Short) == true);

  numerg = Number::decodeToNumber("-0x12");
  testAssert(numerg->shortValue() == -0x12 && instanceof(numerg, Short) == true);
  
  numerg = Number::decodeToNumber("0xFFFFFF");
  testAssert(numerg->intValue() == 0xFFFFFF);

  numerg = Number::decodeToNumber("-12.42");
  float fval = numerg->floatValue();
  System::out->println(RString("float: ") + fval + "; " + numerg->getClass()->toString());
  testAssert(numerg->floatValue() < -12.41 && numerg->floatValue() > -12.43 && 
             (instanceof(numerg, Float) == true || instanceof(numerg, Double) == true));
  numerg = Number::decodeToNumber("12d");
  testAssert(numerg->intValue() == 12 && instanceof(numerg, Double) == true);

  
}


} // namespace lang 
} //namespace acdk 
} //namespace tests 



