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
// $Header: /cvsroot/acdk/acdk/acdk_text/tests/acdk/text/acdk_text_RegExp_Test.cpp,v 1.13 2005/02/05 10:45:33 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Double.h>
#include <acdk/text/RegExp.h>
#include <acdk/text/DecimalFormat.h>
#include <acdk/text/FieldPosition.h>

#include <acdk/tools/aunit/TestRunner.h>


namespace tests {
namespace acdk {
namespace text {

using namespace ::acdk::lang;
using namespace ::acdk::text;


  
BEGIN_DECLARE_TEST( RegExp_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( replace )
  DECLARE_TEST( unicode )
END_DECLARE_TEST( RegExp_Test  )

BEGIN_DEFINE_TEST( RegExp_Test )
  ADD_TEST( RegExp_Test, standard ) 
  ADD_TEST( RegExp_Test, replace ) 
  ADD_TEST( RegExp_Test, unicode ) 
  
END_DEFINE_TEST( RegExp_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);

//static
void
RegExp_Test::standard()
{
  {
    RegExp regexp("([^\\/\\\\]+)[\\/\\\\](.*)");
    RString toMatch = "abc/defg/hij";
    RStringArray sa = regexp.match(toMatch);
    testAssert(sa->length() == 3);
    testAssert(sa[0]->equals("abc/defg/hij") == true);
    testAssert(sa[1]->equals("abc") == true);
    testAssert(sa[2]->equals("defg/hij") == true);
    System::out << sa << endln;
  } 
  
}


void RegExp_Test::replace()
{
  {
    RegExp regexp("ab");
    RString erg = regexp.replace("XDFabASDFaASDFabASDF", "xyz", false);
    testAssert(erg->equals("XDFxyzASDFaASDFabASDF") == true);
    erg = regexp.replace("XDFabASDFaASDFabASDF", "xyz", true);
    testAssert(erg->equals("XDFxyzASDFaASDFxyzASDF") == true);
  }
  {
    RegExp regexp("[^\\/\\\\]+");
    RString erg = regexp.replace("src/acdk/lang", "..", true);
    testAssert(erg->equals("../../..") == true);
    erg = regexp.replace("src\\acdk\\lang", "..", true);
    testAssert(erg->equals("..\\..\\..") == true);
  }
  
}
void 
RegExp_Test::unicode()
{
  RString uctext = _US("Stra\\u00dfe");
  RString match = _US("(.*)");
  {
    System::out->println(uctext);
    RegExp regexp(match);
    RStringArray sa = regexp.match(uctext);
    System::out->println(sa->toString());
    testAssert(sa[0]->equals(uctext) == true);
    testAssert(sa[1]->equals(uctext) == true);
  }
  {
    match = _US("(\\u00df.*)");
    RegExp regexp(match);
    RStringArray sa = regexp.match(uctext);
    System::out->println(sa->toString());
    RString check = _US("\\u00dfe");
    testAssert(sa[0]->equals(check) == true);
    testAssert(sa[1]->equals(check) == true);
  }
}

} // namespace text
} //namespace acdk 
} //namespace tests

