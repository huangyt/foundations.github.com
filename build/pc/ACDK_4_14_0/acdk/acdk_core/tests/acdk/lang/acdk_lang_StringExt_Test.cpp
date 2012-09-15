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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_StringExt_Test.cpp,v 1.7 2005/02/05 10:45:08 kommer Exp $


#if 0
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include "../../../src/acdk/lang/StringExt.h"

namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( StringExt_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( equalsTest )
  DECLARE_TEST( replaceChar )
  DECLARE_TEST( replaceString )
END_DECLARE_TEST( StringExt_Test  )

BEGIN_DEFINE_TEST( StringExt_Test )
  ADD_TEST( StringExt_Test, standard ) 
  ADD_TEST( StringExt_Test, equalsTest ) 
  ADD_TEST( StringExt_Test, replaceChar ) 
  ADD_TEST( StringExt_Test, replaceString ) 
END_DEFINE_TEST( StringExt_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);

void StringExt_Test::standard()
{
  {
    RStringExt subs;
    {
      RStringExt se = new StringExt("Hallo");
      subs = new StringExt(se, 2, 3);
    }
  }
  {
    {
      RStringExt se = RCS("Hallo Leute");
    }

    RStringExt subs;
    {
      RStringExt se = RCS("Hallo Leute");
      subs = new StringExt(se, 6, 5);
      RStringExt subs2 = new StringExt(se, 6, 5);
      testAssert(subs->compareTo(subs2) == 0);
      RStringExt ses = RCS("Leute");
      testAssert(subs->compareTo(ses) == 0);
    }

  }
}


void
StringExt_Test::equalsTest() 
{
  RStringExt s = new StringExt("Hallo Leute");
  testAssert(s->equals("Hallo Leute") == true);
  RStringExt ss = s->substr(0, 5);
  
  testAssert(ss->equals("Hallo") == true);
  testAssert(ss->equals(s->substr(0, 5)) == true);

  RStringExt nss = ss->getNormalized();
  testAssert(nss->equals("Hallo") == true);
  
  testAssert(nss->compareToIgnoreCase(RCS("Hallo")) == 0);
  testAssert(nss->compareToIgnoreCase(RCS("HALLO")) == 0);
  
}

void
StringExt_Test::replaceChar()
{
  RStringExt s = RCS("XXaaXaX");
  RStringExt r1 = s->replace('b', 'c');
  testAssert(s == r1);

  RStringExt r2 = s->replace('X', 'U');
  testAssert(r2->equals("UUaaUaU"));
}

void
StringExt_Test::replaceString()
{
  {
  RStringExt s = RCS("XXaaXaX");
  RStringExt r1 = s->replace("b", "c");
  testAssert(s == r1);
  RStringExt r2 = s->replace("X", "U");
  testAssert(r2->equals("UUaaUaU"));
  }
  
  // replace longer text with smaller
  {
  RStringExt s = new StringExt("ToReplace in a Text");
  RStringExt r1 = s->replace("ToReplace ", "");
  testAssert(r1->equals("in a Text"));
  s = new StringExt("in a Text ToReplace");
  r1 = s->replace(" ToReplace", "");
  testAssert(r1->equals("in a Text"));
  s = new StringExt("in ToReplace a Text");
  r1 = s->replace("ToReplace ", "");
  testAssert(r1->equals("in a Text"));
  }

  // replace smaller with longer
  {
    RStringExt s = new StringExt("sr in a Text");
    RStringExt r1 = s->replace("sr", "StringReplace");
    testAssert(r1->equals("StringReplace in a Text"));
    s = new StringExt("in a Text sr");
    r1 = s->replace("sr", "StringReplace");
    testAssert(r1->equals("in a Text StringReplace"));
    s = new StringExt("in sr a Text");
    r1 = s->replace("sr", "StringReplace");
    testAssert(r1->equals("in StringReplace a Text"));
  }
}


} // namespace lang 
} //namespace acdk 
} //namespace tests 


#endif //0
