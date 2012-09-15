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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_String2_Test.cpp,v 1.3 2005/05/02 23:12:47 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/sys/core_tick.h>
#include <acdk/lang/sys/core_specific.h>
#include <acdk/locale/UnmappableCharacterException.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>

#include <acdk/io/MemWriter.h>
#include <acdk/lang/sys/core_memtrace.h>

namespace tests {
namespace acdk {
namespace lang {
 


BEGIN_DECLARE_TEST( String2_Test )
  DECLARE_TEST( defaultString )
  DECLARE_TEST( isEmpty )
  DECLARE_TEST( isBlank )
  DECLARE_TEST( isType )
  DECLARE_TEST( contains )
  DECLARE_TEST( indexOfAny )
  DECLARE_TEST( join )
  DECLARE_TEST( format )
  DECLARE_TEST( distance )
  DECLARE_TEST( memuse )
END_DECLARE_TEST( String2_Test )

BEGIN_DEFINE_TEST( String2_Test )
  ADD_TEST( String2_Test, defaultString ) 
  ADD_TEST( String2_Test, isEmpty ) 
  ADD_TEST( String2_Test, isBlank ) 
  ADD_TEST( String2_Test, isType ) 
  ADD_TEST( String2_Test, contains ) 
  ADD_TEST( String2_Test, indexOfAny ) 
  ADD_TEST( String2_Test, join ) 
  ADD_TEST( String2_Test, format ) 
  ADD_TEST( String2_Test, distance ) 
  ADD_TEST( String2_Test, memuse ) 
  
END_DEFINE_TEST( String2_Test )

using namespace ::acdk::lang;

void String2_Test::defaultString()
{
  testAssert(String::defaultString("")->equals("") == true);
  testAssert(String::defaultString(Nil)->equals("") == true);
  testAssert(String::defaultString(Nil, "a")->equals("a") == true);
}

void String2_Test::isEmpty()
{
  testAssert(String::isEmpty(Nil) == true);
  testAssert(String::isEmpty("") == true);
  testAssert(String::isEmpty("awsdf") == false);
  testAssert(String::isNotEmpty("") == false);
  testAssert(String::isNotEmpty(Nil) == false);
  testAssert(String::isNotEmpty("asdf") == true);
  
}

void
String2_Test::isBlank()
{
  testAssert(String::isBlank(Nil) == true);
  testAssert(String::isBlank("") == true);
  testAssert(String::isBlank(" \n\t") == true);
  testAssert(String::isBlank("asdf") == false);
  testAssert(String::isNotBlank("asdf") == true);
  testAssert(String::isNotBlank(Nil) == false);
  
}

void
String2_Test::isType()
{
  testAssert(String::isAlpha(Nil) == false);
  testAssert(String::isAlpha("") == true);
  testAssert(String::isAlpha("abc") == true);
  testAssert(String::isAlpha("1") == false);
  testAssert(String::isAlpha("\n") == false);
  // continue other is tests

}

void
String2_Test::contains()
{
  testAssert(String::contains(Nil, 'a') == false);
  testAssert(String::contains("", 'a') == false);
  testAssert(String::contains("bca", 'a') == true);
  testAssert(String::contains("bca", 'a') == true);
  testAssert(String::containsNone("bca", "axx") == false);
  testAssert(String::containsNone(Nil, "axx") == true);
  testAssert(String::containsNone("", "") == true);
  testAssert(String::containsNone("abc", "bca") == false);
  testAssert(String::containsNone("abc", "xy") == true);
  testAssert(String::containsNone("abc", Nil) == true);
  testAssert(String::containsOnly("abc", "xy") == false);
  testAssert(String::containsOnly("abcabc", "abcd") == false);
  testAssert(String::containsOnly("abcabc", "abc") == true);
  testAssert(String::containsOnly(Nil, "abc") == false);
  testAssert(String::containsOnly(Nil, Nil) == false);
  testAssert(String::containsOnly("asdf", Nil) == false);
}

void
String2_Test::indexOfAny()
{
  testAssert(String::indexOfAny(Nil, "a") == -1);
  testAssert(String::indexOfAny("asdf", (RString)Nil) == -1);
  testAssert(String::indexOfAny("asdfas", "sd") == 1);
  testAssert(String::indexOfAny("asdfas", "x") == -1);

  testAssert(String::lastIndexOfAny(Nil, "a") == -1);
  testAssert(String::lastIndexOfAny("asdf", (RString)Nil) == -1);
  testAssert(String::lastIndexOfAny("asdfas", "s") == 5);
  testAssert(String::lastIndexOfAny("asdfas", "x") == -1);
  
}

void
String2_Test::join()
{
  RStringArray sa = new StringArray(0);
  sa->append("first");
  sa->append("second");
  sa->append("third");

  testAssert(String::join((RObjectArray)sa)->equals("firstsecondthird") == true);
  testAssert(String::join((RObjectArray)sa, "|")->equals("first|second|third") == true);
  testAssert(String::join(sa->iterator())->equals("firstsecondthird") == true);
  testAssert(String::join(sa->iterator(), "|")->equals("first|second|third") == true);
  testAssert(String::join(sa->iterator(), '|')->equals("first|second|third") == true);
  testAssert(String::join((RObjectArray)Nil, "|") == Nil);

  testAssert(RString("a b  c")->split()->length() == 3);
  testAssert(RString("a|b|c")->split('|')->length() == 3);
}

void
String2_Test::format()
{
  testAssert(String::deleteWhitespace(Nil) == Nil);
  testAssert(String::deleteWhitespace("")->equals("") == true);
  testAssert(String::deleteWhitespace("abc")->equals("abc") == true);
  testAssert(String::deleteWhitespace("a b\n\tc")->equals("abc") == true);

  testAssert(RString(" ")->repeat(3)->equals("   ") == true);

  testAssert(RString("")->rightPad(3)->equals("   ") == true);
  testAssert(RString("a")->rightPad(3)->equals("a  ") == true);
  testAssert(RString("abcd")->rightPad(3)->equals("abcd") == true);

  testAssert(RString("")->leftPad(3)->equals("   ") == true);
  testAssert(RString("a")->leftPad(3)->equals("  a") == true);
  testAssert(RString("abcd")->leftPad(3)->equals("abcd") == true);

  testAssert(RString("")->center(3)->equals("   ") == true);
  testAssert(RString("a")->center(3)->equals(" a ") == true);
  testAssert(RString("ab")->center(3)->equals("ab ") == true);
  testAssert(RString("abcd")->center(3)->equals("abcd") == true);
}

void
String2_Test::distance()
{
  testAssert(String::indexOfDifference(Nil, "asdf") == -1);
  testAssert(String::indexOfDifference("asdf", Nil) == -1);
  testAssert(String::indexOfDifference("a b c", "a b_c") == 3);
  testAssert(String::getDistance("", "") == 0);
  testAssert(String::getDistance("", "a") == 1);
  testAssert(String::getDistance("a", "a") == 0);
  
  testAssert(String::getDistance("abcd", "a") == 3);
  testAssert(String::getDistance("Roger", "Rger") == 1);
  testAssert(String::getDistance("kommer", "koer") == 2);
  testAssert(String::getDistance("abc", "xyz") == 3);
  
}

void
String2_Test::memuse()
{
  String::valueOf(42.2); // to avoid to track loaded resssource bundles
   ::acdk::lang::sys::core_memtrace trace;
  {
    StringBuffer sb(6);
    sb << String::valueOf(42.2);
    RString s = "asdf";
    sb << s;
    s = (uc2char*)"a\0";
    sb << s;
    RString ts = sb.toString();
    sb << "asdf";
    
  }
  trace.reportUnfreed();
  testAssert(trace.getAllocatedCount() == 0);
  
  {
    ::acdk::lang::sys::core_memtrace trace;
    {
      RStringBuffer sb = new StringBuffer();
      sb->append("asdf");
      sb->lockMem(true);
    }
    trace.reportUnfreed();
  }
  
  {
    ::acdk::lang::sys::core_memtrace trace;
    {
      RString s = "asdf";
      RString t = s->substr(1, 1);
      t->c_str();
    }
    trace.reportUnfreed();
    testAssert(trace.getAllocatedCount() == 0);
  }

}

} // namespace lang 
} //namespace acdk 
} //namespace tests

