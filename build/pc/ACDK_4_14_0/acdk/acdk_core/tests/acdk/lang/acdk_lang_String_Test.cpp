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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_String_Test.cpp,v 1.35 2005/03/19 21:33:03 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/sys/core_tick.h>
#include <acdk/lang/sys/core_specific.h>
#include <acdk/locale/UnmappableCharacterException.h>
#include <acdk/io/File.h>
#include <acdk/io/MemWriter.h>
#if !defined(USE_OLD_STRING)

namespace tests {
namespace acdk {
namespace lang {
 


BEGIN_DECLARE_TEST( String_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( equalsTest )
  DECLARE_TEST( find )
  DECLARE_TEST( findLast )
  DECLARE_TEST( startsWith )
  DECLARE_TEST( endsWith )
  DECLARE_TEST( regionMatches )
  
  DECLARE_TEST( replaceChar )
  DECLARE_TEST( replaceString )
  DECLARE_TEST( toLowerAndUpper )
  DECLARE_TEST( fromByteArray )
  DECLARE_TEST( operatorMultiply )
  DECLARE_TEST( basicUnicode )
  DECLARE_TEST( Utf8Samples )
  DECLARE_TEST( intern )
END_DECLARE_TEST( String_Test )

BEGIN_DEFINE_TEST( String_Test )
  ADD_TEST( String_Test, standard ) 
  ADD_TEST( String_Test, equalsTest ) 
  ADD_TEST( String_Test, find ) 
  ADD_TEST( String_Test, findLast ) 
  ADD_TEST( String_Test, startsWith ) 
  ADD_TEST( String_Test, endsWith ) 
  ADD_TEST( String_Test, regionMatches ) 
  ADD_TEST( String_Test, replaceChar ) 
  ADD_TEST( String_Test, replaceString ) 
  ADD_TEST( String_Test, toLowerAndUpper ) 
  
  ADD_TEST( String_Test, fromByteArray ) 
  ADD_TEST( String_Test, operatorMultiply ) 
  ADD_TEST( String_Test, basicUnicode ) 
  ADD_TEST( String_Test, Utf8Samples ) 
  
  ADD_TEST( String_Test, intern ) 
END_DEFINE_TEST( String_Test )

USING_CLASS(::acdk::lang::, Throwable);
USING_CLASS(::acdk::lang::, Exception);


void String_Test::standard()
{
  RString emptyString = "";
  {
    RString subs;
    {
      RString se = new String("Hallo");
      subs = new String(se, 2, 3);
    }
  }
  {
    {
      RString se = RCS("Hallo Leute");
    }

    RString subs;
    {
      RString se = RCS("Hallo Leute");
      subs = new String(se, 6, 5);
      RString subs2 = new String(se, 6, 5);
      testAssert(subs->compareTo(subs2) == 0);
      RString ses = RCS("Leute");
      testAssert(subs->compareTo(ses) == 0);
    }

  }
}


void
String_Test::equalsTest() 
{
  RString s = new String("Hallo Leute");
  testAssert(s->equals("Hallo Leute") == true);
  RString ss = s->substr(0, 5);
  
  testAssert(ss->equals("Hallo") == true);
  testAssert(ss->equals(s->substr(0, 5)) == true);

  RString nss = ss->getNormalized();
  testAssert(nss->equals("Hallo") == true);
  
  testAssert(nss->compareToIgnoreCase(RCS("Hallo")) == 0);
  testAssert(nss->compareToIgnoreCase(RCS("HALLO")) == 0);
  testAssert(String("a").compareTo("b") < 0);
  testAssert(String("b").compareTo("a") > 0);
  testAssert(nss->compareTo("Hallo") == 0);
  testAssert(nss->compareTo("Hallo Leute") < 0);
  testAssert(nss->compareTo("Hall") > 0);
  testAssert(nss->compareTo("hallo") < 0);
  testAssert(nss->compareTo("xallo") < 0);
  testAssert(nss->compareTo("Xallo") < 0);
  
}

void 
String_Test::find() 
{
  RString s = new String("Hallo Leute");
  int idx = s->indexOf(' ');
  testAssert(idx != -1);
  RString l = s->substr(idx + 1);
  testAssert(l->equals("Leute"));

  idx = s->indexOf("ll");
  testAssert(idx != -1);
  l = s->substr(idx, idx + 2);
  testAssert(l->equals("ll"));
  {
    RString f = "file.h.html";
    int idx = f->indexOf(".html");
    testAssert(idx == 6);
  }
   {
    RString f = "tmlh.h.file";
    int idx = f->lastIndexOf("tmlh.");
    testAssert(idx == 0);
  }
  {
    RString f = "tests/acdk/net/srfsys";
    testAssert(f->elementCount("/") == 3);
    f = "///";
    testAssert(f->elementCount("/") == 3);
  }  
}

void 
String_Test::findLast()
{
  RString s = new String("Hallo Leute");
  int idx = s->lastIndexOf(' ');
  testAssert(idx != -1);
  RString l = s->substr(idx + 1);
  testAssert(l->equals("Leute"));

  idx = s->lastIndexOf("ll");
  testAssert(idx != -1);
  l = s->substr(idx, idx + 2);
  testAssert(l->equals("ll"));

}

void
String_Test::startsWith()
{
  RString s = new String("Hallo Leute");
  testAssert(s->startsWith("Hallo") == true);
  testAssert(s->startsWith("Leute") == false);
}

void
String_Test::endsWith()
{
  RString s = new String("Hallo Leute");
  testAssert(s->endsWith("Hallo") == false);
  testAssert(s->endsWith("Leute") == true);
}

void
String_Test::regionMatches()
{
  RString s = new String("Hallo Leute");
  testAssert(s->regionMatches(6, "xLeute", 1, 5, false) == true);
  testAssert(s->regionMatches(6, "xleute", 1, 5, true) == true);
}

void
String_Test::replaceChar()
{
  RString s = RCS("XXaaXaX");
  RString r1 = s->replace('b', 'c');
  testAssert(s == r1);

  RString r2 = s->replace('X', 'U');
  testAssert(r2->equals("UUaaUaU"));
  {
    RString s = "A Word";
    RString s2 = s->substr(2);
    RString s3 = s2->replace('d', 't');

  }
}

void
String_Test::replaceString()
{
  {
    RString s = "acdk.tools.aunit";
    RString r1 = s->replace(".", "_");
  }
  {
  RString s = RCS("XXaaXaX");
  RString r1 = s->replace("b", "c");
  testAssert(s == r1);
  RString r2 = s->replace("X", "U");
  testAssert(r2->equals("UUaaUaU"));
  }
  
  // replace longer text with smaller
  {
  RString s = new String("ToReplace in a Text");
  RString r1 = s->replace("ToReplace ", "");
  testAssert(r1->equals("in a Text"));
  s = new String("in a Text ToReplace");
  r1 = s->replace(" ToReplace", "");
  testAssert(r1->equals("in a Text"));
  s = new String("in ToReplace a Text");
  r1 = s->replace("ToReplace ", "");
  testAssert(r1->equals("in a Text"));
  }

  // replace smaller with longer
  {
    RString s = new String("sr in a Text");
    RString r1 = s->replace("sr", "StringReplace");
    testAssert(r1->equals("StringReplace in a Text"));
    s = new String("in a Text sr");
    r1 = s->replace("sr", "StringReplace");
    testAssert(r1->equals("in a Text StringReplace"));
    s = new String("in sr a Text");
    r1 = s->replace("sr", "StringReplace");
    testAssert(r1->equals("in StringReplace a Text"));
  }
  
}


void String_Test::toLowerAndUpper()
{
  RString str;
  str = "allUpper";
  RString str2  = str->toLowerCase();
  RString str3 = str->toUpperCase();
  testAssert(str->length() == str->toLowerCase()->length());
  testAssert(str->length() == str->toUpperCase()->length());
  str = "ALL UPPER";
  testAssert(str->length() == str->toLowerCase()->length());
  testAssert(str->length() == str->toUpperCase()->length());

}

void String_Test::fromByteArray()
{
   
  //RString msg = "Ooops";
  RbyteArray ba = new byteArray(2);
  ba[0] = 'R';
  ba[1] = 'K';
  RString str = new String((RcharArray)ba);
  // #### it will copy. provide constructor with byteArray as constructor.
  testAssert(str->equals("RK") == true);
}

using ::acdk::lang::sys::core_tick;
using ::acdk::lang::sys::tick_t;
void
String_Test::operatorMultiply()
{
  RString val = "ABCD ";
  
  RString plusstr = val + "1234 " + val + " XYZ " + val + " -|-" + val;
  StringBuffer sb;
  sb.append(val); sb.append("1234 "); sb.append(val); sb.append(" XYZ ");
  sb.append(val); sb.append(" -|-");sb.append(val); 
  RString sbstr = sb.toString();
  testAssert(sbstr->equals(plusstr) == true);
  int loops = 100000;
  {
    tick_t start = core_tick::now();
    for (int i = 0; i < loops; ++i)
    {
      RString plusstr = val + "1234 " + val + " XYZ " + val + " -|-" + val;
    }
    tick_t end = core_tick::now();
    sys::coreout << "String +: " << int(end - start) << sys::eofl;
  }
  {
    tick_t start = core_tick::now();
    for (int i = 0; i < loops; ++i)
    {
      StringBuffer sb;
      sb.append(val); sb.append("1234 "); sb.append(val); sb.append(" XYZ ");
      sb.append(val); sb.append(" -|-");
      RString pussstr = sb.toString();
    }
    tick_t end = core_tick::now();
    sys::coreout << "StringBuffer.append: " << int(end - start) << sys::eofl;
  }
  // ### other + operations (with int, long,e tc);
}

void printString(IN(RString) str)
{
  return;
  sys::coreout << "String: " << (const char*)str->byte_begin() << ":" << sys::eofl;
  for (String::iterator it = str->begin(); it != str->end(); ++it)
    {
      sys::coreout << "[ " << (int)*it << ": " << (char)*it << "] ";
    }
  sys::coreout << " bytes = ";
  for (String::byte_iterator bit = str->byte_begin(); bit != str->byte_end(); ++bit)
    {
      sys::coreout << "[ " << (int)*bit << ": " << (char)*bit << "] ";
    }
  sys::coreout << sys::eofl;
}

void
String_Test::basicUnicode()
{
  {
    RString s = _US("ue=\\u00fc; ae=\\u00e4; oe=\\u00f6");
    RString us = s->toUpperCase();
    //out.println("lc: " + s + "; uc: " + us + "; = " + "UE=\u00dc; AE=\u00c4; UE=\u00d6");
    testAssert(us->equals(_US("UE=\\u00dc; AE=\\u00c4; OE=\\u00d6")) == true);
  }
  RString uc = _US("Stra\\u00dfen\\u00fcberasschung \\u00e9");
  {
    RString s = uc;
    testAssert(s->equals(uc) == true);
    RString sz = _US("\\u00df");
    int idx = s->indexOf(sz);
    testAssertUc(s->indexOf(_UC("\\u00df")) == 4);
  }
  {
    RString s = "text";
    RString s2 = _US("\\u00fc");
    RString s3 = "a";
    RString erg = s + s2 + s3;
    testAssertUc(erg->equals(_US("text\\u00fca")) == true);
    //System::out->println(erg);
    
  }
 
  {
    RString s1 = _US("Stra\\u00dfen\\u00fcberasschung \\u00e9");
    RString s2 = s1->convert(CCAscii, ::acdk::locale::ReportCodingError, ::acdk::locale::IgnoreCodingError);
    testAssert(s2->equals("Straenberasschung ") == true);
    s2 = s1->convert(CCAscii, ::acdk::locale::ReportCodingError, ::acdk::locale::ReplaceCodingError);
    sys::coreout << "s2: " << s2->c_str() << sys::eofl;
    testAssert(s2->equals("Stra?en?berasschung ?") == true);
    
    sys::coreout << "UCS2: ";
    printString(s1);
    s2 = s1->convert(CCUtf8);
    testAssert(s2->equals(s1) == true);
    sys::coreout << "UTF8:";
    printString(s2);
    s2 = s2->convert(CCUcs2);
    sys::coreout << "UCS2:";
    printString(s2);
    testAssert(s1->equals(s2) == true);
    s2 = s1->replace(_US("\\u00fc"), _US("\\u00e4"));
    testAssertUc(s2->equals(_US("Stra\\u00dfen\\u00e4berasschung \\u00e9")) == true);

    int idx = s1->indexOf(_UC("\\u00fc"));
    testAssertUc(s1->charAt(idx) == _UC("\\u00fc"));

  }
  RString s = _US(
      "German: ss: \\u00df\n" 
      "German umlaut ue: \\u00e4\n"
      "a acute: \\u00e1\n"
      "g acute: \\u01f5\n"
      "greek alpha: \\u0391\n"
      "CYRILLIC CAPITAL LETTER A: \\u0410\n"
      "ARMENIAN SMALL LETTER AYB: \\u0561\n"
      "HEBREW LETTER ALEF: \\u05D0\n"
      "ARABIC LETTER ALEF: \\u0627\n"
      "SYRIAC LETTER ALAPH: \\u0710\n"
      "THAANA LETTER HAA: \\u0780\n"
      "DEVANAGARI LETTER A: \\u0905\n"
      "BENGALI LETTER AU: \\u0994\n"
      "GURMUKHI LETTER A: \\u0A05\n"
      "GUJARATI LETTER A: \\u0A85\n"
      "ORIYA LETTER A: \\u0B05\n"
      "TAMIL LETTER A: \\u0B85\n"
      "TELUGU LETTER A: \\u0C05\n"
      "KANNADA LETTER A: \\u0C85\n"
      "MALAYALAM LETTER A: \\u0D05\n"
      "SINHALA LETTER AYANNA: \\u0D85\n"
      "THAI CHARACTER SARA A: \\u0E30\n"
      "LAO LETTER DO: \\u0E94\n"
      "TIBETAN LETTER KA: \\u0F40\n"
      "MYANMAR LETTER KA: \\u1000\n"
      "GEORGIAN LETTER AN: \\u10D0\n"
      "HANGUL CHOSEONG KIYEOK: \\u1100\n"
      "ETHIOPIC SYLLABLE LA: \\u1208\n"
      "ETHIOPIC SYLLABLE HA: \\u1200\n"
      "CHEROKEE LETTER A: \\u13A0\n"
      "CANADIAN SYLLABICS AAI: \\u1402\n"
      "OGHAM LETTER BEITH: \\u1681\n"
      "RUNIC LETTER ANSUZ A: \\u16A8\n"
      "TAGALOG LETTER A: \\u1700\n"
      "HANUNOO LETTER A: \\u1720\n"
      "BUHID LETTER A: \\u1740\n"
      "TAGBANWA LETTER A: \\u1760\n"
      "KHMER LETTER KA: \\u1780\n"
      "MONGOLIAN LETTER A: \\u1820\n"
      "LIMBU LETTER KA: \\u1901\n"
      "TAI LE LETTER KA: \\u1950\n"
      "BRAILLE PATTERN DOTS-23: \\u2806\n"
      "HIRAGANA LETTER A: \\u3042\n"
      "KATAKANA LETTER A: \\u30A2\n"
      "BOPOMOFO LETTER A: \\u311A\n"
      "CJK Ideograph Extension A: \\u3420\n"
      "CJK Ideograph: \\u4E00\n"
      );
  System::out->println("using encoding: " + System::getProperty("user.encoding"));
  System::out->println("\nsome samples for console:\n" + s);
}


void
String_Test::Utf8Samples()
{
  ::acdk::io::File f(System::getAcdkHome() + "/acdk_core/tests/acdk/lang/utf8examples.txt");
  ::acdk::io::MemWriter ba;
  f.getReader()->trans(&ba);
  RString s = new String((const char*)ba.getBuffer()->data(), CCUtf8);
  //System::out->println(s);
  /*
  for (int i = 0; utf8strings[i] != 0; ++i)
  {
    RString s = new String(utf8strings[i], CCUtf8);
    System::out->println(s);
  }*/
}

void
String_Test::intern()
{
  RString s = new String("asdf");
  s = s->intern();
  RString s2 = new String("asdf");
  s2 = s2->intern();
}


} // namespace lang 
} //namespace acdk 
} //namespace tests

#endif //#if defined(USE_OLD_STRING)
