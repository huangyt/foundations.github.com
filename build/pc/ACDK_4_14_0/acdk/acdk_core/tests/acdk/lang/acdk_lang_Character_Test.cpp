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
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Character_Test.cpp,v 1.15 2005/04/23 16:06:49 kommer Exp $

#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/lang/Throwable.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Integer.h>
#include <acdk/locale/UnmappableCharacterException.h>
#include <acdk/locale/UnicodeTable.h>

/**
  The tests namespace contains the unit tests.

  @see gw_ref[acdk_tools_aunit_man]
*/
namespace tests {
namespace acdk {
namespace lang {
  
BEGIN_DECLARE_TEST( Character_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( invalidLiteral )
  DECLARE_TEST( unicode )
  DECLARE_TEST( misc )
END_DECLARE_TEST( Character_Test  )

BEGIN_DEFINE_TEST( Character_Test )
  ADD_TEST( Character_Test, standard ) 
  ADD_TEST( Character_Test, invalidLiteral ) 
  ADD_TEST( Character_Test, unicode ) 
  ADD_TEST( Character_Test, misc ) 
END_DEFINE_TEST( Character_Test )

using namespace ::acdk::lang;
using namespace ::acdk::locale;


void
Character_Test::standard()
{
  testAssert(UnicodeCharacter::toLowerCase('A') == 'a');
  testAssert(UnicodeCharacter::toUpperCase('b') == 'B');
  testAssertUc(UnicodeCharacter::isLetter(_UC("A")) == true);
  testAssertUc(UnicodeCharacter::isLetter(_UC("\\u00df")) == true); //ss
  
  RString s0a = _US("Roger Ren\\u00e9 Kommer; D\\u00f6rnbergstra\\u00dfe"); // ae
  testAssertUc(UnicodeCharacter::isLetter(_UC("\\u00e4")) == true);
  uc2char c = UnicodeCharacter::toUpperCase(_UC("\\u00e4"));
  uc2char c2 = _UC("\\u00c4");
  testAssert(c == c2);
}

void
Character_Test::invalidLiteral()
{
  try {
    RString s1 = _US("\\uol;a"); 
    testAssertComment(false, "_US() should throw UnmappableCharacterException in invalid character stream");
  } catch (RUnmappableCharacterException ex) {
  }
}

void
Character_Test::unicode()
{
  RString desc = UnicodeCharacter::getCharacterDescription(0x00000915);
  desc = UnicodeCharacter::getCharacterDescription(0x00000915);
  desc = UnicodeCharacter::getCharacterDescription(0x0901);
  for (uc2char i = 0; i < 0xfffe; ++i)
  {
    const UnicodeInfo& ui = UnicodeTable::get(i);
    if (ui.flags & Unpecified || ui.flags & CJKLetter)
      continue;
    if (ui.idx != i)
      testAssertComment(false, SBSTR("index not matching: char: " << Integer::toHexString(i) 
      															<< "; returned: " << Integer::toHexString(ui.idx)));
  }
}

void
Character_Test::misc()
{
  RString s1 = _US("\\u00A0"); 
}

} // namespace lang
} //namespace acdk
} //namespace tests



