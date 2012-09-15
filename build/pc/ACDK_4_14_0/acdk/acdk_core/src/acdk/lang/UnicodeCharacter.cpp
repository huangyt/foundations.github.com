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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/UnicodeCharacter.cpp,v 1.9 2005/03/24 16:58:00 kommer Exp $



#include <acdk.h>
#include "UnicodeCharacter.h"
#include <acdk/locale/UnicodeTable.h>

namespace acdk {
namespace lang {

int UnicodeCharacter::MAX_RADIX = 36;
int UnicodeCharacter::MIN_RADIX = 2;

ucchar UnicodeCharacter::MAX_VALUE = 0xFFFF;
ucchar UnicodeCharacter::MIN_VALUE = 0x0000; 



//static 
int 
UnicodeCharacter::asciiDigit(ucchar ch, int radix)
{
  if (radix < MIN_RADIX || radix > MAX_RADIX)
      return -1;
  if (ch >= '0' && ch <= '9')
    return ch - '0';
  if (ch >= 'A' && ch <= 'Z' && ch < radix + 'A' - 10)
    return ch - 'A' + 10;
  if (ch >= 'a' && ch <= 'z' && ch < radix + 'a' - 10)
    return ch - 'a' + 10;
  return -1;
}

//virtual 
bool 
UnicodeCharacter::equals(IN(RObject) other)
{
  if (other == Nil)
    return false;
  if (instanceof(other, UnicodeCharacter) == false)
    return false;
  return charValue() == RUnicodeCharacter(other)->charValue();
}


//static 
bool 
UnicodeCharacter::isDigit(ucchar c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::Number;
}

//static 
int 
UnicodeCharacter::decVal(ucchar c)
{
  return acdk::locale::UnicodeTable::get(c).decval;
}

//static 
bool 
UnicodeCharacter::isLetter(ucchar c) 
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::Letter;
}

//static 
bool 
UnicodeCharacter::isLowerCase(ucchar c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::DownCase;
}

//static 
bool 
UnicodeCharacter::isSpace(ucchar c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::Space;
}



bool 
UnicodeCharacter::isWhitespaceOnly(uc2char c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::Whitespace;
}


bool 
UnicodeCharacter::isControl(uc2char c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::Control;
}

//static 
bool 
UnicodeCharacter::isSymbol(uc2char c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::Symbol;
}

//static 
bool 
UnicodeCharacter::isTitleCase(uc2char c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::TitleCase;
}

//static 
bool 
UnicodeCharacter::isMark(uc2char c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::Control;
}

bool 
UnicodeCharacter::isPunctation(uc2char c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::Punctation;
}

//static 
bool 
UnicodeCharacter::isUpperCase(ucchar c)
{
  return acdk::locale::UnicodeTable::get(c).flags & acdk::locale::UpCase;
}

//static 
uc2char 
UnicodeCharacter::toLowerCase(ucchar c)
{
  int idx = acdk::locale::UnicodeTable::get(c).downcaseidx;
  if (idx == 0)
    return c;
  return acdk::locale::UnicodeTable::get(idx).idx;
}

//static 
uc2char 
UnicodeCharacter::toUpperCase(ucchar c)
{
  int idx = acdk::locale::UnicodeTable::get(c).upcaseidx;
  if (idx == 0)
    return c;
  return acdk::locale::UnicodeTable::get(idx).idx;
}

//static 
RString 
UnicodeCharacter::getCharacterDescription(uc2char c)
{
  return acdk::locale::UnicodeTable::get(c).name;
}

//static
RString
UnicodeCharacter::toString(ucchar c)
{
  uc2char cbuf[2]; cbuf[1] = 0;
  cbuf[0] = c;
  return SCS(cbuf);
}

//virtual 
RString 
UnicodeCharacter::toString()
{
  return toString(_value);
}


//static 
bool 
UnicodeCharacter::isJavaIdentifierStart(ucchar c)
{
  return (UnicodeCharacter::isLetter(c) && UnicodeCharacter::isUpperCase(c)) || c == ':';
}


//static 
bool 
UnicodeCharacter::isJavaIdentifierPart(ucchar c)
{
  return UnicodeCharacter::isUnicodeIdentifierPart(c);
}

//static 
bool 
UnicodeCharacter::isUnicodeIdentifierStart(ucchar c)
{
  return UnicodeCharacter::isLetter(c) || c == '_';
}


//static 
bool 
UnicodeCharacter::isUnicodeIdentifierPart(ucchar c) 
{
  return UnicodeCharacter::isLetterOrDigit(c) || 
         c == '_' 
         ;
}

//static 
RClass 
UnicodeCharacter::getTYPE() 
{
  return Class::getSingeltonClass(dmi::ClazzInfo::getUcCharClazz());
}


} // lang
} // acdk
