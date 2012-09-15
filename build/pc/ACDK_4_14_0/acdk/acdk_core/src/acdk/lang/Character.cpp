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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Character.cpp,v 1.15 2005/03/24 16:58:00 kommer Exp $



#include <acdk.h>

#include <cctype>
#include <climits>

#include "Character.h"

#define __isdigit(c) isdigit(c)

inline
bool
doisdigit(char c)
{
  return __isdigit(c);  
}

#ifdef isdigit // aaargghhhh, kill them all!!!
#undef isdigit
#endif // isdigit


namespace acdk {
namespace lang {

int Character::MAX_RADIX = 36;
int Character::MIN_RADIX = 2;

char Character::MAX_VALUE = CHAR_MAX;
char Character::MIN_VALUE = CHAR_MIN; 



//static 
int 
Character::asciiDigit(char ch, int radix)
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
Character::equals(IN(RObject) other)
{
  if (other == Nil)
    return false;
  if (instanceof(other, Character) == false)
    return false;
  return charValue() == RCharacter(other)->charValue();
}


//static 
bool 
Character::isDigit(char c)
{
  return doisdigit(c) != 0;
}


bool 
isHexCharDigit(uc2char c)
{
  return (c >= 'a' && c <= 'f') ||
         (c >= 'A' && c <= 'F');
}

//static 
bool 
Character::isHexDigit(char c)
{
  if (isDigit(c) == true)
    return true;
  return isHexCharDigit(c);
}

//static 
bool 
Character::isHexDigit(uc2char c)
{
  if (isDigit(c) == true)
    return true;
  return isHexCharDigit(c);
}
  

//static 
bool 
Character::isLetter(char c)
{
  return isalpha(c) != 0;
}

//static 
bool 
Character::isLowerCase(char c)
{
  return islower(c) != 0;
}

//static 
bool 
Character::isSpace(char c)
{
  return isspace(c) != 0;
}

//static 
bool 
Character::isUpperCase(char c)
{
  return isupper(c) != 0;
}

//static 
char 
Character::toLowerCase(char c)
{
  return char(tolower(c));
}

//static 
char 
Character::toUpperCase(char c)
{
  return char(toupper(c));
}


//static
RString
Character::toString(char c)
{
  char cbuf[2]; cbuf[1] = 0;
  cbuf[0] = c;
  return SCS(cbuf);
}

//virtual 
RString 
Character::toString()
{
  return toString(value);
}


//static 
bool 
Character::isJavaIdentifierStart(char c)
{
  return (Character::isLetter(c) && Character::isUpperCase(c)) || c == ':';
}


//static 
bool 
Character::isJavaIdentifierPart(char c)
{
  return Character::isUnicodeIdentifierPart(c);
}

//static 
bool 
Character::isUnicodeIdentifierStart(char c)
{
  return Character::isLetter(c) || c == '_';
}


//static 
bool 
Character::isUnicodeIdentifierPart(char c) 
{
  return Character::isLetterOrDigit(c) || 
         c == '_' 
         ;
}

//static 
RClass 
Character::getTYPE() 
{
  return Class::getSingeltonClass(dmi::ClazzInfo::getCharClazz());
}


} // Lang
} // acdk
