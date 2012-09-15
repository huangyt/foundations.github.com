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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Character.h,v 1.20 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Character_h
#define acdk_lang_Character_h

#include "UnicodeCharacter.h"


namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Character);

/** 
  Object wrapper for a char (7 bit character).

  API: Java<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/09 19:26:48 $
  @see acdk::lang::UnicodeCharacter
  TYPE is not field, but method getTYPE().
*/

class ACDK_CORE_PUBLIC Character
: extends Object
, implements Comparable
, implements Cloneable
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(Character)
public:
  static RObject create_instance() { return new Character('\0'); }
  static int MAX_RADIX;
  static char MAX_VALUE;
  static int MIN_RADIX;
  static char MIN_VALUE; 
protected:
  char value;
  
public:
  /// creates character '\0'
  Character() : value(0) {}
  Character(char val) : value(val) {}

  virtual char charValue() { return value; }
  virtual int compareTo(IN(RCharacter) other) { return other == Nil ? 1 : value - other->charValue(); }
  /// reimplemented from Object
  foreign int compareTo(IN(RObject) other)
  { 
    return compareTo(RCharacter(other)); 
  }
  int compareTo(ucchar other) { return value - other; }
  /// reimplemented from Object
  foreign bool equals(IN(RObject) other);
  bool equals(ucchar other) { return value == other; }
  /// reimplemented from Object
  foreign int hashCode() { return value; }

  static int asciiDigit(char ch, int idx);
  static int asciiDigit(uc2char ch, int idx) { return UnicodeCharacter::asciiDigit(ch, idx); }
  static bool isDigit(char c);
  static bool isDigit(uc2char c) { return UnicodeCharacter::isDigit(c); }
  /**
    return the decimal value of given digit value
    This call is only value if isDigit(c) is true
    return 0 for '0' - 9 for '9'
  */
  static int decVal(char c) { return c - '0'; }
  /**
    return the decimal value of given digit value
    This call is only value if isDigit(c) is true
  */
  static int  decVal(ucchar c) { return UnicodeCharacter::decVal(c); }
  static bool isHexDigit(char c);
  static bool isHexDigit(uc2char c);
  static bool isLetter(char c);
  static bool isLetter(uc2char c) { return UnicodeCharacter::isLetter(c); }
  static bool isLetterOrDigit(char c) { return isDigit(c) || isLetter(c); }
  static bool isLetterOrDigit(uc2char c) { return isDigit(c) || isLetter(c); }
  static bool isLowerCase(char c);
  static bool isLowerCase(uc2char c) { return UnicodeCharacter::isLowerCase(c); }
  static bool isSpace(char c);
  static bool isSpace(uc2char c) { return UnicodeCharacter::isSpace(c); }
  static bool isWhitespace(char c) { return isSpace(c) || isControl(c); }
  static bool isWhitespace(uc2char c)  { return isSpace(c) || isControl(c); }
  static bool isControl(uc2char c)  { return UnicodeCharacter::isControl(c); }
  static bool isControl(char c)  { return UnicodeCharacter::isControl(c); }
  static bool isUpperCase(char c);
  static bool isUpperCase(uc2char c) { return UnicodeCharacter::isUpperCase(c); }
  static char toLowerCase(char c);
  static uc2char toLowerCase(uc2char c) { return UnicodeCharacter::toLowerCase(c); }
  static char toUpperCase(char c);
  static uc2char toUpperCase(uc2char c) { return UnicodeCharacter::toUpperCase(c); } 
  
  static bool isJavaIdentifierStart(char c);
  static bool isJavaIdentifierStart(uc2char c){ return UnicodeCharacter::isJavaIdentifierStart(c); }
  static bool isJavaIdentifierPart(char c);
  static bool isJavaIdentifierPart(uc2char c){ return UnicodeCharacter::isJavaIdentifierPart(c); }
  static bool isUnicodeIdentifierStart(char c);
  static bool isUnicodeIdentifierStart(uc2char c){ return UnicodeCharacter::isUnicodeIdentifierStart(c); }
  static bool isUnicodeIdentifierPart(char c);
  static bool isUnicodeIdentifierPart(uc2char c) { return UnicodeCharacter::isUnicodeIdentifierPart(c); }
  
  /// reimplemented from Object
  foreign RString toString();
  static RString toString(char c);
  static RString toString(uc2char c) { return UnicodeCharacter::toString(c); }
 
  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) Character(value); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  static RClass getTYPE();
};

} // lang
} // acdk





#endif //acdk_lang_Character_h

