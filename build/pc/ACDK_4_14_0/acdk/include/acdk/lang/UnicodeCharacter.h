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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/UnicodeCharacter.h,v 1.9 2005/04/09 19:26:51 kommer Exp $

#ifndef acdk_lang_UnicodeCharacter_h
#define acdk_lang_UnicodeCharacter_h

#include "Comparable.h"
#include "Cloneable.h"

#include <acdk/io/Serializable.h>
#include "ClassCastException.h"

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(UnicodeCharacter);

/** 
  Wrapper to the basic uc2char type and provides extended information 
  for a given unicode character.
  API: Java, ACDK<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/04/09 19:26:51 $
  
  TYPE is not field, but method getTYPE().
*/

class ACDK_CORE_PUBLIC UnicodeCharacter
: extends Object
, implements Comparable
, implements Cloneable
, implements acdk::io::Serializable
{
  ACDK_WITH_METAINFO(UnicodeCharacter)
public:
  static RObject create_instance() { return new UnicodeCharacter('\0'); }
  static int MAX_RADIX;
  static uc2char MAX_VALUE;
  static int MIN_RADIX;
  static uc2char MIN_VALUE; 
protected:
  uc2char _value;
public:
  UnicodeCharacter(uc2char value) : _value(value) {}

  virtual uc2char charValue() { return _value; }
  virtual int compareTo(IN(RUnicodeCharacter) other) { return other == Nil ? 1 : _value - other->charValue(); }
  /// reimplemented from Object
  foreign int compareTo(IN(RObject) other) 
  { 
    return compareTo(RUnicodeCharacter(other)); 
  }
  static int compareTo(uc2char first, uc2char second) { return first - second; } // ### TODO really?
  static int compareToIgnoreCase(uc2char first, uc2char second) { return toLowerCase(first) - toLowerCase(second); } // ### TODO really?
  /// reimplemented from Object
  foreign bool equals(IN(RObject) other);
  /// reimplemented from Object
  foreign int hashCode() { return _value; }

  /**
    return the integer number of a  digit (0 - 9, A - Z)
    @param ch the character (digit or letter)
    @param idx radix of the number
  */
  static int asciiDigit(uc2char ch, int idx);
  static bool isDigit(uc2char c);
  /**
    return the decimal value of given digit value
    This call is only value if isDigit(c) is true
  */
  static int decVal(ucchar c);
  static bool isLetter(uc2char c);
  static bool isLetterOrDigit(uc2char c) { return isDigit(c) || isLetter(c); }
  static bool isLowerCase(uc2char c);
  static bool isSpace(uc2char c);
  static bool isWhitespace(uc2char c) { return isSpace(c) || isControl(c) || isWhitespaceOnly(c); }
  static bool isWhitespaceOnly(uc2char c);
  static bool isControl(uc2char c);
  static bool isUpperCase(uc2char c);
  static bool isPunctation(uc2char c);
  static bool isSymbol(uc2char c);
  static bool isTitleCase(uc2char c);
  static bool isMark(uc2char c);
  
  static uc2char toLowerCase(uc2char c);
  static uc2char toUpperCase(uc2char c);
  
  static bool isJavaIdentifierStart(uc2char c);
  static bool isJavaIdentifierPart(uc2char c);
  static bool isUnicodeIdentifierStart(uc2char c);
  static bool isUnicodeIdentifierPart(uc2char c);
  /**
    extended return a description of this character
  */
  static RString getCharacterDescription(uc2char c);
  /// reimplemented from Object
  foreign RString toString();
  static RString toString(uc2char c);
 
  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) UnicodeCharacter(_value); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  static RClass getTYPE();
};

} // lang
} // acdk





#endif //acdk_lang_UnicodeCharacter_h

