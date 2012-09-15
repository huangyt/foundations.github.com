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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Long.h,v 1.18 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Long_h
#define acdk_lang_Long_h

#include <acdk.h>

#include "Integer.h"

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Long);

/**
  Object wrapper for the basic jlong type
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.18 $
  @date $Date: 2005/04/09 19:26:48 $
*/  
class ACDK_CORE_PUBLIC Long
: extends Number
, implements Comparable
, implements Cloneable

{
  ACDK_WITH_METAINFO(Long)
public:
  static jlong MAX_VALUE;
  static jlong MIN_VALUE;

protected:
  jlong value;
public:
  static RObject create_instance() { return new Long((jlong)0); }
  /// creates 0 Long
  Long() : value(0) {}
  Long(int l) : Number(), value(l) { }
  Long(jlong l) : Number(), value(l) { }
  Long(IN(RString) str) THROWS1(RNumberFormatException);
  /** java 2.0 */
  int compareTo(IN(RLong) o) { return o == Nil ? 1 : longValue() - o->longValue(); }
  /** java 2.0 */
  foreign int compareTo(IN(RObject) o) { return compareTo(RLong(o)); }
  int compareTo(jlong other) { return value - other; }
  bool equals(IN(RLong) o)
  {
    if (o == Nil)
      return false;
    return value == o->longValue();
  }
  /** java 2.0 */
  foreign bool equals(IN(RObject) obj);
  bool equals(jlong other) { return value == other; }
  
  
   /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) Long(value); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  /** java 2.0 */
  static RLong decode(IN(RString) str) THROWS1(RNumberFormatException);
  /** 
    API   : java 2.0<br>
    Status: not testes<br>
    See also: Number
  */
  static jlong parseLong(IN(RString) s) THROWS1(RNumberFormatException) { return parseLong(s, 10); }
  static jlong parseLong(IN(RString) s, int radix) THROWS1(RNumberFormatException);
  
  /// reimplemented from Object
  foreign int hashCode() { return hashCode(value); }
  static int hashCode(jlong value);
  /// reimplemented from Number
  foreign double doubleValue() { return double(value); }
  /// reimplemented from Number
  foreign float floatValue() { return float(value); }
  /// reimplemented from Number
  foreign jlong longValue() { return jlong(value); }
  /// reimplemented from Number
  foreign int intValue() { return int(value); }
  /// reimplemented from Number
  foreign short shortValue() { return short(value); }
  /// reimplemented from Number
  foreign byte byteValue() { return byte(value); }
  foreign virtual dmi::ScriptVar toScriptVar() { return inOf(value); }

  static RString toBinaryString(int i) { return toString(i, 2); }
  static RString toHexString(int i) { return toString(i, 16); }
  static RString toOctalString(int i) { return toString(i, 8); }
  static RString toBinaryString(jlong i) { return toString(i, 2); }
  static RString toHexString(jlong i) { return toString(i, 16); }
  static RString toOctalString(jlong i) { return toString(i, 8); }

  /// reimplemented from Number
  foreign RString toString() { return toString(value, 10); }
  static RString toString(int i) { return toString(i, 10); }
  static RString toString(int i, int radix) ;
  static RString toString(jlong i) { return toString(i, 10); }
  static RString toString(jlong i, int radix) ;
  static RLong valueOf(IN(RString) str)  THROWS1(RNumberFormatException){ return decode(str); }
  static RLong valueOf(IN(RString) str, int radix) THROWS1(RNumberFormatException);

  static RClass getTYPE();
};

} // lang
} // acdk

#endif //acdk_lang_Long_h

