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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Integer.h,v 1.26 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Integer_h
#define acdk_lang_Integer_h

#include "Number.h"
#include "Cloneable.h"
#include "NumberFormatException.h"

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Integer);

/**
  Object wrapper for the basic int type.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:48 $
*/  
class ACDK_CORE_PUBLIC Integer
: extends Number
, implements Comparable
, implements Cloneable

{
  ACDK_WITH_METAINFO(Integer)
public:
  static int MAX_VALUE;
  static int MIN_VALUE;
  //static RClass TYPE;
  static jlong serialVersionUID; //1360826667806852920
protected:
  int value;
public:
  static RObject create_instance() { return new Integer(0); }
  // creates 0 Integer
  Integer() : value(0) {}
  Integer(int val) 
  : Number(),
    value(val)
  {
  }
  Integer(IN(RString) val) THROWS1(RNumberFormatException);
  
  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) Integer(value); }
  /// reimplemented from Object
  RObject clone() { return clone(allocator()); }


  virtual double doubleValue() { return double(value); }
  virtual float floatValue() { return float(value); }
  virtual jlong longValue() { return jlong(value); }
  virtual int intValue() { return int(value); }
  short shortValue() { return short(value); }
  virtual byte byteValue() { return byte(value); }

  virtual int hashCode() { return intValue(); }
  virtual RString toString();
  
  virtual bool equals(IN(RObject) obj);
  virtual bool equals(IN(RInteger) obj)
  {
    return obj == Nil ? false : obj->intValue() == value;
  }
  bool equals(int other) { return value == other; }
  virtual int compareTo(IN(RInteger) o) { return o == Nil ? 1 : value - o->intValue(); }
  virtual int compareTo(IN(RObject) o) { return compareTo((RInteger)o); }
  int compareTo(int other) { return value - other; }

  foreign virtual dmi::ScriptVar toScriptVar() { return inOf(value); }

  static int parseInt(IN(RString) s, int radix = 10) THROWS1(RNumberFormatException)
  {
    return parseInt(s, radix, false);
  }
  static int parseInt(IN(RString) s, int radix, bool decode) THROWS1(RNumberFormatException);
  static int decode(IN(RString) nm)  THROWS1(RNumberFormatException) 
  { return parseInt(nm, 0, true); } //maybe not really that, what's needed

  static RString toString(int val, int radix = 10);
  static RString toBinaryString(int val);
  static RString toHexString(int val);
  static RString toOctalString(int val);
  static RInteger valueOf(IN(RString) str) THROWS1(RNumberFormatException)
  {
    return new Integer(parseInt(str, 10, true));
  }
  static RInteger valueOf(IN(RString) str, int radix) THROWS1(RNumberFormatException)
  {
    return new Integer(parseInt(str, radix, false));
  }

  static RClass getTYPE();
};

} // lang
} // acdk

#endif //acdk_lang_Integer_h

