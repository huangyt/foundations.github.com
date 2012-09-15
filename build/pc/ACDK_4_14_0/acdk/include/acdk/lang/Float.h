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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Float.h,v 1.22 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Float_h
#define acdk_lang_Float_h

#include "Number.h"
#include "Comparable.h"
#include "Cloneable.h"
#include "NumberFormatException.h"

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Float);
/**
  Object wrapper for the basic float type.
  API: Java with extensions
  @author Roger Rene Kommer
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:48 $
*/  
class ACDK_CORE_PUBLIC Float
: extends Number
, implements Comparable
, implements Cloneable

{
  ACDK_WITH_METAINFO(Float)
public:
  static float MAX_VALUE;
  static float MIN_VALUE;

protected:
  float value;
public:
  static RObject create_instance() { return new Float(0.0); }
  /// creates 0.0 float
  Float() : value(0.0) {}
  Float(float l) : Number(), value(l) { }
  /**
    @see Double
  */
  Float(IN(RString) str, IN(acdk::util::RLocale) locale = Nil) THROWS1(RNumberFormatException);

  virtual int compareTo(IN(RFloat) o) { return o == Nil ? 1 : int(floatValue() - o->floatValue()); }
  foreign int compareTo(IN(RObject) o) { return compareTo(RFloat(o)); }
  int compareTo(float other) { return int(value - other); }

  bool equals(IN(RFloat) o)
  {
    if (o == Nil)
      return false;
    return value == o->floatValue();
  }
  foreign virtual bool equals(IN(RObject) obj);
  bool equals(float other) { return value == other; }
  
  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) Float(value); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  static float parseFloat(IN(RString) s, IN(acdk::util::RLocale) locale = Nil) THROWS1(RNumberFormatException);
  foreign int hashCode() { return hashCode(value); }
  static int hashCode(float value) {  return *((int*)&value); }
  foreign double doubleValue() { return double(value); }
  foreign float floatValue() { return float(value); }
  foreign jlong longValue() { return jlong(value); }
  foreign int intValue() { return int(value); }
  foreign short shortValue() { return short(value); }
  foreign byte byteValue() { return byte(value); }

  foreign RString toString() { return toString(value); }
  foreign virtual dmi::ScriptVar toScriptVar() { return inOf(value); }
  /**
    if locale == Nil uses the C / en_US locals
  */
  static RString toString(float i, IN(acdk::util::RLocale) locale = Nil);
  static RFloat valueOf(IN(RString) str) THROWS1(RNumberFormatException);

  static RClass getTYPE();
};

} // Lang
} // acdk

#endif //acdk_lang_Float_h

