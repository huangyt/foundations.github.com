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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Double.h,v 1.23 2005/04/09 19:26:48 kommer Exp $

#ifndef acdk_lang_Double_h
#define acdk_lang_Double_h

#include "Number.h"
#include "Comparable.h"
#include "Cloneable.h"
#include "NumberFormatException.h"

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Double);

/**
  Object wrapper for the basic double type.
  API: Java with extensions
  @author Roger Rene Kommer
  @version $Revision: 1.23 $
  @date $Date: 2005/04/09 19:26:48 $
*/  
class ACDK_CORE_PUBLIC Double
: extends Number
, implements Comparable
, implements Cloneable

{
  ACDK_WITH_METAINFO(Double)
public:
  static double MAX_VALUE;
  static double MIN_VALUE;
  static double NaN;
  static double NEGATIVE_INFINITY;
  static double POSITIVE_INFINITY;
  //static RClass TYPE;
protected:
  double value;
public:
  static RObject create_instance() { return new Double(0); }
  /// creates 0.0 double
  Double() : value(0.0) {}
  Double(double d) : Number(), value(d) { }
  /**
    parse the string
    @param str string to parse
    @param locale if locale is Nil it uses the C / en_US encoding (not the default)
    @see Number with more extended parsing functions
  */
  Double(IN(RString) str, IN(acdk::util::RLocale) locale = Nil) THROWS1(RNumberFormatException);

  virtual int compareTo(IN(RDouble) o) { return o == Nil ? 1 : int(floatValue() - o->floatValue()); }
  foreign int compareTo(IN(RObject) o) { return compareTo(RDouble(o)); }
  int compareTo(double other) { return int(value - other); }

  bool equals(IN(RDouble) o)
  {
    if (o == Nil)
      return false;
    return value == o->doubleValue();
  }
  foreign bool equals(IN(RObject) obj);
  bool equals(double other) { return value == other; }
  
  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) Double(value); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  /**
    
    @param str string to parse
    @param locale if locale is Nil it uses the C / en_US encoding (not the default)
    @see Number with more extended parsing functions
  */
  static double parseDouble(IN(RString) s, IN(acdk::util::RLocale) locale = Nil) THROWS1(RNumberFormatException);
  foreign int hashCode() { return hashCode(value); }
  static int hashCode(double value);
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
  static RString toString(double i, IN(acdk::util::RLocale) locale = Nil);
  


  static RDouble valueOf(IN(RString) str) THROWS1(RNumberFormatException);
  static RClass getTYPE();
  static double longBitsToDouble(jlong val)
  { 
    return *(double*)&val;
  }
  static jlong doubleToRawLongBits(double val)
  {
    return *(jlong*)&val;
  }
  bool isNaN();
  static bool isNaN(double v);
  bool isInfinite() ;
  static bool isInfinite(double v);
};


} // lang
} // acdk

#endif //acdk_lang_Double_h

