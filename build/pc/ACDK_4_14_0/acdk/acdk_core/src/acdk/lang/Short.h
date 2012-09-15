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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Short.h,v 1.17 2005/04/09 19:26:50 kommer Exp $

#ifndef acdk_lang_Short_h
#define acdk_lang_Short_h

#include "Number.h"
#include "Comparable.h"
#include "Cloneable.h"
#include "NumberFormatException.h"

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Short);

/** 
  Object wrapper for the basic short type.
  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.17 $
  @date $Date: 2005/04/09 19:26:50 $
*/  
class ACDK_CORE_PUBLIC Short
: extends Number
, implements Comparable
, implements Cloneable
{
  ACDK_WITH_METAINFO(Short)
public:
  static short MAX_VALUE;
  static short MIN_VALUE;
protected:
  short value;
public:
  static RObject create_instance() { return new Short(short(0)); }
  Short(IN(RString) str) THROWS1(RNumberFormatException);
  Short(short s);
  static short parseShort(IN(RString) str, int radix = 10) THROWS1(RNumberFormatException);
  static RShort decode(IN(RString) str) THROWS1(RNumberFormatException);
  
  /// reimplemented from Object
  foreign int compareTo(IN(RObject) o);
  virtual int compareTo(IN(RShort) anotherShort);
  int compareTo(short other) { return value - other; }
  bool equals(IN(RShort) o)
  {
    if (o == Nil)
      return false;
    return value == o->shortValue();
  }
  foreign bool equals(IN(RObject) o)
  {
    if (o == Nil)
      return false;
    if (instanceof(o, Short) == false)
      return false;
    return equals(RShort(o));
  }
  bool equals(short other) { return value == other; }

  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) Short(value); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  /// reimplemented from Object
  foreign int hashCode() { return int(value); }

  /// reimplemented from Number
  foreign byte byteValue() { return byte(value); }
  
  /// reimplemented from Number
  foreign short shortValue() { return value; }
  
  /// reimplemented from Number
  foreign int intValue() { return int(value); }

  /// reimplemented from Number
  foreign jlong longValue() { return jlong(value); }

  /// reimplemented from Number
  foreign float floatValue() { return float(value); }

  /// reimplemented from Number
  foreign double doubleValue() { return double(value); }
  
  /// reimplemented from Object
  foreign RString toString();
  foreign virtual dmi::ScriptVar toScriptVar() { return inOf(value); }
  static RString toString(short s, int radix = 10);
  static RShort valueOf(IN(RString) s, int radix = 10) THROWS1(RNumberFormatException);
          
  static RClass getTYPE();
};


} // lang
} // acdk


#endif //acdk_lang_Short_h

