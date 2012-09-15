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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Byte.h,v 1.20 2005/04/16 10:46:45 kommer Exp $

#ifndef acdk_lang_Byte_h
#define acdk_lang_Byte_h

#include "Number.h"
#include "Comparable.h"

#include "Exception.h"
#include "ClassCastException.h"
#include "NumberFormatException.h"

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Byte);

/** 
  Object wrapper to a byte.
  API: Java<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/16 10:46:45 $
  
  TYPE is not field, but method.
*/

class ACDK_CORE_PUBLIC Byte
: extends Number
, implements Comparable
, implements Cloneable
{
  ACDK_WITH_METAINFO(Byte)
private:
  byte value;
protected:
  
public:
  static byte MAX_VALUE;
  static byte MIN_VALUE;
  

  static RObject create_instance() { return new Byte(); }
  /// create 0 byte
  Byte() : value(0) {}
  Byte(byte val) 
  : value(val) 
  {
  }
  Byte(IN(RString) str) THROWS1(RNumberFormatException);
  virtual int compareTo(IN(RByte) o);

  /// reimplemented from Object
  foreign virtual int compareTo(IN(RObject) o);
  int compareTo(byte other) { return value - other; }
  /// reimplemented from Object
  foreign  bool equals(IN(RObject) o);
  bool equals(byte other) { return other == value; }

  foreign byte byteValue() { return byte(value); }
  foreign short shortValue();
  foreign int intValue();
  foreign jlong longValue();
  foreign float floatValue();
  foreign double doubleValue();
  
  /// reimplemented from Object
  foreign RString toString();
  
  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) Byte(value); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  foreign virtual dmi::ScriptVar toScriptVar() { return inOf(value); }
  static RByte decode(IN(RString) str) THROWS1(RNumberFormatException);
  static char parseByte(IN(RString) s, int radix = 10) THROWS1(RNumberFormatException);
  static RByte valueOf(IN(RString) s, int radix = 10) THROWS1(RNumberFormatException);
  
  static RClass getTYPE();
};




} // lang
} // acdk



#endif //acdk_lang_Byte_h

