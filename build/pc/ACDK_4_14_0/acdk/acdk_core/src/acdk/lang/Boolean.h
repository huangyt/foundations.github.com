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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Boolean.h,v 1.16 2005/03/06 11:56:59 kommer Exp $

#ifndef acdk_lang_Boolean_h
#define acdk_lang_Boolean_h

#include <acdk.h>

#include <acdk/io/Serializable.h>

namespace acdk {
namespace lang {

ACDK_DECL_CLASS(Boolean);

/** 
  
  In ACDK the type boolean is replaced with buildin type bool.
  API: Java<br>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/03/06 11:56:59 $
  
  TRUE, FALSE, TYPE are not fields, but methods.
*/

class ACDK_CORE_PUBLIC Boolean
: extends Object
, implements acdk::io::Serializable
, implements acdk::lang::Cloneable
{
  ACDK_WITH_METAINFO(Boolean)
private:
  bool value;
  static RBoolean _falseValue;
  static RBoolean _trueValue;
protected:
  
public:
  /// create false Boolean
  Boolean() : value(false) {}
  static RObject create_instance() { return new Boolean(false); }
  
  /** 
    Instead of the JDK static values FALSE and TRUE, these values are wrapped in two functions
    because TRUE and FALSE will often #defined by system include headers
  */

  static RBoolean getFALSE();
  static RBoolean getTRUE();

  Boolean(bool val) 
  : Object(),
    value(val)
  {
  }


  Boolean(IN(RString) val);
  virtual bool booleanValue() { return value; }

  /// reimplemented from Object
  foreign virtual bool equals(IN(RObject) obj);

  bool equals(bool other) { return value == other; }

  /// reimplemented from Object
  foreign virtual RString toString();

  /// reimplemented from Object
  foreign virtual int hashCode();
  
  /// reimplemented from Object
  foreign RObject clone(sys::Allocator* alc) { return new (alc) Boolean(value); }
  /// reimplemented from Object
  foreign RObject clone() { return clone(allocator()); }

  static bool getBoolean(IN(RString) name);
  static RBoolean valueOf(IN(RString) s);
  static RBoolean valueOf(bool value)
  {
    return value == true ? getTRUE() : getFALSE();
  }
  
  static RClass getTYPE();
};


} // lang
} // acdk

#endif //acdk_lang_Boolean_h

