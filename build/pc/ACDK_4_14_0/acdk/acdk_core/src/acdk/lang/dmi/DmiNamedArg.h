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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiNamedArg.h,v 1.10 2005/02/05 10:44:58 kommer Exp $

#ifndef acdk_lang_dmi_DmiNamedArg_h
#define acdk_lang_dmi_DmiNamedArg_h


#include <acdk.h>
#include "DmiObject.h"

namespace acdk {
namespace lang {
namespace dmi {

ACDK_DECL_CLASS(DmiNamedArg);



/** 
  Wrapper to a NamedArgs
*/
class ACDK_CORE_PUBLIC DmiNamedArg
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(DmiNamedArg)
public:
  RString name;
  RDmiObject value;

  DmiNamedArg(IN(RString) nam, IN(RDmiObject) val)
  : Object()
  , name(nam)
  , value(val)
  {
  }
};

/*
ACDK_DECL_CLASS(DmiNamedArgs);

class ACDK_CORE_PUBLIC DmiNamedArgs
: extends ::acdk::lang::DmiNamedArray
, implements ::acdk::util::Map
{
  RDmiNamedArgArray _pairs;
public:
  DmiNamedArgs(int size)
    : _pairs(new DmiNamedArgArray(size))
  {}

  // from Map
  virtual void clear() 
  {
    _pairs->resize(0);
  }
  virtual bool containsKey(IN(RObject) key);
  virtual bool containsValue(IN(RObject) value);
  virtual acdk::util::RSet entrySet();
  virtual bool equals(IN(RObject) o);
  virtual RObject get(IN(RObject) key);
  virtual int hashCode();
  virtual bool isEmpty();
  virtual acdk::util::RSet keySet();
  virtual RObject put(IN(RObject) key, IN(RObject) value);
  virtual void putAll(IN(acdk::util::RMap) m);
  virtual RObject remove(IN(RObject) o);
  virtual int size() { return _pairs->length(); }
  virtual acdk::util::RCollection values();
};
*/

} // namespace dmi
} // namespace lang
} // namespace acdk

#endif // acdk_lang_dmi_DmiNamedArg_h

