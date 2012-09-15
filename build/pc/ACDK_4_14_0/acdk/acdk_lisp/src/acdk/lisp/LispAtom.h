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


#ifndef acdk_lisp_LispAtom_h
#define acdk_lisp_LispAtom_h

#include "LispVar.h"
#include <acdk/io/ObjectReader.h>
#include <acdk/io/ObjectWriter.h>

namespace acdk {
namespace lisp {


ACDK_DECL_CLASS(LispAtom);

/**
  Strings or Number
  */
class ACDK_ACDK_LISP_PUBLIC LispAtom
: extends LispVar
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(LispAtom)
private:
  ScriptVar _val;
public:
  /// for serialization
  static RObject create_instance() { return new LispAtom(jlong(0)); }

  LispAtom(IN(RString) val) 
  : LispVar(),
    _val(&val)
  {
  }
  LispAtom(jlong l)
  : LispVar(),
    _val(l)
  {
  }
  LispAtom(double l)
  : LispVar(),
    _val(l)
  {
  }
  LispAtom(const ScriptVar& v)
  : LispVar(),
    _val(v)
  {
  }
  virtual RString toString() 
  { 
    return _val.toString(); 
  }
  virtual RString toCode() 
  { 
    return _val.toCode(); 
  }
  ScriptVar& val() { return _val; }
  virtual RObject getObject() 
  {
    return _val.getObjectVar();
  }
  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc) { return new (alc) LispAtom(_val); }

  void writeObject(IN(::acdk::io::RObjectWriter) out, IN(RClass) cls)
  {
    if (cls == GetClass())
      out->writeScriptVar(_val, true, true);
    else
      LispVar::writeObject(out, cls);
  }
  void readObject(IN(::acdk::io::RObjectReader) in, IN(RClass) cls)
  {
    if (cls == GetClass())
      _val = in->readScriptVar(true, true);
    else
      LispVar::readObject(in, cls);
  }
};

} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispAtom_h
