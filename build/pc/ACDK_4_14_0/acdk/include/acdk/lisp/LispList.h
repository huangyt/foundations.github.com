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


#ifndef acdk_lisp_LispList_h
#define acdk_lisp_LispList_h

#include "LispVar.h"


namespace acdk {
namespace lisp {


ACDK_DECL_CLASS(LispList);


class ACDK_ACDK_LISP_PUBLIC LispList
: extends LispVar
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(LispList)
   
  /** 
    remains always nil.
    Used for Nil references.
  */
  static RLispVar _nilVar;
public:
  /// for serialization
  static RObject create_instance() { return new LispList(); }

  RLispVar _car;
  RLispList _cdr;
  LispList(IN(RLispVar) car = Nil, IN(RLispList) cdr = Nil)
  : LispVar(),
    _car(car),
    _cdr(cdr)
  { 
  }
  LispList(int size)
  : LispVar(),
    _car(),
    _cdr()
  { 
    if (--size > 0)
      _cdr = new LispList(size);
  }
  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc);

  inline RETOUT(RLispVar) car()
  {
    if (_car != Nil)
      return _car;
    return _nilVar; 
  }
  inline RETOUT(RLispList) cdr() { return _cdr; }
  inline void setCdr(IN(RLispList) lst) { _cdr = lst; }
  inline void setCar(IN(RLispVar) var) { _car = var; }
  RLispList append(IN(RLispVar) var);
  RLispList push(IN(RLispVar) var) { return append(var); }
  RLispList push(IN(RObject) var);
  RLispList unshift(IN(RObject) var);
  RLispList unshift(IN(RLispVar) var);
  RLispVar pop();
  RString toString();
  RString toCode();
  int length();
  RLispVar get(int i);
  void set(int i, IN(RLispVar) var);
  RETOUT(RLispVar) last()
  {
    if (_cdr == Nil)
      return _car;
    return _cdr->last();
  }
  
};


} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispList_h
