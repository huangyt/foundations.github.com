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


#ifndef acdk_lisp_LispArray_h
#define acdk_lisp_LispArray_h

#include "LispFunction.h"

namespace acdk {
namespace lisp {


ACDK_DECL_CLASS(LispArray);

class ACDK_ACDK_LISP_PUBLIC LispArray
: public LispVar
{
  ACDK_WITH_METAINFO(LispArray)
private:
  RLispVarArray _list;
public:
  LispArray(int size = 0)
  :  LispVar(),
    _list(new LispVarArray(size))
  {
  }
  LispArray(IN(RLispList) other)
  :  LispVar(),
    _list(new LispVarArray(other == Nil ? 0 : other->length()))
  {
    for (int i = 0; i < other->length(); i++)
      _list[i] = get(i);

  }
  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc);
  void append(IN(RLispVar) var)
  {
    int olength = _list->length();
    _list->resize(olength + 1);
    _list[olength] = var;
  }
  RLispVar last()
  {
    if (_list->length() == 0)
      return Nil;
    return _list[_list->length() - 1];
  }
  RString toString()
  {
    StringBuffer buf(200);
    for (int i = 0; i < _list->length(); i++) {
      if (_list[i] != Nil)
        buf.append(_list[i]->toString());
    }
    return buf.toString();
  }
  RString toCode()
  {
    StringBuffer buf(200);
    buf.append("(");
    for (int i = 0; i < _list->length(); i++) {
      if (i != 0)
        buf.append(" ");
      RString tstr = _list[i] == Nil ? RString("Nil") : _list[i]->toCode();
      buf.append(tstr);
    }
    buf.append(")");
    return buf.toString();
  }
  
  int length() { return _list == Nil ? 0 : _list->length(); }
  RLispVar get(int i) { return _list->get(i); }
  void set(int i, IN(RLispVar) var)
  {
    if (i > _list->length() -1) 
      _list->resize(i + 1);
    _list[i] = var;
  }
  
};


} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispArray_h
