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


#ifndef acdk_lisp_StackFrame_h
#define acdk_lisp_StackFrame_h

#include "LispVar.h"
#include <acdk/util/HashMap.h>

namespace acdk {
namespace lisp {

ACDK_DECL_CLASS(LispStackFrame);

class ACDK_ACDK_LISP_PUBLIC LispStackFrame
: public acdk::lang::Object
{
  acdk::util::RHashMap _current;
  RLispStackFrame _parent;
public:
  LispStackFrame(IN(RLispStackFrame) parent = Nil)
  : _current(new acdk::util::HashMap()),
    _parent(parent)
  {
  }
  bool containsKey(IN(RString) symbol) 
  { 
    if (_current->containsKey((RObject)symbol) == true)
      return true;
    if (_parent == Nil)
      return false;
    return _parent->containsKey(symbol);
  }
      
  RLispVar get(IN(RString) symbol)
  {
    RObject obj = _current->get((RObject)symbol);
    if (obj != Nil) 
      return (RLispVar)obj;
    if (_parent == Nil)
      return Nil;
    return _parent->get(symbol);
  }
  void put(IN(RString) symbol, IN(RLispVar) var, bool forcelocal = false)
  {
    if (_parent != Nil && forcelocal == false) {
      if (_parent->get(symbol) != Nil) {
        _parent->put(symbol, var);
        return;
      }
    }
    RObject old = _current->get((RObject)symbol);
    _current->put((RObject)symbol, (RObject)var);
    RObject newsval = _current->get((RObject)symbol);
  }
  void setParent(IN(RLispStackFrame) p) { _parent = p; }
  acdk::util::RSet keySet() { return _current->keySet(); }
  RString toString();
};


} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_StackFrame_h

