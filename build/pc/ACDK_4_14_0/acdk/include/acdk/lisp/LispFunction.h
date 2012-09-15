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


#ifndef acdk_lisp_LispFunction_h
#define acdk_lisp_LispFunction_h

#include "LispList.h"
#include "Function.h"

namespace acdk {
namespace lisp {

ACDK_DECL_CLASS(LispFunction);

class ACDK_ACDK_LISP_PUBLIC LispFunction
: extends LispVar
, implements Function
, implements ::acdk::io::Serializable
{
  ACDK_WITH_METAINFO(LispFunction)
private:
  RString _name;
  RLispList _definition;
  bool _isDefun;
  bool _isMacro;

public:
  // for serialization
  static RObject create_instance() { return new LispFunction(Nil); }

  LispFunction(IN(RLispList) definition);
  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc)
  {
    return new (alc) LispFunction((RLispList)_definition->clone());
  }
  RString name() { return _name; }
  bool isMacro() { return _isMacro; }
  virtual RString toString() { return _name; }
  virtual RString toCode() 
  { 
    return _definition->toCode(); 
  }
  // Fuction
  virtual RString functionName() { return _name; }
  virtual RLispVar eval(IN(RLispEnvironment) env, IN(RLispList) args);
  virtual RString getHelpText();
  virtual RLispList getDefinition() { return _definition; }
  virtual RLispList getDeclDefinition();

  virtual RLispVar evalMacro(IN(RLispEnvironment) env, IN(RLispList) args);
};

} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispFunction_h
