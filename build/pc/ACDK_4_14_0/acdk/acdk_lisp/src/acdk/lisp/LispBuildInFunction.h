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


#ifndef acdk_lisp_LispBuildInFunction_h
#define acdk_lisp_LispBuildInFunction_h

#include "LispFunction.h"

namespace acdk {
namespace lisp {


ACDK_DECL_CLASS(LispBuildInFunction);

class ACDK_ACDK_LISP_PUBLIC LispBuildInFunction
: extends LispVar,
  implements Function
{
  ACDK_WITH_METAINFO(LispBuildInFunction)
private:
  RString _name;
  RFunction _function;
public:
  LispBuildInFunction(IN(RString) name, IN(RFunction) function)
  : LispVar(),
    _name(name),
    _function(function)
  {
  }
  virtual RObject clone() { return clone(allocator()); }
  virtual RObject clone(sys::Allocator* alc)
  {
    return new (alc) LispBuildInFunction(_name, _function);
  }
  RString name() { return _name; }
  virtual RString toString() { return _name; }
  virtual RString toCode() 
  { 
    return getDefinition()->toCode(); 
  }
  RFunction getFunction() { return _function; }
// Fuction
  virtual RString functionName() { return _name; }
  virtual RString getHelpText() { return _function->getHelpText(); }
  virtual RLispList getDefinition() { return _function->getDefinition(); }
  virtual RLispList getDeclDefinition() { return _function->getDeclDefinition(); }
  virtual RLispVar eval(IN(RLispEnvironment) env, IN(RLispList) args);
};



} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispBuildInFunction_h
