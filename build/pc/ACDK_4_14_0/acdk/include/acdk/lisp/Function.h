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


#ifndef acdk_lisp_Function_h
#define acdk_lisp_Function_h

#include "LispVar.h"

namespace acdk {
namespace lisp {

ACDK_DECL_CLASS(LispEnvironment);
ACDK_DECL_CLASS(LispList);

ACDK_DECL_INTERFACE(Function);

class ACDK_ACDK_LISP_PUBLIC Function
      ACDK_INTERFACEBASE
//: implements ::acdk::lang::Cloneable
      
{
  ACDK_WITH_METAINFO(Function)
public:
  virtual RString functionName() = 0;
  /**
    args[0] == the symbol of function
    args[1 + n] == the arguments
  */
  virtual RLispVar eval(IN(RLispEnvironment) env, IN(RLispList) args) = 0;
  virtual RString getHelpText() = 0;
  virtual RLispList getDefinition() = 0;
  virtual RLispList getDeclDefinition() = 0;
};

typedef RLispVar (*EvalFunc)(IN(RLispEnvironment) env, IN(RLispList) args);

} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_Function_h
