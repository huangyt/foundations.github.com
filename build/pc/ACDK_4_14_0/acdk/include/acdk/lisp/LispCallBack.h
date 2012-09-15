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


#ifndef acdk_lisp_LispCallBack_h
#define acdk_lisp_LispCallBack_h


#include "Function.h"

namespace acdk {
namespace lisp {

ACDK_DECL_CLASS(LispCallBack);

class ACDK_ACDK_LISP_PUBLIC LispCallBack
: extends acdk::lang::Object,
  implements Function
{
  ACDK_WITH_METAINFO(LispCallBack)
private:
  EvalFunc _evalfunc;
  bool _evalargs;
  bool _ownScope;
  //char* _helptext;
  RLispList _declCode;
public:
  /**
    @arg lispdecl an valid lisp declaration like "(defun funcname (arg1 arg2)"
    @arg evalfunc the callback function
    @arg evalargs if true the arguments will be evaluated before caling function
    @arg an static char* help string 
  */
  LispCallBack(const char* lispdecl, EvalFunc evalfunc, bool evalargs = true, bool ownsScope = true/*, char* helptext = ""*/);
  virtual RString functionName() 
  { 
    return "<unknown>";//return _code->cdr()->car()->toString();
  }
  virtual RLispVar eval(IN(RLispEnvironment) env, IN(RLispList) args);
  virtual RString getHelpText();
  virtual RLispList getDefinition();
  virtual RLispList getDeclDefinition();
  bool ownScope() { return _ownScope; }
  void ownScope(bool own) { _ownScope = own; }
};


} // namespace lisp
} // namespace acdk


#endif //acdk_lisp_LispCallBack_h
