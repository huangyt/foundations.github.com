
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


#ifndef acdk_tools_mc_TokenStack_h
#define acdk_tools_mc_TokenStack_h

#include "mc.h"

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(TokenStackElement);

class TokenStackElement
: extends acdk::lang::Object
{
public:
  int ttype;
  RString sval;
  RNumber nval;
  TokenStackElement(IN(RStreamTokenizer) in)
  : ttype(in->ttype),
    sval(in->sval),
    nval(in->nval)
  {
  } 
  TokenStackElement(int type, IN(RString) stringval, IN(RNumber) dval)
  : ttype(type),
    sval(stringval),
    nval(dval)
  {
  }
};


ACDK_DECL_CLASS(TokenStack);

class TokenStack
: extends acdk::lang::Object
{
public:
  RStreamTokenizer _in;
  RArrayList _stack; //RTokenStackElement
  TokenStack(IN(RStreamTokenizer) in)
  : Object(),
    _in(in),
    _stack(new ArrayList())
  {
  }
  virtual ~TokenStack()
  {
    if (_stack == Nil)
      return;
    while (_stack->size() > 0)
      pop();
  }
  void push()
  {
    _stack->add(new TokenStackElement(_in));
  }
  void pop()
  {
    RTokenStackElement lelement = RTokenStackElement(_stack->get(_stack->size() - 1));
    _in->pushBack(lelement->ttype, lelement->sval/*, lelement->nval*/);
    _stack->removeRange(_stack->size() - 1, _stack->size());
  }
  void flush()
  {
    _stack = new ArrayList();
  }
  RArrayList stack() { return _stack; }
};

struct WantWhiteSpaceOnStack
{
  bool oldwwsp; 
  RStreamTokenizer in;
  WantWhiteSpaceOnStack(IN(RStreamTokenizer) intokenizer, bool wantws)
  : in(intokenizer)
  {
    oldwwsp = in->wantWhiteSpace(wantws);
  }
  ~WantWhiteSpaceOnStack()
  {
    in->wantWhiteSpace(oldwwsp);
  }
};

struct WantNLOnStack
{
  bool oldwwsp; 
  RStreamTokenizer in;
  WantNLOnStack(IN(RStreamTokenizer) intokenizer, bool wantnl)
  : in(intokenizer)
  {
    oldwwsp = in->wantNewline(wantnl);
  }
  ~WantNLOnStack()
  {
    in->wantNewline(oldwwsp);
  }
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_TokenStack_h
