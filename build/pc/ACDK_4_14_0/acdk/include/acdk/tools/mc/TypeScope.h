
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


#ifndef acdk_tools_mc_TypeScope_h
#define acdk_tools_mc_TypeScope_h

#include "mc.h"
#include <acdk/util/HashMap.h>
#include <acdk/lang/Integer.h>

namespace acdk {
namespace tools {
namespace mc {

ACDK_DECL_CLASS(TypeScope);

enum TypeScopeType
{
  TsUnknown = 0,
  TsEnum = 1
};
ACDK_DEF_LIB_ENUM(ACDK_TOOLS_MC_PUBLIC, TypeScopeType);

class ACDK_TOOLS_MC_PUBLIC TypeScope
: extends acdk::lang::Object
{
public:
  
  RTypeScope _parent;
  acdk::util::RHashMap _types; // RString -> int (Type)
  TypeScope(IN(RTypeScope) parent = Nil)
  : Object()
  , _parent(parent)
  , _types(new ::acdk::util::HashMap())
  {
  }
  /**
    find in current and parent scope the identifier
  */
  TypeScopeType hasType(IN(RString) symbol);
  void addType(TypeScopeType type, IN(RString) symbol)
  {
    _types->put((RObject)symbol, new Integer((int)type));
  }
  /**
    Checks if given Type is supported by ACDK  
  */
  bool checkCompatibleType(IN(RString) str);
  /**
    foreing, typedef, template, friend -> invalid declaration start
    has to be scipped
  */
  bool isInvalidDeclStartIdentifier(IN(RString) str);
  static bool isBasicType(IN(RString) str);
  static bool isObjectType(IN(RString) str);
  
  RString clazzInfoExpr(IN(RString) tpname);
};

} // namespace mc
} // namespace tools
} // namespace acdk

#endif //acdk_tools_mc_TypeScope_h
