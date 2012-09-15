
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

#include "ArgumentInfo.h"

namespace acdk {
namespace tools {
namespace mc {

using namespace acdk::lang::dmi;


RObject 
ArgumentInfo::clone(sys::Allocator* alc)
{
  RArgumentInfo ai = new (alc) ArgumentInfo(_parent, type, name, flags);
  ai->hasDefaultInitializer = hasDefaultInitializer;
  ai->defaultInitializer = defaultInitializer;
  ai->_props = _props;
  ai->dmiName = dmiName;
  ai->_genCode = _genCode;
  return &ai;
}

RString
ArgumentInfo::toCode()
{
  StringBuffer sb(100);
  if (flags & MiAiByval) {
    if (flags & MiAiIn && flags & MiAiOut) {
      sb.append("BYVALINOUT("); sb.append(type); sb.append(") "); sb.append(name);
    } else if (flags & MiAiIn) {
      sb.append("BYVALIN("); sb.append(type); sb.append(") "); sb.append(name);
    } else if (flags & MiAiOut) {
      sb.append("BYVALOUT("); sb.append(type); sb.append(") "); sb.append(name);
    } else {
      sb.append("BYVALIN("); sb.append(type); sb.append(") "); sb.append(name);
    }
  } else if (flags & MiAiIn && flags & MiAiOut) {
    sb.append("INOUT("); sb.append(type); sb.append(") "); sb.append(name);
  } else if (flags & MiAiIn) {
    sb.append("IN("); sb.append(type); sb.append(") "); sb.append(name);
  } else if (flags & MiAiOut) {
    sb.append("OUT("); sb.append(type); sb.append(") "); sb.append(name);
  } else {
    sb.append(type); sb.append(" "); sb.append(name);
  }
  if (defaultInitializer != Nil)
    sb.append(defaultInitializer);
  return sb.toString();
}

//virtual 
RString 
ArgumentInfo::getMetaInfoCIdentifier()
{
  RMethodInfo mi = (RMethodInfo)_parent;
  RClassInfo ci = (RClassInfo)mi->_parent;
  if (mi->argcount == -1)
    mi->argcount = mi->getArgCount();
  return ci->name + "_methods_" +  mi->getJavaSignature(true, mi->argcount) + "_arg_" + name;
}

RString
ArgumentInfo::getScriptVarGetter(IN(RString) type)
{
  
  if (type->equals("bool") == true || type->equals("boolean") == true)
    return "getBoolVar()";
  if (type->equals("char") == true)
    return "getCharVar()";
  if (type->equals("ucchar") == true || type->equals("uc2char") == true)
    return "getUcCharVar()";
  if (type->equals("byte") == true)
    return "getByteVar()";
  if (type->equals("short") == true)
    return "getShortVar()";
  if (type->equals("int") == true)
    return "getIntVar()";
  if (type->equals("jlong") == true)
    return "getLongVar()";
  if (type->equals("float") == true)
    return "getFloatVar()";
  if (type->equals("double") == true)
    return "getDoubleVar()";
  if (type->indexOf("::") >= 0)
    return "getObjectVar()";
  if (type->startsWith("R") == true && Character::isUpperCase(type->charAt(1)) == true)
    return "getObjectVar()";
  return "getIntVar()";
}

RString
ArgumentInfo::castToScriptVar(IN(RString) type)
{
  RString cn = type;
  int idx = cn->lastIndexOf(':');
  if (idx != -1)
    cn = cn->substr(idx + 1);
  
  if (cn->startsWith("R") && Character::isUpperCase(cn->charAt(1)) == true) {
    return "(RObject)";
  }
  return "(" + type + ")";
}


bool 
ArgumentInfo::invokeCodeAttributes(IN(RModuleInfo) cm, IN(RClassInfo) ci, IN(RMethodInfo) mi)
{
  if (_parent != mi)
    _parent = &mi;
  return CodeInfo::invokeCodeAttributes();
}



} // namespace mc
} // namespace tools
} // namespace acdk
