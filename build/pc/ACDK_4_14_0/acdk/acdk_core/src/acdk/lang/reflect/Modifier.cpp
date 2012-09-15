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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Modifier.cpp,v 1.15 2005/02/28 08:14:04 kommer Exp $


#include <acdk.h>
#include "Modifier.h"

#include "../StringBuffer.h"

namespace acdk {
namespace lang {
namespace reflect {

#if 0

//static 
RString 
Modifier::toString(int mod) 
{
  RStringBuffer sb = new StringBuffer();
  /*
  
  if(isPublic(mod)) {
    sb->append("public");
  } else if(isPrivate(mod)) {
    sb->append("private");
  } else if(isProtected(mod)) {
    sb->append("protected");
  }
  
  if(isAbstract(mod)) {
    if(sb->length() > 0) {
      sb->append(' ');
    }
    sb->append("abstract");
  }
  
  if(isStatic(mod)) {
    if(sb->length() > 0) {
      sb->append(' ');
    }
    sb->append("static");
  }
  
  if(isFinal(mod)) {
    if(sb->length() > 0) {
      sb->append(' ');
    }
    sb->append("final");
  }
  
  if(isSynchronized(mod)) {
    if(sb->length() > 0) {
      sb->append(' ');
    }
    sb->append("synchronized");
  }
  
  if(isNative(mod)) {
    if(sb->length() > 0) {
      sb->append(' ');
    }
    sb->append("native");
  }
  
  if(isTransient(mod)) {
    if(sb->length() > 0) {
      sb->append(' ');
    }
    sb->append("transient");
  }
  
  if(isVolatile(mod)) {
    if(sb->length() > 0) {
      sb->append(' ');
    }
    sb->append("volatile");
  }
  
  if(isInterface(mod)) {
    if(sb->length() > 0) {
      sb->append(' ');
    }
    sb->append("interface");
  }
   ## continue;
  InParam   =         0x10001000,
    OutParam   =        0x20002000,
    ReadOnly  =         0x40004000,
    */
  return sb->toString();
}


namespace {

void appendMod(StringBuffer& sb, IN(RString) mod)
{
   if (sb.length() > 0)
      sb.append(" | ");
   sb.append(mod);
}


struct ModifierToCode
{
  const char* name;
  int value;
};
#define MODTABLEENTRY(mod) { #mod, mod },

ModifierToCode modtable[] = 
{
  MODTABLEENTRY(::acdk::lang::dmi::MiPublic)
  MODTABLEENTRY(::acdk::lang::dmi::MiPrivate)
  MODTABLEENTRY(::acdk::lang::dmi::MiProtected)
  MODTABLEENTRY(::acdk::lang::dmi::MiStatic)
  MODTABLEENTRY(::acdk::lang::reflect::Modifier::FINAL)
  MODTABLEENTRY(::acdk::lang::dmi::MiProtected)
  MODTABLEENTRY(::acdk::lang::reflect::Modifier::SYNCHRONIZED)
  MODTABLEENTRY(::acdk::lang::reflect::Modifier::VOLATILE)
  MODTABLEENTRY(::acdk::lang::dmi::MiIvTransientCall)
  MODTABLEENTRY(::acdk::lang::reflect::Modifier::NATIVE)
  MODTABLEENTRY(::acdk::lang::dmi::MiCiInterface)
  MODTABLEENTRY(::acdk::lang::dmi::MiCiAbstract)
  MODTABLEENTRY(::acdk::lang::reflect::MiMcKnownType)
  MODTABLEENTRY(::acdk::lang::dmi::MiCiWeakBind)
  MODTABLEENTRY(::acdk::lang::dmi::MiNonStatic)
  MODTABLEENTRY(::acdk::lang::dmi::MiAiIn)
  MODTABLEENTRY(::acdk::lang::dmi::MiAiOut)
  MODTABLEENTRY(::acdk::lang::dmi::MiReadOnly)
  MODTABLEENTRY(::acdk::lang::dmi::MiAiByval)
  MODTABLEENTRY(::acdk::lang::dmi::MiAiByref)
  MODTABLEENTRY(::acdk::lang::dmi::MiCiCloneable)
  MODTABLEENTRY(::acdk::lang::dmi::MiMiVirtual)
  MODTABLEENTRY(::acdk::lang::dmi::MiMiOneway)
  MODTABLEENTRY(::acdk::lang::dmi::MiCiSerializable)
  MODTABLEENTRY(::acdk::lang::dmi::MiCiThrowable)
  MODTABLEENTRY(::acdk::lang::dmi::MiCiArray)
  MODTABLEENTRY(::acdk::lang::dmi::MiMiConstructor)
  MODTABLEENTRY(::acdk::lang::dmi::MiMiDestructor)
  MODTABLEENTRY(::acdk::lang::dmi::MiEnumInfo)
  MODTABLEENTRY(::acdk::lang::dmi::MiMethodInfo)
  MODTABLEENTRY(::acdk::lang::dmi::MiFieldInfo)
  MODTABLEENTRY(::acdk::lang::dmi::MiDelete)

  { 0, 0 }
  //{ "::acdk::lang::dmi::MiPublic", ::acdk::lang::dmi::MiPublic }
};

RString modifierToCode(int &mod)
{
  StringBuffer sb;
  for (int i = 0; modtable[i].name != 0; ++i)
  {
    if (modtable[i].value & mod)
    {
      appendMod(sb, modtable[i].name);
      mod &= ~modtable[i].value;
    }
  }
  if (mod != 0)
    appendMod(sb, String::valueOf(mod));
  if (sb.length() == 0)
    return "0";
  return sb.toString();
    
}
} // anon namespace


//static 
RString 
Modifier::toCode(int mod)
{
  return String::valueOf(mod);
  //return modifierToCode(mod);
  /*
  StringBuffer sb;
  
  if (mod & PUBLIC)
    appendMod(sb, "::acdk::lang::dmi::MiPublic");
  if (mod & PRIVATE)
    appendMod(sb, "::acdk::lang::dmi::MiPrivate");
  if (mod & PROTECTED)
    appendMod(sb, "::acdk::lang::dmi::MiProtected");
  if (mod & STATIC)
    appendMod(sb, "::acdk::lang::dmi::MiStatic");
  if (mod & FINAL)
    appendMod(sb, "::acdk::lang::reflect::Modifier::FINAL");
  if (mod & SYNCHRONIZED)
    appendMod(sb, "::acdk::lang::reflect::Modifier::SYNCHRONIZED");
  if (mod & VOLATILE)
    appendMod(sb, "::acdk::lang::reflect::Modifier::VOLATILE");
  if (mod & TRANSIENT)
    appendMod(sb, "::acdk::lang::dmi::MiIvTransientCall");
  if (mod & NATIVE)
    appendMod(sb, "::acdk::lang::reflect::Modifier::NATIVE");
  if (mod & INTERFACE)
    appendMod(sb, "::acdk::lang::dmi::MiCiInterface");
  if (mod & ABSTRACT)
    appendMod(sb, "::acdk::lang::dmi::MiCiAbstract");
  if (mod & InParam)
    appendMod(sb, "::acdk::lang::dmi::MiAiIn");
  if (mod & OutParam)
    appendMod(sb, "::acdk::lang::dmi::MiAiOut");
  if (mod & ReadOnly)
    appendMod(sb, "::acdk::lang::dmi::MiReadOnly");
  if (mod & ByVal)
    appendMod(sb, "::acdk::lang::dmi::MiAiByval");
  if (mod & VIRTUAL)
    appendMod(sb, "::acdk::lang::dmi::MiMiVirtual");
  if (mod & ONEWAY)
    appendMod(sb, "::acdk::lang::dmi::MiMiOneway");
  if (mod & ClazzIsKnownType)
    appendMod(sb, "::acdk::lang::reflect::MiMcKnownType");
  if (mod & ClazzBasicType)
    appendMod(sb, "::acdk::lang::dmi::MiCiBasicType");
  if (mod & ClazzThrowable)
    appendMod(sb, "::acdk::lang::dmi::MiCiThrowable");
  if (mod & ClazzArray)
    appendMod(sb, "::acdk::lang::dmi::MiCiArray");
  if (mod & ClazzSerializable)
    appendMod(sb, "::acdk::lang::dmi::MiCiSerializable");
  
  if (mod & IsConstructor)
    appendMod(sb, "::acdk::lang::dmi::MiMiConstructor");
  if (mod & IsDestructor)
    appendMod(sb, "::acdk::lang::dmi::MiMiDestructor");
  
  if (sb.length() == 0)
     sb.append("0");
  return sb.toString();
  */
}
#endif //9
} // reflect
} // lang
} // acdk

