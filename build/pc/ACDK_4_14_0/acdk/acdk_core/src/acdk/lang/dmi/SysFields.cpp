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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/SysFields.cpp,v 1.17 2005/04/13 17:51:43 kommer Exp $



#include <acdk.h>

#include "../Exception.h"
#include "SysFields.h"

namespace acdk {
namespace lang {
namespace dmi {


void 
SysField::set(bool val)
{
  if (type != FT_Bool)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.zval == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.zval = val;
}
 
void 
SysField::set(char val)
{
  if (type != FT_Char && type != FT_Byte && type != FT_UcChar)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.cval == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.cval = val;
}

void 
SysField::set(ucchar val)
{
  if (type != FT_UcChar)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.ucval == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.ucval = val;
}

/*
void 
SysField::set(uc4char val)
{
  if (type != FT_Uc4Char)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.uc4val == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.uc4val = val;
}
*/
void 
SysField::set(byte val)
{
  if (type != FT_Char && type != FT_Byte)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.bval == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.bval = val;
}


void 
SysField::set(short val)
{
  if (type != FT_Short)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.ival == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.sval = val;
}

void 
SysField::set(int val)
{
  if (type != FT_Int)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.ival == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.ival = val;
}

 
void 
SysField::set(jlong val)
{
  if (type != FT_JLong)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.jlval == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.jlval = val;
}

void 
SysField::set(float val)
{
  if (type != FT_Float)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.fval == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.fval = val;
}

void 
SysField::set(double val)
{
  if (type != FT_Double)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.dval == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.dval = val;
}

void 
SysField::set(IN(RObject) val)
{
  if (type != FT_Object)
    THROW1(Exception, "Invalide type in ::acdk::lang::dmi::SysField::set()");
  if (cont.oval == 0)
    THROW1(Exception, "no instance of Object available in ::acdk::lang::dmi::SysField::set()");
  *cont.oval = val;
  if (fieldInfo != 0 && fieldInfo->type != 0 && fieldInfo->type->_castToInterfacePtr != 0 && val != Nil)
    cont.oval->_setInterfacePtr(fieldInfo->type->_castToInterfacePtr(val.impl()));
}

ScriptVar 
SysField::getScriptVar(int flags)
{
  switch(type) {
  case dmi::SysField::FT_Void:
    return ScriptVar();
  case dmi::SysField::FT_Bool :
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.zval);
      else
        return outOf(*cont.zval);
    }
    else
      return ScriptVar(*cont.zval);
  case dmi::SysField::FT_Char:
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.cval);
      else
        return outOf(*cont.cval);
    }
    else
      return ScriptVar(*cont.cval);
  case dmi::SysField::FT_UcChar:
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.ucval);
      else
        return outOf(*cont.ucval);
    }
    else
      return ScriptVar(*cont.ucval);
    /*
  case dmi::SysField::FT_Uc4Char:
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.uc4val);
      else
        return outOf(*cont.uc4val);
    }
    else
      return ScriptVar(*cont.uc4val);
      */
  case dmi::SysField::FT_Byte :
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.bval);
      else
        return outOf(*cont.bval);
    }
    else
      return ScriptVar(*cont.bval);
  case dmi::SysField::FT_Short :
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.sval);
      else
        return outOf(*cont.sval);
    }
    else
      return ScriptVar(*cont.sval);
  case dmi::SysField::FT_Int :
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.ival);
      else
        return outOf(*cont.ival);
    }
    else
      return ScriptVar(*cont.ival);
  case dmi::SysField::FT_JLong :
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.jlval);
      else
        return outOf(*cont.jlval);
    }
    else
      return ScriptVar(*cont.jlval);
  case dmi::SysField::FT_Float :
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.fval);
      else
        return outOf(*cont.fval);
    }
    else
      return ScriptVar(*cont.fval);
  case dmi::SysField::FT_Double :
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.dval);
      else
        return outOf(*cont.dval);
    }
    else
      return ScriptVar(*cont.dval);
  case dmi::SysField::FT_Object :
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*cont.oval);
      else
        return outOf(*cont.oval);
    }
    else
      return ScriptVar(*cont.oval);
  default:
    THROW1(Exception, "Unexpected");
    return ScriptVar();
  }
}

void 
SysField::setScriptVar(const ScriptVar& sv)
{
  switch(type) {
  case dmi::SysField::FT_Void:
    return;
  case dmi::SysField::FT_Bool :
    *cont.zval = sv.getBoolVar();
    break;
  case dmi::SysField::FT_Char:
    *cont.cval = sv.getCharVar();
    break;
   case dmi::SysField::FT_UcChar:
    *cont.ucval = sv.getUcCharVar();
    break;
  case dmi::SysField::FT_Byte :
    *cont.bval = sv.getByteVar();
    break;
  case dmi::SysField::FT_Short :
   *cont.sval = sv.getShortVar(); 
   break;
  case dmi::SysField::FT_Int :
    *cont.ival = sv.getIntVar(); 
    break;
  /*case dmi::SysField::FT_Long :
    *cont.lval = sv.getLongVar(); 
    break;*/
  case dmi::SysField::FT_JLong :
    *cont.jlval = sv.getLongVar(); 
    break;
  case dmi::SysField::FT_Float :
    *cont.fval = sv.getFloatVar(); 
    break;
  case dmi::SysField::FT_Double :
    *cont.dval = sv.getDoubleVar(); 
    break;
  case dmi::SysField::FT_Object :
    *cont.oval = sv.getObjectVar(); 
    break;
  default:
    THROW1(Exception, "Unexpected");
    return;
  }
}

//static
::acdk::lang::dmi::SysField
SysField::getField(const acdk::lang::dmi::ClazzFieldInfo* fieldinfo, ScriptVar& sv) 
{
  const ClazzInfo* ci = fieldinfo->type;
  ClazzFieldInfo* fi = const_cast<ClazzFieldInfo*>(fieldinfo);
  if (ci == ClazzInfo::getBoolClazz())
  {
    return SysField(fi, &sv.getBoolRef());
  }
  if (ci == ClazzInfo::getCharClazz())
  {
    return SysField(fi, &sv.getCharRef());
  }
  if (ci == ClazzInfo::getUcCharClazz())
  {
    return SysField(fi, &sv.getUcCharRef());
  }
  if (ci == ClazzInfo::getByteClazz())
  {
    return SysField(fi, &sv.getByteRef());
  }
  if (ci == ClazzInfo::getShortClazz())
  {
    return SysField(fi, &sv.getShortRef());
  }
  if (ci == ClazzInfo::getIntClazz())
  {
    return SysField(fi, &sv.getIntRef());
  }
  if (ci == ClazzInfo::getLongClazz())
  {
    return SysField(fi, &sv.getLongRef());
  }
  if (ci == ClazzInfo::getFloatClazz())
  {
    return SysField(fi, &sv.getFloatRef());
  }
  if (ci == ClazzInfo::getDoubleClazz())
  {
    return SysField(fi, &sv.getDoubleRef());
  }
  // otherwise
  {
    return SysField(fi, sv.getObjectRef()._ref_this());
  }
}

} // dmi
} // lang
} // acdk

