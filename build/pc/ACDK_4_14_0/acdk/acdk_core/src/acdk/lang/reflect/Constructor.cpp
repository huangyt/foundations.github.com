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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Constructor.cpp,v 1.24 2005/02/05 10:44:59 kommer Exp $


#include <acdk.h>
#include "Constructor.h"
#include "../ObjectArrayImpl.h"
#include "../dmi/ScriptVar.h"
#include "../dmi/AcdkStdWeakTypeDmiClient.h"
#include <acdk/lang/reflect/Modifier.h>

namespace acdk {
namespace lang {
namespace reflect {

//virtual
RClass
Constructor::getDeclaringClass() 
{ 
  return Class::getSingeltonClass(_theClazz); 
}

//virtual
int
Constructor::getModifiers() 
{ 
  return _methodInfo->flags;
}

//virtual
RString
Constructor::getName() 
{ 
  return _methodInfo->name; 
}


int 
Constructor::getParameterCount()
{
  return _methodInfo->getArgumentCount();
}

RParameterArray 
Constructor::getParameters()
{
  RParameterArray pa = new ParameterArray(getParameterCount());
  for (int i = 0; i < _methodInfo->getArgumentCount(); i++) 
  {
    pa[i] = new Parameter(_theClazz, _methodInfo, _methodInfo->methodArgs[i]); 
  }
  return pa;
}
  
RParameter 
Constructor::getParameter(int idx)
{
  if (idx >= getParameterCount())
    THROW1(IndexOutOfBoundsException, "Method::getParameter");
  return new Parameter(_theClazz, _methodInfo, _methodInfo->methodArgs[idx]);
}
  
RParameter 
Constructor::getParameter(IN(RString) paramname)
{
  int i;
  for (i = 0; i < _methodInfo->getArgumentCount(); i++) 
  {
    if (paramname->equals(_methodInfo->methodArgs[i]->name) == true)
      return new Parameter(_theClazz, _methodInfo, _methodInfo->methodArgs[i]); 
  }
  return Nil;
}


//virtual 
bool 
Constructor::equals(IN(RObject) obj)
{
  if (instanceof(obj, Constructor) == false)
    return false;
  return RConstructor(obj)->_methodInfo == _methodInfo; 
}
 
//virtual 
RClassArray 
Constructor::getExceptionTypes()
{
  int count = 0;
  for (; _methodInfo->exceptions && _methodInfo->exceptions[count] != 0; ++count)
    ;

  RClassArray exs = new ClassArray(count);
  for (int i = 0; i < count; ++i)
  {
    exs[i] = Class::getSingeltonClass(_methodInfo->exceptions[i]);
  }
  return exs;
}
 
//virtual 
RClassArray 
Constructor::getParameterTypes()
{
  int count = _methodInfo->getArgumentCount();
  RClassArray paramsclasses = new ClassArray(count);
  for (int i = 0; i < count; i++) {
    paramsclasses[i] = Class::getSingeltonClass(_methodInfo->methodArgs[i]->type);
  }
  return paramsclasses;
}

//virtual 
int 
Constructor::hashCode()
{
  return (int)_methodInfo;
}

//virtual 
RObject 
Constructor::newInstance(IN(RObjectArray) initargs)
{
  dmi::ScriptVarArray sa(initargs->length());
  for (int i = 0; i < sa.size(); i ++)
    sa[i] = initargs[i];
  RClass cls = Class::getSingeltonClass(_theClazz);
  dmi::ScriptVar erg;
  int flag = acdk::lang::dmi::MiPublic | acdk::lang::dmi::MiMiConstructor;
  acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient;
  _theClazz->static_dispatch(cls->getClassName(), erg, sa, 
                              dmiclient, Nil, 
                             flag, _theClazz, 0);
  return erg.getObjectVar();
}

//virtual 
RString 
Constructor::toString()
{
  StringBuffer sb(100);
  dmi::MetaInfo::flagsToTypeDecl(sb, getModifiers(), dmi::TpFtAcdkType);
  sb.append(" ");
  RClass cls = Class::getSingeltonClass(_theClazz);
  sb.append(cls->toString());
  sb.append(".");
  sb.append(getName());
  sb.append("(");
  RClassArray ca = getParameterTypes();
  for (int i = 0; i < ca->length(); i++) {
    if (i > 0)
      sb.append(", ");
    sb.append(ca[i]->toString());
  }
  sb.append(")");
  return sb.toString();
}

} // reflect
} // lang
} // acdk

