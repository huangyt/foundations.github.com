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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/Parameter.cpp,v 1.7 2005/02/05 10:44:59 kommer Exp $


#include <acdk.h>
#include "Method.h"

#include "../ObjectArrayImpl.h"
#include "../dmi/AcdkStdWeakTypeDmiClient.h"

namespace acdk {
namespace lang {
namespace reflect {

using namespace acdk::lang;


//virtual
RMethod
Parameter::getMethod() 
{ 
  return new Method(_theClazz, _methodInfo); 
}

//virtual 
int
Parameter::getModifiers() 
{ 
  return _paramInfo->flags;
}

//virtual 
RString 
Parameter::getName() 
{ 
  return _paramInfo->name; 
}

//virtual 
RClass
Parameter::getType()
{
  return Class::getSingeltonClass(_paramInfo->type);
}


//virtual 
bool 
Parameter::equals(IN(RObject) obj)
{
  if (instanceof(obj, Parameter) == false)
    return false;
  RParameter other = (RParameter)obj;
  if (getName()->equals(other->getName()) == false)
    return false;
  return true;
}

//virtual 
int 
Parameter::hashCode()
{
  int result = 0;
  result = 31 * result + getName()->hashCode();
  result = 31 * result + getType()->hashCode();
  return result;
}

//virtual 
RString 
Parameter::toString()
{
  return getName();
}

//virtual 
RString 
Parameter::toIndentifier()
{
  return getName();
 
}

} // reflect
} // lang
} // acdk



