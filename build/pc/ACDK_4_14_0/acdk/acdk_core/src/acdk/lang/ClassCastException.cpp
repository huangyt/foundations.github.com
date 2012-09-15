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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ClassCastException.cpp,v 1.7 2005/02/05 10:44:55 kommer Exp $

#include <acdk.h>
#include "ClassCastException.h"

namespace acdk {
namespace lang {

ClassCastException::ClassCastException(IN(RClass) fromObject, IN(RClass) toClass)
: Exception() 
, _fromClazz(fromObject != Nil ? fromObject->getClazzInfo() : 0)
, _toClazz(toClass != Nil ? toClass->objectClazzInfo() : 0)
{
  _setWhat();
}

ClassCastException::ClassCastException(const dmi::ClazzInfo* from, const dmi::ClazzInfo* to)
: Exception() 
, _fromClazz(from)
, _toClazz(to)
{
  _setWhat();
}

void 
ClassCastException::_setWhat()
{
  StringBuffer sb;
  sb.append("Cannot cast type [");
  if (_fromClazz != 0)
    _fromClazz->toTypeString(sb);
  else
    sb.append("<unknown>");
  sb.append("] to [");
  if (_toClazz != 0)
    _toClazz->toTypeString(sb);
  else
    sb.append("<unknown>");
  sb.append("]");  
  _what = sb.toString();
}


} // lang
} // acdk



