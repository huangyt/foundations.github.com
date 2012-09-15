
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


#include "MethodAltNameAttribute.h"
#include "MetaCompiler.h"
#include <acdk/locale/Encoding.h>
#include "FieldInfo.h"

namespace acdk {
namespace tools {
namespace mc {


MethodAltNameAttribute::MethodAltNameAttribute(IN(RString) altname, int paramCount)
: _altName(altname)
, _paramCount(paramCount)
{
}



//static 
void 
MethodAltNameAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("MethodAltName", "acdk.tools.mc.MethodAltNameAttribute");
}

//foreign virtual 
bool 
MethodAltNameAttribute::apply(IN(RCodeInfo) cm)
{
  if (instanceof(cm, MethodInfo) == false)
    return false;
  RMethodInfo mi(cm);
  if (_paramCount == -1 || _paramCount == mi->argcount)
    mi->_altName = _altName;
  return true;
}


} // namespace mc
} // namespace tools
} // namespace acdk


