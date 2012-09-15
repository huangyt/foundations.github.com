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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/AcdkDmiClient.cpp,v 1.19 2005/04/18 14:53:15 kommer Exp $

#include <acdk.h>
#include "AcdkDmiClient.h"

namespace acdk {
namespace lang {
namespace dmi {





int 
AcdkDmiClient::getTypeDistance(const ClazzInfo* fromType, const ClazzInfo* toType)
{
  if (fromType == toType)
      return 0;

  return toType->assignDistance(fromType);
}

void splitClassAndNs(IN(RString) compName, OUT(RString) className, OUT(RString) namespaceName) // ### into ClazzInfo?
{
  RString cp = compName->replace("::", "/")->replace('.', '/');
  int lidx = cp->lastIndexOf('/');
  if (lidx == -1)
  {
    className = cp;
    return;
  }
  className = cp->substr(lidx + 1);
  namespaceName = cp->substr(0, lidx);
}


//static
int
AcdkDmiClient::getTypeDistance(const ::acdk::lang::dmi::ScriptVar& arg, const ClazzInfo* tclazz) 
{
  const ClazzInfo* aci = arg.getClazzInfo();
  int ret = getTypeDistance(aci, tclazz);
  if (ret != -1)
    return ret;
  
  if (tclazz == ClazzInfo::getIntClazz() && aci == String::clazzInfo())
  {
    RString sval = arg.getStringVar();
    const ClazzEnumValueInfo* evi = reinterpret_cast<const ClazzEnumValueInfo*>(MetaInfo::findMetaInfo(sval, MiEnumValInfo, false));
    
    if (evi == 0)
      return -1;
    return 1;
  }
  if (arg.isObjectType() == true)
  {
    RObject o = arg.getObjectVar();
    if (o == Nil)
      return 1;
    RObject ret = o->_cast(tclazz);
    if (ret != 0)
      return 1;
  }
  return -1;
}




//virtual 
void
AcdkDmiClient::castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType)
{
 
}


//static 
AcdkDmiClient AcdkDmiClient::_client;




} // dmi
} // lang
} // acdk



