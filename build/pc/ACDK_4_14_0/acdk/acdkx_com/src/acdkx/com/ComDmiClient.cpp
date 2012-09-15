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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/ComDmiClient.cpp,v 1.7 2005/04/18 20:40:37 kommer Exp $



#include "ComDmiClient.h"

namespace acdkx {
namespace com {

using namespace acdk::lang::dmi;

//static 
ComDmiClient ComDmiClient::_client;

//virtual 
int 
ComDmiClient::typeDistance(const ScriptVar& arg, const ClazzInfo* toType)
{
  if (arg.type == ScriptVar::ObjectRefType)
  {
    if (toType->isBasicClazz() == false)
      return 0;
  }
  return AcdkDmiClient::typeDistance(arg, toType);
}
/*
//virtual 
int 
ComDmiClient::typeDistance(const ClazzInfo* fromType, const ClazzInfo* toType)
{
}

//virtual 
void 
ComDmiClient::castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType)
{
}*/

} // namespace com 
} // namespace acdkx 




