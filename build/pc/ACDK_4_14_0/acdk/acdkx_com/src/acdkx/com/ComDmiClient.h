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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/ComDmiClient.h,v 1.7 2005/04/18 20:40:37 kommer Exp $


#ifndef acdkx_com_ComDmiClient_h
#define acdkx_com_ComDmiClient_h

#include <acdk.h>

#include <acdk/lang/dmi/AcdkDmiClient.h>
#include "com.h"

namespace acdkx {
namespace com {

ACDK_DECL_CLASS(ComDmiClient);


class ACDKX_COM_PUBLIC ComDmiClient 
: public acdk::lang::dmi::AcdkDmiClient 
{
public:
  virtual int typeDistance(const acdk::lang::dmi::ScriptVar& arg, const acdk::lang::dmi::ClazzInfo* toType);
  //virtual int typeDistance(const ClazzInfo* fromType, const ClazzInfo* toType);
  //virtual void castTo(ScriptVar& value, const ::acdk::lang::dmi::ClazzInfo* toType);
  static ComDmiClient _client;
  static acdk::lang::dmi::DmiClient& getDmiClient() { return _client; }
};
} // namespace com 
} // namespace acdkx 



#endif //acdkx_com_ComDmiClient_h

