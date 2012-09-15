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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/CreateTypeInfo.h,v 1.7 2005/02/05 10:45:38 kommer Exp $


#ifndef acdkx_com_CreateTypeInfo_h
#define acdkx_com_CreateTypeInfo_h

#include "IUnknown.h"

ACDK_COMIFACE(ICreateTypeInfo2);
ACDK_COMIFACE(ICreateTypeInfo);

namespace acdkx {
namespace com {

ACDK_DECL_CLASS(CreateTypeInfo);

class ACDKX_COM_PUBLIC CreateTypeInfo 
: extends ::acdk::lang::Object
{
  RICreateTypeInfo _iface;
public:
  CreateTypeInfo(IN(RICreateTypeInfo) iface)
    : _iface(iface)
  {
  }
  ~CreateTypeInfo()
  {
  }
  void createClass(IN(RClass) cls);
};

} // namespace com 
} // namespace acdkx 



#endif //acdkx_com_CreateTypeInfo_h

