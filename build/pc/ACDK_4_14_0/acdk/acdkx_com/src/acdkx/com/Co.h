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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/Co.h,v 1.6 2005/02/05 10:45:38 kommer Exp $


#ifndef acdkx_com_Co_h
#define acdkx_com_Co_h

#include "IUnknown.h"
#include "CoException.h"

namespace acdkx {
namespace com {

ACDK_DECL_CLASS(Co);

class ACDKX_COM_PUBLIC Co 
: public ::acdk::lang::Object
{
public:
  // wrapper to CoCreateInstance
  RIUnknown createInstance(REFCLSID clsid);
};

} // namespace com 
} // namespace acdkx 



#endif //acdkx_com_Co_h

