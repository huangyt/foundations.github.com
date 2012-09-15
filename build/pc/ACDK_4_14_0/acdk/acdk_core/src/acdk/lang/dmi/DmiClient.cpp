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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiClient.cpp,v 1.11 2005/02/05 10:44:58 kommer Exp $


#include <acdk.h>
#include <acdk/lang/sys/core_specific.h>


namespace acdk {
namespace lang {
namespace dmi {


DmiClient::DmiClient(int formatFlags)
: _lookupFunc(&StdDispatch::lookupMethod_cb)
, _formatFlags(formatFlags)
{
}

static acdk::lang::sys::core_specific __curInvokeFlags;

//static 
int 
DmiClient::getCurInvokeFlags()
{
  return (int)__curInvokeFlags.get();
}

//static 
void 
DmiClient::setCurInvokeFlags(int flags)
{
  __curInvokeFlags.set((void*)flags);
}
  
} // dmi
} // lang
} // acdk





