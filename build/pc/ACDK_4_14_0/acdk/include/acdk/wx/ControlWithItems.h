// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ControlWithItems.h,v 1.6 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_ControlWithItems_h
#define acdk_wx_ControlWithItems_h

#include "Event.h"
#include "Control.h"

namespace acdk {
namespace wx {

 
ACDK_DECL_CLASS(ControlWithItems);

/**
  see wxControlWithItems
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC ControlWithItems
: extends Control
{
  ACDK_WITH_METAINFO(ControlWithItems)
public:
  ACDK_WX_STD_MEMBERS(ControlWithItems, Control)
  // wxControlWithItems
};



} // wx
} // acdk

#endif //acdk_wx_ControlWithItems_h
