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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ScreenDC.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_ScreenDC_h
#define acdk_wx_ScreenDC_h

#include "DC.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(ScreenDC);
/**
  see wxScreenDC
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC ScreenDC
: extends DC
{
  // wxScreenDC
  ACDK_WITH_METAINFO(ScreenDC)
public:
  // wxScreenDC
  ACDK_WX_STD_MEMBERS(ScreenDC, DC)
  ScreenDC() : DC(new wxScreenDC(), true) {}
  //bool StartDrawingOnTop(wxWindow* window);
  inline bool startDrawingOnTop(IN(RWindow) window) { return getWx()->StartDrawingOnTop(CLS2WXPTR(window)); }
  //bool EndDrawingOnTop();
  inline bool endDrawingOnTop() { return getWx()->EndDrawingOnTop(); }
  
};


} // wx
} // acdk

#endif //acdk_wx_ScreenDC_h
