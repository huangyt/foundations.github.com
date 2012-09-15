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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ToolTip.h,v 1.4 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_ToolTip_h
#define acdk_wx_ToolTip_h

#include "Window.h"
#include "Validator.h"

namespace acdk {
namespace wx {



ACDK_DECL_CLASS(ToolTip);
/**
  see wxToolTip
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC ToolTip
: extends WxObject
{
  ACDK_WITH_METAINFO(ToolTip)
public:
  // wxToolTip
  ACDK_WX_STD_MEMBERS(ToolTip, WxObject)
  ToolTip(IN(RString) text) : WxObject(new wxToolTip(S2WXS(text)), false) {}

  //void SetTip(const wxString& tip);
  inline void setTip(IN(RString)  tip) { getWx()->SetTip(S2WXS(tip)); }
  //wxString GetTip() const;
  inline RString getTip() const { return WXS2S(getWx()->GetTip()); }
  //wxWindow* GetWindow() const;
  inline RWindow getWindow() const { RETURN_WXPTR2CLS(Window, getWx()->GetWindow()); }
  //static void Enable(bool flag);
  inline static void enable(bool flag) { wxToolTip::Enable(flag); }
  //static void SetDelay(long msecs);
  inline static void setDelay(int msecs) { wxToolTip::SetDelay(msecs); }

};


} // wx
} // acdk

#endif //acdk_wx_ToolTip_h
