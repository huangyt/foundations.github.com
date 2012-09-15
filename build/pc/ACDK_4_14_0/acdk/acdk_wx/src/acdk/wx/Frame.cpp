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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Frame.cpp,v 1.4 2005/02/05 10:45:35 kommer Exp $


#include "Frame.h"
#include "MenuBar.h"

namespace acdk {
namespace wx {

Frame::Frame(IN(RString) title) 
: TopLevelWindow(new WxFrameFwd(this, 0, -1, S2WXS(title))) 
{
  ACDK_SAFE_CONSTRUCTOR();
  ((WxFrameFwd*)getWx())->setOwningForward(this);
}

Frame::Frame(IN(RWindow) parent, int id, IN(RString) title, IN(RPoint) pos,
	  IN(RSize) size, int style, IN(RString) name)
: TopLevelWindow(new WxFrameFwd(this, CLS2WXPTR(parent), id, S2WXS(title), CLS2WXREF(pos), 
				CLS2WXREF(size), style, S2WXS(name))) 
{
  ACDK_SAFE_CONSTRUCTOR();
  ((WxFrameFwd*)getWx())->setOwningForward(this);
}


Frame::~Frame()
{
  WXDOUT((void*)this << " Frame::~Frame()");
}

WxFrameFwd::WxFrameFwd(Frame* frame, wxWindow* parent, int id, const wxString& str,
		       const wxPoint& pos, const wxSize& size,
		       long style, const wxString& name)
: wxFrame(parent, id, str, pos, size, style, name)
{
  
}

WxFrameFwd::~WxFrameFwd() 
{ 
  WXDOUT((void*)this << " WxFrameFwd::~WxFrameFwd");
    //if (_forward != 0) _forward->releaseRef(); 
}

} // wx
} // acdk
