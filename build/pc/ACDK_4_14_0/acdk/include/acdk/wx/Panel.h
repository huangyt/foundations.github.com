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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Panel.h,v 1.6 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Panel_h
#define acdk_wx_Panel_h

#include "Window.h"
#include "WindowStyle.h"

#include <wx/panel.h>

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(Panel);

/**
  see wxPanel
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Panel
: extends Window
{
  ACDK_WITH_METAINFO(Panel)
public:
  // wxPanel
  ACDK_WX_STD_MEMBERS(Panel, Window)
  
  
  //void wxPanel(wxWindow *parent, int x, int y, int width, int height, long style = wxTAB_TRAVERSAL | wxNO_BORDER, const wxString& name = wxPanelNameStr);
  Panel(IN(RWindow) parent, int x, int y, int width, int height, int style = TabTraversal | BorderNone) 
  : Window(new wxPanel(CLS2WXPTR(parent), x, y, width, height, style), parent == Nil)
  {
  }
      // Constructor
  //void wxPanel(wxWindow *parent, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxTAB_TRAVERSAL | wxNO_BORDER, const wxString& name = wxPanelNameStr);
  Panel(IN(RWindow) parent, int id = -1, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), 
        int style = TabTraversal | BorderNone) 
  : Window(new wxPanel(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style), parent == Nil)
  {
  }
  
};


} // wx
} // acdk

#endif //acdk_wx_Panel_h
