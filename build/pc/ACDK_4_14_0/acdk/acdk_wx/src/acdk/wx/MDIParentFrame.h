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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/MDIParentFrame.h,v 1.7 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_MDIParentFrame_h
#define acdk_wx_MDIParentFrame_h

#include "MDIChildFrame.h"
#include "MDIClientWindow.h"
#include "ToolBar.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(ToolBar);

ACDK_DECL_CLASS(MDIParentFrame);

/**
  see wxMDIParentFrame
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC MDIParentFrame
: extends Frame
{
  ACDK_WITH_METAINFO(MDIParentFrame)
public:
  // wxMDIParentFrame
  ACDK_WX_STD_MEMBERS(MDIParentFrame, Frame)

  MDIParentFrame(IN(RWindow) parent, int id, IN(RString) title, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), int style =  DefaultFrameStyle | Vscroll | Hscroll,
            IN(RString) name = "frame")
  : Frame(new wxMDIParentFrame(CLS2WXPTR(parent), id, S2WXS(title), CLS2WXREF(pos), CLS2WXREF(size), style, S2WXS(name)), false)
  {
  }
   
    // Get the client window
    //wxMDIClientWindow *GetClientWindow() const { return m_clientWindow; }
  inline RMDIClientWindow getClientWindow() const { RETURN_WXPTR2CLS(MDIClientWindow, getWx()->GetClientWindow()); }

    
  //virtual void GetClientSize(int width, int height) const;
  inline void getClientSize(OUT(int) width, OUT(int) height) const { getWx()->GetClientSize(&width, &height); }
  //wxMDIChildFrame* GetActiveChild() const;
  inline RMDIChildFrame getActiveChild() const { RETURN_WXPTR2CLS(MDIChildFrame, getWx()->GetActiveChild()); }
  //virtual wxWindow* GetToolBar() const;
  inline RWindow getToolBar() const { RETURN_WXPTR2CLS(Window, getWx()->GetToolBar()); }
  //wxMenu* GetWindowMenu() const;

  /* gtk not supported
  inline RMenu getWindowMenu() const 
  { 

    RETURN_WXPTR2CLS(Menu, getWx()->GetWindowMenu()); 
  }
*/
  //virtual void SetToolBar(wxWindow* toolbar);
  inline void setToolBar(IN(RToolBar) toolbar) { getWx()->SetToolBar(CLS2WXPTR(toolbar)); }
  //void SetWindowMenu(wxMenu* menu);
  inline void setWindowMenu(IN(RMenu) menu) 
    { 
#if defined(ACDK_OS_WIN32)
      getWx()->SetWindowMenu(CLS2WXPTR(menu)); 
#endif
    }
  
  //virtual void Cascade();
  inline virtual void cascade() { getWx()->Cascade(); }
    //virtual void Tile();
  inline virtual void tile() { getWx()->Tile(); }
    //virtual void ArrangeIcons();
  inline virtual void arrangeIcons() { getWx()->ArrangeIcons(); }
    //virtual void ActivateNext();
  inline virtual void activateNext() { getWx()->ActivateNext(); }
    //virtual void ActivatePrevious();
  inline virtual void activatePrevious() { getWx()->ActivatePrevious(); }
};

inline
MDIClientWindow::MDIClientWindow(IN(RMDIParentFrame) parent, int style)
  : Window(new wxMDIClientWindow(CLS2WXPTR(parent), style), parent == Nil)
  {
  }

inline
MDIChildFrame::MDIChildFrame(IN(RMDIParentFrame) parent, int id, IN(RString) title, IN(RPoint) pos, IN(RSize) size, int style, IN(RString) name)
  : Frame(new wxMDIChildFrame(CLS2WXPTR(parent), id, S2WXS(title), CLS2WXREF(pos), CLS2WXREF(size), style, S2WXS(name)), parent == Nil)
  {
  }

} // wx
} // acdk

#endif //acdk_wx_MDIParentFrame_h
