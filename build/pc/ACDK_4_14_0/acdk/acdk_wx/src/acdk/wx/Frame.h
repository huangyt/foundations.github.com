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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Frame.h,v 1.16 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Frame_h
#define acdk_wx_Frame_h

#include "Window.h"
#include "MenuBar.h"
#include <acdk/lang/ref/SharedOwning.h>
#include "StatusBar.h"
#include "ToolBar.h"

namespace acdk {
namespace wx {

enum Border;
enum Orientation;
enum ToolBarStyle;

ACDK_DECL_CLASS(TopLevelWindow);

/**
  see wxTopLevelWindow
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:45:35 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC TopLevelWindow
: extends Window
{
  ACDK_WITH_METAINFO(TopLevelWindow)
public:
  ACDK_WX_STD_MEMBERS(TopLevelWindow, Window)
};




ACDK_DECL_CLASS(Frame);

/**
  see wxFrame
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Frame
: extends TopLevelWindow
{
  ACDK_WITH_METAINFO(Frame)
public:
  //acdk::lang::ref::SharedOwning _sharedRefs;
  ACDK_WX_STD_MEMBERS(Frame, TopLevelWindow)
  Frame(IN(RString) title = "");
  Frame(IN(RWindow) parent, int id, IN(RString) title, IN(RPoint) pos = Point::defaultPosition(),
	  IN(RSize) size = Size::defaultSize(), int style = DefaultFrameStyle, 
	  IN(RString) name = "frame");
  foreign ~Frame();
  void setMenuBar(IN(RMenuBar) mbar)
  {
    getWx()->SetMenuBar(mbar->getWx());
  }
  

  //virtual wxToolBar* CreateToolBar(long style = wxNO_BORDER | wxTB_HORIZONTAL | wxTB_FLAT, wxWindowID id = -1, const wxString& name = wxToolBarNameStr);
  inline virtual RToolBar createToolBar(int style = BorderNone | TbHorizontal, int id = -1) 
  { 
    RETURN_WXPTR2CLS(ToolBar, getWx()->CreateToolBar(style, id)); 
  }

/* is protected
  //virtual void PositionToolBar();
  inline virtual void positionToolBar() 
  { 
#if wxUSE_TOOLBAR
   getWx()->PositionToolBar(); 
#endif // wxUSE_TOOLBAR

  }
*/

  //virtual wxStatusBar* CreateStatusBar(int number = 1, long style = wxST_SIZEGRIP, wxWindowID id = 0, const wxString& name = wxStatusLineNameStr);
  inline virtual RStatusBar createStatusBar(int number = 1, int style = StSizegrip, int id = 0, IN(RString)  name = "statusBar") 
  { 
    RETURN_WXPTR2CLS(StatusBar, getWx()->CreateStatusBar(number, style, id, S2WXS(name))); 
  }
    // get the main status bar
  //virtual wxStatusBar *GetStatusBar() const { return m_frameStatusBar; }
  inline virtual RStatusBar getStatusBar() const { RETURN_WXPTR2CLS(StatusBar, getWx()->GetStatusBar()); }

    // sets the main status bar
  //void SetStatusBar(wxStatusBar *statBar) { m_frameStatusBar = statBar; }
  inline void setStatusBar(IN(RStatusBar) statBar) { getWx()->SetStatusBar(CLS2WXPTR(statBar)); }

    // forward these to status bar
  //virtual void SetStatusText(const wxString &text, int number = 0);
  inline virtual void setStatusText(IN(RString)  text, int number = 0) { getWx()->SetStatusText(S2WXS(text), number); }
  //virtual void SetStatusWidths(int n, const int widths_field);
  inline virtual void setStatusWidths(int n, IN(RintArray) widths_field) 
  { 
    getWx()->SetStatusWidths(n, widths_field->data()); 
  }
  //void PushStatusText(const wxString &text, int number = 0);
  inline void pushStatusText(IN(RString)  text, int number = 0) { getWx()->PushStatusText(S2WXS(text), number); }
  //void PopStatusText(int number = 0);
  inline void popStatusText(int number = 0) { getWx()->PopStatusText(number); }

    // set the status bar pane the help will be shown in
  //void SetStatusBarPane(int n) { m_statusBarPane = n; }
  inline void setStatusBarPane(int n) { getWx()->SetStatusBarPane(n); }
  //int GetStatusBarPane() const { return m_statusBarPane; }
  inline int getStatusBarPane() const { return getWx()->GetStatusBarPane(); }

  inline virtual RStatusBar onCreateStatusBar(int number = 1, int id = 0, IN(RString)  name = "") 
  { 
#if wxUSE_STATUSBAR
    // wxStatusBar
    RETURN_WXPTR2CLS(StatusBar, getWx()->OnCreateStatusBar(number, wxST_SIZEGRIP, id, S2WXS(name))); 
#else
    return Nil;
#endif
  }
  //virtual void PositionStatusBar();
  inline virtual void positionStatusBar() 
  { 
#if wxUSE_STATUSBAR
    getWx()->PositionStatusBar(); 
#endif
  }

};


class ACDK_WX_PUBLIC WxFrameFwd
: public wxFrame
, public AcdkForwarder<Frame>
{
public:
  WxFrameFwd(Frame* frame, wxWindow* parent, int id, const wxString& str, 
	     const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
	     long style = wxDEFAULT_FRAME_STYLE, const wxString& name = _T("frame"));
  ~WxFrameFwd();
  ACDK_WXWINDOW_FORWARD_FUNCS

};



} // wx
} // acdk

#endif //acdk_wx_Frame_h
