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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/SplitterWindow.h,v 1.6 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_SplitterWindow_h
#define acdk_wx_SplitterWindow_h

#include "Window.h"

namespace acdk {
namespace wx {

/**
  see SplitterWindow
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum SplitMode
{
    SplitHorizontal  /* wxSPLIT_HORIZONTAL*/ = 1,
    SplitVertical  /* wxSPLIT_VERTICAL*/
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, SplitMode);

ACDK_DECL_CLASS(SplitterWindow);

/**
  see wxSplitterWindow
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC SplitterWindow
: extends Window
{
  ACDK_WITH_METAINFO(SplitterWindow)
public:
  ACDK_WX_STD_MEMBERS(SplitterWindow, Window)
  
  SplitterWindow() : Window(new wxSplitterWindow()) {}
  
  SplitterWindow(IN(RWindow) parent, int id = -1, IN(RPoint) pos = Point::defaultPosition(), 
                  IN(RSize) size = Size::defaultSize(), int style = wxSP_3D, IN(RString) name = "splitter")
  : Window(new wxSplitterWindow(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style, S2WXS(name)))
  {
  }

    
    
    // Gets the only or left/top pane
    //wxWindow *GetWindow1() const { return m_windowOne; }
  inline RWindow getWindow1() const { RETURN_WXPTR2CLS(Window, getWx()->GetWindow1()); }

    // Gets the right/bottom pane
  //wxWindow *GetWindow2() const { return m_windowTwo; }
  inline RWindow getWindow2() const { RETURN_WXPTR2CLS(Window, getWx()->GetWindow2()); }

    // Sets the split mode
    //void SetSplitMode(int mode);
  inline void setSplitMode(int mode) { getWx()->SetSplitMode(mode); }

    // Gets the split mode
    //wxSplitMode GetSplitMode() const { return m_splitMode; };
  inline SplitMode getSplitMode() const { return (SplitMode)(int)getWx()->GetSplitMode(); }

    // Initialize with one window
  //void Initialize(wxWindow *window);
  inline void initialize(IN(RWindow) window) { getWx()->Initialize(CLS2WXPTR(window)); }

    // Associates the given window with window 2, drawing the appropriate sash
    // and changing the split mode.
    // Does nothing and returns FALSE if the window is already split.
    // A sashPosition of 0 means choose a default sash position,
    // negative sashPosition specifies the size of right/lower pane as it's
    // absolute value rather than the size of left/upper pane.
    //virtual bool SplitVertically(wxWindow *window1, wxWindow *window2, int sashPosition = 0)
        
  inline virtual bool splitVertically(IN(RWindow) window1, IN(RWindow) window2, int sashPosition = 0) { return getWx()->SplitVertically(CLS2WXPTR_L(window1), CLS2WXPTR_L(window2), sashPosition); }
    //virtual bool SplitHorizontally(wxWindow *window1, wxWindow *window2, int sashPosition = 0);
  inline virtual bool splitHorizontally(IN(RWindow) window1, IN(RWindow) window2, int sashPosition = 0) { return getWx()->SplitHorizontally(CLS2WXPTR_L(window1), CLS2WXPTR_L(window2), sashPosition); }

    // Removes the specified (or second) window from the view
    // Doesn't actually delete the window.
    //bool Unsplit(wxWindow *toRemove = (wxWindow *) NULL);
  inline bool unsplit(IN(RWindow) toRemove = Nil) { return getWx()->Unsplit(CLS2WXPTR(toRemove)); }

    // Replaces one of the windows with another one (neither old nor new
    // parameter should be NULL)
    //bool ReplaceWindow(wxWindow *winOld, wxWindow *winNew);
  inline bool replaceWindow(IN(RWindow) winOld, IN(RWindow) winNew) { return getWx()->ReplaceWindow(CLS2WXPTR(winOld), CLS2WXPTR(winNew)); }

    // Is the window split?
    //bool IsSplit() const { return (m_windowTwo != NULL); }
  inline bool isSplit() const { return getWx()->IsSplit(); }

    // Sets the sash size
    //void SetSashSize(int width) { m_sashSize = width; }
  inline void setSashSize(int width) { getWx()->SetSashSize(width); }

    // Sets the border size
    //void SetBorderSize(int width) { m_borderSize = width; }
  inline void setBorderSize(int width) { getWx()->SetBorderSize(width); }

    // Gets the sash size
    //int GetSashSize() const { return m_sashSize; }
  inline int getSashSize() const { return getWx()->GetSashSize(); }

    // Gets the border size
    //int GetBorderSize() const { return m_borderSize; }
  inline int getBorderSize() const { return getWx()->GetBorderSize(); }

    // Set the sash position
    //void SetSashPosition(int position, bool redraw = TRUE);
  inline void setSashPosition(int position, bool redraw = true) { getWx()->SetSashPosition(position, redraw); }

    // Gets the sash position
    //int GetSashPosition() const { return m_sashPosition; }
  inline int getSashPosition() const { return getWx()->GetSashPosition(); }

    // If this is zero, we can remove panes by dragging the sash.
    //void SetMinimumPaneSize(int min);
  inline void setMinimumPaneSize(int min) { getWx()->SetMinimumPaneSize(min); }
    //int GetMinimumPaneSize() const { return m_minimumPaneSize; }
  inline int getMinimumPaneSize() const { return getWx()->GetMinimumPaneSize(); }

    // NB: the OnXXX() functions below are for backwards compatibility only,
    //     don't use them in new code but handle the events instead!

    // called when the sash position is about to change, may return a new value
    // for the sash or -1 to prevent the change from happening at all
    //virtual int OnSashPositionChanging(int newSashPosition);
  inline virtual int onSashPositionChanging(int newSashPosition) { return getWx()->OnSashPositionChanging(newSashPosition); }

    // Called when the sash position is about to be changed, return
    // FALSE from here to prevent the change from taking place.
    // Repositions sash to minimum position if pane would be too small.
    // newSashPosition here is always positive or zero.
    //virtual bool OnSashPositionChange(int newSashPosition);
  inline virtual bool onSashPositionChange(int newSashPosition) { return getWx()->OnSashPositionChange(newSashPosition); }

    // If the sash is moved to an extreme position, a subwindow
    // is removed from the splitter window, and the app is
    // notified. The app should delete or hide the window.
    //virtual void OnUnsplit(wxWindow *removed);
  inline virtual void onUnsplit(IN(RWindow) removed) { getWx()->OnUnsplit(CLS2WXPTR(removed)); }

    // Called when the sash is double-clicked.
    // The default behaviour is to remove the sash if the
    // minimum pane size is zero.
    //virtual void OnDoubleClickSash(int x, int y);
  inline virtual void onDoubleClickSash(int x, int y) { getWx()->OnDoubleClickSash(x, y); }

};


} // wx
} // acdk

#endif //acdk_wx_SplitterWindow_h
