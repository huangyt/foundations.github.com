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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Caret.h,v 1.8 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Caret_h
#define acdk_wx_Caret_h

#include "Window.h"


namespace acdk {
namespace wx {


ACDK_DECL_CLASS(Caret);

/**
  see wxCaret
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC Caret
: extends WxNonCopyStruct<wxCaret>
{
  typedef WxNonCopyStruct<wxCaret> Super;
public:
  ACDK_WX_STD_MEMBERS(Caret, Super)
  Caret() : Super(new wxCaret(), true) {}
  Caret(IN(RWindow) window, int width, int height) 
    : Super(new wxCaret(CLS2WXPTR(window), width, height))
  {
  }
  Caret(IN(RWindow) window, IN(RSize) size)
    : Super(new wxCaret(CLS2WXPTR(window), CLS2WXREF(size)))
  {
  }
  //bool Create(wxWindowBase *window, int width, int height);
  inline bool create(IN(RWindow) window, int width, int height) { return getWx()->Create(CLS2WXPTR(window), width, height); }
        // same as ctor
    //bool Create(wxWindow *window, const wxSize& size)
  inline bool create(IN(RWindow) window, IN(RSize) size) { return getWx()->Create(CLS2WXPTR(window), CLS2WXREF(size)); }

    // accessors
    // ---------

        // is the caret valid?
    //bool IsOk() const { return m_width != 0 && m_height != 0; }
  inline bool isOk() const { return getWx()->IsOk(); }

        // is the caret currently shown?
    //bool IsVisible() const { return m_countVisible > 0; }
  inline bool isVisible() const { return getWx()->IsVisible(); }

        // get the caret position
    //void GetPosition(int *x, int *y) const
  inline void getPosition(OUT(int) x, OUT(int) y) { getWx()->GetPosition(&x, &y); }
  //wxPoint GetPosition() const { return wxPoint(m_x, m_y); }
  inline RPoint getPosition() const { return WXREF2CLS(getWx()->GetPosition()); }
  //void GetSize(int *width, int *height) const;
  inline void getSize(OUT(int) width, OUT(int) height) { getWx()->GetSize(&width, &height); }
  //wxSize GetSize() const { return wxSize(m_width, m_height); }
  inline RSize getSize() const { return WXREF2CLS(getWx()->GetSize()); }

        // get the window we're associated with
    //wxWindow *GetWindow() const { return (wxWindow *)m_window; }
  inline RWindow getWindow() const { RETURN_WXPTR2CLS(Window, getWx()->GetWindow()); }

        // change the size of the caret
    //void SetSize(int width, int height) {
  inline void setSize(int width, int height) { getWx()->SetSize(width, height); }
    //void SetSize(const wxSize& size) { SetSize(size.x, size.y); }
  inline void setSize(IN(RSize) size) { getWx()->SetSize(CLS2WXREF(size)); }

        // move the caret to given position (in logical coords)
    //void Move(int x, int y) { m_x = x; m_y = y; DoMove(); }
  inline void move(int x, int y) { getWx()->Move(x, y); }
    //void Move(const wxPoint& pt) { m_x = pt.x; m_y = pt.y; DoMove(); }
  inline void move(IN(RPoint) pt) { getWx()->Move(CLS2WXREF(pt)); }

        // show/hide the caret (should be called by wxWindow when needed):
        // Show() must be called as many times as Hide() + 1 to make the caret
        // visible
    //virtual void Show(bool show)
  inline virtual void show(bool show = true) { getWx()->Show(show); }
  //virtual void Hide() { Show(FALSE); }
  inline virtual void hide() { getWx()->Hide(); }

        // blink time is measured in milliseconds and is the time elapsed
        // between 2 inversions of the caret (blink time of the caret is common
        // to all carets in the Universe, so these functions are static)
    //static int GetBlinkTime();
  inline static void setBlinkTime(int milliseconds) { wxCaret::SetBlinkTime(milliseconds); }
};

inline void Window::setCaret(IN(RCaret) caret) { getWx()->SetCaret(CLS2WXPTR(caret)); }
inline RCaret Window::getCaret() const { RETURN_WXPTR2CLS(Caret, getWx()->GetCaret()); }

} // wx
} // acdk

#endif //acdk_wx_Caret_h
