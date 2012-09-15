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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ScrolledWindow.h,v 1.6 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_ScrolledWindow_h
#define acdk_wx_ScrolledWindow_h

#include "Panel.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(ScrolledWindow);

/**
  see wxScrolledWindow
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC ScrolledWindow
: extends Panel
{
  ACDK_WITH_METAINFO(ScrolledWindow)
public:
  // wxScrolledWindow
  ACDK_WX_STD_MEMBERS(ScrolledWindow, Panel)
  ScrolledWindow(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), 
           int style = 0, IN(RString) name = "scrolledWindow")
  : Panel(new wxScrolledWindow(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style, S2WXS(name)),  parent == Nil)
  {
  }

  // configure the scrolling
  //virtual void SetScrollbars(int pixelsPerUnitX, int pixelsPerUnitY, int noUnitsX, int noUnitsY, int xPos = 0, int yPos = 0, bool noRefresh = FALSE );
  inline virtual void setScrollbars(int pixelsPerUnitX, int pixelsPerUnitY, int noUnitsX, int noUnitsY, int xPos = 0, int yPos = 0, bool noRefresh = false) { getWx()->SetScrollbars(pixelsPerUnitX, pixelsPerUnitY, noUnitsX, noUnitsY, xPos, yPos, noRefresh); }
  
  // scroll to the given (in logical coords) position
  //virtual void Scroll(int x, int y);
  inline virtual void scroll(int x, int y) { getWx()->Scroll(x, y); }
  
  // get/set the page size for this orientation (wxVERTICAL/wxHORIZONTAL)
  //int GetScrollPageSize(int orient) const;
  inline int getScrollPageSize(int orient) const { return getWx()->GetScrollPageSize(orient); }
  //void SetScrollPageSize(int orient, int pageSize);
  inline void setScrollPageSize(int orient, int pageSize) { getWx()->SetScrollPageSize(orient, pageSize); }
  
  // Set the x, y scrolling increments.
  //void SetScrollRate( int xstep, int ystep );
  inline void setScrollRate(int xstep, int ystep) { getWx()->SetScrollRate(xstep, ystep); }
  
  // get the size of one logical unit in physical ones
  //virtual void GetScrollPixelsPerUnit(int *pixelsPerUnitX, int *pixelsPerUnitY) const;
  inline virtual void getScrollPixelsPerUnit(OUT(int) pixelsPerUnitX, OUT(int) pixelsPerUnitY) const { getWx()->GetScrollPixelsPerUnit(&pixelsPerUnitX, &pixelsPerUnitY); }
  
  // Enable/disable Windows scrolling in either direction. If TRUE, wxWindows
  // scrolls the canvas and only a bit of the canvas is invalidated; no
  // Clear() is necessary. If FALSE, the whole canvas is invalidated and a
  // Clear() is necessary. Disable for when the scroll increment is used to
  // actually scroll a non-constant distance
  //virtual void EnableScrolling(bool x_scrolling, bool y_scrolling);
  inline virtual void enableScrolling(bool x_scrolling, bool y_scrolling) { getWx()->EnableScrolling(x_scrolling, y_scrolling); }
  
  // Get the view start
  //virtual void GetViewStart(int *x, int *y) const;
  inline virtual void getViewStart(OUT(int) x, OUT(int) y) const { getWx()->GetViewStart(&x, &y); }
  
  // Set the scale factor, used in PrepareDC
  //void SetScale(double xs, double ys) { m_scaleX = xs; m_scaleY = ys; }
  inline void setScale(double xs, double ys) { getWx()->SetScale(xs, ys); }
  //double GetScaleX() const { return m_scaleX; }
  inline double getScaleX() const { return getWx()->GetScaleX(); }
  //double GetScaleY() const { return m_scaleY; }
  inline double getScaleY() const { return getWx()->GetScaleY(); }
  
  // translate between scrolled and unscrolled coordinates
  //void CalcScrolledPosition(int x, int y, int *xx, int *yy) const;
  inline void calcScrolledPosition(int x, int y, OUT(int) xx, OUT(int) yy) const { getWx()->CalcScrolledPosition(x, y, &xx, &yy); }
  
  //wxPoint CalcScrolledPosition(const wxPoint& pt) const;
  inline RPoint calcScrolledPosition(IN(RPoint) pt) const { return WXVAL2CLS(Point, getWx()->CalcScrolledPosition(CLS2WXREF(pt))); }
  
  //void CalcUnscrolledPosition(int x, int y, int *xx, int *yy) const;
  inline void calcUnscrolledPosition(int x, int y, OUT(int) xx, OUT(int) yy) const { getWx()->CalcUnscrolledPosition(x, y, &xx, &yy); }
  //wxPoint CalcUnscrolledPosition(const wxPoint& pt) const;
  inline RPoint calcUnscrolledPosition(IN(RPoint) pt) const { return WXVAL2CLS(Point, getWx()->CalcUnscrolledPosition(CLS2WXREF(pt))); }
  
  /* implementation
  // Adjust the scrollbars
  virtual void AdjustScrollbars(void);
  
  // Calculate scroll increment
  virtual int CalcScrollInc(wxScrollWinEvent& event);
  
  // Normally the wxScrolledWindow will scroll itself, but in some rare
  // occasions you might want it to scroll [part of] another window (e.g. a
  // child of it in order to scroll only a portion the area between the
  // scrollbars (spreadsheet: only cell area will move).
  */
  //virtual void SetTargetWindow(wxWindow *target);
  inline virtual void setTargetWindow(IN(RWindow) target) { getWx()->SetTargetWindow(CLS2WXPTR(target)); }
  //virtual wxWindow *GetTargetWindow() const;
    inline virtual RWindow getTargetWindow() /*const*/ { RETURN_WXPTR2CLS(Window, getWx()->GetTargetWindow()); }
  
  //void SetTargetRect(const wxRect& rect) { m_rectToScroll = rect; }
  inline void setTargetRect(IN(RRect) rect) 
  { 
#if defined(__WXMSW__)
      getWx()->SetTargetRect(CLS2WXREF(rect)); 
#endif
  }
  //wxRect GetTargetRect() const { return m_rectToScroll; }
  inline RRect getTargetRect() const 
  { 
#if defined(__WXMSW__)
      return WXVAL2CLS(Rect, getWx()->GetTargetRect()); 
#else
      return new Rect(0, 0, 0, 0);
#endif
  }
};


} // wx
} // acdk

#endif //acdk_wx_ScrolledWindow_h
