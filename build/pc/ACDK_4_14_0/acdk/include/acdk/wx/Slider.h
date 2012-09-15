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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Slider.h,v 1.5 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Slider_h
#define acdk_wx_Slider_h

#include "Choice.h"

namespace acdk {
namespace wx {

enum Direction;

/**
  see Slider
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum SliderStyle
{
  SlHorizontal      = wxSL_HORIZONTAL     ,  // wxSL_HORIZONTAL      wxHORIZONTAL // 4
  SlVertical        = wxSL_VERTICAL       ,  // wxSL_VERTICAL        wxVERTICAL   // 8
// The next one is obsolete - use scroll events instead
  SlNotifyDrag     = wxSL_NOTIFY_DRAG    ,  // wxSL_NOTIFY_DRAG     0x0000
  SlTicks           = wxSL_TICKS          ,  // wxSL_TICKS           0x0010
  SlAutoticks       = wxSL_AUTOTICKS      ,  // wxSL_AUTOTICKS       wxSL_TICKS // we don't support manual ticks
  SlLabels          = wxSL_LABELS         ,  // wxSL_LABELS          0x0020
  SlLeft            = wxSL_LEFT           ,  // wxSL_LEFT            0x0040
  SlTop             = wxSL_TOP            ,  // wxSL_TOP             0x0080
  SlRight           = wxSL_RIGHT          ,  // wxSL_RIGHT           0x0100
  SlBottom          = wxSL_BOTTOM         ,  // wxSL_BOTTOM          0x0200
  SlBoth            = wxSL_BOTH           ,  // wxSL_BOTH            0x0400
  SlSelrange        = wxSL_SELRANGE         // wxSL_SELRANGE        0x0800
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, SliderStyle);


ACDK_DECL_CLASS(Slider);

/**
  see wxSlider
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Slider
: extends Control
{
  ACDK_WITH_METAINFO(Slider)
public:
  // wxSlider
  ACDK_WX_STD_MEMBERS(Slider, Control)

  Slider(IN(RWindow) parent, int id, int value, int minValue, int maxValue, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), 
            int style = OrientHorizontal,
            IN(RStringArray) choices = Nil,
            IN(RValidator) validator = Validator::defaultValidator(),
            IN(RString) name = "Slider")
  : Control(new wxSlider(CLS2WXPTR(parent), id, value, minValue, maxValue, CLS2WXREF(pos), CLS2WXREF(size), style, CLS2WXREF(validator), S2WXS(name)), parent == Nil)
  {
  }
  //int GetValue() const;
  inline int getValue() const { return getWx()->GetValue(); }
  //void SetValue(int v);
  inline void setValue(int v) { getWx()->SetValue(v); }
  //void GetSize(int w, int h) const;
  inline void getSize(OUT(int) w, OUT(int) h) const { getWx()->GetSize(&w, &h); }
  //void GetPosition(int x, int y) const;
  inline void getPosition(OUT(int) x, OUT(int) y) const { getWx()->GetPosition(&x, &y); }
  //bool Show(bool show = TRUE);
  inline bool show(bool show = true) { return getWx()->Show(show); }
  //void SetRange(int minValue, int maxValue);
  inline void setRange(int minValue, int maxValue) { getWx()->SetRange(minValue, maxValue); }
  //int GetMin() const { return m_rangeMin; }
  inline int getMin() const { return getWx()->GetMin(); }
  //int GetMax() const { return m_rangeMax; }
  inline int getMax() const { return getWx()->GetMax(); }
 
  // For trackbars only
  //void SetTickFreq(int n, int pos);
  inline void setTickFreq(int n, int pos) { getWx()->SetTickFreq(n, pos); }
  //int GetTickFreq() const { return m_tickFreq; }
  inline int getTickFreq() const { return getWx()->GetTickFreq(); }
  //void SetPageSize(int pageSize);
  inline void setPageSize(int pageSize) { getWx()->SetPageSize(pageSize); }
  //int GetPageSize() const;
  inline int getPageSize() const { return getWx()->GetPageSize(); }
  //void ClearSel();
  inline void clearSel() { getWx()->ClearSel(); }
  //void ClearTicks();
  inline void clearTicks() { getWx()->ClearTicks(); }
  //void SetLineSize(int lineSize);
  inline void setLineSize(int lineSize) { getWx()->SetLineSize(lineSize); }
  //int GetLineSize() const;
  inline int getLineSize() const { return getWx()->GetLineSize(); }
  //int GetSelEnd() const;
  inline int getSelEnd() const { return getWx()->GetSelEnd(); }
  //int GetSelStart() const;
  inline int getSelStart() const { return getWx()->GetSelStart(); }
  //void SetSelection(int minPos, int maxPos);
  inline void setSelection(int minPos, int maxPos) { getWx()->SetSelection(minPos, maxPos); }
  //void SetThumbLength(int len);
  inline void setThumbLength(int len) { getWx()->SetThumbLength(len); }
  //int GetThumbLength() const;
  inline int getThumbLength() const { return getWx()->GetThumbLength(); }
  //void SetTick(int tickPos);
  inline void setTick(int tickPos) { getWx()->SetTick(tickPos); }
};


} // wx
} // acdk

#endif //acdk_wx_Slider_h
