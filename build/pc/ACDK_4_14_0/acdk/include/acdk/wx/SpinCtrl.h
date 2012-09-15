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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/SpinCtrl.h,v 1.8 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_SpinCtrl_h
#define acdk_wx_SpinCtrl_h

#include "SpinButton.h"

namespace acdk {
namespace wx {

enum Direction;


ACDK_DECL_CLASS(SpinCtrl);

/**
  see wxSpinCtrl
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC SpinCtrl
: extends Control
{
  ACDK_WITH_METAINFO(SpinCtrl)
public:
  // wxSpinCtrl
  ACDK_WX_STD_MEMBERS(SpinCtrl, Control)
  SpinCtrl(IN(RWindow) parent, int id, IN(RString) value = "", IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), 
            int style = SpArrowKeys, int min = 0, int max = 100, int initial = 0,
            IN(RString) name = "SpinCtrl")
  : Control(new wxSpinCtrl(CLS2WXPTR(parent), id, S2WXS(value), CLS2WXREF(pos), CLS2WXREF(size), style, min, max, initial, S2WXS(name)), parent == Nil)
  {
  }
  //void SetValue(const wxString& text);
  inline void setValue(IN(RString)  text) { getWx()->SetValue(S2WXS(text)); }

  //int GetValue() const;
  inline int getValue() const { return getWx()->GetValue(); }
  //int GetMin() const { return m_min; }
  inline int getMin() const { return getWx()->GetMin(); }
  //int GetMax() const { return m_max; }
  inline int getMax() const { return getWx()->GetMax(); }
  //void SetValue(int val);
  inline void setValue(int val) { getWx()->SetValue(val); }
  //void SetRange(int minVal, int maxVal);
  inline void setRange(int minVal, int maxVal) { getWx()->SetRange(minVal, maxVal); }

    // is this spin button vertically oriented?
  //bool IsVertical() const { return (m_windowStyle & wxSP_VERTICAL) != 0; }
  inline bool isVertical() const 
  { 
#if defined(ACDK_OS_WIN32)
   return getWx()->IsVertical(); 
#else
   return false;
#endif
  }

};


} // wx
} // acdk

#endif //acdk_wx_SpinCtrl_h
