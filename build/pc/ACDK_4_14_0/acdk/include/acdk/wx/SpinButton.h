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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/SpinButton.h,v 1.5 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_SpinButton_h
#define acdk_wx_SpinButton_h

#include "Choice.h"

namespace acdk {
namespace wx {

enum Direction;

/**
 wxSpinButton flags.
 Note that a wxSpinCtrl is sometimes defined as
 a wxTextCtrl, and so the flags must be different
 from wxTextCtrl's.
 */

enum SpinButtonStyle
{
  SpHorizontal       = wxSP_HORIZONTAL      ,  // wxSP_HORIZONTAL       wxHORIZONTAL // 4
  SpVertical         = wxSP_VERTICAL        ,  // wxSP_VERTICAL         wxVERTICAL   // 8
  SpArrowKeys       = wxSP_ARROW_KEYS      ,  // wxSP_ARROW_KEYS       0x1000
  SpWrap             = wxSP_WRAP              // wxSP_WRAP             0x2000
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, SpinButtonStyle);

ACDK_DECL_CLASS(SpinButton);
/**
  see wxSpinButton
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC SpinButton
: extends Control
{
  ACDK_WITH_METAINFO(SpinButton)
public:
  // wxSpinButton
  ACDK_WX_STD_MEMBERS(SpinButton, Control)
  SpinButton(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), 
            int style = SpVertical | SpArrowKeys,
            IN(RString) name = "SpinButton")
  : Control(new wxSpinButton(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style, S2WXS(name)), parent == Nil)
  {
  }
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
  inline bool isVertical() const { return getWx()->IsVertical(); }
};


} // wx
} // acdk

#endif //acdk_wx_SpinButton_h
