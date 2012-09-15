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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Gauge.h,v 1.5 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Gauge_h
#define acdk_wx_Gauge_h

#include "Choice.h"

namespace acdk {
namespace wx {

enum Direction;

/**
  see Gauge
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum GaugeStyle
{
  GaHorizontal      = wxGA_HORIZONTAL     ,  // wxGA_HORIZONTAL      wxHORIZONTAL
  GaVertical        = wxGA_VERTICAL       ,  // wxGA_VERTICAL        wxVERTICAL
  GaProgressbar     = wxGA_PROGRESSBAR    ,  // wxGA_PROGRESSBAR     0x0010
// Windows only
  GaSmooth          = wxGA_SMOOTH           // wxGA_SMOOTH          0x0020
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, GaugeStyle);

ACDK_DECL_CLASS(Gauge);
/**
  see wxGauge
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Gauge
: extends Control
{
  ACDK_WITH_METAINFO(Gauge)
public:
  // wxGauge
  ACDK_WX_STD_MEMBERS(Gauge, Control)

  Gauge(IN(RWindow) parent, int id, int range, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), 
            int style = OrientHorizontal,
            IN(RStringArray) choices = Nil,
            IN(RValidator) validator = Validator::defaultValidator(),
            IN(RString) name = "Gauge")
  : Control(new wxGauge(CLS2WXPTR(parent), id, range, CLS2WXREF(pos), CLS2WXREF(size), style, CLS2WXREF(validator), S2WXS(name)), parent == Nil)
  {
  }
  
  //void SetShadowWidth(int w);
  inline void setShadowWidth(int w) { getWx()->SetShadowWidth(w); }
  //void SetBezelFace(int w);
  inline void setBezelFace(int w) { getWx()->SetBezelFace(w); }
  //void SetRange(int r);
  inline void setRange(int r) { getWx()->SetRange(r); }
  //void SetValue(int pos);
  inline void setValue(int pos) { getWx()->SetValue(pos); }
  
  //int GetShadowWidth() const;
  inline int getShadowWidth() const { return getWx()->GetShadowWidth(); }
  //int GetBezelFace() const;
  inline int getBezelFace() const { return getWx()->GetBezelFace(); }
  //int GetRange() const;
  inline int getRange() const { return getWx()->GetRange(); }
  //int GetValue() const;
  inline int getValue() const { return getWx()->GetValue(); }
  
  //bool SetForegroundColour(const wxColour& col);
  inline bool setForegroundColour(IN(RColour) col) { return getWx()->SetForegroundColour(CLS2WXREF(col)); }
  //bool SetBackgroundColour(const wxColour& col);
  inline bool setBackgroundColour(IN(RColour) col) { return getWx()->SetBackgroundColour(CLS2WXREF(col)); }
  
  // overriden base class virtuals
  //virtual bool AcceptsFocus() const { return FALSE; }
  inline bool acceptsFocus() const { return getWx()->AcceptsFocus(); }

};


} // wx
} // acdk

#endif //acdk_wx_Gauge_h
