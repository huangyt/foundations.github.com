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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/FontData.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_FontData_h
#define acdk_wx_FontData_h

#include "Font.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(FontData);

/**
  see wxFontData
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC FontData
: extends WxObject
{
  ACDK_WITH_METAINFO(FontData)
public:
  // wxFontData
  ACDK_WX_STD_MEMBERS(FontData, WxObject)
  FontData() : WxObject(new wxFontData(), true) {}
  //void EnableEffects(bool enable);
  inline void enableEffects(bool enable) { getWx()->EnableEffects(enable); }
  //bool GetAllowSymbols();
  inline bool getAllowSymbols() { return getWx()->GetAllowSymbols(); }
  //wxColour& GetColour();
  inline RColour getColour() { return WXVAL2CLS(Colour, getWx()->GetColour()); }
  //wxFont GetChosenFont();
  inline RFont getChosenFont() { return WXVAL2CLS(Font, getWx()->GetChosenFont()); }
  //bool GetEnableEffects();
  inline bool getEnableEffects() { return getWx()->GetEnableEffects(); }
  //wxFont GetInitialFont();
  inline RFont getInitialFont() { return WXVAL2CLS(Font, getWx()->GetInitialFont()); }
  //bool GetShowHelp();
  inline bool getShowHelp() { return getWx()->GetShowHelp(); }
  //void SetAllowSymbols(bool allowSymbols);
  inline void setAllowSymbols(bool allowSymbols) { getWx()->SetAllowSymbols(allowSymbols); }
  //void SetChosenFont(const wxFont& font);
  inline void setChosenFont(IN(RFont) font) { getWx()->SetChosenFont(CLS2WXREF(font)); }
  //void SetColour(const wxColour& colour);
  inline void setColour(IN(RColour) colour) { getWx()->SetColour(CLS2WXREF(colour)); }
  //void SetInitialFont(const wxFont&font);
  inline void setInitialFont(IN(RFont) font) { getWx()->SetInitialFont(CLS2WXREF(font)); }
  //void SetRange(int min, int max);
  inline void setRange(int min, int max) { getWx()->SetRange(min, max); }
  //void SetShowHelp(bool showHelp);
  inline void setShowHelp(bool showHelp) { getWx()->SetShowHelp(showHelp); }
  
};


} // wx
} // acdk

#endif //acdk_wx_FontData_h
