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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ColourData.h,v 1.3 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_ColourData_h
#define acdk_wx_ColourData_h

#include "Colour.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(ColourData);

/**
  see wxColourData
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC ColourData
: extends WxObject
{
  ACDK_WITH_METAINFO(ColourData)
public:
  // wxColourData
  ACDK_WX_STD_MEMBERS(ColourData, WxObject)
  ColourData() : WxObject(new wxColourData(), true) {}
  //bool GetChooseFull() const;
  inline bool getChooseFull() const { return getWx()->GetChooseFull(); }
  //wxColour& GetColour() const;
  inline RColour getColour() const { return WXVAL2CLS(Colour, getWx()->GetColour()); }
  //wxColour& GetCustomColour(int i) const;
  inline RColour getCustomColour(int i)  { return WXVAL2CLS(Colour, getWx()->GetCustomColour(i)); }
  //void SetChooseFull(const bool flag);
  inline void setChooseFull(bool flag) { getWx()->SetChooseFull(flag); }
  //void SetColour(const wxColour& colour);
  inline void setColour(IN(RColour) colour) { getWx()->SetColour(CLS2WXREF(colour)); }
  //void SetCustomColour(int i, const wxColour& colour);
  inline void setCustomColour(int i, IN(RColour) colour) { getWx()->SetCustomColour(i, CLS2WXREF(colour)); }
};


} // wx
} // acdk

#endif //acdk_wx_ColourData_h
