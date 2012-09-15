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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/BitmapButton.h,v 1.6 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_BitmapButton_h
#define acdk_wx_BitmapButton_h
 
#include "Button.h"
#include "Bitmap.h"

namespace acdk {
namespace wx {

/**
  see wxBitmapButton
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum BitmapButtonStyle
{
  BuNoautodraw      = wxBU_NOAUTODRAW     ,  // wxBU_NOAUTODRAW      0x0000
  BuAutodraw        = wxBU_AUTODRAW       ,  // wxBU_AUTODRAW        0x0004

// These flags affect label alignment
  BuLeft            = wxBU_LEFT           ,  // wxBU_LEFT            0x0040
  BuTop             = wxBU_TOP            ,  // wxBU_TOP             0x0080
  BuRight           = wxBU_RIGHT          ,  // wxBU_RIGHT           0x0100
  BuBottom          = wxBU_BOTTOM           // wxBU_BOTTOM          0x0200
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, BitmapButtonStyle);


ACDK_DECL_CLASS(BitmapButton);

/**
  see wxBitmapButton
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC BitmapButton
: extends Button
{
  ACDK_WITH_METAINFO(BitmapButton)
public:
  // wxBitmapButton
  ACDK_WX_STD_MEMBERS(BitmapButton, Button)
  BitmapButton(IN(RWindow) parent, int id, IN(RBitmap) bitmap, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), 
           int style = BuAutodraw, IN(RValidator) validator = Nil, IN(RString) name = "button")
  : Button(new wxBitmapButton(CLS2WXPTR(parent), id, CLS2WXREF(bitmap), CLS2WXREF(pos), CLS2WXREF(size), style, validator == Nil ? wxDefaultValidator : CLS2WXREF(validator), S2WXS(name)),
            parent == Nil)
  {
  }
  //wxBitmap& GetBitmapDisabled() const;
  inline RBitmap getBitmapDisabled() const { return WXVAL2CLS(Bitmap, getWx()->GetBitmapDisabled()); }
  //void SetBitmapDisabled(const wxBitmap& bitmap);
  inline void setBitmapDisabled(IN(RBitmap) bitmap) { getWx()->SetBitmapDisabled(CLS2WXREF(bitmap)); }
  //wxBitmap& GetBitmapFocus() const;
  inline RBitmap getBitmapFocus() const { return WXVAL2CLS(Bitmap, getWx()->GetBitmapFocus()); }
  //void SetBitmapFocus(const wxBitmap& bitmap);
  inline void setBitmapFocus(IN(RBitmap) bitmap) { getWx()->SetBitmapFocus(CLS2WXREF(bitmap)); }
  //wxBitmap& GetBitmapLabel() const;
  inline RBitmap getBitmapLabel() const { return WXVAL2CLS(Bitmap, getWx()->GetBitmapLabel()); }
  //void SetBitmapLabel(const wxBitmap& bitmap);
  inline void setBitmapLabel(IN(RBitmap) bitmap) { getWx()->SetBitmapLabel(CLS2WXREF(bitmap)); }
  //wxBitmap& GetBitmapSelected() const;
  inline RBitmap getBitmapSelected() const { return WXVAL2CLS(Bitmap, getWx()->GetBitmapSelected()); }
  //void SetBitmapSelected(const wxBitmap& bitmap);
  inline void setBitmapSelected(IN(RBitmap) bitmap) { getWx()->SetBitmapSelected(CLS2WXREF(bitmap)); }
};


} // wx
} // acdk

#endif //acdk_wx_BitmapButton_h
