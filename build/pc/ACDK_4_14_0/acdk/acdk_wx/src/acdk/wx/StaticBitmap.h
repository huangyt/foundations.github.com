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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/StaticBitmap.h,v 1.4 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_StaticBitmap_h
#define acdk_wx_StaticBitmap_h

#include "Control.h"
#include "Bitmap.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(StaticBitmap);

/**
  see wxStaticBitmap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC StaticBitmap
: extends Control
{
  // wxStaticBitmap
  ACDK_WITH_METAINFO(StaticBitmap)
public:
  // wxStaticBitmap
  ACDK_WX_STD_MEMBERS(StaticBitmap, Control)

  StaticBitmap(IN(RWindow) parent, int id, IN(RBitmap) label, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), 
            int style = 0, IN(RString) name = "staticBitmap")
  : Control(new wxStaticBitmap(CLS2WXPTR(parent), id, CLS2WXREF(label), CLS2WXREF(pos), CLS2WXREF(size), 
                           style, S2WXS(name)), parent == Nil)
  {
  }
  //wxBitmap& GetBitmap() const;
  inline RBitmap getBitmap() const { return WXVAL2CLS(Bitmap, getWx()->GetBitmap()); }
  //void SetBitmap(const wxBitmap& label);
  inline void setBitmap(IN(RBitmap) label) { getWx()->SetBitmap(CLS2WXREF(label)); }

};


} // wx
} // acdk

#endif //acdk_wx_StaticBitmap_h