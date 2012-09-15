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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/BitmapDataObject.h,v 1.3 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_BitmapDataObject_h
#define acdk_wx_BitmapDataObject_h

#include "DataObjectSimple.h"
#include "Bitmap.h"

namespace acdk {
namespace wx {

enum DataObjectDirection;

ACDK_DECL_CLASS(BitmapDataObject);

/**
  see wxBitmapDataObject
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC BitmapDataObject
: extends DataObjectSimple
{
  ACDK_WITH_METAINFO(BitmapDataObject)
public:
  ACDK_WX_STD_MEMBERS(BitmapDataObject, DataObjectSimple)
  //void BitmapDataObject(const wxBitmap& bitmap = wxNullBitmap) ;
  inline BitmapDataObject(IN(RBitmap) bitmap = Bitmap::getNullBitmap()) 
  : DataObjectSimple(new wxBitmapDataObject(CLS2WXREF(bitmap))) {}  
  
  //wxBitmap GetBitmap() const;
  inline RBitmap getBitmap() const { return WXVAL2CLS(Bitmap, getWx()->GetBitmap()); }
  //void SetBitmap(const wxBitmap& bitmap);
  inline void setBitmap(IN(RBitmap) bitmap) { getWx()->SetBitmap(CLS2WXREF(bitmap)); }

};


} // wx
} // acdk

#endif //acdk_wx_BitmapDataObject_h
