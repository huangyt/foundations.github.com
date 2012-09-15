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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Icon.h,v 1.5 2005/04/21 09:52:28 kommer Exp $

#ifndef acdk_wx_Icon_h
#define acdk_wx_Icon_h


#include "Bitmap.h"

namespace acdk {
namespace wx {

#if ACDK_CHECK_WX_VERSION(2, 6)
# define INT2WXBITMAPTYPE(value) (wxBitmapType)value
#else
# define INT2WXBITMAPTYPE(value) value
#endif

ACDK_DECL_CLASS(Icon);

/**
  see wxIcon
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/04/21 09:52:28 $
*/
class ACDK_WX_PUBLIC Icon
: extends GDIObject
{
  ACDK_WITH_METAINFO(Icon)
public:
  ACDK_WX_STD_VAL_MEMBERS(Icon, GDIObject)
  // wxIcon
  Icon() : GDIObject(new wxIcon()) {}

  Icon(IN(RString) name, int type, int desiredWidth = -1, int desiredHeight = -1)
    : GDIObject(new wxIcon(S2WXS(name), INT2WXBITMAPTYPE(type), desiredWidth, desiredHeight), true)
  {
  }
  //bool LoadFile(const wxString& name, long type);
  inline bool loadFile(IN(RString)  name, int type) { return getWx()->LoadFile(S2WXS(name), INT2WXBITMAPTYPE(type)); }
  //int GetDepth() const;
  inline int getDepth() const { return getWx()->GetDepth(); }
  //void SetDepth(int depth);
  inline void setDepth(int depth) { getWx()->SetDepth(depth); }
  //int GetHeight() const;
  inline int getHeight() const { return getWx()->GetHeight(); }
  //void SetHeight(int height);
  inline void setHeight(int height) { getWx()->SetHeight(height); }
  //int GetWidth() const;
  inline int getWidth() const { return getWx()->GetWidth(); }
  //void SetWidth(int width);
  inline void setWidth(int width) { getWx()->SetWidth(width); }
  //bool Ok() const;
  inline bool ok() const { return getWx()->Ok(); }
  //void SetOk(bool isOk);
  // not exists on win32: inline void setOk(bool isOk) { getWx()->SetOk(isOk); }

  static RIcon nullIcon() { return new Icon(wxNullIcon); }

};

} // wx
} // acdk

#endif //acdk_wx_Icon_h
