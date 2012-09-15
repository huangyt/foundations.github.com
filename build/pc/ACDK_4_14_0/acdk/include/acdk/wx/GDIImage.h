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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/GDIImage.h,v 1.13 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_GDIImage_h
#define acdk_wx_GDIImage_h

#include "WxObject.h"
#include "Colour.h"
#include "GDIObject.h"
#include "Structs.h"

namespace acdk {
namespace wx {



ACDK_DECL_CLASS(GDIImage);

/**
  see wxGDIImage
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/02/05 10:45:35 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC GDIImage
: extends GDIObject
{
  ACDK_WITH_METAINFO(GDIImage)
public:
  foreign GDIImage(wxGDIObject* obj, bool owns = true) : GDIObject(obj, owns) {}
  foreign GDIImage(const wxGDIObject& obj, bool owns = false) : GDIObject(obj, owns) {}
  //ACDK_WX_STD_MEMBERS(GDIObject, WxObject)
      //ACDK_WX_STD_MEMBERS(GDIImage, GDIObject)
/*
  //

  //GDIImage() : GDIObject(new wxGDIImage()) {}
  //bool Ok() const { return GetHandle() != 0; }
  inline bool ok() const { return getWx()->Ok(); }


    //int GetWidth() const { return IsNull() ? 0 : GetGDIImageData()->m_width; }
  inline int getWidth() const { return getWx()->GetWidth(); }
    //int GetHeight() const { return IsNull() ? 0 : GetGDIImageData()->m_height; }
  inline int getHeight() const { return getWx()->GetHeight(); }
    //int GetDepth() const { return IsNull() ? 0 : GetGDIImageData()->m_depth; }
  inline int getDepth() const { return getWx()->GetDepth(); }

    //void SetWidth(int w) { EnsureHasData(); GetGDIImageData()->m_width = w; }
  inline void setWidth(int w) { getWx()->SetWidth(w); }
    //void SetHeight(int h) { EnsureHasData(); GetGDIImageData()->m_height = h; }
  inline void setHeight(int h) { getWx()->SetHeight(h); }
    //void SetDepth(int d) { EnsureHasData(); GetGDIImageData()->m_depth = d; }
  inline void setDepth(int d) { getWx()->SetDepth(d); }

    //void SetSize(int w, int h)
  inline void setSize(int w, int h) { getWx()->SetSize(w, h); }
    //void SetSize(const wxSize& size) { SetSize(size.x, size.y); }
  inline void setSize(IN(RSize) size) { getWx()->SetSize(CLS2WXREF(size)); }

    // forward some of base class virtuals to wxGDIImageRefData
    //bool FreeResource(bool force);
    inline bool freeResource(bool force) { return getWx()->FreeResource(force); }
*/
};



} // wx
} // acdk

#endif //acdk_wx_GDIImage_h
