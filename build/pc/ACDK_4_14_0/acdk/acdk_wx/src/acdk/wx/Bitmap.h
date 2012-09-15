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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Bitmap.h,v 1.16 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Bitmap_h
#define acdk_wx_Bitmap_h

#include "GDIImage.h"
#include "Icon.h"

namespace acdk {
namespace wx {

/**
  see wxBitmapType
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum BitmapType
{
    BitmapTypeInvalid = wxBITMAP_TYPE_INVALID,  // wxBITMAP_TYPE_INVALID,          // should be == 0 for compatibility!
    BitmapTypeBmp = wxBITMAP_TYPE_BMP,  // wxBITMAP_TYPE_BMP,
    BitmapTypeBmpResource = wxBITMAP_TYPE_BMP_RESOURCE,  // wxBITMAP_TYPE_BMP_RESOURCE,
    BitmapTypeResource = wxBITMAP_TYPE_RESOURCE,  // wxBITMAP_TYPE_RESOURCE = wxBITMAP_TYPE_BMP_RESOURCE,
    BitmapTypeIco = wxBITMAP_TYPE_ICO,  // wxBITMAP_TYPE_ICO,
    BitmapTypeIcoResource = wxBITMAP_TYPE_ICO_RESOURCE,  // wxBITMAP_TYPE_ICO_RESOURCE,
    BitmapTypeCur = wxBITMAP_TYPE_CUR,  // wxBITMAP_TYPE_CUR,
    BitmapTypeCurResource = wxBITMAP_TYPE_CUR_RESOURCE,  // wxBITMAP_TYPE_CUR_RESOURCE,
    BitmapTypeXbm = wxBITMAP_TYPE_XBM,  // wxBITMAP_TYPE_XBM,
    BitmapTypeXbmData = wxBITMAP_TYPE_XBM_DATA,  // wxBITMAP_TYPE_XBM_DATA,
    BitmapTypeXpm = wxBITMAP_TYPE_XPM,  // wxBITMAP_TYPE_XPM,
    BitmapTypeXpmData = wxBITMAP_TYPE_XPM_DATA,  // wxBITMAP_TYPE_XPM_DATA,
    BitmapTypeTif = wxBITMAP_TYPE_TIF,  // wxBITMAP_TYPE_TIF,
    BitmapTypeTifResource = wxBITMAP_TYPE_TIF_RESOURCE,  // wxBITMAP_TYPE_TIF_RESOURCE,
    BitmapTypeGif = wxBITMAP_TYPE_GIF,  // wxBITMAP_TYPE_GIF,
    BitmapTypeGifResource = wxBITMAP_TYPE_GIF_RESOURCE,  // wxBITMAP_TYPE_GIF_RESOURCE,
    BitmapTypePng = wxBITMAP_TYPE_PNG,  // wxBITMAP_TYPE_PNG,
    BitmapTypePngResource = wxBITMAP_TYPE_PNG_RESOURCE,  // wxBITMAP_TYPE_PNG_RESOURCE,
    BitmapTypeJpeg = wxBITMAP_TYPE_JPEG,  // wxBITMAP_TYPE_JPEG,
    BitmapTypeJpegResource = wxBITMAP_TYPE_JPEG_RESOURCE,  // wxBITMAP_TYPE_JPEG_RESOURCE,
    BitmapTypePnm = wxBITMAP_TYPE_PNM,  // wxBITMAP_TYPE_PNM,
    BitmapTypePnmResource = wxBITMAP_TYPE_PNM_RESOURCE,  // wxBITMAP_TYPE_PNM_RESOURCE,
    BitmapTypePcx = wxBITMAP_TYPE_PCX,  // wxBITMAP_TYPE_PCX,
    BitmapTypePcxResource = wxBITMAP_TYPE_PCX_RESOURCE,  // wxBITMAP_TYPE_PCX_RESOURCE,
    BitmapTypePict = wxBITMAP_TYPE_PICT,  // wxBITMAP_TYPE_PICT,
    BitmapTypePictResource = wxBITMAP_TYPE_PICT_RESOURCE,  // wxBITMAP_TYPE_PICT_RESOURCE,
    BitmapTypeIcon = wxBITMAP_TYPE_ICON,  // wxBITMAP_TYPE_ICON,
    BitmapTypeIconResource = wxBITMAP_TYPE_ICON_RESOURCE,  // wxBITMAP_TYPE_ICON_RESOURCE,
    BitmapTypeAni = wxBITMAP_TYPE_ANI,  // wxBITMAP_TYPE_ANI,
    BitmapTypeIff = wxBITMAP_TYPE_IFF,  // wxBITMAP_TYPE_IFF,
    BitmapTypeMaccursor = wxBITMAP_TYPE_MACCURSOR,  // wxBITMAP_TYPE_MACCURSOR,
    BitmapTypeMaccursorResource = wxBITMAP_TYPE_MACCURSOR_RESOURCE,  // wxBITMAP_TYPE_MACCURSOR_RESOURCE,
    BitmapTypeAny = wxBITMAP_TYPE_ANY  // wxBITMAP_TYPE_ANY = 50
};

ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, BitmapType);


ACDK_DECL_CLASS(Bitmap);

/**
  see wxBitmap
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC Bitmap
: extends GDIImage
{
  ACDK_WITH_METAINFO(Bitmap)
public:
  ACDK_WX_STD_VAL_MEMBERS(Bitmap, GDIImage)

  Bitmap() : GDIImage(new wxBitmap(), true) {}

  Bitmap(IN(RString) name, BitmapType type)
    : GDIImage(new wxBitmap(S2WXS(name), (wxBitmapType)type), true)
  {
  }
  foreign Bitmap(const char** xpmData)
  : GDIImage(new wxBitmap(xpmData), true)
  {
  }
  Bitmap(IN(RIcon) icon) : GDIImage(new wxBitmap(CLS2WXREF(icon)), true) {}
  Bitmap(int width, int height, int depth = -1) : GDIImage(new wxBitmap(width, height, depth), true) {}
  static RBitmap nullBitmap() { return new Bitmap(wxNullBitmap); }
  static RBitmap getNullBitmap() { return new Bitmap(wxNullBitmap); }

};


ACDK_DECL_CLASS(ImageList);

/**
  see wxImageList
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC ImageList
: extends WxObject
{
  ACDK_WITH_METAINFO(ImageList)
public:
  ACDK_WX_STD_MEMBERS(ImageList, WxObject)
  ImageList() : WxObject(new wxImageList(), false) { }
  //void wxImageList(int width, int height, bool mask = TRUE, int initialCount = 1);
  ImageList(int width, int height, bool mask = true, int initialCount = 1) 
  : WxObject(new wxImageList(width, height, mask, initialCount), false)
  {}

  //virtual int GetImageCount() const;
  inline virtual int getImageCount() const { return getWx()->GetImageCount(); }
  //virtual bool GetSize( int index, int &width, int &height ) const;
  inline virtual bool getSize(int index, OUT(int) width, OUT(int) height) const { return getWx()->GetSize(index, width, height); }
  
  //int Add( const wxBitmap& bitmap );
  inline int add(IN(RBitmap) bitmap) { return getWx()->Add(CLS2WXREF(bitmap)); }
  //int Add( const wxBitmap& bitmap, const wxBitmap& mask );
  inline int add(IN(RBitmap) bitmap, IN(RBitmap) mask) { return getWx()->Add(CLS2WXREF(bitmap), CLS2WXREF(mask)); }
  //int Add( const wxBitmap& bitmap, const wxColour& maskColour );
  inline int add(IN(RBitmap) bitmap, IN(RColour) maskColour) { return getWx()->Add(CLS2WXREF(bitmap), CLS2WXREF(maskColour)); }
  //bool Replace( int index, const wxBitmap &bitmap );
  inline bool replace(int index, IN(RBitmap) bitmap) { return getWx()->Replace(index, CLS2WXREF(bitmap)); }
  //bool Remove( int index );
  inline bool remove(int index) { return getWx()->Remove(index); }
  //bool RemoveAll();
  inline bool removeAll() { return getWx()->RemoveAll(); }
};


} // wx
} // acdk

#endif //acdk_wx_Bitmap_h
