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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Region.h,v 1.5 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Region_h
#define acdk_wx_Region_h

#include "GDIObject.h"
#include "Colour.h"
#include "Bitmap.h"

namespace acdk {
namespace wx {

/**
  see Region
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum RegionContain
{
    Outregion = wxOutRegion,  // wxOutRegion = 0,
    Partregion = wxPartRegion,  // wxPartRegion = 1,
    Inregion = wxInRegion  // wxInRegion = 2
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, RegionContain);

ACDK_DECL_CLASS(Region);

/**
  see wxRegion
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Region
: extends GDIObject
{
  ACDK_WITH_METAINFO(Region)
public:
  // wxRegion
  ACDK_WX_STD_MEMBERS(Region, GDIObject)

  Region() : GDIObject(new wxRegion(), true) {}
  Region(int x, int y, int w, int h) : GDIObject(new wxRegion(x, y, w, h), true) {}
  //void wxRegion(const wxPoint& topLeft, const wxPoint& bottomRight);
  Region(IN(RPoint) topLeft, IN(RPoint) bottomRight) : GDIObject(new wxRegion(CLS2WXREF(topLeft), CLS2WXREF(bottomRight)), true) { }
  //void wxRegion(const wxRect& rect);
  Region(IN(RRect) rect) : GDIObject(new wxRegion(CLS2WXREF(rect)), true) {}
  
  // not supported wxRegion(size_t n, const wxPoint *points, int fillStyle = wxODDEVEN_RULE );
  //void wxRegion( const wxBitmap& bmp, const wxColour& transColour = wxNullColour, int   tolerance = 0)
  Region(IN(RBitmap) bmp, IN(RColour) transColour = Colour::getNullColour(), int tolerance = 0)
    : GDIObject(new wxRegion(CLS2WXREF(bmp), CLS2WXREF(transColour), tolerance), true) {}
  
  // Clear current region
  //void Clear();
  inline void clear() { getWx()->Clear(); }
  
  // Move the region
  //bool Offset(wxCoord x, wxCoord y);
  inline bool offset(int x, int y) { return getWx()->Offset(x, y); }
  
  // Union rectangle or region with this.
  //bool Union(wxCoord x, wxCoord y, wxCoord width, wxCoord height) { return Combine(x, y, width, height, wxRGN_OR); }
  inline bool unionRegion(int x, int y, int width, int height) { return getWx()->Union(x, y, width, height); }
  //bool Union(const wxRect& rect) { return Combine(rect, wxRGN_OR); }
  inline bool unionRegion(IN(RRect) rect) { return getWx()->Union(CLS2WXREF(rect)); }
  //bool Union(const wxRegion& region) { return Combine(region, wxRGN_OR); }
  inline bool unionRegion(IN(RRegion) region) { return getWx()->Union(CLS2WXREF(region)); }
  
  // Intersect rectangle or region with this.
  //bool Intersect(wxCoord x, wxCoord y, wxCoord width, wxCoord height) { return Combine(x, y, width, height, wxRGN_AND); }
  inline bool intersect(int x, int y, int width, int height) { return getWx()->Intersect(x, y, width, height); }
  //bool Intersect(const wxRect& rect)  { return Combine(rect, wxRGN_AND); }
  inline bool intersect(IN(RRect) rect) { return getWx()->Intersect(CLS2WXREF(rect)); }
  //bool Intersect(const wxRegion& region)  { return Combine(region, wxRGN_AND); }
  inline bool intersect(IN(RRegion) region) { return getWx()->Intersect(CLS2WXREF(region)); }
  
  // Subtract rectangle or region from this:
  // Combines the parts of 'this' that are not part of the second region.
  //bool Subtract(wxCoord x, wxCoord y, wxCoord width, wxCoord height) { return Combine(x, y, width, height, wxRGN_DIFF); }
  inline bool subtract(int x, int y, int width, int height) { return getWx()->Subtract(x, y, width, height); }
  //bool Subtract(const wxRect& rect)  { return Combine(rect, wxRGN_DIFF); }
  inline bool subtract(IN(RRect) rect) { return getWx()->Subtract(CLS2WXREF(rect)); }
  //bool Subtract(const wxRegion& region)  { return Combine(region, wxRGN_DIFF); }
  inline bool subtract(IN(RRegion) region) { return getWx()->Subtract(CLS2WXREF(region)); }
  
  // XOR: the union of two combined regions except for any overlapping areas.
  //bool Xor(wxCoord x, wxCoord y, wxCoord width, wxCoord height) { return Combine(x, y, width, height, wxRGN_XOR); }
  inline bool xorRegion(int x, int y, int width, int height) { return getWx()->Xor(x, y, width, height); }
  //bool Xor(const wxRect& rect)  { return Combine(rect, wxRGN_XOR); }
  inline bool xorRegion(IN(RRect) rect) { return getWx()->Xor(CLS2WXREF(rect)); }
  //bool Xor(const wxRegion& region)  { return Combine(region, wxRGN_XOR); }
  inline bool xorRegion(IN(RRegion) region) { return getWx()->Xor(CLS2WXREF(region)); }
  
  // Information on region
  // ---------------------
  
  // Outer bounds of region
  //void GetBox(int x, int y,  int w,  int h) const;
  inline void getBox(OUT(int) x, OUT(int) y, OUT(int) w, OUT(int) h) const { getWx()->GetBox(x, y, w, h); }
  //wxRect GetBox() const ;
  inline RRect getBox() const { return WXVAL2CLS(Rect, getWx()->GetBox()); }
  
  // Is region empty?
  //bool Empty() const;
  inline bool empty() const { return getWx()->Empty(); }
  //inline bool IsEmpty() const { return Empty(); }
  inline bool isEmpty() const { return getWx()->IsEmpty(); }
  
  // Tests
  // Does the region contain the point (x,y)?
  //wxRegionContain Contains(wxCoord x, wxCoord y) const;
  inline RegionContain contains(int x, int y) const { return (RegionContain)getWx()->Contains(x, y); }
  // Does the region contain the point pt?
  //wxRegionContain Contains(const wxPoint& pt) const;
  inline RegionContain contains(IN(RPoint) pt) const { return (RegionContain)getWx()->Contains(CLS2WXREF(pt)); }
  // Does the region contain the rectangle (x, y, w, h)?
  //wxRegionContain Contains(wxCoord x, wxCoord y, wxCoord w, wxCoord h) const;
  inline RegionContain contains(int x, int y, int w, int h) const { return (RegionContain)getWx()->Contains(x, y, w, h); }
  // Does the region contain the rectangle rect?
  //wxRegionContain Contains(const wxRect& rect) const;
  inline RegionContain contains(IN(RRect) rect) const { return (RegionContain)getWx()->Contains(CLS2WXREF(rect)); }
  
  // Convert the region to a B&W bitmap with the white pixels being inside
  // the region.
  //wxBitmap ConvertToBitmap() const;
  inline RBitmap convertToBitmap() const { return WXVAL2CLS(Bitmap, getWx()->ConvertToBitmap()); }
  
  /**
    From wxWidgets: Use the non-transparent pixels of a wxBitmap for the region to combine
    with this region.  If the bitmap has a mask then it will be used,
    otherwise the colour to be treated as transparent may be specified,
    along with an optional tolerance value.
  */
  //bool Union(const wxBitmap& bmp, const wxColour& transColour = wxNullColour, int   tolerance = 0);
  inline bool unionRegion(IN(RBitmap) bmp, IN(RColour) transColour = Colour::getNullColour(), int tolerance = 0) { return getWx()->Union(CLS2WXREF(bmp), CLS2WXREF(transColour), tolerance); }
};


} // wx
} // acdk

#endif //acdk_wx_Region_h
