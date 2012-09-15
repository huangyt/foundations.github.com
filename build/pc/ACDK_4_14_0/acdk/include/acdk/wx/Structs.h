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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Structs.h,v 1.10 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Structs_h
#define acdk_wx_Structs_h

#include "WxObject.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(Point);

typedef WxValStruct<wxPoint> PointSuper;

/**
  see wxPoint
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Point
: extends PointSuper
{
  ACDK_WITH_METAINFO(Point)
  typedef WxValStruct<wxPoint> Super;
public:
  Point() : Super(wxPoint()) {}
  foreign Point(const wxPoint& pt) : Super(pt) {}
  Point(int xpos, int ypos) : Super(wxPoint(xpos, ypos)) {}
  static RPoint defaultPosition();
  
  int x() const { return _wxObject.x; }
  int y() const { return _wxObject.y; }
  int x(int v) { return _wxObject.x = v; }
  int y(int v) { return _wxObject.y = v; }
};
inline RPoint fromWx(const wxPoint& wx) { return new Point(wx); }


typedef WxValStruct<wxSize> SizeSuper;

ACDK_DECL_CLASS(Size);
/**
  see wxSize
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Size
: extends SizeSuper
{
  ACDK_WITH_METAINFO(Size)
  typedef WxValStruct<wxSize> Super;
public:
  Size() : Super(wxSize()) {}
  foreign Size(const wxSize& other) : Super(other) {}
  Size(int xpos, int ypos) : Super(wxSize(xpos, ypos)) {}
  static RSize defaultSize();
  int x() const { return _wxObject.x; }
  int y() const { return _wxObject.y; }
  int x(int v) { return _wxObject.x = v; }
  int y(int v) { return _wxObject.y = v; }
};
inline RSize fromWx(const wxSize& wx) { return new Size(wx); }


typedef WxValStruct<wxRect> RectSuper;

ACDK_DECL_CLASS(Rect);
/**
  see wxRect
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Rect
: extends RectSuper
{
  ACDK_WITH_METAINFO(Rect)
  typedef WxValStruct<wxRect> Super;
public:
  

  Rect() : Super(wxRect()) {}
  foreign Rect(const wxRect& other) : Super(other) {}
  Rect(int xpos, int ypos, int widthsize, int heightsize) 
    : Super(wxRect(xpos, ypos, widthsize, heightsize)) {}
  int x() const { return _wxObject.x; }
  int y() const { return _wxObject.y; }
  int width() const { return _wxObject.width; }
  int height() const { return _wxObject.height; }

  int x(int v)  { return _wxObject.x = v; }
  int y(int v)  { return _wxObject.y = v; }
  int width(int v)  { return _wxObject.width = v; }
  int height(int v)  { return _wxObject.height = v; }
};
inline RRect fromWx(const wxRect& wx) { return new Rect(wx); }


} // wx
} // acdk

#endif //acdk_wx_Structs_h
