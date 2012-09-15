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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Cursor.h,v 1.12 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Cursor_h
#define acdk_wx_Cursor_h

#include "WxObject.h"

namespace acdk {
namespace wx {

/**
  see wxCursor
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum StockCursor
{
  CursorNone = wxCURSOR_NONE,  // wxCURSOR_NONE,          // should be 0
    CursorArrow = wxCURSOR_ARROW,  // wxCURSOR_ARROW,
    CursorRightArrow = wxCURSOR_RIGHT_ARROW,  // wxCURSOR_RIGHT_ARROW,
    CursorBullseye = wxCURSOR_BULLSEYE,  // wxCURSOR_BULLSEYE,
    CursorChar = wxCURSOR_CHAR,  // wxCURSOR_CHAR,
    CursorCross = wxCURSOR_CROSS,  // wxCURSOR_CROSS,
    CursorHand = wxCURSOR_HAND,  // wxCURSOR_HAND,
    CursorIbeam = wxCURSOR_IBEAM,  // wxCURSOR_IBEAM,
    CursorLeftButton = wxCURSOR_LEFT_BUTTON,  // wxCURSOR_LEFT_BUTTON,
    CursorMagnifier = wxCURSOR_MAGNIFIER,  // wxCURSOR_MAGNIFIER,
    CursorMiddleButton = wxCURSOR_MIDDLE_BUTTON,  // wxCURSOR_MIDDLE_BUTTON,
    CursorNoEntry = wxCURSOR_NO_ENTRY,  // wxCURSOR_NO_ENTRY,
    CursorPaintBrush = wxCURSOR_PAINT_BRUSH,  // wxCURSOR_PAINT_BRUSH,
    CursorPencil = wxCURSOR_PENCIL,  // wxCURSOR_PENCIL,
    CursorPointLeft = wxCURSOR_POINT_LEFT,  // wxCURSOR_POINT_LEFT,
    CursorPointRight = wxCURSOR_POINT_RIGHT,  // wxCURSOR_POINT_RIGHT,
    CursorQuestionArrow = wxCURSOR_QUESTION_ARROW,  // wxCURSOR_QUESTION_ARROW,
    CursorRightButton = wxCURSOR_RIGHT_BUTTON,  // wxCURSOR_RIGHT_BUTTON,
    CursorSizenesw = wxCURSOR_SIZENESW,  // wxCURSOR_SIZENESW,
    CursorSizens = wxCURSOR_SIZENS,  // wxCURSOR_SIZENS,
    CursorSizenwse = wxCURSOR_SIZENWSE,  // wxCURSOR_SIZENWSE,
    CursorSizewe = wxCURSOR_SIZEWE,  // wxCURSOR_SIZEWE,
    CursorSizing = wxCURSOR_SIZING,  // wxCURSOR_SIZING,
    CursorSpraycan = wxCURSOR_SPRAYCAN,  // wxCURSOR_SPRAYCAN,
    CursorWait = wxCURSOR_WAIT,  // wxCURSOR_WAIT,
    CursorWatch = wxCURSOR_WATCH,  // wxCURSOR_WATCH,
    CursorBlank = wxCURSOR_BLANK,  // wxCURSOR_BLANK,
    /*
    #ifdef __WXGTK__
    wxCURSOR_DEFAULT, // standard X11 cursor
    #endif
    #ifdef __WXMAC__
    wxCURSOR_COPY_ARROW , // MacOS Theme Plus arrow 
    #endif
    #ifdef __X__
    // Not yet implemented for Windows
    wxCURSOR_CROSS_REVERSE,
    wxCURSOR_DOUBLE_ARROW,
    wxCURSOR_BASED_ARROW_UP,
    wxCURSOR_BASED_ARROW_DOWN,
    #endif // X11
    */
    CursorArrowwait = wxCURSOR_ARROWWAIT,  // wxCURSOR_ARROWWAIT,
    CursorMax = wxCURSOR_MAX  // wxCURSOR_MAX
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, StockCursor);

ACDK_DECL_CLASS(Cursor);

/**
  see wxCursor
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC Cursor
: extends WxObject
{
  ACDK_WITH_METAINFO(Cursor) 
public:
  ACDK_WX_STD_VAL_MEMBERS(Cursor, WxObject)

  Cursor() : WxObject(new wxCursor(), true) {}
  Cursor(int cursor_type) : WxObject(new wxCursor(cursor_type), true) {}

  static RCursor getStandardCursor() { return new Cursor(*wxSTANDARD_CURSOR); }
  static RCursor getHourglassCursor() { return new Cursor(*wxHOURGLASS_CURSOR); }
  static RCursor getCrossCursor() { return new Cursor(*wxCROSS_CURSOR); }
  static RCursor getNullCursor() { return new Cursor(wxNullCursor); }
};

inline RCursor fromWx(const wxCursor& wx) { return new Cursor(wx); }

} // wx
} // acdk

#endif //acdk_wx_Cursor_h
