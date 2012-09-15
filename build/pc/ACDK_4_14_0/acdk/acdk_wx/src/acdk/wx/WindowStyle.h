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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/WindowStyle.h,v 1.8 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_WindowStyle_h
#define acdk_wx_WindowStyle_h


namespace acdk {
namespace wx {

/**
  see Window
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum WindowStyle
{
  StayOnTop            /* wxSTAY_ON_TOP          */ = 0x8000,
  Iconize                /* wxICONIZE              */ = 0x4000,
  Minimize               /* wxMINIMIZE             */ = wxICONIZE,
  Maximize               /* wxMAXIMIZE             */ = 0x2000,
  SystemMenu            /* wxSYSTEM_MENU          */ = 0x0800,
  MinimizeBox           /* wxMINIMIZE_BOX         */ = 0x0400,
  MaximizeBox           /* wxMAXIMIZE_BOX         */ = 0x0200,
  TinyCaptionHoriz     /* wxTINY_CAPTION_HORIZ   */ = 0x0100,
  TinyCaptionVert      /* wxTINY_CAPTION_VERT    */ = 0x0080,
  ResizeBorder          /* wxRESIZE_BORDER        */ = 0x0040,
  DialogNoParent       /* wxDIALOG_NO_PARENT     */ = 0x0001,  // Don't make owned by apps top window
  FrameNoTaskbar       /* wxFRAME_NO_TASKBAR     */ = 0x0002,  // No taskbar button (MSW only)
  FrameToolWindow      /* wxFRAME_TOOL_WINDOW    */ = 0x0004,  // No taskbar button, no system menu
  FrameFloatOnParent  /* wxFRAME_FLOAT_ON_PARENT*/ = 0x0008,  // Always 
  
  FrameNoWindowMenu   /* wxFRAME_NO_WINDOW_MENU */ = 0x0100,
  DefaultDialogStyle   /* wxDEFAULT_DIALOG_STYLE */ = wxDEFAULT_DIALOG_STYLE,
  //wxDEFAULT_FRAME = wxDEFAULT_FRAME_STYLE,
  DefaultFrameStyle = wxDEFAULT_FRAME_STYLE,
  //wxDEFAULT_FRAME_STYLE  = SystemMenu | ResizeBorder | MinimizeBox | MaximizeBox | wxCAPTION | wxCLIP_CHILDREN
// Override CTL3D etc. control colour processing to allow own background
// colour.
// Override CTL3D or native 3D styles for children
  No3d                 = wxNO_3D                ,  // wxNO_3D                 0x00800000

// OBSOLETE - use wxNO_3D instead
  UserColours          = wxUSER_COLOURS         ,  // wxUSER_COLOURS          wxNO_3D

// wxALWAYS_SHOW_SB: instead of hiding the scrollbar when it is not needed,
// disable it - but still show (see also wxLB_ALWAYS_SB style)
//
// NB: as this style is only supported by wxUniversal so far as it doesn't use
//     wxUSER_COLOURS/wxNO_3D, we reuse the same style value
  AlwaysShowSb        = wxALWAYS_SHOW_SB       ,  // wxALWAYS_SHOW_SB        0x00800000

// Clip children when painting, which reduces flicker in e.g. frames and
// splitter windows, but can't be used in a panel where a static box must be
// 'transparent' (panel paints the background for it)
  ClipChildren         = wxCLIP_CHILDREN        ,  // wxCLIP_CHILDREN         0x00400000

// Note we're reusing the wxCAPTION style because we won't need captions
// for subwindows/controls
  ClipSiblings         = wxCLIP_SIBLINGS        ,  // wxCLIP_SIBLINGS         0x20000000

  TransparentWindow    = wxTRANSPARENT_WINDOW   ,  // wxTRANSPARENT_WINDOW    0x00100000

// Add this style to a panel to get tab traversal working outside of dialogs
// (on by default for wxPanel, wxDialog, wxScrolledWindow)
  TabTraversal         = wxTAB_TRAVERSAL        ,  // wxTAB_TRAVERSAL         0x00080000

// Add this style if the control wants to get all keyboard messages (under
// Windows, it won't normally get the dialog navigation key events)
  WantsChars           = wxWANTS_CHARS          ,  // wxWANTS_CHARS           0x00040000

// Make window retained (mostly Motif, I think) -- obsolete (VZ)?
  Retained              = wxRETAINED             ,  // wxRETAINED              0x00020000
  Backingstore          = wxBACKINGSTORE         ,  // wxBACKINGSTORE          wxRETAINED

// set this flag to create a special popup window: it will be always shown on
// top of other windows, will capture the mouse and will be dismissed when the
// mouse is clicked outside of it or if it loses focus in any other way
  PopupWindow          = wxPOPUP_WINDOW         ,  // wxPOPUP_WINDOW          0x00020000

// don't invalidate the whole window (resulting in a PAINT event) when the
// window is resized (currently, makes sense for wxMSW only)
  NoFullRepaintOnResize = wxNO_FULL_REPAINT_ON_RESIZE,  // wxNO_FULL_REPAINT_ON_RESIZE 0x00010000
  Vscroll               = wxVSCROLL              ,  // wxVSCROLL               0x80000000
  Hscroll               = wxHSCROLL              ,  // wxHSCROLL               0x40000000
  Caption               = wxCAPTION                // wxCAPTION               0x20000000
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, WindowStyle);


enum HitTest
{
  HtNowhere  /* wxHT_NOWHERE*/,

    // scrollbar
    HtScrollbarFirst  /* wxHT_SCROLLBAR_FIRST*/ = HtNowhere,
    HtScrollbarArrowLine1  /* wxHT_SCROLLBAR_ARROW_LINE_1*/,    // left or upper arrow to scroll by line
    HtScrollbarArrowLine2  /* wxHT_SCROLLBAR_ARROW_LINE_2*/,    // right or down
    HtScrollbarArrowPage1  /* wxHT_SCROLLBAR_ARROW_PAGE_1*/,    // left or upper arrow to scroll by page
    HtScrollbarArrowPage2  /* wxHT_SCROLLBAR_ARROW_PAGE_2*/,    // right or down
    HtScrollbarThumb  /* wxHT_SCROLLBAR_THUMB*/,           // on the thumb
    HtScrollbarBar1  /* wxHT_SCROLLBAR_BAR_1*/,           // bar to the left/above the thumb
    HtScrollbarBar2  /* wxHT_SCROLLBAR_BAR_2*/,           // bar to the right/below the thumb
    HtScrollbarLast  /* wxHT_SCROLLBAR_LAST*/,

    // window
    HtWindowOutside  /* wxHT_WINDOW_OUTSIDE*/,            // not in this window at all
    HtWindowInside  /* wxHT_WINDOW_INSIDE*/,             // in the client area
    HtWindowVertScrollbar  /* wxHT_WINDOW_VERT_SCROLLBAR*/,     // on the vertical scrollbar
    HtWindowHorzScrollbar  /* wxHT_WINDOW_HORZ_SCROLLBAR*/,     // on the horizontal scrollbar
    HtWindowCorner  /* wxHT_WINDOW_CORNER*/,             // on the corner between 2 scrollbars

    HtMax  /* wxHT_MAX*/
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, HitTest);

/// border flags: the values are chosen for backwards compatibility
enum Border
{
    // this is different from wxBORDER_NONE as by default the controls do have
    // border
    BorderDefault  /* wxBORDER_DEFAULT*/ = 0,
    BorderNone    /* wxBORDER_NONE  */ = 0x00200000,
    BorderStatic  /* wxBORDER_STATIC*/ = 0x01000000,
    BorderSimple  /* wxBORDER_SIMPLE*/ = 0x02000000,
    BorderRaised  /* wxBORDER_RAISED*/ = 0x04000000,
    BorderSunken  /* wxBORDER_SUNKEN*/ = 0x08000000,
    BorderDouble  /* wxBORDER_DOUBLE*/ = 0x10000000,

    // a mask to extract border style from the combination of flags
    BorderMask    /* wxBORDER_MASK  */ = 0x1f200000
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, Border);


enum Orientation
{
    OrientHorizontal               /* wxHORIZONTAL             */ = 0x0004,
    OrientVertical                 /* wxVERTICAL               */ = 0x0008,
    OrientBoth                     /* wxBOTH                   */ = (OrientHorizontal | OrientVertical)
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, Orientation);


enum ItemKind
{
    ItemSeparator = wxITEM_SEPARATOR,  // wxITEM_SEPARATOR = -1,
    ItemNormal = wxITEM_NORMAL,  // wxITEM_NORMAL,
    ItemCheck = wxITEM_CHECK,  // wxITEM_CHECK,
    ItemRadio = wxITEM_RADIO,  // wxITEM_RADIO,
    ItemMax = wxITEM_MAX  // wxITEM_MAX
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, ItemKind);

/**
 * Extra window style flags (use wxWS_EX prefix to make it clear that they
 * should be passed to wxWindow::SetExtraStyle(), not SetWindowStyle())
 */
enum WindowExtraStyle
{
// by default, TransferDataTo/FromWindow() only work on direct children of the
// window (compatible behaviour), set this flag to make them recursively
// descend into all subwindows
  WsExValidateRecursively    = wxWS_EX_VALIDATE_RECURSIVELY   ,  // wxWS_EX_VALIDATE_RECURSIVELY    0x00000001

// wxCommandEvents and the objects of the derived classes are forwarded to the
// parent window and so on recursively by default. Using this flag for the
// given window allows to block this propagation at this window, i.e. prevent
// the events from being propagated further upwards. The dialogs have this
// flag on by default.
  WsExBlockEvents            = wxWS_EX_BLOCK_EVENTS           ,  // wxWS_EX_BLOCK_EVENTS            0x00000002

// don't use this window as an implicit parent for the other windows: this must
// be used with transient windows as otherwise there is the risk of creating a
// dialog/frame with this window as a parent which would lead to a crash if the
// parent is destroyed before the child
  WsExTransient               = wxWS_EX_TRANSIENT              ,  // wxWS_EX_TRANSIENT               0x00000004

// Use this style to add a context-sensitive help to the window (currently for
// Win32 only and it doesn't work if wxMINIMIZE_BOX or wxMAXIMIZE_BOX are used)
  FrameExContexthelp  = wxFRAME_EX_CONTEXTHELP ,  // wxFRAME_EX_CONTEXTHELP  0x00000004
  DialogExContexthelp = wxDIALOG_EX_CONTEXTHELP  // wxDIALOG_EX_CONTEXTHELP 0x00000004
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, WindowExtraStyle);

enum Direction
{
  //Left                    = wxLEFT                   ,  // wxLEFT                    = 0x0010,
  //Right                   = wxRIGHT                  ,  // wxRIGHT                   = 0x0020,
  Up                      = wxUP                     ,  // wxUP                      = 0x0040,
  Down                    = wxDOWN                   ,  // wxDOWN                    = 0x0080,
   
  //Top                     = wxTOP                    ,  // wxTOP                     = wxUP,
  //Bottom                  = wxBOTTOM                 ,  // wxBOTTOM                  = wxDOWN,
   
  North                   = wxNORTH                  ,  // wxNORTH                   = wxUP,
  South                   = wxSOUTH                  ,  // wxSOUTH                   = wxDOWN,
  West                    = wxWEST                   ,  // wxWEST                    = wxLEFT,
  East                    = wxEAST                   ,  // wxEAST                    = wxRIGHT,
    
  All                     = wxALL                      // wxALL                     = (wxUP | wxDOWN | wxRIGHT | wxLEFT)
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, Direction);

enum SizeFlag 
{
  // Use internally-calculated width if -1
  SizeAutoWidth       = wxSIZE_AUTO_WIDTH      ,  // wxSIZE_AUTO_WIDTH       0x0001
// Use internally-calculated height if -1
  SizeAutoHeight      = wxSIZE_AUTO_HEIGHT     ,  // wxSIZE_AUTO_HEIGHT      0x0002
// Use internally-calculated width and height if each is -1
  SizeAuto             = wxSIZE_AUTO            ,  // wxSIZE_AUTO             (wxSIZE_AUTO_WIDTH|wxSIZE_AUTO_HEIGHT)
// Ignore missing (-1) dimensions (use existing).
// For readability only: test for wxSIZE_AUTO_WIDTH/HEIGHT in code.
  SizeUseExisting     = wxSIZE_USE_EXISTING    ,  // wxSIZE_USE_EXISTING     0x0000
// Allow -1 as a valid position
  SizeAllowMinusOne  = wxSIZE_ALLOW_MINUS_ONE ,  // wxSIZE_ALLOW_MINUS_ONE  0x0004
// Don't do parent client adjustments (for implementation only)
  SizeNoAdjustments   = wxSIZE_NO_ADJUSTMENTS  ,  // wxSIZE_NO_ADJUSTMENTS   0x0008
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, SizeFlag);

} // wx
} // acdk

#endif //acdk_wx_WindowStyle_h
