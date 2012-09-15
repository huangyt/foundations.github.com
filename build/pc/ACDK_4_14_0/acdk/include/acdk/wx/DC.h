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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/DC.h,v 1.9 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_DC_h
#define acdk_wx_DC_h

#include "Pen.h"
#include "Icon.h"
#include "Region.h"

namespace acdk {
namespace wx {

/**
  see wxDC
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum GdiFlags
{
    // Text font families
    GdiDefault    = wxDEFAULT   ,  // wxDEFAULT    = 70,
    GdiDecorative = wxDECORATIVE,  // wxDECORATIVE,
    GdiRoman = wxROMAN,  // wxROMAN,
    GdiScript = wxSCRIPT,  // wxSCRIPT,
    GdiSwiss = wxSWISS,  // wxSWISS,
    GdiModern = wxMODERN,  // wxMODERN,
    GdiTeletype = wxTELETYPE,  // wxTELETYPE,  /* @@@@ */

    // Proportional or Fixed width fonts (not yet used)
    GdiVariable   = wxVARIABLE  ,  // wxVARIABLE   = 80,
    GdiFixed = wxFIXED,  // wxFIXED,

    GdiNormal     = wxNORMAL    ,  // wxNORMAL     = 90,
    GdiLight = wxLIGHT,  // wxLIGHT,
    GdiBold = wxBOLD,  // wxBOLD,
    // Also wxNORMAL for normal (non-italic text)
    GdiItalic = wxITALIC,  // wxITALIC,
    GdiSlant = wxSLANT,  // wxSLANT,

    // Pen styles
    GdiSolid      = wxSOLID     ,  // wxSOLID      =   100,
    GdiDot = wxDOT,  // wxDOT,
    GdiLongDash = wxLONG_DASH,  // wxLONG_DASH,
    GdiShortDash = wxSHORT_DASH,  // wxSHORT_DASH,
    GdiDotDash = wxDOT_DASH,  // wxDOT_DASH,
    GdiUserDash = wxUSER_DASH,  // wxUSER_DASH,

    GdiTransparent = wxTRANSPARENT,  // wxTRANSPARENT,

    // Brush & Pen Stippling. Note that a stippled pen cannot be dashed!!
    // Note also that stippling a Pen IS meaningfull, because a Line is
    GdiStippleMaskOpaque = wxSTIPPLE_MASK_OPAQUE,  // wxSTIPPLE_MASK_OPAQUE, //mask is used for blitting monochrome using text fore and back ground colors
    GdiStippleMask = wxSTIPPLE_MASK,  // wxSTIPPLE_MASK,        //mask is used for masking areas in the stipple bitmap (TO DO)
    // drawn with a Pen, and without any Brush -- and it can be stippled.
    GdiStipple = wxSTIPPLE,  // wxSTIPPLE =          110,
    GdiBdiagonalHatch = wxBDIAGONAL_HATCH,  // wxBDIAGONAL_HATCH,
    GdiCrossdiagHatch = wxCROSSDIAG_HATCH,  // wxCROSSDIAG_HATCH,
    GdiFdiagonalHatch = wxFDIAGONAL_HATCH,  // wxFDIAGONAL_HATCH,
    GdiCrossHatch = wxCROSS_HATCH,  // wxCROSS_HATCH,
    GdiHorizontalHatch = wxHORIZONTAL_HATCH,  // wxHORIZONTAL_HATCH,
    GdiVerticalHatch = wxVERTICAL_HATCH,  // wxVERTICAL_HATCH,

    GdiJoinBevel = wxJOIN_BEVEL,  // wxJOIN_BEVEL =     120,
    GdiJoinMiter = wxJOIN_MITER,  // wxJOIN_MITER,
    GdiJoinRound = wxJOIN_ROUND,  // wxJOIN_ROUND,

    GdiCapRound = wxCAP_ROUND,  // wxCAP_ROUND =      130,
    GdiCapProjecting = wxCAP_PROJECTING,  // wxCAP_PROJECTING,
    GdiCapButt = wxCAP_BUTT  // wxCAP_BUTT
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, GdiFlags);

ACDK_DECL_CLASS(DC);


/**
  see wxDC
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:34 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC DC
: extends WxObject
{
  // wxDC
  ACDK_WITH_METAINFO(DC)
public:
  // wxDC
  ACDK_WX_STD_MEMBERS(DC, WxObject)

  /* DC is abstract is on gtk abstract
  DC()
  : WxObject(new wxDC(), true)
  {
  }
  */
  //virtual void BeginDrawing() { }
  inline virtual void beginDrawing() { getWx()->BeginDrawing(); }
  //virtual void EndDrawing() { }
  inline virtual void endDrawing() { getWx()->EndDrawing(); }

    // graphic primitives
    // ------------------

  //virtual void DrawObject(wxDrawObject* drawobject);
  // ### @todo inline virtual void drawObject(IN(RDrawObject) drawobject) { getWx()->DrawObject(CLS2WXPTR(drawobject)); }

  //bool FloodFill(wxCoord x, wxCoord y, const wxColour& col, int style = wxFLOOD_SURFACE);
  inline bool floodFill(int x, int y, IN(RColour) col, int style = wxFLOOD_SURFACE) { return getWx()->FloodFill(x, y, CLS2WXREF(col), style); }

  //bool FloodFill(const wxPoint& pt, const wxColour& col, int style = wxFLOOD_SURFACE);
  inline bool floodFill(IN(RPoint) pt, IN(RColour) col, int style = wxFLOOD_SURFACE) { return getWx()->FloodFill(CLS2WXREF(pt), CLS2WXREF(col), style); }

  //bool GetPixel(wxCoord x, wxCoord y, wxColour *col) const { return DoGetPixel(x, y, col); }
  inline bool getPixel(int x, int y, OUT(RColour) col) const
  {
    wxColour wcol;
    bool b = getWx()->GetPixel(x, y, &wcol);
    col = new Colour(wcol);
    return b;
  }

  //bool GetPixel(const wxPoint& pt, wxColour *col) const;
  // ### @todo col is out?
  inline bool getPixel(IN(RPoint) pt, IN(RColour) col) const { return getWx()->GetPixel(CLS2WXREF(pt), CLS2WXPTR(col)); }

  //void DrawLine(wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2);
  inline void drawLine(int x1, int y1, int x2, int y2) { getWx()->DrawLine(x1, y1, x2, y2); }
  //void DrawLine(const wxPoint& pt1, const wxPoint& pt2);
  inline void drawLine(IN(RPoint) pt1, IN(RPoint) pt2) { getWx()->DrawLine(CLS2WXREF(pt1), CLS2WXREF(pt2)); }
  //void CrossHair(wxCoord x, wxCoord y);
  inline void crossHair(int x, int y) { getWx()->CrossHair(x, y); }
  //void CrossHair(const wxPoint& pt);
  inline void crossHair(IN(RPoint) pt) { getWx()->CrossHair(CLS2WXREF(pt)); }

  //void DrawArc(wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2, wxCoord xc, wxCoord yc);
  inline void drawArc(int x1, int y1, int x2, int y2, int xc, int yc) { getWx()->DrawArc(x1, y1, x2, y2, xc, yc); }
  //void DrawArc(const wxPoint& pt1, const wxPoint& pt2, const wxPoint& centre);
  inline void drawArc(IN(RPoint) pt1, IN(RPoint) pt2, IN(RPoint) centre) { getWx()->DrawArc(CLS2WXREF(pt1), CLS2WXREF(pt2), CLS2WXREF(centre)); }
  //void DrawCheckMark(wxCoord x, wxCoord y, wxCoord width, wxCoord height);
  inline void drawCheckMark(int x, int y, int width, int height) { getWx()->DrawCheckMark(x, y, width, height); }
  //void DrawCheckMark(const wxRect& rect);
  inline void drawCheckMark(IN(RRect) rect) { getWx()->DrawCheckMark(CLS2WXREF(rect)); }

  //void DrawEllipticArc(wxCoord x, wxCoord y, wxCoord w, wxCoord h, double sa, double ea);
  inline void drawEllipticArc(int x, int y, int w, int h, double sa, double ea) { getWx()->DrawEllipticArc(x, y, w, h, sa, ea); }
  //void DrawEllipticArc(const wxPoint& pt, const wxSize& sz, double sa, double ea);
  inline void drawEllipticArc(IN(RPoint) pt, IN(RSize) sz, double sa, double ea) { getWx()->DrawEllipticArc(CLS2WXREF(pt), CLS2WXREF(sz), sa, ea); }
  //void DrawPoint(wxCoord x, wxCoord y);
  inline void drawPoint(int x, int y) { getWx()->DrawPoint(x, y); }
  //void DrawPoint(const wxPoint& pt);
  inline void drawPoint(IN(RPoint) pt) { getWx()->DrawPoint(CLS2WXREF(pt)); }

  //void DrawLines(int n, wxPoint points, wxCoord xoffset = 0, wxCoord yoffset = 0);
  inline void drawLines(IN(RPointArray) points, int xoffset = 0, int yoffset = 0)
  {
    acdk::lang::sys::core_vector<wxPoint> pvec(points->length());
    for (int i = 0; i < pvec.size(); ++i)
      pvec[i] = CLS2WXREF(points[i]);
    getWx()->DrawLines(pvec.size(), pvec.data(), xoffset, yoffset);
  }
  // not supported void DrawLines(const wxList *list, wxCoord xoffset = 0, wxCoord yoffset = 0);

  //void DrawPolygon(int n, wxPoint points, wxCoord xoffset = 0, wxCoord yoffset = 0, int fillStyle = wxODDEVEN_RULE);
  inline void drawPolygon(IN(RPointArray) points, int xoffset = 0, int yoffset = 0, int fillStyle = wxODDEVEN_RULE)
  {
    acdk::lang::sys::core_vector<wxPoint> pvec(points->length());
    for (int i = 0; i < pvec.size(); ++i)
      pvec[i] = CLS2WXREF(points[i]);

    getWx()->DrawPolygon(pvec.size(), pvec.data(), xoffset, yoffset, fillStyle);
  }

  // not supported void DrawPolygon(const wxList *list, wxCoord xoffset = 0, wxCoord yoffset = 0, int fillStyle = wxODDEVEN_RULE);
  //void DrawRectangle(wxCoord x, wxCoord y, wxCoord width, wxCoord height);
  inline void drawRectangle(int x, int y, int width, int height) { getWx()->DrawRectangle(x, y, width, height); }
  //void DrawRectangle(const wxPoint& pt, const wxSize& sz);
  inline void drawRectangle(IN(RPoint) pt, IN(RSize) sz) { getWx()->DrawRectangle(CLS2WXREF(pt), CLS2WXREF(sz)); }
  //void DrawRectangle(const wxRect& rect);
  inline void drawRectangle(IN(RRect) rect) { getWx()->DrawRectangle(CLS2WXREF(rect)); }
  //void DrawRoundedRectangle(wxCoord x, wxCoord y, wxCoord width, wxCoord height, double radius);
  inline void drawRoundedRectangle(int x, int y, int width, int height, double radius) { getWx()->DrawRoundedRectangle(x, y, width, height, radius); }
  //void DrawRoundedRectangle(const wxPoint& pt, const wxSize& sz, double radius);
  inline void drawRoundedRectangle(IN(RPoint) pt, IN(RSize) sz, double radius) { getWx()->DrawRoundedRectangle(CLS2WXREF(pt), CLS2WXREF(sz), radius); }
  //void DrawRoundedRectangle(const wxRect& r, double radius);
  inline void drawRoundedRectangle(IN(RRect) r, double radius) { getWx()->DrawRoundedRectangle(CLS2WXREF(r), radius); }

  //void DrawCircle(wxCoord x, wxCoord y, wxCoord radius);
  inline void drawCircle(int x, int y, int radius) { getWx()->DrawCircle(x, y, radius); }
  //void DrawCircle(const wxPoint& pt, wxCoord radius);
  inline void drawCircle(IN(RPoint) pt, int radius) { getWx()->DrawCircle(CLS2WXREF(pt), radius); }
  //void DrawEllipse(wxCoord x, wxCoord y, wxCoord width, wxCoord height);
  inline void drawEllipse(int x, int y, int width, int height) { getWx()->DrawEllipse(x, y, width, height); }
  //void DrawEllipse(const wxPoint& pt, const wxSize& sz);
  inline void drawEllipse(IN(RPoint) pt, IN(RSize) sz) { getWx()->DrawEllipse(CLS2WXREF(pt), CLS2WXREF(sz)); }
  //void DrawEllipse(const wxRect& rect);
  inline void drawEllipse(IN(RRect) rect) { getWx()->DrawEllipse(CLS2WXREF(rect)); }

  //void DrawIcon(const wxIcon& icon, wxCoord x, wxCoord y);
  inline void drawIcon(IN(RIcon) icon, int x, int y) { getWx()->DrawIcon(CLS2WXREF(icon), x, y); }
  //void DrawIcon(const wxIcon& icon, const wxPoint& pt);
  inline void drawIcon(IN(RIcon) icon, IN(RPoint) pt) { getWx()->DrawIcon(CLS2WXREF(icon), CLS2WXREF(pt)); }

  //void DrawBitmap(const wxBitmap &bmp, wxCoord x, wxCoord y, bool useMask = FALSE);
  inline void drawBitmap(IN(RBitmap) bmp, int x, int y, bool useMask = false) { getWx()->DrawBitmap(CLS2WXREF(bmp), x, y, useMask); }
  //void DrawBitmap(const wxBitmap &bmp, const wxPoint& pt, bool useMask = FALSE);
  inline void drawBitmap(IN(RBitmap) bmp, IN(RPoint) pt, bool useMask = false) { getWx()->DrawBitmap(CLS2WXREF(bmp), CLS2WXREF(pt), useMask); }

  //void DrawText(const wxString& text, wxCoord x, wxCoord y);
  inline void drawText(IN(RString)  text, int x, int y) { getWx()->DrawText(S2WXS(text), x, y); }
  //void DrawText(const wxString& text, const wxPoint& pt);
  inline void drawText(IN(RString)  text, IN(RPoint) pt) { getWx()->DrawText(S2WXS(text), CLS2WXREF(pt)); }

  //void DrawRotatedText(const wxString& text, wxCoord x, wxCoord y, double angle);
  inline void drawRotatedText(IN(RString)  text, int x, int y, double angle) { getWx()->DrawRotatedText(S2WXS(text), x, y, angle); }
  //void DrawRotatedText(const wxString& text, const wxPoint& pt, double angle);
  inline void drawRotatedText(IN(RString)  text, IN(RPoint) pt, double angle) { getWx()->DrawRotatedText(S2WXS(text), CLS2WXREF(pt), angle); }

  // this version puts both optional bitmap and the text into the given
  // rectangle and aligns is as specified by alignment parameter; it also
  // will emphasize the character with the given index if it is != -1 and
  // return the bounding rectangle if required
  //virtual void DrawLabel(const wxString& text, const wxBitmap& image, const wxRect& rect, int alignment = wxALIGN_LEFT | wxALIGN_TOP, int indexAccel = -1, wxRect* rectBounding = NULL);
  inline virtual void drawLabel(IN(RString)  text, IN(RBitmap) image, IN(RRect) rect, int alignment = wxALIGN_LEFT | wxALIGN_TOP, int indexAccel = -1)
  {
    getWx()->DrawLabel(S2WXS(text), CLS2WXREF(image), CLS2WXREF(rect), alignment, indexAccel, NULL);
  }
  //void DrawLabel(const wxString& text, const wxRect& rect, int alignment = wxALIGN_LEFT | wxALIGN_TOP, int indexAccel = -1);
  inline void drawLabel(IN(RString)  text, IN(RRect) rect, int alignment = wxALIGN_LEFT | wxALIGN_TOP, int indexAccel = -1)
  {
    getWx()->DrawLabel(S2WXS(text), CLS2WXREF(rect), alignment, indexAccel);
  }

  //bool Blit(wxCoord xdest, wxCoord ydest, wxCoord width, wxCoord height, wxDC *source, wxCoord xsrc, wxCoord ysrc, int rop = wxCOPY, bool useMask = FALSE, wxCoord xsrcMask = -1, wxCoord ysrcMask = -1);
  inline bool blit(int xdest, int ydest, int width, int height, IN(RDC) source, int xsrc, int ysrc, int rop = wxCOPY, bool useMask = false, int xsrcMask = -1, int ysrcMask = -1)
  {
    return getWx()->Blit(xdest, ydest, width, height, CLS2WXPTR(source), xsrc, ysrc, rop, useMask, xsrcMask, ysrcMask);
  }
  //bool Blit(const wxPoint& destPt, const wxSize& sz, wxDC *source, const wxPoint& srcPt, int rop = wxCOPY, bool useMask = FALSE, const wxPoint& srcPtMask = x);
  inline bool blit(IN(RPoint) destPt, IN(RSize) sz, IN(RDC) source, IN(RPoint) srcPt, int rop = wxCOPY, bool useMask = false, IN(RPoint) srcPtMask = RPoint(new Point(-1, -1)))
  { return getWx()->Blit(CLS2WXREF(destPt), CLS2WXREF(sz), CLS2WXPTR(source), CLS2WXREF(srcPt), rop, useMask, CLS2WXREF(srcPtMask)); }

  // TODO: this API needs fixing (wxPointList, why (!const) "wxList *"?)
  //void DrawSpline(wxCoord x1, wxCoord y1, wxCoord x2, wxCoord y2, wxCoord x3, wxCoord y3);
  inline void drawSpline(int x1, int y1, int x2, int y2, int x3, int y3) { getWx()->DrawSpline(x1, y1, x2, y2, x3, y3); }
  //void DrawSpline(int n, wxPoint points[]);
  inline void drawSpline(IN(RPointArray) points)
  {
    acdk::lang::sys::core_vector<wxPoint> pvec(points->length());
    for (int i = 0; i < pvec.size(); ++i)
      pvec[i] = CLS2WXREF(points[i]);
    getWx()->DrawSpline(pvec.size(), pvec.data());
  }

  //void DrawSpline(wxList *points) { DoDrawSpline(points); }

  // global DC operations
  // --------------------

  //virtual void Clear() = 0;
  inline virtual void clear() { getWx()->Clear(); }

  //virtual bool StartDoc(const wxString& message) { return TRUE; }
  inline virtual bool startDoc(IN(RString)  message) { return getWx()->StartDoc(S2WXS(message)); }
  //virtual void EndDoc() { }
  inline virtual void endDoc() { getWx()->EndDoc(); }

  //virtual void StartPage() { }
  inline virtual void startPage() { getWx()->StartPage(); }
  //virtual void EndPage() { }
  inline virtual void endPage() { getWx()->EndPage(); }

  // set objects to use for drawing
  // ------------------------------

  //virtual void SetFont(const wxFont& font) = 0;
  inline virtual void setFont(IN(RFont) font) { getWx()->SetFont(CLS2WXREF(font)); }
  //virtual void SetPen(const wxPen& pen) = 0;
  inline virtual void setPen(IN(RPen) pen) { getWx()->SetPen(CLS2WXREF(pen)); }
  //virtual void SetBrush(const wxBrush& brush) = 0;
  inline virtual void setBrush(IN(RBrush) brush) { getWx()->SetBrush(CLS2WXREF(brush)); }
  //virtual void SetBackground(const wxBrush& brush) = 0;
  inline virtual void setBackground(IN(RBrush) brush) { getWx()->SetBackground(CLS2WXREF(brush)); }
  //virtual void SetBackgroundMode(int mode) = 0;
  inline virtual void setBackgroundMode(int mode) { getWx()->SetBackgroundMode(mode); }
  //virtual void SetPalette(const wxPalette& palette) = 0;
  // ### @todo inline virtual void setPalette(IN(RPalette) palette) { getWx()->SetPalette(CLS2WXREF(palette)); }

  // clipping region
  // ---------------

  //void SetClippingRegion(wxCoord x, wxCoord y, wxCoord width, wxCoord height);
  inline void setClippingRegion(int x, int y, int width, int height) { getWx()->SetClippingRegion(x, y, width, height); }
  //void SetClippingRegion(const wxPoint& pt, const wxSize& sz);
  inline void setClippingRegion(IN(RPoint) pt, IN(RSize) sz) { getWx()->SetClippingRegion(CLS2WXREF(pt), CLS2WXREF(sz)); }
  //void SetClippingRegion(const wxRect& rect);
  inline void setClippingRegion(IN(RRect) rect) { getWx()->SetClippingRegion(CLS2WXREF(rect)); }
  //void SetClippingRegion(const wxRegion& region);
  inline void setClippingRegion(IN(RRegion) region) { getWx()->SetClippingRegion(CLS2WXREF(region)); }

  //virtual void DestroyClippingRegion() = 0;
  inline virtual void destroyClippingRegion() { getWx()->DestroyClippingRegion(); }

  //void GetClippingBox(wxCoord *x, wxCoord *y, wxCoord *w, wxCoord *h) const;
  inline void getClippingBox(OUT(int) x, OUT(int) y, OUT(int) w, OUT(int) h) const { getWx()->GetClippingBox(&x, &y, &w, &h); }

  //void GetClippingBox(wxRect& rect) const;
  inline void getClippingBox(IN(RRect) rect) const { getWx()->GetClippingBox(CLS2WXREF(rect)); }

  // text extent
  // -----------

  //virtual wxCoord GetCharHeight() const = 0;
  inline int getCharHeight() const { return getWx()->GetCharHeight(); }
  inline int getCharWidth() const { return getWx()->GetCharWidth(); }

  // only works for single line strings
  // ### @todo void GetTextExtent(const wxString& string, wxCoord *x, wxCoord *y, wxCoord *descent = NULL, wxCoord *externalLeading = NULL, wxFont *theFont = NULL) const;

  // works for single as well as multi-line strings
  // ### @todo virtual void GetMultiLineTextExtent(const wxString& text, wxCoord *width, wxCoord *height, wxCoord *heightLine = NULL, wxFont *font = NULL);

  // size and resolution
  // -------------------

  // in device units
  //void GetSize(int width, int height) const;
  inline void getSize(OUT(int) width, OUT(int) height) const { getWx()->GetSize(&width, &height); }
  //wxSize GetSize() const;
  inline RSize getSize() const { return WXVAL2CLS(Size, getWx()->GetSize()); }

  // in mm
  //void GetSizeMM(int width, int height) const;
  inline void getSizeMM(OUT(int) width, OUT(int) height) const { getWx()->GetSizeMM(&width, &height); }
  //wxSize GetSizeMM() const;
  inline RSize getSizeMM() const { return WXVAL2CLS(Size, getWx()->GetSizeMM()); }

  // coordinates conversions
  // -----------------------

  // This group of functions does actual conversion of the input, as you'd
  // expect.
  //wxCoord DeviceToLogicalX(wxCoord x) const;
  inline int deviceToLogicalX(int x) const { return getWx()->DeviceToLogicalX(x); }
  //wxCoord DeviceToLogicalY(wxCoord y) const;
  inline int deviceToLogicalY(int y) const { return getWx()->DeviceToLogicalY(y); }
  //wxCoord DeviceToLogicalXRel(wxCoord x) const;
  inline int deviceToLogicalXRel(int x) const { return getWx()->DeviceToLogicalXRel(x); }
  //wxCoord DeviceToLogicalYRel(wxCoord y) const;
  inline int deviceToLogicalYRel(int y) const { return getWx()->DeviceToLogicalYRel(y); }
  //wxCoord LogicalToDeviceX(wxCoord x) const;
  inline int logicalToDeviceX(int x) const { return getWx()->LogicalToDeviceX(x); }
  //wxCoord LogicalToDeviceY(wxCoord y) const;
  inline int logicalToDeviceY(int y) const { return getWx()->LogicalToDeviceY(y); }
  //wxCoord LogicalToDeviceXRel(wxCoord x) const;
  inline int logicalToDeviceXRel(int x) const { return getWx()->LogicalToDeviceXRel(x); }
  //wxCoord LogicalToDeviceYRel(wxCoord y) const;
  inline int logicalToDeviceYRel(int y) const { return getWx()->LogicalToDeviceYRel(y); }

  // query DC capabilities
  // ---------------------

  //bool CanDrawBitmap() const = 0;
  inline bool canDrawBitmap() const { return getWx()->CanDrawBitmap(); }
  //bool CanGetTextExtent() const = 0;
  inline bool canGetTextExtent() const { return getWx()->CanGetTextExtent(); }

  // colour depth
  //virtual int GetDepth() const = 0;
  inline int getDepth() const { return getWx()->GetDepth(); }

  // Resolution in Pixels per inch
  //wxSize GetPPI() const = 0;
  inline RSize getPPI() const { return WXVAL2CLS(Size, getWx()->GetPPI()); }

  //bool Ok() const { return m_ok; }
  inline bool ok() const { return getWx()->Ok(); }

  // accessors
  // ---------

  // const...
  //int GetBackgroundMode() const { return m_backgroundMode; }
  inline int getBackgroundMode() const { return getWx()->GetBackgroundMode(); }
  //const wxBrush&  GetBackground() const { return m_backgroundBrush; }
  inline RBrush getBackground() const { return WXVAL2CLS(Brush, getWx()->GetBackground()); }
  //const wxBrush&  GetBrush() const { return m_brush; }
  inline RBrush getBrush() const { return WXVAL2CLS(Brush, getWx()->GetBrush()); }
  //const wxFont&   GetFont() const { return m_font; }
  inline RFont getFont() const { return WXVAL2CLS(Font, getWx()->GetFont()); }
  //const wxPen&    GetPen() const { return m_pen; }
  inline RPen getPen() const { return WXVAL2CLS(Pen, getWx()->GetPen()); }
  //const wxColour& GetTextBackground() const { return m_textBackgroundColour; }
  inline RColour getTextBackground() const { return WXVAL2CLS(Colour, getWx()->GetTextBackground()); }
  //const wxColour& GetTextForeground() const { return m_textForegroundColour; }
  inline RColour getTextForeground() const { return WXVAL2CLS(Colour, getWx()->GetTextForeground()); }
  /*
  // ... and non const
  wxBrush&  GetBackground() { return m_backgroundBrush; }
  wxBrush&  GetBrush() { return m_brush; }
  wxFont&   GetFont() { return m_font; }
  wxPen&    GetPen() { return m_pen; }
  wxColour& GetTextBackground() { return m_textBackgroundColour; }
  wxColour& GetTextForeground() { return m_textForegroundColour; }
  */

  //void SetTextForeground(const wxColour& colour);
  inline void setTextForeground(IN(RColour) colour) { getWx()->SetTextForeground(CLS2WXREF(colour)); }
  //void SetTextBackground(const wxColour& colour);
  inline void setTextBackground(IN(RColour) colour) { getWx()->SetTextBackground(CLS2WXREF(colour)); }

  //int GetMapMode() const { return m_mappingMode; }
  inline int getMapMode() const { return getWx()->GetMapMode(); }
  //virtual void SetMapMode(int mode) = 0;
  inline virtual void setMapMode(int mode) { getWx()->SetMapMode(mode); }

  //virtual void GetUserScale(double x, double y) const;
  inline virtual void getUserScale(OUT(double) x, OUT(double) y) const { getWx()->GetUserScale(&x, &y); }
  //virtual void SetUserScale(double x, double y) = 0;
  inline virtual void setUserScale(double x, double y) { getWx()->SetUserScale(x, y); }

  //virtual void GetLogicalScale(double x, double y);
  inline virtual void getLogicalScale(OUT(double) x, OUT(double) y) { getWx()->GetLogicalScale(&x, &y); }
  //virtual void SetLogicalScale(double x, double y);
  inline virtual void setLogicalScale(double x, double y) { getWx()->SetLogicalScale(x, y); }

  //void GetLogicalOrigin(wxCoord x, wxCoord y) const;
  inline void getLogicalOrigin(OUT(int) x, OUT(int) y) const { getWx()->GetLogicalOrigin(&x, &y); }
  //wxPoint GetLogicalOrigin() const;
  inline RPoint getLogicalOrigin() const { return WXVAL2CLS(Point, getWx()->GetLogicalOrigin()); }
  //virtual void SetLogicalOrigin(wxCoord x, wxCoord y) = 0;
  inline virtual void setLogicalOrigin(int x, int y) { getWx()->SetLogicalOrigin(x, y); }

  //void GetDeviceOrigin(wxCoord x, wxCoord y) const;
  inline void getDeviceOrigin(OUT(int) x, OUT(int) y) const { getWx()->GetDeviceOrigin(&x, &y); }
  //wxPoint GetDeviceOrigin() const;
  inline RPoint getDeviceOrigin() const { return WXVAL2CLS(Point, getWx()->GetDeviceOrigin()); }
  //virtual void SetDeviceOrigin(wxCoord x, wxCoord y) = 0;
  inline virtual void setDeviceOrigin(int x, int y) { getWx()->SetDeviceOrigin(x, y); }

  //virtual void SetAxisOrientation(bool xLeftRight, bool yBottomUp) = 0;
  inline virtual void setAxisOrientation(bool xLeftRight, bool yBottomUp) { getWx()->SetAxisOrientation(xLeftRight, yBottomUp); }

  //int GetLogicalFunction() const { return m_logicalFunction; }
  inline int getLogicalFunction() const { return getWx()->GetLogicalFunction(); }
  //virtual void SetLogicalFunction(int function) = 0;
  inline virtual void setLogicalFunction(int function) { getWx()->SetLogicalFunction(function); }

  // Sometimes we need to override optimization, e.g. if other software is
  // drawing onto our surface and we can't be sure of who's done what.
  //

  // Some platforms have a DC cache, which should be cleared
  // at appropriate points such as after a series of DC operations.
  // Put ClearCache in the wxDC implementation class, since it has to be
  // static.
  // static void ClearCache() ;

  // bounding box
  // ------------

  //void CalcBoundingBox(wxCoord x, wxCoord y);
  inline void calcBoundingBox(int x, int y) { getWx()->CalcBoundingBox(x, y); }

  //void ResetBoundingBox();
  inline void resetBoundingBox() { getWx()->ResetBoundingBox(); }

  // Get the final bounding box of the PostScript or Metafile picture.
  //wxCoord MinX() const { return m_minX; }
  inline int minX() const { return getWx()->MinX(); }
  //wxCoord MaxX() const { return m_maxX; }
  inline int maxX() const { return getWx()->MaxX(); }
  //wxCoord MinY() const { return m_minY; }
  inline int minY() const { return getWx()->MinY(); }
  //wxCoord MaxY() const { return m_maxY; }
  inline int maxY() const { return getWx()->MaxY(); }


};


ACDK_DECL_CLASS(PaintEvent);
/*
 wxEVT_PAINT
 wxEVT_NC_PAINT
 wxEVT_PAINT_ICON
*/
/**
  see wxPaintEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:34 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC PaintEvent
: extends Event
{
  ACDK_WITH_METAINFO(PaintEvent)
public:
  // wxPaintEvent
  ACDK_WX_STD_EVENT_MEMBERS(PaintEvent, Event)
  PaintEvent(int id = 0)
    : Event(new wxPaintEvent(id))
  {
  }
  inline virtual RObject clone() const { RETURN_WXPTR2CLS(PaintEvent, (wxPaintEvent*)getWx()->Clone()); }

  static int EvtPaint;
  static int EvtNcPaint;
  static int EvtPaintIcon;

};


ACDK_DECL_CLASS(EraseEvent);

/**
  see wxEraseEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:34 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC EraseEvent
: extends Event
{
  ACDK_WITH_METAINFO(EraseEvent)
public:
  // wxEraseEvent
  ACDK_WX_STD_EVENT_MEMBERS(EraseEvent, Event)

  EraseEvent(int id = 0, IN(RDC) dc = Nil)
  : Event(new wxEraseEvent(id, CLS2WXPTR(dc)))
  {
  }
  //wxDC* GetDC() const
  inline RDC getDC() const { RETURN_WXPTR2CLS(DC, getWx()->GetDC()); }
  static int EvtEraseBackground;

};

} // wx
} // acdk

#endif //acdk_wx_DC_h
