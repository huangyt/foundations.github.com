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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Pen.h,v 1.11 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Pen_h
#define acdk_wx_Pen_h

#include "Bitmap.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(Pen);
/**
  see wxPen
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Pen
: extends GDIObject
{
  ACDK_WITH_METAINFO(Pen)
public:
  ACDK_WX_STD_VAL_MEMBERS(Pen, GDIObject)
  Pen() : GDIObject(new wxPen(), true) {}
  Pen(IN(RColour) col, int width, int style)
    : GDIObject(new wxPen(CLS2WXREF(col), width, style), true) {}

  Pen(IN(RBitmap) stipple, int width) 
#if defined(ACDK_OS_WIN32)
      : GDIObject(new wxPen(CLS2WXREF(stipple), width), true) 
#endif
	{}
  //foreign Pen(const wxPen& other) : GDIObject(new wxPen(other)) {}

  //virtual bool Ok() const { return (m_refData != NULL) ; }
  inline virtual bool ok() const { return getWx()->Ok(); }

  // Override in order to recreate the pen
  //void SetColour(const wxColour& col) ;
  inline void setColour(IN(RColour) col) { getWx()->SetColour(CLS2WXREF(col)); }
  //void SetColour(unsigned char r, unsigned char g, unsigned char b);
  inline void setColour(char r, char g, char b) { getWx()->SetColour(r, g, b); }

  //void SetWidth(int width)  ;
  inline void setWidth(int width) { getWx()->SetWidth(width); }
  //void SetStyle(int style)  ;
  inline void setStyle(int style) { getWx()->SetStyle(style); }
  //void SetStipple(const wxBitmap& stipple)  ;
  // ### @todo not supported on Gtk? inline void setStipple(IN(RBitmap) stipple) { getWx()->SetStipple(CLS2WXREF(stipple)); } // wxBitmap
  //void SetDashes(int nb_dashes, const wxDash *dash)  ;
  // ### @todo implement inline void setDashes(int nb_dashes, IN(RDash) dash) { getWx()->SetDashes(nb_dashes, CLS2WXPTR(dash)); }
  //void SetJoin(int join)  ;
  inline void setJoin(int join) { getWx()->SetJoin(join); }
  //void SetCap(int cap)  ;
  inline void setCap(int cap) { getWx()->SetCap(cap); }

  //inline wxColour& GetColour() const { return (M_PENDATA ? M_PENDATA->m_colour : wxNullColour); };
  // ### @todo not supported on gtk inline RColour getColour() const { return WXVAL2CLS(wxColor, getWx()->GetColour()); }
  //inline int GetWidth() const { return (M_PENDATA ? M_PENDATA->m_width : 0); };
  inline int getWidth() const { return getWx()->GetWidth(); }
  //inline int GetStyle() const { return (M_PENDATA ? M_PENDATA->m_style : 0); };
  inline int getStyle() const { return getWx()->GetStyle(); }
  //inline int GetJoin() const { return (M_PENDATA ? M_PENDATA->m_join : 0); };
  inline int getJoin() const { return getWx()->GetJoin(); }
  //inline int GetCap() const { return (M_PENDATA ? M_PENDATA->m_cap : 0); };
  inline int getCap() const { return getWx()->GetCap(); }
  /** ??
  inline int GetDashes(wxDash **ptr) const
  {
    *ptr = (M_PENDATA ? (wxDash*)M_PENDATA->m_dash : (wxDash*) NULL);
    return (M_PENDATA ? M_PENDATA->m_nbDash : 0);
  }
  */
  //inline wxBitmap *GetStipple() const { return (M_PENDATA ? (& M_PENDATA->m_stipple) : (wxBitmap*) NULL); };
  // ### @todo not supported on gtk inline RBitmap getStipple() const { RETURN_WXPTR2CLS(Bitmap, getWx()->GetStipple()); }

  // Internal
    /** not supported on gtk
  //bool RealizeResource();
  inline bool realizeResource() { return getWx()->RealizeResource(); }
  //WXHANDLE GetResourceHandle() const;
  //bool FreeResource(bool force = FALSE);
  inline bool freeResource(bool force = false) { return getWx()->FreeResource(force); }
  //bool IsFree() const;
  // ### not supported on GTK? inline bool isFree() const { return getWx()->IsFree(); }
  //void Unshare();
  // ### not supported on GTK? inline void unshare() { getWx()->Unshare(); }
  */
  static RPen getRedPen() { return new Pen(*wxRED_PEN); }
  static RPen getCyanPen() { return new Pen(*wxCYAN_PEN); }
  static RPen getGreenPen() { return new Pen(*wxGREEN_PEN); }
  static RPen getBlackPen() { return new Pen(*wxBLACK_PEN); }
  static RPen getWhitePen() { return new Pen(*wxWHITE_PEN); }
  static RPen getTransparentPen() { return new Pen(*wxTRANSPARENT_PEN); }
  static RPen getBlackDashedPen() { return new Pen(*wxBLACK_DASHED_PEN); }
  static RPen getGreyPen() { return new Pen(*wxGREY_PEN); }
  static RPen getMediumGreyPen() { return new Pen(*wxMEDIUM_GREY_PEN); }
  static RPen getLightGreyPen() { return new Pen(*wxLIGHT_GREY_PEN); }
  static RPen getNullPen() { return new Pen(wxNullPen); }

};

ACDK_DECL_CLASS(Brush);
/**
  see wxBrush
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Brush
: extends GDIObject
{
  ACDK_WITH_METAINFO(Brush)
public:
  ACDK_WX_STD_VAL_MEMBERS(Brush, GDIObject)
  Brush() : GDIObject(new wxBrush(), true) {}
  Brush(IN(RColour) col, int style) : GDIObject(new wxBrush(CLS2WXREF(col), style), true) {}
  Brush(IN(RBitmap) stipple) : GDIObject(new wxBrush(CLS2WXREF(stipple)), true) {}
  Brush(IN(RBrush) brush) : GDIObject(new wxBrush(CLS2WXREF(brush)), true) {}
  //virtual void SetColour(const wxColour& col);
  inline virtual void setColour(IN(RColour) col) { getWx()->SetColour(CLS2WXREF(col)); }
    //virtual void SetColour(unsigned char r, unsigned char g, unsigned char b);
  inline virtual void setColour(char r, char g, char b) { getWx()->SetColour(r, g, b); }
    //virtual void SetStyle(int style);
  inline virtual void setStyle(int style) { getWx()->SetStyle(style); }
    //virtual void SetStipple(const wxBitmap& stipple);
  inline virtual void setStipple(IN(RBitmap) stipple) { getWx()->SetStipple(CLS2WXREF(stipple)); }

  //wxColour GetColour() const;
  inline RColour getColour() const { return WXVAL2CLS(Colour, getWx()->GetColour()); }
  //int GetStyle() const;
  inline int getStyle() const { return getWx()->GetStyle(); }
  //wxBitmap *GetStipple() const;
  inline RBitmap getStipple() const 
  { 
    wxBitmap* wxb = getWx()->GetStipple();
    if (wxb == 0)
      return Nil;
    return WXVAL2CLS(Bitmap, *wxb); 
  }

    //bool Ok() const { return m_refData != NULL; }
  inline bool ok() const { return getWx()->Ok(); }

  static RBrush getNullBrush() { return new Brush(wxNullBrush); }
  static RBrush getBlueBrush() { return new Brush(*wxBLUE_BRUSH); }
  static RBrush getGreenBrush() { return new Brush(*wxGREEN_BRUSH); }
  static RBrush getWhiteBrush() { return new Brush(*wxWHITE_BRUSH); }
  static RBrush getBlackBrush() { return new Brush(*wxBLACK_BRUSH); }
  static RBrush getGreyBrush() { return new Brush(*wxGREY_BRUSH); }
  static RBrush getMediumGreyBrush() { return new Brush(*wxMEDIUM_GREY_BRUSH); }
  static RBrush getLightGreyBrush() { return new Brush(*wxLIGHT_GREY_BRUSH); }
  static RBrush getTransparentBrush() { return new Brush(*wxTRANSPARENT_BRUSH); }
  static RBrush getCyanBrush() { return new Brush(*wxCYAN_BRUSH); }
  static RBrush getRedBrush() { return new Brush(*wxRED_BRUSH); }
};

} // wx
} // acdk

#endif //acdk_wx_Pen_h
