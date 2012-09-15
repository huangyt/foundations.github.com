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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/StatusBar.h,v 1.5 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_StatusBar_h
#define acdk_wx_StatusBar_h

#include "Control.h"

namespace acdk {
namespace wx {

  
/**
  see StatusBar
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum StatusBarStyle
{
  StSizegrip         = wxST_SIZEGRIP        ,  // wxST_SIZEGRIP         0x0010
/*
 * wxStaticText flags
 */
  StNoAutoresize    = wxST_NO_AUTORESIZE   ,  // wxST_NO_AUTORESIZE    0x0001
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, StatusBarStyle);

ACDK_DECL_CLASS(StatusBar);
/**
  see wxStatusBar
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC StatusBar
: extends Window
{
  ACDK_WITH_METAINFO(StatusBar)
public:
  // wxStatusBar
  ACDK_WX_STD_MEMBERS(StatusBar, Window)
  StatusBar(IN(RWindow) parent, int id, int style = 0)
  : Window(new wxStatusBar(CLS2WXPTR(parent), id, style), parent == Nil)
  {
  }
  
  //virtual void SetFieldsCount(int number = 1, const int *widths = NULL);
  inline virtual void setFieldsCount(int number = 1, IN(RintArray) widths = Nil) 
  { 
    if (widths == Nil)
      getWx()->SetFieldsCount(number); 
    getWx()->SetFieldsCount(number, widths->data()); 
  }
  //int GetFieldsCount() const { return m_nFields; }
  inline int getFieldsCount() const { return getWx()->GetFieldsCount(); }

    // field text
    // ----------

    //virtual void SetStatusText(const wxString& text, int number = 0);
  inline virtual void setStatusText(IN(RString)  text, int number = 0) { getWx()->SetStatusText(S2WXS(text), number); }
    //virtual wxString GetStatusText(int number = 0) const = 0;
  inline virtual RString getStatusText(int number = 0) const { return WXS2S(getWx()->GetStatusText(number)); }

    //void PushStatusText(const wxString& text, int number = 0);
  inline void pushStatusText(IN(RString)  text, int number = 0) { getWx()->PushStatusText(S2WXS(text), number); }
    //void PopStatusText(int number = 0);
  inline void popStatusText(int number = 0) { getWx()->PopStatusText(number); }

    // fields widths
    // -------------

    // set status field widths as absolute numbers: positive widths mean that
    // the field has the specified absolute width, negative widths are
    // interpreted as the sizer options, i.e. the extra space (total space
    // minus the sum of fixed width fields) is divided between the fields with
    // negative width according to the abs value of the width (field with width
    // -2 grows twice as much as one with width -1 &c)
    //virtual void SetStatusWidths(int n, const int widths[]);
    //virtual void SetStatusWidths(int n, const int widths);
  inline virtual void setStatusWidths(int n, IN(RintArray) widths) 
  { 
    getWx()->SetStatusWidths(n, widths->data()); 
  }
    // geometry
    // --------

    // Get the position and size of the field's internal bounding rectangle
    //virtual bool GetFieldRect(int i, wxRect& rect) const = 0;
  inline virtual bool getFieldRect(int i, OUT(RRect) rect) const 
  { 
    wxRect wrect;
    bool ret = getWx()->GetFieldRect(i, wrect); 
    rect = new Rect(wrect);
    return ret;
  }

    // sets the minimal vertical size of the status bar
    //virtual void SetMinHeight(int height) = 0;
  inline virtual void setMinHeight(int height) { getWx()->SetMinHeight(height); }

    // get the dimensions of the horizontal and vertical borders
    //virtual int GetBorderX() const = 0;
  inline virtual int getBorderX() const { return getWx()->GetBorderX(); }
    //virtual int GetBorderY() const = 0;
  inline virtual int getBorderY() const { return getWx()->GetBorderY(); }

    // don't want status bars to accept the focus at all
    //virtual bool AcceptsFocus() const { return FALSE; }
  inline virtual bool acceptsFocus() const { return getWx()->AcceptsFocus(); }
};


} // wx
} // acdk

#endif //acdk_wx_StatusBar_h
