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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/RadioBox.h,v 1.7 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_RadioBox_h
#define acdk_wx_RadioBox_h

#include "Choice.h"

namespace acdk {
namespace wx {

enum Direction;

/**
  see RadioBox
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum RadioBoxStyle
{
  RaLefttoright    = wxRA_LEFTTORIGHT   ,  // wxRA_LEFTTORIGHT    0x0001
  RaToptobottom    = wxRA_TOPTOBOTTOM   ,  // wxRA_TOPTOBOTTOM    0x0002

// New, more intuitive names to specify majorDim argument
  RaSpecifyCols   = wxRA_SPECIFY_COLS  ,  // wxRA_SPECIFY_COLS   wxHORIZONTAL
  RaSpecifyRows   = wxRA_SPECIFY_ROWS  ,  // wxRA_SPECIFY_ROWS   wxVERTICAL

// Old names for compatibility
  RaHorizontal     = wxRA_HORIZONTAL    ,  // wxRA_HORIZONTAL     wxHORIZONTAL
  RaVertical       = wxRA_VERTICAL,        // wxRA_VERTICAL       wxVERTICAL
  RbGroup          = wxRB_GROUP         ,  // wxRB_GROUP          0x0004
  RbSingle         = wxRB_SINGLE          // wxRB_SINGLE         0x0008
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, RadioBoxStyle);

ACDK_DECL_CLASS(RadioBox);

const int MaxRadioBoxElements = 32;

/**
  see wxRadioBox
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC RadioBox
: extends Control
{
  ACDK_WITH_METAINFO(RadioBox)
private:
  
  static inline int getLengthFromStringArray(IN(RStringArray) sa)
  {
    if (sa == Nil)
      return 0;
    return sa->length() > MaxRadioBoxElements ? MaxRadioBoxElements : sa->length();
  }
  foreign static inline wxString* getWxStringFromStringArray(IN(RStringArray) sa)
  {
    if (sa == Nil)
      return 0;
    static wxString strings[MaxRadioBoxElements];
    for (int i = 0; i < MaxRadioBoxElements && i < sa->length(); ++i)
      strings[i] = S2WXS(sa[i]);
    return strings;
  }
public:
  // wxRadioBox
  ACDK_WX_STD_MEMBERS(RadioBox, Control)

  RadioBox(IN(RWindow) parent, int id, IN(RString) title, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), 
            int majorDim = 0,
            int style = OrientHorizontal,
            IN(RStringArray) choices = Nil,
            IN(RValidator) validator = Validator::defaultValidator(),

            IN(RString) name = "RadioBox")
  : Control(new wxRadioBox(CLS2WXPTR(parent), id, S2WXS(title), CLS2WXREF(pos), CLS2WXREF(size), 
                           getLengthFromStringArray(choices), getWxStringFromStringArray(choices), majorDim, style, CLS2WXREF(validator), S2WXS(name)), parent == Nil)
  {
  }
  
  //void SetSelection(int n);
  inline void setSelection(int n) { getWx()->SetSelection(n); }
  //int GetSelection() const;
  inline int getSelection() const { return getWx()->GetSelection(); }
  //wxString GetStringSelection() const;
  inline RString getStringSelection() const { return WXS2S(getWx()->GetStringSelection()); }
  //bool SetStringSelection(const wxString& s);
  inline bool setStringSelection(IN(RString)  s) { return getWx()->SetStringSelection(S2WXS(s)); }
  
    // string access
  //int GetCount() const = 0;
  inline int getCount() const { return getWx()->GetCount(); }
  //int FindString(const wxString& s) const;
  inline int findString(IN(RString)  s) const { return getWx()->FindString(S2WXS(s)); }
  
  //wxString GetString(int n) const = 0;
  inline RString getString(int n) const { return WXS2S(getWx()->GetString(n)); }
  //void SetString(int n, const wxString& label) = 0;
  inline void setString(int n, IN(RString)  label) { getWx()->SetString(n, S2WXS(label)); }
  
  // change the individual radio button state
  //void Enable(int n, bool enable = TRUE) = 0;
  inline void enable(int n, bool enable = true) { getWx()->Enable(n, enable); }
  //void Show(int n, bool show = TRUE) = 0;
  inline void show(int n, bool show = true) { getWx()->Show(n, show); }
  
  // layout parameters
  //int GetColumnCount() const = 0;
  //inline int getColumnCount() const { return getWx()->GetColumnCount(); }
  //int GetRowCount() const = 0;
  //inline int getRowCount() const { return getWx()->GetRowCount(); }
  
  // return the item above/below/to the left/right of the given one
  //int GetNextItem(int item, wxDirection dir, long style) const;
  //inline int getNextItem(int item, Direction dir, int style) const { return getWx()->GetNextItem(item, (wxDirection)dir, style); }
  
  //void SetLabelFont(const wxFont& WXUNUSED(font));
  //inline void setLabelFont(IN(RFont) font ) { getWx()->SetLabelFont(CLS2WXREF(font));} 
   //void SetButtonFont(const wxFont& font) { SetFont(font); }
    //inline void setButtonFont(IN(RFont) font) { getWx()->SetButtonFont(CLS2WXREF(font)); }

};


} // wx
} // acdk

#endif //acdk_wx_RadioBox_h
