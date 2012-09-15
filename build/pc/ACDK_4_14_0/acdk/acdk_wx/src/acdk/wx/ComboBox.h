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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ComboBox.h,v 1.7 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_ComboBox_h
#define acdk_wx_ComboBox_h

#include "Choice.h"

namespace acdk {
namespace wx {

/**
  see wxComboBox
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum ComboBoxStyle
{
  CbSimple         = wxCB_SIMPLE        ,  // wxCB_SIMPLE         0x0004
  CbSort           = wxCB_SORT          ,  // wxCB_SORT           0x0008
  CbReadonly       = wxCB_READONLY      ,  // wxCB_READONLY       0x0010
  CbDropdown       = wxCB_DROPDOWN        // wxCB_DROPDOWN       0x0020
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, ComboBoxStyle);

ACDK_DECL_CLASS(ComboBox);

/**
  see wxComboBox
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC ComboBox
: extends Control
{
  ACDK_WITH_METAINFO(ComboBox)
public:
  // wxComboBox
  ACDK_WX_STD_MEMBERS(ComboBox, Control)

  ComboBox(IN(RWindow) parent, int id, IN(RString) value = "", IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), int style = 0,
            IN(RStringArray) choices = Nil,
            IN(RValidator) validator = Validator::defaultValidator(),
            IN(RString) name = "ComboBox")
  : Control(new wxComboBox(CLS2WXPTR(parent), id, S2WXS(value), CLS2WXREF(pos), CLS2WXREF(size), 0, 0, style, CLS2WXREF(validator), S2WXS(name)), parent == Nil)
  {
    if (choices != Nil)
    {
      for (int i = 0; i < choices->length(); ++i)
        append(choices[i]);
    }
  }
    //inline void select(int n) { getWx()->Select(n); }
    
    // set/get the number of columns in the control (as they're not supporte on
    // most platforms, they do nothing by default)
  //void SetColumns(int n = 1 ) { }
  //inline void setColumns(int n = 1) { getWx()->SetColumns(n); }
   //int GetColumns() const { return 1 ; }
    //inline int getColumns() const { return getWx()->GetColumns(); }


  //int DoAppend(const wxString& item);
  inline void append(IN(RString)  item) {  getWx()->Append(S2WXS(item)); }
  //void Delete(int n);
  inline void deleteItem(int n) { getWx()->Delete(n); }
  //void Clear();
  inline void clear() { getWx()->Clear(); }
  
  //int GetCount() const;
  inline int getCount() const { return getWx()->GetCount(); }
  //int GetSelection() const;
  inline int getSelection() const { return getWx()->GetSelection(); }

  
  //int FindString(const wxString& s) const;
  inline int findString(IN(RString)  s)  { return getWx()->FindString(S2WXS(s)); }
  //wxString GetString(int n) const;
  inline RString getString(int n) const { return WXS2S(getWx()->GetString(n)); }
  //void SetString(int n, const wxString& s);
  //inline void setString(int n, IN(RString)  s) { getWx()->SetString(n, S2WXS(s)); }
  //wxString GetStringSelection() const;
  inline RString getStringSelection() const { return WXS2S(getWx()->GetStringSelection()); }
  //virtual bool SetStringSelection(const wxString& s);
  inline void setStringSelection(IN(RString)  s) { getWx()->SetStringSelection(S2WXS(s)); }

  //wxString GetValue() const { return GetLabel(); }
  inline RString getValue() const { return WXS2S(getWx()->GetValue()); }
  //void SetValue(const wxString& value);
  inline void setValue(IN(RString)  value) { getWx()->SetValue(S2WXS(value)); }
  
  // Clipboard operations
  //void Copy();
  inline void copy() { getWx()->Copy(); }
  //void Cut();
  inline void cut() { getWx()->Cut(); }
  //void Paste();
  inline void paste() { getWx()->Paste(); }
  //void SetInsertionPoint(long pos);
  inline void setInsertionPoint(int pos) { getWx()->SetInsertionPoint(pos); }
  //void SetInsertionPointEnd();
  inline void setInsertionPointEnd() { getWx()->SetInsertionPointEnd(); }
  //long GetInsertionPoint() const;
  inline int getInsertionPoint() const { return getWx()->GetInsertionPoint(); }
  //long GetLastPosition() const;
  inline int getLastPosition() const { return getWx()->GetLastPosition(); }
  //void Replace(long from, long to, const wxString& value);
  inline void replace(int from, int to, IN(RString)  value) { getWx()->Replace(from, to, S2WXS(value)); }
  //void Remove(long from, long to);
  inline void remove(int from, int to) { getWx()->Remove(from, to); }
  //void SetSelection(int n) { wxChoice::SetSelection(n); }
  inline void setSelection(int n) { getWx()->SetSelection(n); }
  //void SetSelection(long from, long to);
  inline void setSelection(int from, int to) { getWx()->SetSelection(from, to); }
  //void SetEditable(bool editable);
  inline void setEditable(bool editable) { getWx()->SetEditable(editable); }
 
};


} // wx
} // acdk

#endif //acdk_wx_ComboBox_h
