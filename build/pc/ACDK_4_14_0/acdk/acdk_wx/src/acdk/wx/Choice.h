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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Choice.h,v 1.6 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Choice_h
#define acdk_wx_Choice_h

#include "ControlWithItems.h"

namespace acdk {
namespace wx {



ACDK_DECL_CLASS(Choice);

/**
  see wxChoice
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC Choice
: extends ControlWithItems
{
  ACDK_WITH_METAINFO(Choice)
public:
  // wxChoice
  ACDK_WX_STD_MEMBERS(Choice, ControlWithItems)

  Choice(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), int style = 0,
            IN(RStringArray) choices = Nil,
            IN(RValidator) validator = Validator::defaultValidator(),
            IN(RString) name = "Choice")
  : ControlWithItems(new wxChoice(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), 0, 0, style, CLS2WXREF(validator), S2WXS(name)), parent == Nil)
  {
    if (choices != Nil)
    {
      for (int i = 0; i < choices->length(); ++i)
        append(choices[i]);
    }
  }

    // don't override this
  //void Select(int n) { SetSelection(n); }
  inline void select(int n) { getWx()->Select(n); }
    
    // set/get the number of columns in the control (as they're not supporte on
    // most platforms, they do nothing by default)
  //void SetColumns(int n = 1 ) { }
  inline void setColumns(int n = 1) { getWx()->SetColumns(n); }
   //int GetColumns() const { return 1 ; }
  inline int getColumns() const { return getWx()->GetColumns(); }


  //int DoAppend(const wxString& item);
  inline int append(IN(RString)  item) { return getWx()->Append(S2WXS(item)); }
  //void Delete(int n);
  inline void deleteItem(int n) { getWx()->Delete(n); }
  //void Clear();
  inline void clear() { getWx()->Clear(); }
  
  //int GetCount() const;
  inline int getCount() const { return getWx()->GetCount(); }
  //int GetSelection() const;
  inline int getSelection() const { return getWx()->GetSelection(); }
  //void SetSelection(int n);
  inline void setSelection(int n) { getWx()->SetSelection(n); }
  
  //int FindString(const wxString& s) const;
  inline int findString(IN(RString)  s) const { return getWx()->FindString(S2WXS(s)); }
  //wxString GetString(int n) const;
  inline RString getString(int n) const { return WXS2S(getWx()->GetString(n)); }
  //void SetString(int n, const wxString& s);
  inline void setString(int n, IN(RString)  s) { getWx()->SetString(n, S2WXS(s)); }
  //wxString GetStringSelection() const;
  inline RString getStringSelection() const { return WXS2S(getWx()->GetStringSelection()); }
  //virtual bool SetStringSelection(const wxString& s);
  inline bool setStringSelection(IN(RString)  s) { return getWx()->SetStringSelection(S2WXS(s)); }

};


} // wx
} // acdk

#endif //acdk_wx_Choice_h
