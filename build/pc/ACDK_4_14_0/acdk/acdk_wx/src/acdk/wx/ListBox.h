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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ListBox.h,v 1.8 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_ListBox_h
#define acdk_wx_ListBox_h

#include "Event.h"
#include "ControlWithItems.h"

namespace acdk {
namespace wx {

/**
  see ListBox, wxListBox
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum ListBoxStyle
{
  LbSort           = wxLB_SORT          ,  // wxLB_SORT           0x0010
  LbSingle         = wxLB_SINGLE        ,  // wxLB_SINGLE         0x0020
  LbMultiple       = wxLB_MULTIPLE      ,  // wxLB_MULTIPLE       0x0040
  LbExtended       = wxLB_EXTENDED      ,  // wxLB_EXTENDED       0x0080
// wxLB_OWNERDRAW is Windows-only
  LbOwnerdraw      = wxLB_OWNERDRAW     ,  // wxLB_OWNERDRAW      0x0100
  LbNeededSb      = wxLB_NEEDED_SB     ,  // wxLB_NEEDED_SB      0x0200
  LbAlwaysSb      = wxLB_ALWAYS_SB     ,  // wxLB_ALWAYS_SB      0x0400
  LbHscroll        = wxLB_HSCROLL       ,  // wxLB_HSCROLL        wxHSCROLL
// always show an entire number of rows
  LbIntHeight     = wxLB_INT_HEIGHT      // wxLB_INT_HEIGHT     0x0800
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, ListBoxStyle);
 
ACDK_DECL_CLASS(ListBox);


/**
  see wxListBox
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC ListBox
: extends ControlWithItems
{
  ACDK_WITH_METAINFO(ListBox)
public:
  ACDK_WX_STD_MEMBERS(ListBox, ControlWithItems)
  ListBox() : ControlWithItems(new wxListBox()) {}
  ListBox(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), 
          IN(RSize) size = Size::defaultSize(), 
          IN(RStringArray)  choices = Nil, int style = 0, 
          IN(RValidator) validator = Validator::defaultValidator(), 
          IN(RString)  name = "") 
  : ControlWithItems(new wxListBox(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), 0, NULL, style, CLS2WXREF(validator), S2WXS(name)))
  {
    if (choices == Nil)
      return;
    for (int i = 0; i < choices->length(); ++i)
      append(choices[i]);
    
  }

  //virtual void Clear();
  inline void clear() { getWx()->Clear(); }
  //virtual void Delete(int n);
  inline void deleteItem(int n) { getWx()->Delete(n); }
  //virtual int GetCount() const;
  inline int getCount() const { return getWx()->GetCount(); }
  //virtual wxString GetString(int n) const;
  inline RString getString(int n) const { return WXS2S(getWx()->GetString(n)); }
  //virtual void SetString(int n, const wxString& s);
  inline void setString(int n, IN(RString)  s) { getWx()->SetString(n, S2WXS(s)); }
  //virtual int FindString(const wxString& s) const;
  inline int findString(IN(RString)  s) const { return getWx()->FindString(S2WXS(s)); }
  //virtual bool IsSelected(int n) const;
  inline bool isSelected(int n) const { return getWx()->IsSelected(n); }
  //virtual void SetSelection(int n, bool select = TRUE);
  inline void setSelection(int n, bool select = true) { getWx()->SetSelection(n, select); }
  //virtual int GetSelection() const;
  inline int getSelection() const { return getWx()->GetSelection(); }
  
  inline int append(IN(RString)  item) { return getWx()->Append(S2WXS(item)); }
  
  //virtual void DoSetItems(const wxArrayString& items, void **clientData);
  
  //virtual void DoSetItemClientData(int n, void* clientData);
  //virtual void* DoGetItemClientData(int n) const;
  //virtual void DoSetItemClientObject(int n, wxClientData* clientData);
  //virtual wxClientData* DoGetItemClientObject(int n) const;

  //wxString GetStringSelection() const;
  inline RString getStringSelection() const { return WXS2S(getWx()->GetStringSelection()); }

  //void Insert(const wxString& item, int pos);
  inline void insert(IN(RString)  item, int pos) { getWx()->Insert(S2WXS(item), pos); }
  //void Insert(const wxString& item, int pos, wxClientData *clientData);
  inline void insert(IN(RString)  item, int pos, IN(RObject) clientData) 
  { 
    wxClientData* wxCd = 0;
    if (clientData != 0)
      wxCd = new wxAcdkClientData(clientData);
    getWx()->Insert(S2WXS(item), pos, wxCd); 
  }


  //void InsertItems(int nItems, const wxString *items, int pos);
  //void InsertItems(const wxArrayString& items, int pos);
  inline void insertItems(IN(RStringArray) items, int pos) 
  { 
    wxArrayString wxarr;
    for (int i = 0; i < items->length(); ++i)
      wxarr.Add(S2WXS(items[i]));
    getWx()->InsertItems(wxarr, pos); 
  }

  // ### @todo Allow set RObject as data
  // inline void set(int n, IN(RString)  items, void **, = NULL) { getWx()->Set(n, S2WXS(items), **, NULL); }
    //void Set(const wxArrayString& items, void **clientData = NULL);
  inline void set(IN(RStringArray) items) 
  { 
    //getWx()->Set(CLS2WXREF(items), **, NULL); 
  }

    // multiple selection logic
  
  //virtual void Select(int n);
  inline virtual void select(int n) { getWx()->Select(n); }
  //void Deselect(int n);
  inline void deselect(int n) { getWx()->Deselect(n); }
  //void DeselectAll(int itemToLeaveSelected = -1);
  inline void deselectAll(int itemToLeaveSelected = -1) { getWx()->DeselectAll(itemToLeaveSelected); }

  //virtual bool SetStringSelection(const wxString& s, bool select = TRUE);
  inline virtual bool setStringSelection(IN(RString)  s, bool select = true) { return getWx()->SetStringSelection(S2WXS(s), select); }

    // works for single as well as multiple selection listboxes (unlike
    // GetSelection which only works for listboxes with single selection)
 //virtual int GetSelections(wxArrayInt& aSelections) const = 0;
  inline virtual int getSelections(IN(RintArray) aSelections) const 
  { 
    wxArrayInt wxSelections;
    int ret = getWx()->GetSelections(wxSelections);
    for (int i = 0; i < (int)wxSelections.GetCount(); ++i)
      aSelections->append(wxSelections[i]);
    return ret;
  }

    // set the specified item at the first visible item or scroll to max
    // range.
  //void SetFirstItem(int n);
  inline void setFirstItem(int n) { getWx()->SetFirstItem(n); }
  //void SetFirstItem(const wxString& s);
  inline void setFirstItem(IN(RString)  s) { getWx()->SetFirstItem(S2WXS(s)); }

    // ensures that the given item is visible scrolling the listbox if
    // necessary
 //virtual void EnsureVisible(int n);
  inline virtual void ensureVisible(int n) { getWx()->EnsureVisible(n); }

    // a combination of Append() and EnsureVisible(): appends the item to the
    // listbox and ensures that it is visible i.e. not scrolled out of view
 //void AppendAndEnsureVisible(const wxString& s);
  inline void appendAndEnsureVisible(IN(RString)  s) { getWx()->AppendAndEnsureVisible(S2WXS(s)); }

    // return TRUE if the listbox allows multiple selection
 //bool HasMultipleSelection() const;
  inline bool hasMultipleSelection() const { return getWx()->HasMultipleSelection(); }

    // return TRUE if this listbox is sorted
  //bool IsSorted() const;
  inline bool isSorted() const { return getWx()->IsSorted(); }

    // emulate selecting or deselecting the item event.GetInt() (depending on
    // event.GetExtraLong())
  //void Command(wxCommandEvent& event);
  inline void command(IN(RCommandEvent) event) { getWx()->Command(CLS2WXREF(event)); }

    // compatibility - these functions are deprecated, use the new ones
    // instead
  //bool Selected(int n) const;
  inline bool selected(int n) const { return getWx()->Selected(n); }
  
};



} // wx
} // acdk

#endif //acdk_wx_ListBox_h
