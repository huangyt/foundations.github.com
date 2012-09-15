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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/MenuItem.h,v 1.13 2005/02/06 15:05:53 kommer Exp $

#ifndef acdk_wx_MenuItem_h
#define acdk_wx_MenuItem_h

#include "WindowStyle.h"

namespace acdk {
namespace wx {

enum ItemKind;

ACDK_DECL_CLASS(Menu);
ACDK_DECL_CLASS(MenuItem);
/**
  see wxMenuItem
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.13 $
  @date $Date: 2005/02/06 15:05:53 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC MenuItem
: extends WxObject
{
  ACDK_WITH_METAINFO(MenuItem)
  typedef WxObject Super;
public:
  ACDK_WX_STD_MEMBERS(MenuItem, WxObject)
  
    // the menu we're in
    //wxMenu *GetMenu() const { return m_parentMenu; }
  inline RMenu getMenu() const;
  //void SetMenu(wxMenu* menu) { m_parentMenu = menu; }
  inline void setMenu(IN(RMenu) menu);
  
  // get/set id
  //void SetId(int itemid) { m_id = itemid; }
  inline void setId(int itemid) { getWx()->SetId(itemid); }
  //int  GetId() const { return m_id; }
  inline int getId() const { return getWx()->GetId(); }
  //bool IsSeparator() const { return m_id == wxID_SEPARATOR; }
  inline bool isSeparator() const { return getWx()->IsSeparator(); }
  
  // the item's text (or name)
  //
  // NB: the item's text includes the accelerators and mnemonics info (if
  //     any), i.e. it may contain '&' or '_' or "\t..." and thus is
  //     different from the item's label which only contains the text shown
  //     in the menu
  //virtual void SetText(const wxString& str) { m_text = str; }
  inline virtual void setText(IN(RString)  str) { getWx()->SetText(S2WXS(str)); }
  //wxString GetLabel() const { return GetLabelFromText(m_text); }
  inline RString getLabel() const { return WXS2S(getWx()->GetLabel()); }
  //const wxString& GetText() const { return m_text; }
  inline RString getText() const { return WXS2S(getWx()->GetText()); }
  
  // get the label from text (implemented in platform-specific code)
  //static wxString GetLabelFromText(const wxString& text);
  inline static RString getLabelFromText(IN(RString)  text) { return WXS2S(wxMenuItem::GetLabelFromText(S2WXS(text))); }
  
  // what kind of menu item we are
  //wxItemKind GetKind() const { return m_kind; }
  inline ItemKind getKind() const { return (ItemKind)getWx()->GetKind(); }
  //void SetKind(wxItemKind kind) { m_kind = kind; }
  inline void setKind(ItemKind kind) { getWx()->SetKind((wxItemKind)kind); }
  
  //virtual void SetCheckable(bool checkable) { m_kind = checkable ? wxITEM_CHECK : wxITEM_NORMAL; }
  inline virtual void setCheckable(bool checkable) { getWx()->SetCheckable(checkable); }
  //bool IsCheckable() const    { return m_kind == wxITEM_CHECK || m_kind == wxITEM_RADIO; }
  inline bool isCheckable() const { return getWx()->IsCheckable(); }
  
  //bool IsSubMenu() const { return m_subMenu != NULL; }
  inline bool isSubMenu() const { return getWx()->IsSubMenu(); }
  //void SetSubMenu(wxMenu *menu) { m_subMenu = menu; }
  inline void setSubMenu(IN(RMenu) menu);
  //wxMenu *GetSubMenu() const { return m_subMenu; }
  inline RMenu getSubMenu() const;
  
  // state
  //virtual void Enable(bool enable = true) { m_isEnabled = enable; }
  inline virtual void enable(bool enable = true) { getWx()->Enable(enable); }
  //virtual bool IsEnabled() const { return m_isEnabled; }
  inline virtual bool isEnabled() const { return getWx()->IsEnabled(); }
  
  //virtual void Check(bool check = true) { m_isChecked = check; }
  inline virtual void check(bool check = true) { getWx()->Check(check); }
  //virtual bool IsChecked() const { return m_isChecked; }
  inline virtual bool isChecked() const { return getWx()->IsChecked(); }
  //void Toggle() { Check(!m_isChecked); }
  inline void toggle() { getWx()->Toggle(); }
  
  // help string (displayed in the status bar by default)
  //void SetHelp(const wxString& str) { m_help = str; }
  inline void setHelp(IN(RString)  str) { getWx()->SetHelp(S2WXS(str)); }
  //const wxString& GetHelp() const { return m_help; }
  inline RString getHelp() const { return WXS2S(getWx()->GetHelp()); }

};

} // wx
} // acdk

#endif //acdk_wx_MenuItem_h
