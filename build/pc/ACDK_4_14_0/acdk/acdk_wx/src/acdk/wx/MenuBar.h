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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/MenuBar.h,v 1.8 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_MenuBar_h
#define acdk_wx_MenuBar_h

#include "Window.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(MenuBar);
/**
  see wxMenuBar
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC MenuBar
: extends Window
{
  ACDK_WITH_METAINFO(MenuBar)
public:
  ACDK_WX_STD_MEMBERS(MenuBar, Window)
  // wxMenuBar
  MenuBar() : Window(new wxMenuBar()) {}
  void append(IN(RMenu) menu, IN(RString) title)
  {
    getWx()->Append(menu->getWx(), S2WXS(title));
  }
  //virtual bool Insert(size_t pos, wxMenu *menu, const wxString& title);
  inline virtual bool insert(int pos, IN(RMenu) menu, IN(RString)  title) { return getWx()->Insert(pos, CLS2WXPTR(menu), S2WXS(title)); }
  //virtual wxMenu *Remove(size_t pos);
  inline virtual RMenu remove(int pos) { RETURN_WXPTR2CLS(Menu, getWx()->Remove(pos)); }
  
  //virtual wxMenu *Replace(size_t pos, wxMenu *menu, const wxString& title);
  inline virtual RMenu replace(int pos, IN(RMenu) menu, IN(RString)  title) { RETURN_WXPTR2CLS(Menu, getWx()->Replace(pos, CLS2WXPTR(menu), S2WXS(title))); }
  
  //virtual void EnableTop( size_t pos, bool flag );
  inline virtual void enableTop(int pos, bool flag) { getWx()->EnableTop(pos, flag); }
  //virtual void SetLabelTop( size_t pos, const wxString& label );
  inline virtual void setLabelTop(int pos, IN(RString)  label) { getWx()->SetLabelTop(pos, S2WXS(label)); }
  //virtual wxString GetLabelTop( size_t pos ) const;
  inline virtual RString getLabelTop(int pos) const { return WXS2S(getWx()->GetLabelTop(pos)); }
};

inline RMenuBar 
Menu::getMenuBar() const { RETURN_WXPTR2CLS(MenuBar, getWx()->GetMenuBar()); }

inline void 
Menu::attach(IN(RMenuBar) menubar) { getWx()->Attach(CLS2WXPTR(menubar)); }


} // wx
} // acdk

#endif //acdk_wx_MenuBar_h
