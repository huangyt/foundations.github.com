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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/CheckBox.h,v 1.4 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_CheckBox_h
#define acdk_wx_CheckBox_h

#include "Control.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(CheckBox);

/**
  see wxCheckBox
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC CheckBox
: extends Control
{
  ACDK_WITH_METAINFO(CheckBox)
public:
  // wxCheckBox
  ACDK_WX_STD_MEMBERS(CheckBox, Control)
  static RObject create_instance() { return new CheckBox(0, false); }
  CheckBox(IN(RWindow) parent, int id, IN(RString) label, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), 
            int style = 0,
            IN(RValidator) validator = Validator::defaultValidator(),
            IN(RString) name = "CheckBox")
  : Control(new wxCheckBox(CLS2WXPTR(parent), id, S2WXS(label), CLS2WXREF(pos), CLS2WXREF(size), style, CLS2WXREF(validator), S2WXS(name)), parent == Nil)
  {
  }
  //bool IsChecked() const { return GetValue(); }
  inline bool isChecked() const { return getWx()->IsChecked(); }
  //void SetValue(bool value);
  inline void setValue(bool value) { getWx()->SetValue(value); }
  //bool GetValue() const;
  inline bool getValue() const { return getWx()->GetValue(); }
  //void SetLabel(const wxString& label);
  inline void setLabel(IN(RString)  label) { getWx()->SetLabel(S2WXS(label)); }
};


} // wx
} // acdk

#endif //acdk_wx_CheckBox_h
