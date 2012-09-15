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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Button.h,v 1.10 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Button_h
#define acdk_wx_Button_h

#include "Control.h"
#include "Validator.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(Button);

/**
  see wxButton
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.10 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC Button
: extends Control
{
  ACDK_WITH_METAINFO(Button)
public:
  // wxButton
  ACDK_WX_STD_MEMBERS(Button, Control)
  Button(IN(RWindow) parent, int id, IN(RString) label, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), 
           int style = 0, IN(RValidator) validator = Nil)
  : Control(new wxButton(CLS2WXPTR(parent), id, S2WXS(label), CLS2WXREF(pos), CLS2WXREF(size), style, validator == Nil ? wxDefaultValidator : CLS2WXREF(validator)),
            parent == Nil)
  {
  }
  RString getLabel() { return WXS2S(getWx()->GetLabel()); } 
  void setLabel(IN(RString) label) { getWx()->SetLabel(S2WXS(label)); }
  void setDefault() { getWx()->wxButton::SetDefault(); }
};


} // wx
} // acdk

#endif //acdk_wx_Button_h
