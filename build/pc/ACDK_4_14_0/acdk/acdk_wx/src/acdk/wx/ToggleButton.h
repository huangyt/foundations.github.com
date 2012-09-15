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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ToggleButton.h,v 1.5 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_ToggleButton_h
#define acdk_wx_ToggleButton_h

#include "Control.h"
#include "Validator.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(ToggleButton);
/**
  see wxToggleButton
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.5 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC ToggleButton
: extends Control
{
  ACDK_WITH_METAINFO(ToggleButton)
public:
  // wxToggleButton
  ACDK_WX_STD_MEMBERS(ToggleButton, Control)
  ToggleButton(IN(RWindow) parent, int id, IN(RString) label, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), 
           int style = 0, IN(RValidator) validator = Nil)
  : Control(new wxToggleButton(CLS2WXPTR(parent), id, S2WXS(label), CLS2WXREF(pos), CLS2WXREF(size), style, validator == Nil ? wxDefaultValidator : CLS2WXREF(validator)),
            parent == Nil)
  {
  }
  //bool GetValue() const;
  inline bool getValue() const { return getWx()->GetValue(); }
  //void SetValue(const bool state);
  inline void setValue(bool state) { getWx()->SetValue(state); }

 
};


} // wx
} // acdk

#endif //acdk_wx_ToggleButton_h
