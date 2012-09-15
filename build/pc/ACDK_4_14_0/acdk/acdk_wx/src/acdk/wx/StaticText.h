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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/StaticText.h,v 1.8 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_StaticText_h
#define acdk_wx_StaticText_h

#include "Window.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(StaticText);

/**
  see wxStaticText
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC StaticText
: extends Control
{
  ACDK_WITH_METAINFO(StaticText)
public:
  // wxStaticText
  ACDK_WX_STD_MEMBERS(StaticText, Control)

  StaticText(IN(RWindow) parent, int id, IN(RString) label, 
                IN(RPoint) pos = Point::defaultPosition(),
                IN(RSize) size = Size::defaultSize(),
                int style = 0,
                IN(RString) name = "statictext")
  : Control(new wxStaticText(CLS2WXPTR(parent), id, S2WXS(label), pos->toWx(), size->toWx(), style, S2WXS(name)))
  {
  }

  
    // override some methods to resize the window properly
    //virtual void SetLabel(const wxString& label);
  inline virtual void setLabel(IN(RString)  label) { getWx()->SetLabel(S2WXS(label)); }
    //virtual bool SetFont( const wxFont &font );
  // ### TODO inline virtual bool setFont(IN(RFont) font) { return getWx()->SetFont(CLS2WXREF(font)); }
};


} // wx
} // acdk

#endif //acdk_wx_StaticText_h
