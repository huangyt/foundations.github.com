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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/TextEntryDialog.h,v 1.3 2005/02/05 10:45:35 kommer Exp $
#ifndef acdk_wx_TextEntryDialog_h
#define acdk_wx_TextEntryDialog_h

#include "Dialog.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(TextEntryDialog);

/**
  see wxTextEntryDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC TextEntryDialog
: extends Dialog
{
  ACDK_WITH_METAINFO(TextEntryDialog)
public:

  ACDK_WX_STD_MEMBERS(TextEntryDialog, Dialog)
  //wxTextEntryDialog
  TextEntryDialog(IN(RWindow) parent, IN(RString) message, IN(RString) caption = "Please enter text", IN(RString) defaultValue = "", 
                  int style = MbOk | MbCancel | Centre, IN(RPoint) pos = Point::defaultPosition())
  : Dialog(new wxTextEntryDialog(CLS2WXPTR(parent), S2WXS(message), S2WXS(caption), S2WXS(defaultValue), style, CLS2WXREF(pos)), parent == Nil)
  {
  }
  //wxString GetValue() const;
  inline RString getValue() const { return WXS2S(getWx()->GetValue()); }
  //void SetValue(const wxString& value);
  inline void setValue(IN(RString)  value) { getWx()->SetValue(S2WXS(value)); }
  inline int showModal() { return getWx()->ShowModal(); }
};

} // wx
} // acdk

#endif //acdk_wx_TextEntryDialog_h
