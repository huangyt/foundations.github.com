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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/SingleChoiceDialog.h,v 1.3 2005/02/05 10:45:35 kommer Exp $
/*
not yet implemented
#ifndef acdk_wx_SingleChoiceDialog_h
#define acdk_wx_SingleChoiceDialog_h

#include "Dialog.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(SingleChoiceDialog);

class ACDK_WX_PUBLIC SingleChoiceDialog
: extends Dialog
{
  ACDK_WITH_METAINFO(SingleChoiceDialog)
public:

  ACDK_WX_STD_MEMBERS(SingleChoiceDialog, Dialog)
  //wxSingleChoiceDialog
  SingleChoiceDialog(IN(RWindow) parent, IN(RFontData) fontData)
  : Dialog(new wxSingleChoiceDialog(CLS2WXPTR(parent), CLS2WXREF(fontData)), parent == Nil)
  {
  }
  
  inline int showModal() { return getWx()->ShowModal(); }
};

} // wx
} // acdk

#endif //acdk_wx_SingleChoiceDialog_h
*/
