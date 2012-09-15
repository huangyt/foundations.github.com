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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/FontDialog.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_FontDialog_h
#define acdk_wx_FontDialog_h

#include "FontData.h"
#include "Dialog.h"
#include "Window.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(FontDialog);

/**
  see wxFontDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC FontDialog
: extends Dialog
{
  ACDK_WITH_METAINFO(FontDialog)
public:

  ACDK_WX_STD_MEMBERS(FontDialog, Dialog)
  //wxFontDialog
  FontDialog(IN(RWindow) parent, IN(RFontData) fontData)
  : Dialog(new wxFontDialog(CLS2WXPTR(parent), CLS2WXREF(fontData)), parent == Nil)
  {
  }
  FontDialog(IN(RWindow) parent)
  : Dialog(new wxFontDialog(CLS2WXPTR(parent)), parent == Nil)
  {
  }
  FontDialog()
  : Dialog(new wxFontDialog(), true)
  {
  }
 
  //const wxFontData& GetFontData() const;
  inline RFontData getFontData() const { return WXVAL2CLS(FontData, getWx()->GetFontData()); }
  //int ShowModal();
  inline int showModal() { return getWx()->ShowModal(); }
};

} // wx
} // acdk

#endif //acdk_wx_FontDialog_h
