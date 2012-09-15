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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ColourDialog.h,v 1.3 2005/02/05 10:45:34 kommer Exp $
#ifndef acdk_wx_ColourDialog_h
#define acdk_wx_ColourDialog_h

#include "Dialog.h"
#include "ColourData.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(ColourDialog);

/**
  see wxColourDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC ColourDialog
: extends Dialog
{
  ACDK_WITH_METAINFO(ColourDialog)
public:

  ACDK_WX_STD_MEMBERS(ColourDialog, Dialog)
  //wxColourDialog
  ColourDialog(IN(RWindow) parent, IN(RColourData) data = Nil)
  : Dialog(new wxColourDialog(CLS2WXPTR(parent), CLS2WXPTR(data)), parent == Nil)
  {
  }
  //wxColourData& GetColourData();
  inline RColourData getColourData() { return WXVAL2CLS(ColourData, getWx()->GetColourData()); }

  inline int showModal() { return getWx()->ShowModal(); }
};

} // wx
} // acdk

#endif //acdk_wx_ColourDialog_h
