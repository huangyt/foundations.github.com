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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/MemoryDC.h,v 1.4 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_MemoryDC_h
#define acdk_wx_MemoryDC_h

#include "DC.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(MemoryDC);

/**
  see wxMemoryDC
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC MemoryDC
: extends DC
{
  // wxMemoryDC
  ACDK_WITH_METAINFO(MemoryDC)
public:
  // wxMemoryDC
  ACDK_WX_STD_MEMBERS(MemoryDC, DC)
  MemoryDC() : DC(new wxMemoryDC()) {}
  MemoryDC(IN(RDC) dc) : DC(new wxMemoryDC(CLS2WXPTR(dc)), true) {}
  //void SelectObject(const wxBitmap& bitmap);
  inline void selectObject(IN(RBitmap) bitmap) { getWx()->SelectObject(CLS2WXREF(bitmap)); }
};


} // wx
} // acdk

#endif //acdk_wx_MemoryDC_h
