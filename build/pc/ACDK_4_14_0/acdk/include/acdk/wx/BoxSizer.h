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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/BoxSizer.h,v 1.3 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_BoxSizer_h
#define acdk_wx_BoxSizer_h

#include "Sizer.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(BoxSizer);

class ACDK_WX_PUBLIC BoxSizer
: extends Sizer
{
  ACDK_WITH_METAINFO(BoxSizer)
public:
  // wxBoxSizer
  ACDK_WX_STD_MEMBERS(BoxSizer, Sizer)

  BoxSizer(int orient)
  : Sizer(new wxBoxSizer(orient), false)
  {
  }
  //void RecalcSizes();
  inline void recalcSizes() { getWx()->RecalcSizes(); }
  //wxSize CalcMin();
  inline RSize calcMin() { return WXVAL2CLS(Size, getWx()->CalcMin()); }
  //int GetOrientation();
  inline int getOrientation() { return getWx()->GetOrientation(); }
  //void SetOrientation(int orient)
  inline void setOrientation(int orient) { getWx()->SetOrientation(orient); }
};



} // wx
} // acdk

#endif //acdk_wx_BoxSizer_h
