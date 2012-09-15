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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/PostScriptDC.h,v 1.2 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_PostScriptDC_h
#define acdk_wx_PostScriptDC_h
/*
is not enabled by default
#include "DC.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(PostScriptDC);


class ACDK_WX_PUBLIC PostScriptDC
: extends DC
{
  // wxPostScriptDC
  ACDK_WITH_METAINFO(PostScriptDC)
public:
  // wxPostScriptDC
  ACDK_WX_STD_MEMBERS(PostScriptDC, DC)

  PostScriptDC(IN(RString) output, bool interactive = true, IN(RWindow) parent = Nil) 
  : DC(new wxPostScriptDC(S2WXS(output), interactive, CLS2WXPTR(parent))) 
  {}
  static void setResolution(int ppi) { wxPostScriptDC::SetResolution(ppi); }
  static int getResolution() { return wxPostScriptDC::GetResolution(); }
};


} // wx
} // acdk
*/
  
#endif //acdk_wx_PostScriptDC_h
