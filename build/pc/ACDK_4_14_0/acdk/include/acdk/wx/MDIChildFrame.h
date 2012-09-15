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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/MDIChildFrame.h,v 1.4 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_MDIChildFrame_h
#define acdk_wx_MDIChildFrame_h

#include "Frame.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(MDIParentFrame);

ACDK_DECL_CLASS(MDIChildFrame);

/**
  see wxMDIChildFrame
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC MDIChildFrame
: extends Frame
{
  ACDK_WITH_METAINFO(MDIChildFrame)
public:
  // wxMDIChildFrame
  ACDK_WX_STD_MEMBERS(MDIChildFrame, Frame)

  MDIChildFrame(IN(RMDIParentFrame) parent, int id, IN(RString) title, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), int style =  DefaultFrameStyle,
            IN(RString) name = "frame");

  //void Activate();
  inline void activate() { getWx()->Activate(); }
  //void Maximize();
  inline void maximize() 
  { 
#if defined(ACDK_OS_WIN32)
    getWx()->Maximize();
#else
    getWx()->Maximize(false); 
#endif
  }
  //void Restore();
  inline void restore() { getWx()->Restore(); }

};


} // wx
} // acdk

#endif //acdk_wx_MDIChildFrame_h
