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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Dialog.h,v 1.11 2005/03/11 11:11:50 kommer Exp $

#ifndef acdk_wx_Dialog_h
#define acdk_wx_Dialog_h

#include "Window.h"
#include "Frame.h"
#include "WindowStyle.h"

namespace acdk {
namespace wx {

enum WindowStyle;


ACDK_DECL_CLASS(Dialog);

/**
  see wxDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/03/11 11:11:50 $
*/
class ACDK_WX_PUBLIC Dialog
: extends TopLevelWindow
{
  ACDK_WITH_METAINFO(Dialog)
public:
  ACDK_WX_STD_MEMBERS(Dialog, TopLevelWindow)

  Dialog() : TopLevelWindow(new wxDialog()) {}
    /*
Dialog(IN(RWindow) parent, IN(RString) title, bool modal, int x = -1, int y= -1, 
          int width = 500, int height = 500, int style = DefaultDialogStyle, IN(RString) name = "")
          : TopLevelWindow(new wxDialog(parent->getWx(), style, S2WXS(title), modal, wxPoint(x, y), wxSize(width, height), style, S2WXS(name)))
    {
    }
    */
    // Constructor with no modal flag - the new convention.
  Dialog(IN(RWindow) parent, int id,
             IN(RString) title,
             IN(RPoint) pos = Point::defaultPosition(),
             IN(RSize) size = Size::defaultSize(),
             int style = DefaultDialogStyle,
             IN(RString) name = "")
             : TopLevelWindow(new wxDialog(CLS2WXPTR(parent), id, S2WXS(title), pos->toWx(), size->toWx(), style, S2WXS(name)))
    {
        
    }

//    virtual bool IsModal() const;
  virtual bool isModal() const { return getWx()->IsModal(); }

    // For now, same as Show(TRUE) but returns return code
//    virtual int ShowModal();
  virtual int showModal() { return getWx()->ShowModal(); }

    // may be called to terminate the dialog with the given return code
//    virtual void EndModal(int retCode);
  virtual void endModal(int retCode) { getWx()->EndModal(retCode); }
};

} // wx
} // acdk

#endif //acdk_wx_Dialog_h
