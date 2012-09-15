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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/DirDialog.h,v 1.4 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_DirDialog_h
#define acdk_wx_DirDialog_h

#include "Dialog.h"

namespace acdk {
namespace wx {

/**
  see wxDirDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum DirDialogFlags
{
    DdNewDirButton = wxDD_NEW_DIR_BUTTON,  // wxDD_NEW_DIR_BUTTON 
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, DirDialogFlags);

ACDK_DECL_CLASS(DirDialog);

/**
  see wxDirDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC DirDialog
: extends Dialog
{
  ACDK_WITH_METAINFO(DirDialog)
public:
  ACDK_WX_STD_MEMBERS(DirDialog, Dialog)
  /**
    @param style combination of DirDialogFlags
  */
  DirDialog(IN(RWindow) parent, 
             IN(RString) message,
             IN(RString) defaultPath = "",
             int style = 0,
             IN(RPoint) pos = Point::defaultPosition(),
             IN(RSize) size = Size::defaultSize()
             )
  : Dialog(new wxDirDialog(CLS2WXPTR(parent), S2WXS(message), S2WXS(defaultPath), style, CLS2WXREF(pos), CLS2WXREF(size)), parent == Nil)
  {
  }
  //wxString GetPath() const;
  inline RString getPath() const { return WXS2S(getWx()->GetPath()); }
  //wxString GetMessage() const;
  inline RString getMessage() const { return WXS2S(getWx()->GetMessage()); }
  //long GetStyle() const;
  inline int getStyle() const { return getWx()->GetStyle(); }
  //void SetMessage(const wxString& message);
  inline void setMessage(IN(RString)  message) { getWx()->SetMessage(S2WXS(message)); }
  //void SetPath(const wxString& path);
  inline void setPath(IN(RString)  path) { getWx()->SetPath(S2WXS(path)); }
  //void SetStyle(long style);
  inline void setStyle(int style) { getWx()->SetStyle(style); }
  //int ShowModal();
  inline int showModal() { return getWx()->ShowModal(); }
};

} // wx
} // acdk

#endif //acdk_wx_DirDialog_h
