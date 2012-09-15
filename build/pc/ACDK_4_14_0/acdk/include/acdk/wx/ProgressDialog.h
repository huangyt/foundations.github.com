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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ProgressDialog.h,v 1.6 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_ProgressDialog_h
#define acdk_wx_ProgressDialog_h

#include "Frame.h"

namespace acdk {
namespace wx {
/**
  see ProgressDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum ProgressDialogStyle
{
  PdCanAbort          = wxPD_CAN_ABORT         ,  // wxPD_CAN_ABORT          0x0001
  PdAppModal          = wxPD_APP_MODAL         ,  // wxPD_APP_MODAL          0x0002
  PdAutoHide          = wxPD_AUTO_HIDE         ,  // wxPD_AUTO_HIDE          0x0004
  PdElapsedTime       = wxPD_ELAPSED_TIME      ,  // wxPD_ELAPSED_TIME       0x0008
  PdEstimatedTime     = wxPD_ESTIMATED_TIME    ,  // wxPD_ESTIMATED_TIME     0x0010
// wxGA_SMOOTH = 0x0020 may also be used with wxProgressDialog
// NO!!! This is wxDIALOG_MODAL and will cause the progress dialog to
// be modal. No progress will then be made at all.
  PdRemainingTime     = wxPD_REMAINING_TIME    ,  // wxPD_REMAINING_TIME     0x0040
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, ProgressDialogStyle);

ACDK_DECL_CLASS(ProgressDialog);

/**
  Underlying wxProgressDialog is owned by this
  class
  see wxProgressDialog
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC ProgressDialog
: extends Window
{
  ACDK_WITH_METAINFO(ProgressDialog)
public:
  ACDK_WX_STD_MEMBERS(ProgressDialog, Window)

  //void wxProgressDialog(const wxString& title, const wxString& message, int maximum = 100, wxWindow * parent = NULL, int style); // = wxPD_AUTO_HIDE | wxPD_APP_MODAL)
  ProgressDialog(IN(RString) title, IN(RString)  message, int maximum = 100, IN(RWindow) parent = Nil, int style = PdAutoHide | PdAppModal) 
    : Window(new wxProgressDialog(S2WXS(title), S2WXS(message), maximum, CLS2WXPTR(parent), style), true)
  { 
  }
  //void Resume();
  inline void resume() { getWx()->Resume(); }
  //bool Update( int value, const wxString& newmsg);
  inline bool update(int value, IN(RString)  newmsg = "") { return getWx()->Update(value, S2WXS(newmsg)); }
};


} // wx
} // acdk

#endif //acdk_wx_ProgressDialog_h
