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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Control.h,v 1.9 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Control_h
#define acdk_wx_Control_h

#include "Window.h"
#include "Validator.h"

namespace acdk {
namespace wx {


/// Standard menu IDs
enum ControlId
{
    CID_LOWEST = 4999,

    CID_OPEN,
    CID_CLOSE,
    CID_NEW,
    CID_SAVE,
    CID_SAVEAS,
    CID_REVERT,
    CID_EXIT,
    CID_UNDO,
    CID_REDO,
    CID_HELP,
    CID_PRINT,
    CID_PRINT_SETUP,
    CID_PREVIEW,
    CID_ABOUT,
    CID_HELP_CONTENTS,
    CID_HELP_COMMANDS,
    CID_HELP_PROCEDURES,
    CID_HELP_CONTEXT,
    CID_CLOSE_ALL,

    CID_CUT = 5030,
    CID_COPY,
    CID_PASTE,
    CID_CLEAR,
    CID_FIND,
    CID_DUPLICATE,
    CID_SELECTALL,

    CID_FILE1 = 5050,
    CID_FILE2,
    CID_FILE3,
    CID_FILE4,
    CID_FILE5,
    CID_FILE6,
    CID_FILE7,
    CID_FILE8,
    CID_FILE9,

    // Standard button IDs
    CID_OK = 5100,
    CID_CANCEL,
    CID_APPLY,
    CID_YES,
    CID_NO,
    CID_STATIC,
    CID_FORWARD,
    CID_BACKWARD,
    CID_DEFAULT,
    CID_MORE,
    CID_SETUP,
    CID_RESET,
    CID_CONTEXT_HELP,
    CID_YESTOALL,
    CID_NOTOALL,
    CID_ABORT,
    CID_RETRY,
    CID_IGNORE,

    // System menu IDs (used by wxUniv):
    CID_SYSTEM_MENU = 5200,
    CID_CLOSE_FRAME,
    CID_MOVE_FRAME,
    CID_RESIZE_FRAME,
    CID_MAXIMIZE_FRAME,
    CID_ICONIZE_FRAME,
    CID_RESTORE_FRAME,

    // IDs used by generic file dialog (13 consecutive starting from this value)
    CID_FILEDLGG = 5900,

    CID_HIGHEST = 5999
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, ControlId);


ACDK_DECL_CLASS(Control);

/**
  see wxControl
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC Control
: extends Window
{
  ACDK_WITH_METAINFO(Control)
public:
  // wxControl
  ACDK_WX_STD_MEMBERS(Control, Window)

  Control(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(),
            IN(RSize) size = Size::defaultSize(), int style = 0,
            IN(RValidator) validator = Validator::defaultValidator(),
            IN(RString) name = "control")
            : Window(new wxControl(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style, CLS2WXREF(validator), S2WXS(name)))
    {
      ownsWxObject(false);
    }
  // Simulates an event
    //virtual void Command(wxCommandEvent& event) { ProcessCommand(event); }
  inline virtual void command(IN(RCommandEvent) event) { getWx()->Command(CLS2WXREF(event)); }
};


} // wx
} // acdk

#endif //acdk_wx_Control_h
