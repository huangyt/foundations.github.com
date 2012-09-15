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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Window.cpp,v 1.26 2005/02/05 10:45:35 kommer Exp $

#include "wx.h"

#include "LayoutConstraints.h"
#include "Caret.h"
#include "Dialog.h"
#include "FileDialog.h"

#include "TreeCtrl.h"
#include "XmlResource.h"
#include "Timer.h"

#include <acdk/lang/System.h>


#if defined(ACDK_OS_WIN32)
#if !ACDK_CHECK_WX_VERSION(2, 5)
int WXDLLEXPORT wxEntry(WXHINSTANCE hInstance, WXHINSTANCE hPrevInstance, char *lpszCmdLine, int nCmdShow, bool enterLoop);
#else
//int WXDLLEXPORT wxEntry(int argc, char **argv);
#endif
#endif //defined(ACDK_OS_WIN32)

namespace acdk {
namespace wx {

ACDK_DEFINE_WX_EVENT(TimerEvent, EvtTimer, wxEVT_TIMER);

//static
RPoint
Point::defaultPosition()
{
  static RPoint p = new Point(wxDefaultPosition);
  return p;
}

//static
RSize
Size::defaultSize()
{
  static RSize d = new Size(wxDefaultSize);
  return d;
}

/*
//virtual
bool
EventDispatcher::ProcessEvent(wxEvent& event)
{
  if (event.m_callbackUserData == 0)
    return wxEvtHandler::ProcessEvent(event);
  EventDispatcherArg* eda;
  if ((eda = dynamic_cast<EventDispatcherArg*>(event.m_callbackUserData)) == 0)
    return wxEvtHandler::ProcessEvent(event);
  Event tev(&event);
  (eda->_targetObj->*(eda->_targetFunc))(&tev);
  return false;
}
*/


void
wxWindowFwd::onEvent(wxEvent& event)
{
   if (event.m_callbackUserData == 0)
    return;
  EventDispatcherArg* eda = dynamic_cast<EventDispatcherArg*>(event.m_callbackUserData);
  if (eda == 0)
    return;
  try {
    if (eda->_dmiDelegate != Nil)
    {
      //acdk::lang::dmi::RDmiObjectArray oa = new acdk::lang::dmi::DmiObjectArray(1);
      acdk::lang::dmi::ScriptVarArray args(1);
      args[0] = inOf(EvtHandler::getEvent(event));
      eda->_dmiDelegate->call(args);
      return;
    }
    WxObject* objptr = &eda->_targetObj;
    (objptr->*(eda->_targetFunc))(EvtHandler::getEvent(event)); // implicite reinterpret cast.
  } catch (RThrowable ex) {
    acdk::lang::System::err->println("Unhandled exception in wxWindowFwd::OnEvent: " + ex->getMessage());
    throw;
  }
}



void
Window::initDispatch()
{

}

//static
int
Window::messageBox(IN(RString) message, IN(RString) caption, int style, IN(RWindow) parent, int x, int y)
{
  return wxMessageBox(S2WXS(message), S2WXS(caption), style, CLS2WXPTR(parent), x, y);
}

} // wx
} // acdk


