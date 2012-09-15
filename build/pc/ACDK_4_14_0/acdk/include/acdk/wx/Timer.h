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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Timer.h,v 1.4 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Timer_h
#define acdk_wx_Timer_h

#include "Window.h"
#include "Validator.h"
#include <wx/timer.h>

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(TimerEvent);

/**
  see wxTimerEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC TimerEvent
: extends Event
{
  ACDK_WITH_METAINFO(TimerEvent)
public:
  ACDK_WX_STD_EVENT_MEMBERS(TimerEvent, Event)
  TimerEvent(int id = 0, int interval = 0)
  : Event(new wxTimerEvent(id, interval))
  {
  }
  //int GetInterval();
  inline int getInterval() { return getWx()->GetInterval(); }
  static int EvtTimer;
  //EVT_TIMER
};

ACDK_DECL_CLASS(Timer);
/**
  see wxTimer
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Timer
: extends WxObject
{
  ACDK_WITH_METAINFO(Timer)
public:
  // wxTimer
  // wxTimerEvent
  ACDK_WX_STD_MEMBERS(Timer, WxObject)
  Timer() : WxObject(new wxTimer(), true) {}

  Timer(IN(RWindow) parent, int id = -1)
  : WxObject(new wxTimer(CLS2WXPTR(parent), id))
  {
  }
  //virtual bool Start(int milliseconds = -1, bool oneShot = FALSE);
  inline virtual bool start(int milliseconds = -1, bool oneShot = false) { return getWx()->Start(milliseconds, oneShot); }
    //virtual void Stop();
  inline virtual void stop() { getWx()->Stop(); }

    //virtual bool IsRunning() const { return m_id != 0; }
  inline virtual bool isRunning() const { return getWx()->IsRunning(); }
};


} // wx
} // acdk

#endif //acdk_wx_Timer_h
