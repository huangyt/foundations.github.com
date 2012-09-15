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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Event.cpp,v 1.20 2005/05/09 13:47:58 kommer Exp $


#include "Event.h"
#include "Window.h"
#include "TextCtrl.h"
#include "DC.h"
#include <acdk/lang/System.h>
#include <acdk/lang/sys/core_hashmap.h>


inline int hash(int v, int maxsize)
{
  return (v * 31) % maxsize;
}

namespace acdk {
namespace wx {





typedef REvent (*EventCreator)(wxEvent& wxevent);



ACDK_DEFINE_WX_EVENT(Event, EvtNull, wxEVT_NULL);

ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandButtonClicked, wxEVT_COMMAND_BUTTON_CLICKED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandCheckboxClicked, wxEVT_COMMAND_CHECKBOX_CLICKED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandChoiceSelected, wxEVT_COMMAND_CHOICE_SELECTED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandListboxSelected, wxEVT_COMMAND_LISTBOX_SELECTED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandListboxDoubleclicked, wxEVT_COMMAND_LISTBOX_DOUBLECLICKED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandChecklistboxToggled, wxEVT_COMMAND_CHECKLISTBOX_TOGGLED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandMenuSelected, wxEVT_COMMAND_MENU_SELECTED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandSliderUpdated, wxEVT_COMMAND_SLIDER_UPDATED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandRadioboxSelected, wxEVT_COMMAND_RADIOBOX_SELECTED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandRadiobuttonSelected, wxEVT_COMMAND_RADIOBUTTON_SELECTED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandScrollbarUpdated, wxEVT_COMMAND_SCROLLBAR_UPDATED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandVlboxSelected, wxEVT_COMMAND_VLBOX_SELECTED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandComboboxSelected, wxEVT_COMMAND_COMBOBOX_SELECTED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandToolRclicked, wxEVT_COMMAND_TOOL_RCLICKED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandToolEnter, wxEVT_COMMAND_TOOL_ENTER);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandSpinctrlUpdated, wxEVT_COMMAND_SPINCTRL_UPDATED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandLeftClick, wxEVT_COMMAND_LEFT_CLICK);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandLeftDclick, wxEVT_COMMAND_LEFT_DCLICK);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandRightClick, wxEVT_COMMAND_RIGHT_CLICK);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandRightDclick, wxEVT_COMMAND_RIGHT_DCLICK);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandSetFocus, wxEVT_COMMAND_SET_FOCUS);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandKillFocus, wxEVT_COMMAND_KILL_FOCUS);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandEnter, wxEVT_COMMAND_ENTER);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandTextUpdated, wxEVT_COMMAND_TEXT_UPDATED);
ACDK_DEFINE_WX_EVENT(CommandEvent, EvtCommandToggleButtonClicked, wxEVT_COMMAND_TOGGLEBUTTON_CLICKED);

ACDK_DEFINE_WX_EVENT(CloseEvent, EvtCloseWindow, wxEVT_CLOSE_WINDOW);
ACDK_DEFINE_WX_EVENT(CloseEvent, EvtEndSession, wxEVT_END_SESSION);
ACDK_DEFINE_WX_EVENT(CloseEvent, EvtQueryEndSession, wxEVT_QUERY_END_SESSION);

ACDK_DEFINE_WX_EVENT(MoveEvent, EvtMove, wxEVT_MOVE);

ACDK_DEFINE_WX_EVENT(SizeEvent, EvtSize, wxEVT_SIZE);

ACDK_DEFINE_WX_EVENT(ActivateEvent, EvtActivate, wxEVT_ACTIVATE);
ACDK_DEFINE_WX_EVENT(ActivateEvent, EvtActivateApp, wxEVT_ACTIVATE_APP);


ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollTop, wxEVT_SCROLL_TOP);
ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollBottom, wxEVT_SCROLL_BOTTOM);
ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollLineup, wxEVT_SCROLL_LINEUP);
ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollLinedown, wxEVT_SCROLL_LINEDOWN);
ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollPageup, wxEVT_SCROLL_PAGEUP);
ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollPagedown, wxEVT_SCROLL_PAGEDOWN);
ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollThumbtrack, wxEVT_SCROLL_THUMBTRACK);
ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollThumbrelease, wxEVT_SCROLL_THUMBRELEASE);
ACDK_DEFINE_WX_EVENT(ScrollEvent, EvtScrollEndscroll, wxEVT_SCROLL_ENDSCROLL);

ACDK_DEFINE_WX_EVENT(ScrollWinEvent, EvtScrollwinTop, wxEVT_SCROLLWIN_TOP);
ACDK_DEFINE_WX_EVENT(ScrollWinEvent, EvtScrollwinBottom, wxEVT_SCROLLWIN_BOTTOM);
ACDK_DEFINE_WX_EVENT(ScrollWinEvent, EvtScrollwinLineup, wxEVT_SCROLLWIN_LINEUP);
ACDK_DEFINE_WX_EVENT(ScrollWinEvent, EvtScrollwinLinedown, wxEVT_SCROLLWIN_LINEDOWN);
ACDK_DEFINE_WX_EVENT(ScrollWinEvent, EvtScrollwinPageup, wxEVT_SCROLLWIN_PAGEUP);
ACDK_DEFINE_WX_EVENT(ScrollWinEvent, EvtScrollwinPagedown, wxEVT_SCROLLWIN_PAGEDOWN);
ACDK_DEFINE_WX_EVENT(ScrollWinEvent, EvtScrollwinThumbtrack, wxEVT_SCROLLWIN_THUMBTRACK);
ACDK_DEFINE_WX_EVENT(ScrollWinEvent, EvtScrollwinThumbrelease, wxEVT_SCROLLWIN_THUMBRELEASE);


ACDK_DEFINE_WX_EVENT(KeyEvent, EvtChar, wxEVT_CHAR);
ACDK_DEFINE_WX_EVENT(KeyEvent, EvtCharHook, wxEVT_CHAR_HOOK);
ACDK_DEFINE_WX_EVENT(KeyEvent, EvtNavigationKey, wxEVT_NAVIGATION_KEY);
ACDK_DEFINE_WX_EVENT(KeyEvent, EvtKeyDown, wxEVT_KEY_DOWN);
ACDK_DEFINE_WX_EVENT(KeyEvent, EvtKeyUp, wxEVT_KEY_UP);


ACDK_DEFINE_WX_EVENT(MouseEvent, EvtLeftDown, wxEVT_LEFT_DOWN);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtLeftUp, wxEVT_LEFT_UP);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtMiddleDown, wxEVT_MIDDLE_DOWN);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtMiddleUp, wxEVT_MIDDLE_UP);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtRightDown, wxEVT_RIGHT_DOWN);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtRightUp, wxEVT_RIGHT_UP);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtMotion, wxEVT_MOTION);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtEnterWindow, wxEVT_ENTER_WINDOW);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtLeaveWindow, wxEVT_LEAVE_WINDOW);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtLeftDclick, wxEVT_LEFT_DCLICK);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtMiddleDclick, wxEVT_MIDDLE_DCLICK);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtRightDclick, wxEVT_RIGHT_DCLICK);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtMousewheel, wxEVT_MOUSEWHEEL);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcLeftDown, wxEVT_NC_LEFT_DOWN);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcLeftUp, wxEVT_NC_LEFT_UP);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcMiddleDown, wxEVT_NC_MIDDLE_DOWN);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcMiddleUp, wxEVT_NC_MIDDLE_UP);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcRightDown, wxEVT_NC_RIGHT_DOWN);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcRightUp, wxEVT_NC_RIGHT_UP);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcMotion, wxEVT_NC_MOTION);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcEnterWindow, wxEVT_NC_ENTER_WINDOW);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcLeaveWindow, wxEVT_NC_LEAVE_WINDOW);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcLeftDclick, wxEVT_NC_LEFT_DCLICK);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcMiddleDclick, wxEVT_NC_MIDDLE_DCLICK);
ACDK_DEFINE_WX_EVENT(MouseEvent, EvtNcRightDclick, wxEVT_NC_RIGHT_DCLICK);

ACDK_DEFINE_WX_EVENT(SetCursorEvent, EvtSetCursor, wxEVT_SET_CURSOR);

ACDK_DEFINE_WX_EVENT(PaintEvent, EvtPaint, wxEVT_PAINT);
ACDK_DEFINE_WX_EVENT(PaintEvent, EvtNcPaint, wxEVT_NC_PAINT);
ACDK_DEFINE_WX_EVENT(PaintEvent, EvtPaintIcon, wxEVT_PAINT_ICON);

ACDK_DEFINE_WX_EVENT(EraseEvent, EvtEraseBackground, wxEVT_ERASE_BACKGROUND);

ACDK_DEFINE_WX_EVENT(FocusEvent, EvtSetFocus, wxEVT_SET_FOCUS);
ACDK_DEFINE_WX_EVENT(FocusEvent, EvtKillFocus, wxEVT_KILL_FOCUS);
ACDK_DEFINE_WX_EVENT(FocusEvent, EvtChildFocus, wxEVT_CHILD_FOCUS);

ACDK_DEFINE_WX_EVENT(IdleEvent, EvtIdle, wxEVT_IDLE);
ACDK_DEFINE_WX_EVENT(UpdateUIEvent, EvtUpdateUi, wxEVT_UPDATE_UI);

ACDK_DEFINE_WX_EVENT(HelpEvent, EvtHelp, wxEVT_HELP);

ACDK_DEFINE_WX_EVENT(InitDialogEvent, EvtInitDialog, wxEVT_INIT_DIALOG);


//static 
bool 
UpdateUIEvent::canUpdate(IN(RWindow) win) 
{ 
  return wxUpdateUIEvent::CanUpdate(CLS2WXPTR(win)); 
}

typedef acdk::lang::sys::core_flathashmap<int, UserEventType> EventTypeMap;

EventTypeMap&
getWxToAcdkEventMap()
{
  static EventTypeMap _eventMap;
  return _eventMap;
}



typedef acdk::lang::sys::core_vector<UserEventType> UserEventVec;

UserEventVec&
getUserEventTable()
{
  static UserEventVec userEventVec;
  return userEventVec;
}



//foreign
void
Event::registerUserEvent(int et, CreateEventFunc func)
{
  //UserEventVec& uevv = getUserEventTable();
  //uevv.push_back(UserEventType(et, func));
  EventTypeMap& etm = getWxToAcdkEventMap();
  etm.put(et, UserEventType(et, func));
}

REvent
getUserEvent(int et, wxEvent& event)
{
  /*
  UserEventVec& uevv = getUserEventTable();
  UserEventVec::iterator it = uevv.begin();
  UserEventVec::iterator end = uevv.end();
  for (; it < end; ++it)
  {
    if (it->eventType == et)
      return it->createEvent(event);
  }*/
  EventTypeMap& etm = getWxToAcdkEventMap();
  EventTypeMap::iterator it = etm.get(et);
  if (it == etm.end())
    return Nil;
  return (*it).second.createEvent(event);
  //return Nil;
}

REvent
EvtHandler::getEvent(wxEvent& event)
{
  REvent ev = getUserEvent(event.GetEventType(), event);
  if (ev != Nil)
      return ev;
  return new Event(event);
  /*
  //setUpEventTable();
  int evtype = event.GetEventType();
  if (evtype < 10000)
    return new Event(event);
  evtype -= 10000;
  EventType etype = wxEvent2EventType[evtype];
  if (etype < 0 || etype >= EvtMaxPredefined || event2EventCreator[etype] == 0)
  {
    REvent ev = getUserEvent(event.GetEventType(), event);
    if (ev != Nil)
      return ev;
    return new Event(event);
  }
  return event2EventCreator[etype](event);
  */
}

void EventCracker::evtDispatch(wxEvent& ev) // ### dead code
{
  //Event tev(&ev, false);
  (_targetObj->*(_targetFunc))(EvtHandler::getEvent(ev));
}

//static
//EventDispatcher EventDispatcher::eventDispatcher;
EventDispatcher  globalEventDispatcher;

void
EvtHandler::connect(int eventType, int id, ObjectEventFunction oef, IN(RObject) userData)
{
  Window* winptr;
  if (id == -1 && (winptr = dynamic_cast<Window*>(this)) != 0)
    id = winptr->getId();
  registerId(id);

  WXDOUT("connect: et=" << eventType << "; id: " << id);

  _dispatcher.push_back(EventCracker(this, oef, eventType, id));
  EventCracker& evc = _dispatcher.back();
  EventDispatcherArg* eda = new EventDispatcherArg(this, oef);
  evc._eda = eda;
  //int wxevent = eventToWxEvent(eventType);
  getWx()->Connect(id, eventType, (wxObjectEventFunction)&wxWindowFwd::onEvent, eda);
}

void
EvtHandler::connect(int eventType, int id, IN(acdk::lang::dmi::RDmiDelegate) del)
{

  Window* winptr;
  if (id == -1 && (winptr = dynamic_cast<Window*>(this)) != 0)
    id = winptr->getId();
  registerId(id);
  _dispatcher.push_back(EventCracker(del, eventType, id));
  EventCracker& evc = _dispatcher.back();
  EventDispatcherArg* eda = new EventDispatcherArg(del);
  evc._eda = eda;
  //int wxevent = eventToWxEvent(eventType);
  WXDOUT("connect: et=" << eventType << "; id: " << id);
  getWx()->Connect(id, eventType, (wxObjectEventFunction)&wxWindowFwd::onEvent, eda);
}

void
EvtHandler::connectToThis(int et, int id, IN(RString) method)
{
  connect(et, id, new acdk::lang::dmi::StdDmiDelegate(this, method));
}

void
EvtHandler::connectToClass(int et, int id, IN(RString) className, IN(RString) method)
{
  connect(et, id, new acdk::lang::dmi::StdDmiDelegate(Class::forName(className), method));
}


void
EvtHandler::disconnectAll()
{
  REvtHandler keepAlive = this;
  wxEvtHandler* wx = getWx();
  for (EventCrackerVec::iterator it = _dispatcher.begin(); it < _dispatcher.end(); ++it)
  {
    WXDOUT("disconnect: et=" << it->_eventType << "; id: " << it->_id);
    //(*it)._targetObj = Nil;
    //(*it)._userData = Nil;
    wx->Disconnect(it->_id, it->_id, it->_eventType, (wxObjectEventFunction)&wxWindowFwd::onEvent);
    delete it->_eda;
  }
  _dispatcher = EventCrackerVec();
}

void
EventDispatcher::OnEvent(wxEvent& event)
{
  if (event.m_callbackUserData == 0)
  {
    WXDOUT("event.m_callbackUserData == 0" << event.GetEventType());
    return;
  }
  EventDispatcherArg* eda = dynamic_cast<EventDispatcherArg*>(event.m_callbackUserData);
  if (eda == 0)
  {
    WXDOUT("EventDispatcherArg == 0" << event.GetEventType());
    return;
  }
  try {
    Event tev(&event);
    WxObject* objptr = &eda->_targetObj;
    (objptr->*(eda->_targetFunc))(&tev);
  } catch (RThrowable ex) {
    acdk::lang::System::err->println("Caught Exception in OnEvent: " + ex->getMessage());
  }
}

acdk::lang::sys::core_vector<int>&
getIdVec()
{
  static acdk::lang::sys::core_vector<int> ids;
  return ids;
}

//static
int
EvtHandler::getFreeId()
{
  int i;
  acdk::lang::sys::core_vector<int>& vec = getIdVec();
  for (i = 1; vec.find(i) != vec.end(); ++i)
    ;
  //registerId(i);
  return i;
}

//static
void
EvtHandler::registerId(int id)
{
  getIdVec().push_back(id);
}


} // wx
} // acdk


