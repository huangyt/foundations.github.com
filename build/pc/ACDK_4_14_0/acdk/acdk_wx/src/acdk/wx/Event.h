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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Event.h,v 1.25 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Event_h
#define acdk_wx_Event_h

#include "Config.h"
#include <acdk/lang/dmi/DmiDelegate.h>
#include "WxObject.h"
#include "Structs.h"
#include "ClientData.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(Window);

#if 0
  uncommented events are currently not mapped
    //EvtNull  , //= wxEVT_NULL,
    EvtFirst  , //= wxEVT_FIRST, 
    EvtUserFirst  , //= wxEVT_USER_FIRST, 
    /*
    EvtCommandButtonClicked  , //= wxEVT_COMMAND_BUTTON_CLICKED, 
    EvtCommandCheckboxClicked  , //= wxEVT_COMMAND_CHECKBOX_CLICKED, 
    EvtCommandChoiceSelected  , //= wxEVT_COMMAND_CHOICE_SELECTED, 
    EvtCommandListboxSelected  , //= wxEVT_COMMAND_LISTBOX_SELECTED, 
    EvtCommandListboxDoubleclicked  , //= wxEVT_COMMAND_LISTBOX_DOUBLECLICKED, 
    EvtCommandChecklistboxToggled  , //= wxEVT_COMMAND_CHECKLISTBOX_TOGGLED, 
    EvtCommandMenuSelected  , //= wxEVT_COMMAND_MENU_SELECTED, 
    EvtCommandSliderUpdated  , //= wxEVT_COMMAND_SLIDER_UPDATED, 
    EvtCommandRadioboxSelected  , //= wxEVT_COMMAND_RADIOBOX_SELECTED, 
    EvtCommandRadiobuttonSelected  , //= wxEVT_COMMAND_RADIOBUTTON_SELECTED, 
    EvtCommandScrollbarUpdated  , //= wxEVT_COMMAND_SCROLLBAR_UPDATED, 
    EvtCommandVlboxSelected  , //= wxEVT_COMMAND_VLBOX_SELECTED, 
    EvtCommandComboboxSelected  , //= wxEVT_COMMAND_COMBOBOX_SELECTED, 
    EvtCommandToolRclicked  , //= wxEVT_COMMAND_TOOL_RCLICKED, 
    EvtCommandToolEnter  , //= wxEVT_COMMAND_TOOL_ENTER, 
    EvtCommandSpinctrlUpdated  , //= wxEVT_COMMAND_SPINCTRL_UPDATED, 
    */
    EvtSocket  , //= wxEVT_SOCKET, 
    //EvtTimer   , //= wxEVT_TIMER , 
    /*
    EvtLeftDown  , //= wxEVT_LEFT_DOWN, 
    EvtLeftUp  , //= wxEVT_LEFT_UP, 
    EvtMiddleDown  , //= wxEVT_MIDDLE_DOWN, 
    EvtMiddleUp  , //= wxEVT_MIDDLE_UP, 
    EvtRightDown  , //= wxEVT_RIGHT_DOWN, 
    EvtRightUp  , //= wxEVT_RIGHT_UP, 
    EvtMotion  , //= wxEVT_MOTION, 
    EvtEnterWindow  , //= wxEVT_ENTER_WINDOW, 
    EvtLeaveWindow  , //= wxEVT_LEAVE_WINDOW, 
    EvtLeftDclick  , //= wxEVT_LEFT_DCLICK, 
    EvtMiddleDclick  , //= wxEVT_MIDDLE_DCLICK, 
    EvtRightDclick  , //= wxEVT_RIGHT_DCLICK, 
    EvtSetFocus  , //= wxEVT_SET_FOCUS, 
    EvtKillFocus  , //= wxEVT_KILL_FOCUS, 
    EvtChildFocus  , //= wxEVT_CHILD_FOCUS, 
    EvtMousewheel  , //= wxEVT_MOUSEWHEEL, 
    EvtNcLeftDown  , //= wxEVT_NC_LEFT_DOWN, 
    EvtNcLeftUp  , //= wxEVT_NC_LEFT_UP, 
    EvtNcMiddleDown  , //= wxEVT_NC_MIDDLE_DOWN, 
    EvtNcMiddleUp  , //= wxEVT_NC_MIDDLE_UP, 
    EvtNcRightDown  , //= wxEVT_NC_RIGHT_DOWN, 
    EvtNcRightUp  , //= wxEVT_NC_RIGHT_UP, 
    EvtNcMotion  , //= wxEVT_NC_MOTION, 
    EvtNcEnterWindow  , //= wxEVT_NC_ENTER_WINDOW, 
    EvtNcLeaveWindow  , //= wxEVT_NC_LEAVE_WINDOW, 
    EvtNcLeftDclick  , //= wxEVT_NC_LEFT_DCLICK, 
    EvtNcMiddleDclick  , //= wxEVT_NC_MIDDLE_DCLICK, 
    EvtNcRightDclick  , //= wxEVT_NC_RIGHT_DCLICK, 
    */
    /*
    EvtChar  , //= wxEVT_CHAR, 
    EvtCharHook  , //= wxEVT_CHAR_HOOK, 
    EvtNavigationKey  , //= wxEVT_NAVIGATION_KEY, 
    EvtKeyDown  , //= wxEVT_KEY_DOWN, 
    EvtKeyUp  , //= wxEVT_KEY_UP, 
    EvtSetCursor  , //= wxEVT_SET_CURSOR, 
    */
    /*
    EvtScrollTop  , //= wxEVT_SCROLL_TOP, 
    EvtScrollBottom  , //= wxEVT_SCROLL_BOTTOM, 
    EvtScrollLineup  , //= wxEVT_SCROLL_LINEUP, 
    EvtScrollLinedown  , //= wxEVT_SCROLL_LINEDOWN, 
    EvtScrollPageup  , //= wxEVT_SCROLL_PAGEUP, 
    EvtScrollPagedown  , //= wxEVT_SCROLL_PAGEDOWN, 
    EvtScrollThumbtrack  , //= wxEVT_SCROLL_THUMBTRACK, 
    EvtScrollThumbrelease  , //= wxEVT_SCROLL_THUMBRELEASE, 
    EvtScrollEndscroll  , //= wxEVT_SCROLL_ENDSCROLL, 
    EvtScrollwinTop  , //= wxEVT_SCROLLWIN_TOP, 
    EvtScrollwinBottom  , //= wxEVT_SCROLLWIN_BOTTOM, 
    EvtScrollwinLineup  , //= wxEVT_SCROLLWIN_LINEUP, 
    EvtScrollwinLinedown  , //= wxEVT_SCROLLWIN_LINEDOWN, 
    EvtScrollwinPageup  , //= wxEVT_SCROLLWIN_PAGEUP, 
    EvtScrollwinPagedown  , //= wxEVT_SCROLLWIN_PAGEDOWN, 
    EvtScrollwinThumbtrack  , //= wxEVT_SCROLLWIN_THUMBTRACK, 
    EvtScrollwinThumbrelease  , //= wxEVT_SCROLLWIN_THUMBRELEASE, 
    */
    //EvtSize  , //= wxEVT_SIZE, 
    //EvtMove  , //= wxEVT_MOVE, 
    /*
    EvtCloseWindow  , //= wxEVT_CLOSE_WINDOW, 
    EvtEndSession  , //= wxEVT_END_SESSION, 
    EvtQueryEndSession  , //= wxEVT_QUERY_END_SESSION, 
    */
    //EvtActivateApp  , //= wxEVT_ACTIVATE_APP, 
    EvtPower  , //= wxEVT_POWER, 
    //EvtActivate  , //= wxEVT_ACTIVATE, 
    EvtCreate  , //= wxEVT_CREATE, 
    EvtDestroy  , //= wxEVT_DESTROY, 
    EvtShow  , //= wxEVT_SHOW, 
    EvtIconize  , //= wxEVT_ICONIZE, 
    EvtMaximize  , //= wxEVT_MAXIMIZE, 
    EvtMouseCaptureChanged  , //= wxEVT_MOUSE_CAPTURE_CHANGED, 
    /*
    EvtPaint  , //= wxEVT_PAINT, 
    EvtEraseBackground  , //= wxEVT_ERASE_BACKGROUND, 
    EvtNcPaint  , //= wxEVT_NC_PAINT, 
    EvtPaintIcon  , //= wxEVT_PAINT_ICON, 
    */
    EvtMenuOpen  , //= wxEVT_MENU_OPEN, 
    EvtMenuClose  , //= wxEVT_MENU_CLOSE, 
    EvtMenuHighlight  , //= wxEVT_MENU_HIGHLIGHT, 

    EvtContextMenu  , //= wxEVT_CONTEXT_MENU, 
    EvtSysColourChanged  , //= wxEVT_SYS_COLOUR_CHANGED, 
    EvtDisplayChanged  , //= wxEVT_DISPLAY_CHANGED, 
    EvtSettingChanged  , //= wxEVT_SETTING_CHANGED, 
    EvtQueryNewPalette  , //= wxEVT_QUERY_NEW_PALETTE, 
    EvtPaletteChanged  , //= wxEVT_PALETTE_CHANGED, 
    EvtJoyButtonDown  , //= wxEVT_JOY_BUTTON_DOWN, 
    EvtJoyButtonUp  , //= wxEVT_JOY_BUTTON_UP, 
    EvtJoyMove  , //= wxEVT_JOY_MOVE, 
    EvtJoyZmove  , //= wxEVT_JOY_ZMOVE, 
    // only on windows, not used EvtDropFiles  , //= wxEVT_DROP_FILES, 
    EvtDrawItem  , //= wxEVT_DRAW_ITEM, 
    EvtMeasureItem  , //= wxEVT_MEASURE_ITEM, 
    EvtCompareItem  , //= wxEVT_COMPARE_ITEM, 
    //EvtInitDialog  , //= wxEVT_INIT_DIALOG, 
    //EvtIdle  , //= wxEVT_IDLE, 
    EvtUpdateUi  , //= wxEVT_UPDATE_UI, 
    /*
    EvtCommandLeftClick  , //= wxEVT_COMMAND_LEFT_CLICK, 
    EvtCommandLeftDclick  , //= wxEVT_COMMAND_LEFT_DCLICK, 
    EvtCommandRightClick  , //= wxEVT_COMMAND_RIGHT_CLICK, 
    EvtCommandRightDclick  , //= wxEVT_COMMAND_RIGHT_DCLICK, 
    EvtCommandSetFocus  , //= wxEVT_COMMAND_SET_FOCUS, 
    EvtCommandKillFocus  , //= wxEVT_COMMAND_KILL_FOCUS, 
    EvtCommandEnter  , //= wxEVT_COMMAND_ENTER, 
    */
    //EvtHelp  , ///* wxEVT_HELP*/ = 1050,
    EvtDetailedHelp,   ///* wxEVT_DETAILED_HELP*/ = 1051,    
    EvtMaxPredefined

#endif //0

#define ACDK_WX_STD_EVENT_MEMBERS(Class, Super) \
  ACDK_WX_STD_MEMBERS(Class, Super) \
  static REvent createEvent(wxEvent& wxevent) { return new Class((wx##Class&)wxevent); }

/// to use in cpp to register event
#define ACDK_DEFINE_WX_EVENT(Class, AcdkEvent, WxEvent) \
int Class::AcdkEvent = WxEvent; \
AutoUserEventRegister AcdkEvent##_reg(Class::AcdkEvent, Class::createEvent)

ACDK_DECL_CLASS(Event);

typedef REvent (*CreateEventFunc)(wxEvent& event);

foreign struct UserEventType
{
  int eventType;
  CreateEventFunc createEvent;
  UserEventType(int et, CreateEventFunc ce) : eventType(et), createEvent(ce) {}
};



/**
  see wxEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/

ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC Event
: extends WxObject
{
  ACDK_WITH_METAINFO(Event)
public:
  // wxEvent
  ACDK_WX_STD_EVENT_MEMBERS(Event, WxObject)
  // no constructors, because wxEvent is abstract
  void setEventType(int typ) { getWx()->SetEventType(typ); }
  int getEventType() { return getWx()->GetEventType(); }
  //RWxObject getEventObject() { return new WxObject(getWx()->GetEventObject(), false); }
  inline RWxObject getEventObject() { RETURN_WXPTR2CLS(WxObject, getWx()->GetEventObject()); }
  void setEventObject(IN(RWxObject) obj) { getWx()->SetEventObject(obj == Nil ? 0 : obj->getWx()); }
  int getTimestamp() { return getWx()->GetTimestamp(); }
  void setTimestamp(int ts = 0) { getWx()->SetTimestamp((long)ts); }
  int getId() { return getWx()->GetId(); }
  void setId(int id) { getWx()->SetId(id); }
  void skip(bool doskip = true) { getWx()->Skip(doskip); }
  bool getSkipped() { return getWx()->GetSkipped(); };
  bool isCommandEvent() { return getWx()->IsCommandEvent(); }
  foreign static void registerUserEvent(int et, CreateEventFunc func);
  static int EvtNull;
};

foreign struct AutoUserEventRegister
{
  AutoUserEventRegister(int et, CreateEventFunc ce) 
  {
    Event::registerUserEvent(et, ce);
  }
};


ACDK_DECL_CLASS(CloseEvent);

/**
  see wxCloseEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/

ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC CloseEvent
: extends Event
{
  ACDK_WITH_METAINFO(CloseEvent)
public:
  ACDK_WX_STD_EVENT_MEMBERS(CloseEvent, Event)
  
  //bool CanVeto();
  inline bool canVeto() { return getWx()->CanVeto(); }
  //bool GetLoggingOff() const;
  inline bool getLoggingOff() const { return getWx()->GetLoggingOff(); }
  /* 
  not supported by wxWidgets
  //bool GetSessionEnding() const;
  inline bool getSessionEnding() const { return getWx()->GetSessionEnding(); }
  //bool GetForce() const;
  inline bool getForce() const { return getWx()->GetForce(); }
  //void SetForce(bool force) const;
  inline void setForce(bool force) const { getWx()->SetForce(force); }
  */
  
  //void SetCanVeto(bool canVeto);
  inline void setCanVeto(bool canVeto) { getWx()->SetCanVeto(canVeto); }
  //void SetLoggingOff(bool loggingOff) const;
  inline void setLoggingOff(bool loggingOff)  { getWx()->SetLoggingOff(loggingOff); }
  //void Veto(bool veto = TRUE);
  inline void veto(bool veto = true) { getWx()->Veto(veto); }
  static int EvtCloseWindow;
  static int EvtEndSession;
  static int EvtQueryEndSession;
};

ACDK_DECL_CLASS(MoveEvent);

/**
  see wxMoveEvent, wxEVT_MOVE
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC MoveEvent
: extends Event
{
  ACDK_WITH_METAINFO(MoveEvent)
public:
  // wxMoveEvent
  ACDK_WX_STD_EVENT_MEMBERS(MoveEvent, Event)
  RPoint getPosition() 
  { 
    wxPoint pt = getWx()->GetPosition();
    return new Point(pt.x, pt.y); 
  }
  static int EvtMove;
};

ACDK_DECL_CLASS(SizeEvent);
/**
  see wxSizeEvent, wxEVT_SIZE
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC SizeEvent
: extends Event
{
  ACDK_WITH_METAINFO(SizeEvent)
public:
  // wxSizeEvent
  ACDK_WX_STD_EVENT_MEMBERS(SizeEvent, Event)
  //wxSize GetSize() const { return m_size; }
  inline RSize getSize() const { return WXVAL2CLS(Size, getWx()->GetSize()); }
  static int EvtSize;
};

ACDK_DECL_CLASS(CommandEvent);

/**
  see wxCommandEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC CommandEvent
: extends Event
{
  ACDK_WITH_METAINFO(CommandEvent)
public:
  // wxCommandEvent
  ACDK_WX_STD_EVENT_MEMBERS(CommandEvent, Event)

  CommandEvent(int commandType = EvtNull, int id = 0)
  : Event(new wxCommandEvent(commandType, id))
  {
  }
    //  int GetSelection() const { return m_commandInt; }
    int getSelection() const { return getWx()->GetSelection(); }
  
  
  //    void SetString(const wxString& s) { m_commandString = s; }
  void setString(IN(RString) s) { getWx()->SetString(S2WXS(s)); }
  //    wxString GetString() const { return m_commandString; }
  //  wxString getString() const { return getWx()->GetString(); }
  RString getString() const { return WXS2S(getWx()->GetString()); }
  // Get checkbox value
  //    bool IsChecked() const { return m_commandInt != 0; }
  bool isChecked() const { return getWx()->IsChecked(); }
  
  // TRUE if the listbox event was a selection.
  //    bool IsSelection() const { return (m_extraLong != 0); }
  bool isSelection() const { return getWx()->IsSelection(); }
  
  //    void SetExtraLong(long extraLong) { m_extraLong = extraLong; }
  void setExtraLong(int extraLong) { getWx()->SetExtraLong(extraLong); }
  //    long GetExtraLong() const { return m_extraLong ; }
  int getExtraLong() const { return getWx()->GetExtraLong(); }
  
  //    void SetInt(int i) { m_commandInt = i; }
  void setInt(int i) { getWx()->SetInt(i); }
  //    long GetInt() const { return m_commandInt ; }
  int getInt() const { return getWx()->GetInt(); }
  static int EvtCommandButtonClicked;
  static int EvtCommandCheckboxClicked;
  static int EvtCommandChoiceSelected;
  static int EvtCommandListboxSelected;
  static int EvtCommandListboxDoubleclicked;
  static int EvtCommandChecklistboxToggled;
  static int EvtCommandMenuSelected;
  static int EvtCommandSliderUpdated;
  static int EvtCommandRadioboxSelected;
  static int EvtCommandRadiobuttonSelected;
  static int EvtCommandScrollbarUpdated;
  static int EvtCommandVlboxSelected;
  static int EvtCommandComboboxSelected;
  static int EvtCommandToolRclicked;
  static int EvtCommandToolEnter;
  static int EvtCommandSpinctrlUpdated;
  static int EvtCommandLeftClick;
  static int EvtCommandLeftDclick;
  static int EvtCommandRightClick;
  static int EvtCommandRightDclick;
  static int EvtCommandSetFocus;
  static int EvtCommandKillFocus;
  static int EvtCommandEnter;
  static int EvtCommandTextUpdated;
   // see in NoteBook.cpp
  static int EvtCommandToggleButtonClicked;

  
};


ACDK_DECL_CLASS(ActivateEvent);

/**
  see wxActivateEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC ActivateEvent
: extends Event
{
  ACDK_WITH_METAINFO(ActivateEvent)
public:
  // wxActivateEvent
  ACDK_WX_STD_EVENT_MEMBERS(ActivateEvent, Event)

  ActivateEvent(int commandType = EvtNull, bool active = true, int id = 0)
  : Event(new wxActivateEvent(commandType, active, id))
  {
  }
  //bool GetActive() const;
  inline bool getActive() const { return getWx()->GetActive(); }
  static int EvtActivate;
  static int EvtActivateApp;
};



ACDK_DECL_CLASS(NotifyEvent);

/**
  see wxNotifyEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
class ACDK_WX_PUBLIC NotifyEvent
: extends CommandEvent
{
  ACDK_WITH_METAINFO(NotifyEvent)
public:
  ACDK_WX_STD_EVENT_MEMBERS(NotifyEvent, CommandEvent)

  NotifyEvent(int commandType = EvtNull, int id = 0)
    : CommandEvent(new wxNotifyEvent(commandType, id))
  {
  }
    // veto the operation (usually it's allowed by default)
    //void Veto() { m_bAllow = FALSE; }
  inline void veto() { getWx()->Veto(); }

    // allow the operation if it was disabled by default
    //void Allow() { m_bAllow = TRUE; }
  inline void allow() { getWx()->Allow(); }

    // for implementation code only: is the operation allowed?
    //bool IsAllowed() const { return m_bAllow; }
  inline bool isAllowed() const { return getWx()->IsAllowed(); }

  inline virtual RObject clone() const { RETURN_WXPTR2CLS(Event, getWx()->Clone()); }

};

ACDK_DECL_CLASS(ScrollEvent);

/*
 wxEVT_SCROLL_TOP
 wxEVT_SCROLL_BOTTOM
 wxEVT_SCROLL_LINEUP
 wxEVT_SCROLL_LINEDOWN
 wxEVT_SCROLL_PAGEUP
 wxEVT_SCROLL_PAGEDOWN
 wxEVT_SCROLL_THUMBTRACK
 wxEVT_SCROLL_THUMBRELEASE
 wxEVT_SCROLL_ENDSCROLL
*/
/**
  see wxScrollEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
class ACDK_WX_PUBLIC ScrollEvent
: extends CommandEvent
{
  ACDK_WITH_METAINFO(ScrollEvent)
public:
  ACDK_WX_STD_EVENT_MEMBERS(ScrollEvent, CommandEvent)

  ScrollEvent(int commandType = EvtNull, int id = 0, int pos = 0, int orient = 0)
    : CommandEvent(new wxScrollEvent(commandType, id, pos, orient))
  {
  }
  inline virtual RObject clone() const { RETURN_WXPTR2CLS(ScrollEvent, (wxScrollEvent*)getWx()->Clone()); }
  //int GetOrientation() const { return (int) m_extraLong ; }
  inline int getOrientation() const { return getWx()->GetOrientation(); }
  //int GetPosition() const { return m_commandInt ; }
  inline int getPosition() const { return getWx()->GetPosition(); }
  //void SetOrientation(int orient) { m_extraLong = (long) orient; }
  inline void setOrientation(int orient) { getWx()->SetOrientation(orient); }
  //void SetPosition(int pos) { m_commandInt = pos; }
  inline void setPosition(int pos) { getWx()->SetPosition(pos); }

  static int EvtScrollTop;
  static int EvtScrollBottom;
  static int EvtScrollLineup;
  static int EvtScrollLinedown;
  static int EvtScrollPageup;
  static int EvtScrollPagedown;
  static int EvtScrollThumbtrack;
  static int EvtScrollThumbrelease;
  static int EvtScrollEndscroll;
  
   
};


ACDK_DECL_CLASS(ScrollWinEvent);
/*
 wxEVT_SCROLLWIN_TOP
 wxEVT_SCROLLWIN_BOTTOM
 wxEVT_SCROLLWIN_LINEUP
 wxEVT_SCROLLWIN_LINEDOWN
 wxEVT_SCROLLWIN_PAGEUP
 wxEVT_SCROLLWIN_PAGEDOWN
 wxEVT_SCROLLWIN_THUMBTRACK
 wxEVT_SCROLLWIN_THUMBRELEASE
*/
/**
  see wxScrollWinEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC ScrollWinEvent
: extends Event
{
  ACDK_WITH_METAINFO(ScrollWinEvent)
public:
  // wxScrollWinEvent
  ACDK_WX_STD_EVENT_MEMBERS(ScrollWinEvent, Event)
  ScrollWinEvent(int commandType = EvtNull, int pos = 0, int orient = 0)
    : Event(new wxScrollWinEvent(commandType, pos, orient))
  {
  }
  //int GetOrientation() const { return (int) m_extraLong ; }
  inline int getOrientation() const { return getWx()->GetOrientation(); }
  //int GetPosition() const { return m_commandInt ; }
  inline int getPosition() const { return getWx()->GetPosition(); }
  //void SetOrientation(int orient) { m_extraLong = (long) orient; }
  inline void setOrientation(int orient) { getWx()->SetOrientation(orient); }
  //void SetPosition(int pos) { m_commandInt = pos; }
  inline void setPosition(int pos) { getWx()->SetPosition(pos); }
  
  static int EvtScrollwinTop;
  static int EvtScrollwinBottom;
  static int EvtScrollwinLineup;
  static int EvtScrollwinLinedown;
  static int EvtScrollwinPageup;
  static int EvtScrollwinPagedown;
  static int EvtScrollwinThumbtrack;
  static int EvtScrollwinThumbrelease;
};


/// Virtual keycodes
enum KeyCode
{
  KC_BACK    = WXK_BACK   ,  // WXK_BACK    =    8,
    KC_TAB     = WXK_TAB    ,  // WXK_TAB     =    9,
    KC_RETURN  = WXK_RETURN ,  // WXK_RETURN  =    13,
    KC_ESCAPE  = WXK_ESCAPE ,  // WXK_ESCAPE  =    27,
    KC_SPACE   = WXK_SPACE  ,  // WXK_SPACE   =    32,
    KC_DELETE  = WXK_DELETE ,  // WXK_DELETE  =    127,
    
    KC_START   = WXK_START  ,  // WXK_START   = 300,
    KC_LBUTTON = WXK_LBUTTON,  // WXK_LBUTTON,
    KC_RBUTTON = WXK_RBUTTON,  // WXK_RBUTTON,
    KC_CANCEL = WXK_CANCEL,  // WXK_CANCEL,
    KC_MBUTTON = WXK_MBUTTON,  // WXK_MBUTTON,
    KC_CLEAR = WXK_CLEAR,  // WXK_CLEAR,
    KC_SHIFT = WXK_SHIFT,  // WXK_SHIFT,
    KC_ALT = WXK_ALT,  // WXK_ALT,
    KC_CONTROL = WXK_CONTROL,  // WXK_CONTROL,
    KC_MENU = WXK_MENU,  // WXK_MENU,
    KC_PAUSE = WXK_PAUSE,  // WXK_PAUSE,
    KC_CAPITAL = WXK_CAPITAL,  // WXK_CAPITAL,
    KC_PRIOR = WXK_PRIOR,  // WXK_PRIOR,  // Page up
    KC_NEXT = WXK_NEXT,  // WXK_NEXT,   // Page down
    KC_END = WXK_END,  // WXK_END,
    KC_HOME = WXK_HOME,  // WXK_HOME,
    KC_LEFT = WXK_LEFT,  // WXK_LEFT,
    KC_UP = WXK_UP,  // WXK_UP,
    KC_RIGHT = WXK_RIGHT,  // WXK_RIGHT,
    KC_DOWN = WXK_DOWN,  // WXK_DOWN,
    KC_SELECT = WXK_SELECT,  // WXK_SELECT,
    KC_PRINT = WXK_PRINT,  // WXK_PRINT,
    KC_EXECUTE = WXK_EXECUTE,  // WXK_EXECUTE,
    KC_SNAPSHOT = WXK_SNAPSHOT,  // WXK_SNAPSHOT,
    KC_INSERT = WXK_INSERT,  // WXK_INSERT,
    KC_HELP = WXK_HELP,  // WXK_HELP,
    KC_NUMPAD0 = WXK_NUMPAD0,  // WXK_NUMPAD0,
    KC_NUMPAD1 = WXK_NUMPAD1,  // WXK_NUMPAD1,
    KC_NUMPAD2 = WXK_NUMPAD2,  // WXK_NUMPAD2,
    KC_NUMPAD3 = WXK_NUMPAD3,  // WXK_NUMPAD3,
    KC_NUMPAD4 = WXK_NUMPAD4,  // WXK_NUMPAD4,
    KC_NUMPAD5 = WXK_NUMPAD5,  // WXK_NUMPAD5,
    KC_NUMPAD6 = WXK_NUMPAD6,  // WXK_NUMPAD6,
    KC_NUMPAD7 = WXK_NUMPAD7,  // WXK_NUMPAD7,
    KC_NUMPAD8 = WXK_NUMPAD8,  // WXK_NUMPAD8,
    KC_NUMPAD9 = WXK_NUMPAD9,  // WXK_NUMPAD9,
    KC_MULTIPLY = WXK_MULTIPLY,  // WXK_MULTIPLY,
    KC_ADD = WXK_ADD,  // WXK_ADD,
    KC_SEPARATOR = WXK_SEPARATOR,  // WXK_SEPARATOR,
    KC_SUBTRACT = WXK_SUBTRACT,  // WXK_SUBTRACT,
    KC_DECIMAL = WXK_DECIMAL,  // WXK_DECIMAL,
    KC_DIVIDE = WXK_DIVIDE,  // WXK_DIVIDE,
    KC_F1 = WXK_F1,  // WXK_F1,
    KC_F2 = WXK_F2,  // WXK_F2,
    KC_F3 = WXK_F3,  // WXK_F3,
    KC_F4 = WXK_F4,  // WXK_F4,
    KC_F5 = WXK_F5,  // WXK_F5,
    KC_F6 = WXK_F6,  // WXK_F6,
    KC_F7 = WXK_F7,  // WXK_F7,
    KC_F8 = WXK_F8,  // WXK_F8,
    KC_F9 = WXK_F9,  // WXK_F9,
    KC_F10 = WXK_F10,  // WXK_F10,
    KC_F11 = WXK_F11,  // WXK_F11,
    KC_F12 = WXK_F12,  // WXK_F12,
    KC_F13 = WXK_F13,  // WXK_F13,
    KC_F14 = WXK_F14,  // WXK_F14,
    KC_F15 = WXK_F15,  // WXK_F15,
    KC_F16 = WXK_F16,  // WXK_F16,
    KC_F17 = WXK_F17,  // WXK_F17,
    KC_F18 = WXK_F18,  // WXK_F18,
    KC_F19 = WXK_F19,  // WXK_F19,
    KC_F20 = WXK_F20,  // WXK_F20,
    KC_F21 = WXK_F21,  // WXK_F21,
    KC_F22 = WXK_F22,  // WXK_F22,
    KC_F23 = WXK_F23,  // WXK_F23,
    KC_F24 = WXK_F24,  // WXK_F24,
    KC_NUMLOCK = WXK_NUMLOCK,  // WXK_NUMLOCK,
    KC_SCROLL = WXK_SCROLL,  // WXK_SCROLL,
    KC_PAGEUP = WXK_PAGEUP,  // WXK_PAGEUP,
    KC_PAGEDOWN = WXK_PAGEDOWN,  // WXK_PAGEDOWN,
    
    KC_NUMPAD_SPACE = WXK_NUMPAD_SPACE,  // WXK_NUMPAD_SPACE,
    KC_NUMPAD_TAB = WXK_NUMPAD_TAB,  // WXK_NUMPAD_TAB,
    KC_NUMPAD_ENTER = WXK_NUMPAD_ENTER,  // WXK_NUMPAD_ENTER,
    KC_NUMPAD_F1 = WXK_NUMPAD_F1,  // WXK_NUMPAD_F1,
    KC_NUMPAD_F2 = WXK_NUMPAD_F2,  // WXK_NUMPAD_F2,
    KC_NUMPAD_F3 = WXK_NUMPAD_F3,  // WXK_NUMPAD_F3,
    KC_NUMPAD_F4 = WXK_NUMPAD_F4,  // WXK_NUMPAD_F4,
    KC_NUMPAD_HOME = WXK_NUMPAD_HOME,  // WXK_NUMPAD_HOME,
    KC_NUMPAD_LEFT = WXK_NUMPAD_LEFT,  // WXK_NUMPAD_LEFT,
    KC_NUMPAD_UP = WXK_NUMPAD_UP,  // WXK_NUMPAD_UP,
    KC_NUMPAD_RIGHT = WXK_NUMPAD_RIGHT,  // WXK_NUMPAD_RIGHT,
    KC_NUMPAD_DOWN = WXK_NUMPAD_DOWN,  // WXK_NUMPAD_DOWN,
    KC_NUMPAD_PRIOR = WXK_NUMPAD_PRIOR,  // WXK_NUMPAD_PRIOR,
    KC_NUMPAD_PAGEUP = WXK_NUMPAD_PAGEUP,  // WXK_NUMPAD_PAGEUP,
    KC_NUMPAD_NEXT = WXK_NUMPAD_NEXT,  // WXK_NUMPAD_NEXT,
    KC_NUMPAD_PAGEDOWN = WXK_NUMPAD_PAGEDOWN,  // WXK_NUMPAD_PAGEDOWN,
    KC_NUMPAD_END = WXK_NUMPAD_END,  // WXK_NUMPAD_END,
    KC_NUMPAD_BEGIN = WXK_NUMPAD_BEGIN,  // WXK_NUMPAD_BEGIN,
    KC_NUMPAD_INSERT = WXK_NUMPAD_INSERT,  // WXK_NUMPAD_INSERT,
    KC_NUMPAD_DELETE = WXK_NUMPAD_DELETE,  // WXK_NUMPAD_DELETE,
    KC_NUMPAD_EQUAL = WXK_NUMPAD_EQUAL,  // WXK_NUMPAD_EQUAL,
    KC_NUMPAD_MULTIPLY = WXK_NUMPAD_MULTIPLY,  // WXK_NUMPAD_MULTIPLY,
    KC_NUMPAD_ADD = WXK_NUMPAD_ADD,  // WXK_NUMPAD_ADD,
    KC_NUMPAD_SEPARATOR = WXK_NUMPAD_SEPARATOR,  // WXK_NUMPAD_SEPARATOR,
    KC_NUMPAD_SUBTRACT = WXK_NUMPAD_SUBTRACT,  // WXK_NUMPAD_SUBTRACT,
    KC_NUMPAD_DECIMAL = WXK_NUMPAD_DECIMAL,  // WXK_NUMPAD_DECIMAL,
    KC_NUMPAD_DIVIDE = WXK_NUMPAD_DIVIDE,  // WXK_NUMPAD_DIVIDE
    KC_A = 'A',
    KC_B = 'B',
    KC_C = 'C',
    KC_D = 'D',
    KC_E = 'E',
    KC_F = 'F',
    KC_G = 'G',
    KC_H = 'H',
    KC_I = 'I',
    KC_J = 'K',
    KC_L = 'L',
    KC_M = 'M',
    KC_N = 'N',
    KC_O = 'O',
    KC_P = 'P',
    KC_Q = 'Q',
    KC_R = 'R',
    KC_S = 'S',
    KC_T = 'T',
    KC_U = 'U',
    KC_V = 'V',
    KC_W = 'W',
    KC_X = 'X',
    KC_Y = 'Y',
    KC_Z = 'Z'
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, KeyCode);

ACDK_DECL_CLASS(KeyEvent);

/**
  see wxKeyEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC KeyEvent
: extends Event
{
  ACDK_WITH_METAINFO(KeyEvent)
public:
  ACDK_WX_STD_EVENT_MEMBERS(KeyEvent, Event)
  // wxKeyEvent
//  bool ControlDown() const { return m_controlDown; }
  bool controlDown() const { return getWx()->ControlDown(); }
//    bool MetaDown() const { return m_metaDown; }
  bool metaDown() const { return getWx()->MetaDown(); }
//    bool AltDown() const { return m_altDown; }
  bool altDown() const { return getWx()->AltDown(); }
//    bool ShiftDown() const { return m_shiftDown; }
  bool shiftDown() const { return getWx()->ShiftDown(); }

    // exclude MetaDown() from HasModifiers() because NumLock under X is often
    // configured as mod2 modifier, yet the key events even when it is pressed
    // should be processed normally, not like Ctrl- or Alt-key
//    bool HasModifiers() const { return ControlDown() || AltDown(); }
  bool hasModifiers() const { return getWx()->HasModifiers(); }

    // get the key code: an ASCII7 char or an element of wxKeyCode enum
//    int GetKeyCode() const { return (int)m_keyCode; }
  int getKeyCode() const { return getWx()->GetKeyCode(); }

    // get the raw key code (platform-dependent)
//    wxUint32 GetRawKeyCode() const { return m_rawCode; }
  int getRawKeyCode() const { return getWx()->GetRawKeyCode(); }

    // get the raw key flags (platform-dependent)
//    wxUint32 GetRawKeyFlags() const { return m_rawFlags; }
  int getRawKeyFlags() const { return getWx()->GetRawKeyFlags(); }

    // Find the position of the event
//    void GetPosition(int *xpos, int *ypos) const
  void getPosition(OUT(int) xpos, OUT(int) ypos) { getWx()->GetPosition(&xpos, &ypos); }
    
//    wxPoint GetPosition() const
  RPoint getPosition() const { return new Point(getWx()->GetPosition()); }

    // Get X position
//    int GetX() const { return m_x; }
  int getX() const { return getWx()->GetX(); }

    // Get Y position
//    int GetY() const { return m_y; }
  int getY() const { return getWx()->GetY(); }

    // deprecated
//    long KeyCode() const { return m_keyCode; }
  int keyCode() const 
    { 
#if ACDK_CHECK_WX_VERSION(2, 5)
      return getWx()->GetKeyCode(); 
#else
      return getWx()->KeyCode(); 
#endif
    }  

  static int EvtChar;
  static int EvtCharHook;
  static int EvtNavigationKey;
  static int EvtKeyDown;
  static int EvtKeyUp;

  
};

/**
  see wxMouseEvent, MouseEvent

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
enum MouseMotion
{
  MouseBtnAny     = wxMOUSE_BTN_ANY    ,  // wxMOUSE_BTN_ANY     = -1,
  MouseBtnNone    = wxMOUSE_BTN_NONE   ,  // wxMOUSE_BTN_NONE    = -1,
  MouseBtnLeft    = wxMOUSE_BTN_LEFT   ,  // wxMOUSE_BTN_LEFT    = 0,
  MouseBtnMiddle  = wxMOUSE_BTN_MIDDLE ,  // wxMOUSE_BTN_MIDDLE  = 1,
  MouseBtnRight   = wxMOUSE_BTN_RIGHT    // wxMOUSE_BTN_RIGHT   = 2
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, MouseMotion);

ACDK_DECL_CLASS(MouseEvent);
/*
 wxEVT_LEFT_DOWN
 wxEVT_LEFT_UP
 wxEVT_MIDDLE_DOWN
 wxEVT_MIDDLE_UP
 wxEVT_RIGHT_DOWN
 wxEVT_RIGHT_UP
 wxEVT_MOTION
 wxEVT_ENTER_WINDOW
 wxEVT_LEAVE_WINDOW
 wxEVT_LEFT_DCLICK
 wxEVT_MIDDLE_DCLICK
 wxEVT_RIGHT_DCLICK
 wxEVT_NC_LEFT_DOWN
 wxEVT_NC_LEFT_UP,
 wxEVT_NC_MIDDLE_DOWN,
 wxEVT_NC_MIDDLE_UP,
 wxEVT_NC_RIGHT_DOWN,
 wxEVT_NC_RIGHT_UP,
 wxEVT_NC_MOTION,
 wxEVT_NC_ENTER_WINDOW,
 wxEVT_NC_LEAVE_WINDOW,
 wxEVT_NC_LEFT_DCLICK,
 wxEVT_NC_MIDDLE_DCLICK,
 wxEVT_NC_RIGHT_DCLICK,
*/
/**
  see wxMouseEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
class ACDK_WX_PUBLIC MouseEvent
: extends Event
{
  ACDK_WITH_METAINFO(MouseEvent)
public:
  ACDK_WX_STD_EVENT_MEMBERS(MouseEvent, Event)

  MouseEvent(int commandType = EvtNull)
    : Event(new wxMouseEvent(commandType))
  {
  }
  inline virtual RObject clone() const { RETURN_WXPTR2CLS(MouseEvent, (wxMouseEvent*)getWx()->Clone()); }
   // Was it a button event? (*doesn't* mean: is any button *down*?)
  //bool IsButton() const { return Button(wxMOUSE_BTN_ANY); }
  inline bool isButton() const { return getWx()->IsButton(); }
  
  // Was it a down event from button 1, 2 or 3 or any?
  //bool ButtonDown(int but = wxMOUSE_BTN_ANY) const;
  inline bool buttonDown(int but = MouseBtnAny) const { return getWx()->ButtonDown(but); }
  
  // Was it a dclick event from button 1, 2 or 3 or any?
  //bool ButtonDClick(int but = wxMOUSE_BTN_ANY) const;
  inline bool buttonDClick(int but = MouseBtnAny) const { return getWx()->ButtonDClick(but); }
  
  // Was it a up event from button 1, 2 or 3 or any?
  //bool ButtonUp(int but = wxMOUSE_BTN_ANY) const;
  inline bool buttonUp(int but = MouseBtnAny) const { return getWx()->ButtonUp(but); }
  
  // Was the given button 1,2,3 or any changing state?
  //bool Button(int but) const;
  inline bool button(int but) const { return getWx()->Button(but); }
  
  // Was the given button 1,2,3 or any in Down state?
  //bool ButtonIsDown(int but) const;
  inline bool buttonIsDown(int but) const { return getWx()->ButtonIsDown(but); }
  
  // Get the button which is changing state (wxMOUSE_BTN_NONE if none)
  //int GetButton() const;
  inline int getButton() const { return getWx()->GetButton(); }
  
  // Find state of shift/control keys
  //bool ControlDown() const { return m_controlDown; }
  inline bool controlDown() const { return getWx()->ControlDown(); }
  //bool MetaDown() const { return m_metaDown; }
  inline bool metaDown() const { return getWx()->MetaDown(); }
  //bool AltDown() const { return m_altDown; }
  inline bool altDown() const { return getWx()->AltDown(); }
  //bool ShiftDown() const { return m_shiftDown; }
  inline bool shiftDown() const { return getWx()->ShiftDown(); }
  
  // Find which event was just generated
  //bool LeftDown() const { return (m_eventType == wxEVT_LEFT_DOWN); }
  inline bool leftDown() const { return getWx()->LeftDown(); }
  //bool MiddleDown() const { return (m_eventType == wxEVT_MIDDLE_DOWN); }
  inline bool middleDown() const { return getWx()->MiddleDown(); }
  //bool RightDown() const { return (m_eventType == wxEVT_RIGHT_DOWN); }
  inline bool rightDown() const { return getWx()->RightDown(); }
  
  //bool LeftUp() const { return (m_eventType == wxEVT_LEFT_UP); }
  inline bool leftUp() const { return getWx()->LeftUp(); }
  //bool MiddleUp() const { return (m_eventType == wxEVT_MIDDLE_UP); }
  inline bool middleUp() const { return getWx()->MiddleUp(); }
  //bool RightUp() const { return (m_eventType == wxEVT_RIGHT_UP); }
  inline bool rightUp() const { return getWx()->RightUp(); }
  
  //bool LeftDClick() const { return (m_eventType == wxEVT_LEFT_DCLICK); }
  inline bool leftDClick() const { return getWx()->LeftDClick(); }
  //bool MiddleDClick() const { return (m_eventType == wxEVT_MIDDLE_DCLICK); }
  inline bool middleDClick() const { return getWx()->MiddleDClick(); }
  //bool RightDClick() const { return (m_eventType == wxEVT_RIGHT_DCLICK); }
  inline bool rightDClick() const { return getWx()->RightDClick(); }
  
  // Find the current state of the mouse buttons (regardless
  // of current event type)
  //bool LeftIsDown() const { return m_leftDown; }
  inline bool leftIsDown() const { return getWx()->LeftIsDown(); }
  //bool MiddleIsDown() const { return m_middleDown; }
  inline bool middleIsDown() const { return getWx()->MiddleIsDown(); }
  //bool RightIsDown() const { return m_rightDown; }
  inline bool rightIsDown() const { return getWx()->RightIsDown(); }
  
  // True if a button is down and the mouse is moving
  //bool Dragging() const
  inline bool dragging() const { return getWx()->Dragging(); }
  
  // True if the mouse is moving, and no button is down
  //bool Moving() const { return (m_eventType == wxEVT_MOTION); }
  inline bool moving() const { return getWx()->Moving(); }
  
  // True if the mouse is just entering the window
  //bool Entering() const { return (m_eventType == wxEVT_ENTER_WINDOW); }
  inline bool entering() const { return getWx()->Entering(); }
  
  // True if the mouse is just leaving the window
  //bool Leaving() const { return (m_eventType == wxEVT_LEAVE_WINDOW); }
  inline bool leaving() const { return getWx()->Leaving(); }
  
  // Find the position of the event
  //void GetPosition(int xpos, int ypos) const;
  inline void getPosition(OUT(int) xpos, OUT(int) ypos) const { getWx()->GetPosition(&xpos, &ypos); }
  
  
  // Find the position of the event
  //wxPoint GetPosition() const { return wxPoint(m_x, m_y); }
  inline RPoint getPosition() const { return WXVAL2CLS(Point, getWx()->GetPosition()); }
  
  // Find the logical position of the event given the DC
  //wxPoint GetLogicalPosition(const wxDC& dc) const ;
  //### @todo inline RPoint getLogicalPosition(IN(RDC) dc) const { return WXVAL2CLS(Point, getWx()->GetLogicalPosition(CLS2WXREF(dc))); }
  
  // Get X position
  //int GetX() const { return m_x; }
  inline int getX() const { return getWx()->GetX(); }
  
  // Get Y position
  //int  GetY() const { return m_y; }
  inline int getY() const { return getWx()->GetY(); }
  
  // Get wheel rotation, positive or negative indicates direction of
  // rotation.  Current devices all send an event when rotation is equal to
  // +/-WheelDelta, but this allows for finer resolution devices to be
  // created in the future.  Because of this you shouldn't assume that one
  // event is equal to 1 line or whatever, but you should be able to either
  // do partial line scrolling or wait until +/-WheelDelta rotation values
  // have been accumulated before scrolling.
  //int GetWheelRotation() const { return m_wheelRotation; }
  inline int getWheelRotation() const { return getWx()->GetWheelRotation(); }
  
  // Get wheel delta, normally 120.  This is the threshold for action to be
  // taken, and one such action (for example, scrolling one increment)
  // should occur for each delta.
  //int GetWheelDelta() const { return m_wheelDelta; }
  inline int getWheelDelta() const { return getWx()->GetWheelDelta(); }
  
  // Returns the configured number of lines (or whatever) to be scrolled per
  // wheel action.  Defaults to one.
  //int GetLinesPerAction() const { return m_linesPerAction; }
  inline int getLinesPerAction() const { return getWx()->GetLinesPerAction(); }
  
  // Is the system set to do page scrolling?
  //bool IsPageScroll() const { return ((unsigned int)m_linesPerAction == UINT_MAX); }
  inline bool isPageScroll() const { return getWx()->IsPageScroll(); }
  
  inline void setX(int x) { getWx()->m_x = x; }
  inline void setY(int y) { getWx()->m_y = y; }
  inline void setLeftDown(bool down)  { getWx()->m_leftDown = down; }
  inline void setMiddleDown(bool down)  { getWx()->m_middleDown = down; }
  inline void setRightDown(bool down)  { getWx()->m_rightDown = down; }
  inline void setControlDown(bool down)  { getWx()->m_controlDown = down; }
  inline void setShiftDown(bool down)  { getWx()->m_shiftDown = down; }
  inline void setAltDown(bool down)  { getWx()->m_altDown = down; }
  inline void setMetaDown(bool down)  { getWx()->m_metaDown = down; }
  /* currently not supported
  int           m_wheelRotation;
    int           m_wheelDelta;
    int           m_linesPerAction;
  */
  static int EvtLeftDown;
  static int EvtLeftUp;
  static int EvtMiddleDown;
  static int EvtMiddleUp;
  static int EvtRightDown;
  static int EvtRightUp;
  static int EvtMotion;
  static int EvtEnterWindow;
  static int EvtLeaveWindow;
  static int EvtLeftDclick;
  static int EvtMiddleDclick;
  static int EvtRightDclick;
  static int EvtMousewheel;
  static int EvtNcLeftDown;
  static int EvtNcLeftUp;
  static int EvtNcMiddleDown;
  static int EvtNcMiddleUp;
  static int EvtNcRightDown;
  static int EvtNcRightUp;
  static int EvtNcMotion;
  static int EvtNcEnterWindow;
  static int EvtNcLeaveWindow;
  static int EvtNcLeftDclick;
  static int EvtNcMiddleDclick;
  static int EvtNcRightDclick;

  
};


ACDK_DECL_CLASS(SetCursorEvent);
/*
wxEVT_SET_CURSOR
*/
/**
  see wxSetCursorEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC SetCursorEvent
: extends Event
{
  ACDK_WITH_METAINFO(SetCursorEvent)
public:
  // wxSetCursorEvent
  ACDK_WX_STD_EVENT_MEMBERS(SetCursorEvent, Event)
  SetCursorEvent(int x = 0, int y = 0)
    : Event(new wxSetCursorEvent(x, y))
  {
  }
  inline virtual RObject clone() const { RETURN_WXPTR2CLS(SetCursorEvent, (wxSetCursorEvent*)getWx()->Clone()); }
  //int GetX() const { return m_x; }
  inline int getX() const { return getWx()->GetX(); }
  //int GetY() const { return m_y; }
  inline int getY() const { return getWx()->GetY(); }
  //void SetCursor(const wxCursor& cursor) { m_cursor = cursor; }
  //### @todo inline void setCursor(IN(RCursor) cursor) { getWx()->SetCursor(CLS2WXREF(cursor)); }
  //const wxCursor& GetCursor() const { return m_cursor; }
  //### @todo inline RCursor getCursor() const { return WXVAL2CLS(Cursor, getWx()->GetCursor()); }
  //bool HasCursor() const { return m_cursor.Ok(); }
  inline bool hasCursor() const { return getWx()->HasCursor(); }

  static int EvtSetCursor;
};


ACDK_DECL_CLASS(FocusEvent);

/**
  see wxFocusEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC FocusEvent
: extends Event
{
  ACDK_WITH_METAINFO(FocusEvent)
public:
  // wxFocusEvent
  ACDK_WX_STD_EVENT_MEMBERS(FocusEvent, Event)

  FocusEvent(int commandType = EvtNull, int id = 0)
  : Event(new wxFocusEvent(commandType,  id))
  {
  }
  static int EvtSetFocus;
  static int EvtKillFocus;
  static int EvtChildFocus; //???

};



ACDK_DECL_CLASS(IdleEvent);

/**
  see wxIdleEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC IdleEvent
: extends Event
{
  ACDK_WITH_METAINFO(IdleEvent)
public:
  // wxIdleEvent
  ACDK_WX_STD_EVENT_MEMBERS(IdleEvent, Event)

  IdleEvent()
  : Event(new wxIdleEvent())
  {
  }
  static int EvtIdle;
};

enum UpdateUIMode
{
        /// Send UI update events to all windows
    UpdateUiProcessAll = wxUPDATE_UI_PROCESS_ALL,  // wxUPDATE_UI_PROCESS_ALL,

        /// Send UI update events to windows that have
        /// the wxWS_EX_PROCESS_UI_UPDATES flag specified
    UpdateUiProcessSpecified = wxUPDATE_UI_PROCESS_SPECIFIED,  // wxUPDATE_UI_PROCESS_SPECIFIED
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, UpdateUIMode);

ACDK_DECL_CLASS(UpdateUIEvent);
/**
  see wxUpdateUIEvent
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC UpdateUIEvent
: extends CommandEvent
{
  ACDK_WITH_METAINFO(UpdateUIEvent)
public:
  // wxIdleEvent
  ACDK_WX_STD_EVENT_MEMBERS(UpdateUIEvent, CommandEvent)

  UpdateUIEvent(int windowId)
  : CommandEvent(new wxUpdateUIEvent(windowId))
  {
  }
  //bool GetChecked() const { return m_checked; }
  inline bool getChecked() const { return getWx()->GetChecked(); }
    //bool GetEnabled() const { return m_enabled; }
  inline bool getEnabled() const { return getWx()->GetEnabled(); }
    //wxString GetText() const { return m_text; }
  inline RString getText() const { return WXS2S(getWx()->GetText()); }
    //bool GetSetText() const { return m_setText; }
  inline bool getSetText() const { return getWx()->GetSetText(); }
    //bool GetSetChecked() const { return m_setChecked; }
  inline bool getSetChecked() const { return getWx()->GetSetChecked(); }
    //bool GetSetEnabled() const { return m_setEnabled; }
  inline bool getSetEnabled() const { return getWx()->GetSetEnabled(); }

    //void Check(bool check) { m_checked = check; m_setChecked = true; }
  inline void check(bool check) { getWx()->Check(check); }
    //void Enable(bool enable) { m_enabled = enable; m_setEnabled = true; }
  inline void enable(bool enable) { getWx()->Enable(enable); }
    //void SetText(const wxString& text) { m_text = text; m_setText = true; }
  inline void setText(IN(RString)  text) { getWx()->SetText(S2WXS(text)); }

    // Sets the interval between updates in milliseconds.
    // Set to -1 to disable updates, or to 0 to update as frequently as possible.
    //static void SetUpdateInterval(long updateInterval) { sm_updateInterval = updateInterval; }
  inline static void setUpdateInterval(int updateInterval) { wxUpdateUIEvent::SetUpdateInterval(updateInterval); }

    // Returns the current interval between updates in milliseconds
    //static long GetUpdateInterval() { return sm_updateInterval; }
  inline static int getUpdateInterval() { return wxUpdateUIEvent::GetUpdateInterval(); }

    // Can we update this window?
    //static bool CanUpdate(wxWindowBase *win);
  static bool canUpdate(IN(RWindow) win);

    // Reset the update time to provide a delay until the next
    // time we should update
    //static void ResetUpdateTime();
  inline static void resetUpdateTime() { wxUpdateUIEvent::ResetUpdateTime(); }

    // Specify how wxWidgets will send update events: to
    // all windows, or only to those which specify that they
    // will process the events.
    //static void SetMode(wxUpdateUIMode mode) { sm_updateMode = mode; }
  inline static void setMode(UpdateUIMode mode) { wxUpdateUIEvent::SetMode((wxUpdateUIMode)mode); }

    // Returns the UI update mode
    //static wxUpdateUIMode GetMode() { return sm_updateMode; }
  inline static UpdateUIMode getMode() { return (UpdateUIMode) wxUpdateUIEvent::GetMode(); }

  static int EvtUpdateUi;
};


ACDK_DECL_CLASS(HelpEvent);

/**
  see wxHelpEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC HelpEvent
: extends CommandEvent
{
  ACDK_WITH_METAINFO(HelpEvent)
public:
  // wxHelpEvent
  ACDK_WX_STD_EVENT_MEMBERS(HelpEvent, CommandEvent)

  HelpEvent(int eventType = EvtNull, int id = 0, IN(RPoint) point = Point::defaultPosition())
  : CommandEvent(new wxHelpEvent(eventType, id, CLS2WXREF(point)))
  {
  }
  //const wxPoint& GetPosition() const;
  inline RPoint getPosition() const { return WXVAL2CLS(Point, getWx()->GetPosition()); }
  //void SetPosition(const wxPoint& pt);
  inline void setPosition(IN(RPoint) pt) { getWx()->SetPosition(CLS2WXREF(pt)); }

  static int EvtHelp;
};

ACDK_DECL_CLASS(InitDialogEvent);

/**
  see wxInitDialogEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/ 
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC InitDialogEvent
: extends Event
{
  ACDK_WITH_METAINFO(InitDialogEvent)
public:
  // wxInitDialogEvent
  ACDK_WX_STD_EVENT_MEMBERS(InitDialogEvent, Event)

  InitDialogEvent()
  : Event(new wxInitDialogEvent())
  {
  }
  static int EvtInitDialog;
};



typedef void (WxObject::*ObjectEventFunction)(IN(REvent) );

class EventDispatcherArg;

/**
  internal to crack/dispatch events from wxWidgets to acdk_wx
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/
foreign
class ACDK_WX_PUBLIC EventCracker
: public wxEvtHandler
{
public:
  WxObject* _targetObj;
  ObjectEventFunction _targetFunc;
  int _eventType;
  int _id;
  EventDispatcherArg* _eda;
  acdk::lang::dmi::RDmiDelegate _dmiDelegate;
  EventCracker(WxObject* targetobj, ObjectEventFunction targetfunc, int evtType, int id)
  : _targetObj(targetobj)
  , _targetFunc(targetfunc)
  , _eventType(evtType)
  , _id(id)
  , _eda(0)
  {
  }
  EventCracker(IN(acdk::lang::dmi::RDmiDelegate) dmiDelegate, int evtType, int id)
  : _targetObj(0)
  , _targetFunc(0)
  , _eventType(evtType)
  , _id(id)
  , _eda(0)
  , _dmiDelegate(dmiDelegate)
  {
  }
  EventCracker(const EventCracker& other)
    : _targetObj(other._targetObj)
    , _targetFunc(other._targetFunc)
    , _eventType(other._eventType)
    , _id(other._id)
    , _eda(other._eda)
    , _dmiDelegate(other._dmiDelegate)
  {
  }
  /// call the event method of the target object
  void evtDispatch(wxEvent& ev);
};


/**
  internal to crack/dispatch events from wxWidgets to acdk_wx
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/
foreign
class ACDK_WX_PUBLIC EventDispatcher
: public wxEvtHandler
{
public:
  EventDispatcher() {}
  //virtual bool ProcessEvent(wxEvent& event);
  void OnEvent(wxEvent& event);
  
};

foreign
class ACDK_WX_PUBLIC EventDispatcherArg
: public wxObject
{
public:
  RWxObject _targetObj;
  foreign ObjectEventFunction _targetFunc;
  RObject _userData;
  acdk::lang::dmi::RDmiDelegate _dmiDelegate;

  EventDispatcherArg(WxObject* targetobj, ObjectEventFunction targetfunc)
  : _targetObj(targetobj)
  , _targetFunc(targetfunc)
  {
  }
  EventDispatcherArg(IN(acdk::lang::dmi::RDmiDelegate) dmiDelegate)
  : _targetObj(Nil)
  , _targetFunc(0)
  , _dmiDelegate(dmiDelegate)
  {
  }
  EventDispatcherArg(const EventDispatcherArg& other)
  : _targetObj(other._targetObj)
  , _targetFunc(other._targetFunc)
  , _dmiDelegate(other._dmiDelegate)
  {
  }
};


extern EventDispatcher globalEventDispatcher;



ACDK_DECL_CLASS(EvtHandler);

/**
  see wxEvtHandler
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.25 $
  @date $Date: 2005/02/05 10:45:34 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC EvtHandler
: extends WxObject
{
  ACDK_WITH_METAINFO(EvtHandler)

  //wxEvtHandler* _wxEvtHandler;
  typedef acdk::lang::sys::core_vector<EventCracker> EventCrackerVec;
  foreign EventCrackerVec _dispatcher;
public:
  ACDK_WX_STD_MEMBERS(EvtHandler, WxObject)
  EvtHandler() : WxObject(new wxEvtHandler()) {}
  foreign void connect(int id, int eventType, wxObjectEventFunction func, wxObject *userData = 0)
  {
    getWx()->Connect(id, eventType, func, userData);
  }
  foreign void connect(int et, int id, ObjectEventFunction oef, IN(RObject) userData = Nil);
  void connect(int et, int id, IN(acdk::lang::dmi::RDmiDelegate) del);
  void connectToThis(int et, int id, IN(RString) method);
  void connectToClass(int et, int id, IN(RString) className, IN(RString) method);
  void disconnectAll();
  
  //wxEvtHandler *GetNextHandler() const { return m_nextHandler; }
  inline REvtHandler getNextHandler() const { RETURN_WXPTR2CLS(EvtHandler, getWx()->GetNextHandler()); }
  //wxEvtHandler *GetPreviousHandler() const { return m_previousHandler; }
  inline REvtHandler getPreviousHandler() const { RETURN_WXPTR2CLS(EvtHandler, getWx()->GetPreviousHandler()); }
    //void SetNextHandler(wxEvtHandler *handler) { m_nextHandler = handler; }
  inline void setNextHandler(IN(REvtHandler) handler) { getWx()->SetNextHandler(CLS2WXPTR(handler)); }
    //void SetPreviousHandler(wxEvtHandler *handler) { m_previousHandler = handler; }
  inline void setPreviousHandler(IN(REvtHandler) handler) { getWx()->SetPreviousHandler(CLS2WXPTR(handler)); }

    //void SetEvtHandlerEnabled(bool enabled) { m_enabled = enabled; }
  inline void setEvtHandlerEnabled(bool enabled) { getWx()->SetEvtHandlerEnabled(enabled); }
    //bool GetEvtHandlerEnabled() const { return m_enabled; }
  inline bool getEvtHandlerEnabled() const { return getWx()->GetEvtHandlerEnabled(); }

    // process an event right now
    //virtual bool ProcessEvent(wxEvent& event);
  inline virtual bool processEvent(IN(REvent) event) { return getWx()->ProcessEvent(CLS2WXREF(event)); }

    // add an event to be processed later
  //void AddPendingEvent(wxEvent& event);
  inline void addPendingEvent(IN(REvent) event) { getWx()->AddPendingEvent(CLS2WXREF(event)); }

    // process all pending events
    //void ProcessPendingEvents();
  inline void processPendingEvents() { getWx()->ProcessPendingEvents(); }

  //bool ProcessThreadEvent(wxEvent& event);
  inline bool processThreadEvent(IN(REvent) event) { return getWx()->ProcessThreadEvent(CLS2WXREF(event)); }

    // User data can be associated with each wxEvtHandler
  //void SetClientObject( wxClientData *data ) { DoSetClientObject(data); }
  inline void setClientObject(IN(RObject) data) 
  { 
    wxClientData* wxcd = 0;
    if (data != Nil)
      wxcd = new wxAcdkClientData(data);
    getWx()->SetClientObject(wxcd); 
  }
  //wxClientData *GetClientObject() const { return DoGetClientObject(); }
  inline RObject getClientObject() const 
  { 
    wxClientData* wxcd = getWx()->GetClientObject(); 
    if (wxcd == 0)
      return Nil;
    wxAcdkClientData* awxcd = dynamic_cast<wxAcdkClientData*>(wxcd);
    if (awxcd == 0)
      return Nil;
    return awxcd->GetData();
  }

  static REvent getEvent(wxEvent& event);
  static int getFreeId();
  static void registerId(int id);
};



} // wx
} // acdk

#endif //acdk_wx_Event_h
