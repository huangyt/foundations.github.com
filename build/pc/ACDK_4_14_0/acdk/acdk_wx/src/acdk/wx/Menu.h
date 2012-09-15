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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Menu.h,v 1.11 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Menu_h
#define acdk_wx_Menu_h

#include "MenuItem.h"
#include "Event.h"
#include "WindowStyle.h"
namespace acdk {
namespace wx {
  
ACDK_DECL_CLASS(Window);
ACDK_DECL_CLASS(EvtHandler);
ACDK_DECL_CLASS(MenuBar);

enum ItemKind;

ACDK_DECL_CLASS(Menu);

/**
  see wxMenu
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Menu
: extends EvtHandler
{
  ACDK_WITH_METAINFO(Menu)
public:
  //wxMenu
  ACDK_WX_STD_MEMBERS(Menu, EvtHandler)

  Menu(int style = 0) : EvtHandler(new wxMenu((long)style)) {}
  Menu(IN(RString) name, int style) : EvtHandler(new wxMenu(S2WXS(name), (long)style)) {}
  
  void append(int id, IN(RString) label, IN(RString) help = "", ItemKind kind = ItemNormal)
  {
    getWx()->Append(id, S2WXS(label), S2WXS(help), (wxItemKind)kind);
  }
  /** doesn't seem to work */
  void append(IN(RString) label, IN(acdk::lang::dmi::RDmiDelegate) delegate, IN(RString) help = "", ItemKind kind = ItemNormal)
  {
    int id = getFreeId();
    getWx()->Append(id, S2WXS(label), S2WXS(help), (wxItemKind)kind);
    connect(CommandEvent::EvtCommandMenuSelected, id, delegate);
  }
  void appendSeparator()
  {
    getWx()->AppendSeparator();
  }
  // append a check item
    //void AppendCheckItem(int id,const wxString& text, const wxString& help = wxEmptyString);
  inline void appendCheckItem(int id, IN(RString)  text, IN(RString)  help = "") { getWx()->AppendCheckItem(id, S2WXS(text), S2WXS(help)); }


    // append a radio item
    //void AppendRadioItem(int id, const wxString& text, const wxString& help = wxEmptyString);
  inline void appendRadioItem(int id, IN(RString)  text, IN(RString)  help = "") { getWx()->AppendRadioItem(id, S2WXS(text), S2WXS(help)); }
    
    // append a submenu
    //void Append(int id, const wxString& text, wxMenu *submenu, const wxString& help = wxEmptyString);
  inline void append(int id, IN(RString)  text, IN(RMenu) submenu, IN(RString)  help = "") { getWx()->Append(id, S2WXS(text), CLS2WXPTR(submenu), S2WXS(help)); }

    // the most generic form of Append() - append anything
    //void Append(wxMenuItem *item) { DoAppend(item); }
  inline void append(IN(RMenuItem) item) { item->ownsWxObject(false); getWx()->Append(CLS2WXPTR(item)); }

    // insert a break in the menu (only works when appending the items, not
    // inserting them)
    //virtual void Break() { }
  inline virtual void insertBreak() { getWx()->Break(); }

    // insert an item before given position
    //bool Insert(size_t pos, wxMenuItem *item);
  inline bool insert(int pos, IN(RMenuItem) item) { item->ownsWxObject(false); return getWx()->Insert(pos, CLS2WXPTR(item)); }

    // insert an item before given position
    //void Insert(size_t pos, int id, const wxString& text, const wxString& help = wxEmptyString, wxItemKind kind = wxITEM_NORMAL);
  inline void insert(int pos, int id, IN(RString)  text, IN(RString)  help = "", ItemKind kind = ItemNormal) { getWx()->Insert(pos, id, S2WXS(text), S2WXS(help), (wxItemKind)kind); }

    // insert a separator
    //void InsertSeparator(size_t pos);
  inline void insertSeparator(int pos) { getWx()->InsertSeparator(pos); }

    // insert a check item
    //void InsertCheckItem(size_t pos, int id, const wxString& text, const wxString& help = wxEmptyString);
  inline void insertCheckItem(int pos, int id, IN(RString)  text, IN(RString)  help = "") { getWx()->InsertCheckItem(pos, id, S2WXS(text), S2WXS(help)); }

    // insert a radio item
    //void InsertRadioItem(size_t pos, int id, const wxString& text, const wxString& help = wxEmptyString);
  inline void insertRadioItem(int pos, int id, IN(RString)  text, IN(RString)  help = "") { getWx()->InsertRadioItem(pos, id, S2WXS(text), S2WXS(help)); }

    // insert a submenu
    //void Insert(size_t pos, int id, const wxString& text, wxMenu *submenu, const wxString& help = wxEmptyString);
  inline void insert(int pos, int id, IN(RString)  text, IN(RMenu) submenu, IN(RString)  help = "") { getWx()->Insert(pos, id, S2WXS(text), CLS2WXPTR(submenu), S2WXS(help)); }

    // prepend an item to the menu
    //void Prepend(wxMenuItem *item);
  inline void prepend(IN(RMenuItem) item) { item->ownsWxObject(false); getWx()->Prepend(CLS2WXPTR(item)); }

    // prepend any item to the menu
    //void Prepend(int id, const wxString& text, const wxString& help = wxEmptyString, wxItemKind kind = wxITEM_NORMAL);
  inline void prepend(int id, IN(RString)  text, IN(RString)  help = "", ItemKind kind = ItemNormal) { getWx()->Prepend(id, S2WXS(text), S2WXS(help), (wxItemKind)kind); }

    // prepend a separator
    //void PrependSeparator();
    inline void prependSeparator() { getWx()->PrependSeparator(); }

    // prepend a check item
    //void PrependCheckItem(int id, const wxString& text, const wxString& help = wxEmptyString);
    inline void prependCheckItem(int id, IN(RString)  text, IN(RString)  help = "") { getWx()->PrependCheckItem(id, S2WXS(text), S2WXS(help)); }

    // prepend a radio item
    //void PrependRadioItem(int id, const wxString& text, const wxString& help = wxEmptyString);
    inline void prependRadioItem(int id, IN(RString)  text, IN(RString)  help = "") { getWx()->PrependRadioItem(id, S2WXS(text), S2WXS(help)); }
    // prepend a submenu
    //void Prepend(int id, const wxString& text, wxMenu *submenu, const wxString& help = wxEmptyString);
    inline void prepend(int id, IN(RString)  text, IN(RMenu) submenu, IN(RString)  help = "") 
    { 
      submenu->ownsWxObject(false); 
      getWx()->Prepend(id, S2WXS(text), CLS2WXPTR(submenu), S2WXS(help)); 
    }

    // detach an item from the menu, but don't delete it so that it can be
    // added back later (but if it's not, the caller is responsible for
    // deleting it!)
    //wxMenuItem *Remove(int id) { return Remove(FindChildItem(id)); }
    inline RMenuItem remove(int id) { RETURN_WXPTR2CLS(MenuItem, getWx()->Remove(id)); }
    //wxMenuItem *Remove(wxMenuItem *item);
    inline RMenuItem remove(IN(RMenuItem) item) { RETURN_WXPTR2CLS(MenuItem, getWx()->Remove(CLS2WXPTR(item))); }

    // delete an item from the menu (submenus are not destroyed by this
    // function, see Destroy)
    //bool Delete(int id) { return Delete(FindChildItem(id)); }
    inline bool deleteItem(int id) { return getWx()->Delete(id); }
    //bool Delete(wxMenuItem *item);
    inline bool deleteItem(IN(RMenuItem) item) { return getWx()->Delete(CLS2WXPTR(item)); }

    // delete the item from menu and destroy it (if it's a submenu)
    //bool Destroy(int id) { return Destroy(FindChildItem(id)); }
    inline bool destroy(int id) { return getWx()->Destroy(id); }
    //bool Destroy(wxMenuItem *item);
    inline bool destroy(IN(RMenuItem) item) { return getWx()->Destroy(CLS2WXPTR(item)); }

    // menu items access
    // -----------------

    // get the items
    //size_t GetMenuItemCount() const { return m_items.GetCount(); }
    inline int getMenuItemCount() const { return getWx()->GetMenuItemCount(); }

    /* ### TODO
    const wxMenuItemList& GetMenuItems() const { return m_items; }
    wxMenuItemList& GetMenuItems() { return m_items; }
    */
    // search
    //virtual int FindItem(const wxString& item) const;
    inline virtual int findItem(IN(RString)  item) const { return getWx()->FindItem(S2WXS(item)); }
    //wxMenuItem* FindItem(int id, wxMenu **menu = NULL) const;
    //## ??inline RMenuItem findItem(int id, wxMenu **, = NULL) const { RETURN_WXPTR2CLS(MenuItem, getWx()->FindItem(id, **, NULL)); }

    // get/set items attributes
    //void Enable(int id, bool enable);
    inline void enable(int id, bool enable) { getWx()->Enable(id, enable); }
    //bool IsEnabled(int id) const;
    inline bool isEnabled(int id) const { return getWx()->IsEnabled(id); }

    //void Check(int id, bool check);
    inline void check(int id, bool check) { getWx()->Check(id, check); }
    //bool IsChecked(int id) const;
    inline bool isChecked(int id) const { return getWx()->IsChecked(id); }

    //void SetLabel(int id, const wxString& label);
    inline void setLabel(int id, IN(RString)  label) { getWx()->SetLabel(id, S2WXS(label)); }
    //wxString GetLabel(int id) const;
    inline RString getLabel(int id) const { return WXS2S(getWx()->GetLabel(id)); }

    //virtual void SetHelpString(int id, const wxString& helpString);
    inline virtual void setHelpString(int id, IN(RString)  helpString) { getWx()->SetHelpString(id, S2WXS(helpString)); }
    //virtual wxString GetHelpString(int id) const;
    inline virtual RString getHelpString(int id) const { return WXS2S(getWx()->GetHelpString(id)); }

    // misc accessors
    // --------------

    // the title
    //virtual void SetTitle(const wxString& title) { m_title = title; }
    inline virtual void setTitle(IN(RString)  title) { getWx()->SetTitle(S2WXS(title)); }
    //const wxString GetTitle() const { return m_title; }
    inline RString getTitle() const { return WXS2S(getWx()->GetTitle()); }

    // event handler
    //void SetEventHandler(wxEvtHandler *handler) { m_eventHandler = handler; }
    inline void setEventHandler(IN(REvtHandler) handler) { handler->ownsWxObject(false); getWx()->SetEventHandler(CLS2WXPTR(handler)); }
    //wxEvtHandler *GetEventHandler() const { return m_eventHandler; }
    inline REvtHandler getEventHandler() const { RETURN_WXPTR2CLS(EvtHandler, getWx()->GetEventHandler()); }

    // invoking window
    //void SetInvokingWindow(wxWindow *win) { m_invokingWindow = win; }
    inline void setInvokingWindow(IN(RWindow) win);
    //wxWindow *GetInvokingWindow() const { return m_invokingWindow; }
    inline RWindow getInvokingWindow() const;

    // style
    //long GetStyle() const { return m_style; }
    inline int getStyle() const { return getWx()->GetStyle(); }

    // implementation helpers
    // ----------------------

    // Updates the UI for a menu and all submenus recursively. source is the
    // object that has the update event handlers defined for it. If NULL, the
    // menu or associated window will be used.
    //void UpdateUI(wxEvtHandler* source = (wxEvtHandler*)NULL);
    //### TODO inline void updateUI(IN(REvtHandler) source = Nil) { getWx()->UpdateUI(CLS2WXPTR(source); }

    // get the menu bar this menu is attached to (may be NULL, always NULL for
    // popup menus)
    //wxMenuBar *GetMenuBar() const { return m_menuBar; }
    inline RMenuBar getMenuBar() const;

    // called when the menu is attached/detached to/from a menu bar
    //virtual void Attach(wxMenuBarBase *menubar);
    inline virtual void attach(IN(RMenuBar) menubar);
    //virtual void Detach();
    inline virtual void detach() { getWx()->Detach(); }

    // is the menu attached to a menu bar (or is it a popup one)?
    //bool IsAttached() const { return m_menuBar != NULL; }
    inline bool isAttached() const { return getWx()->IsAttached(); }

    // set/get the parent of this menu
    //void SetParent(wxMenu *parent) { m_menuParent = parent; }
    inline void setParent(IN(RMenu) parent) { getWx()->SetParent(CLS2WXPTR(parent)); }
    //wxMenu *GetParent() const { return m_menuParent; }
    inline RMenu getParent() const { RETURN_WXPTR2CLS(Menu, getWx()->GetParent()); }
};

inline RMenu 
MenuItem::getMenu() const { RETURN_WXPTR2CLS(Menu, getWx()->GetMenu()); }
  //void SetMenu(wxMenu* menu) { m_parentMenu = menu; }
inline void 
MenuItem::setMenu(IN(RMenu) menu) { getWx()->SetMenu(CLS2WXPTR(menu)); }
  

inline void 
MenuItem::setSubMenu(IN(RMenu) menu) { getWx()->SetSubMenu(CLS2WXPTR(menu)); }
  //wxMenu *GetSubMenu() const { return m_subMenu; }
inline RMenu 
MenuItem::getSubMenu() const { RETURN_WXPTR2CLS(Menu, getWx()->GetSubMenu()); }
  

} // wx
} // acdk

#endif //acdk_wx_Menu_h
