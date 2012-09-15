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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Window.h,v 1.19 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Window_h
#define acdk_wx_Window_h

#include "WxObject.h"
#include "Event.h"
#include "Menu.h"

#include "Colour.h"
#include "GDIObject.h"
#include "WindowStyle.h"
#include "Cursor.h"
#include "DropTarget.h"
#include "Font.h"
#include "Region.h"
#include "DC.h"

namespace acdk {
namespace wx {

enum HitTest;
enum Border;

ACDK_DECL_CLASS(Validator);
ACDK_DECL_CLASS(Sizer);

enum StdIds
{
    IdLowest  /* wxID_LOWEST*/ = 4999,
    IdOpen  /* wxID_OPEN*/,
    IdClose  /* wxID_CLOSE*/,
    IdNew  /* wxID_NEW*/,
    IdSave  /* wxID_SAVE*/,
    IdSaveas  /* wxID_SAVEAS*/,
    IdRevert  /* wxID_REVERT*/,
    IdExit  /* wxID_EXIT*/,
    IdUndo  /* wxID_UNDO*/,
    IdRedo  /* wxID_REDO*/,
    IdHelp  /* wxID_HELP*/,
    IdPrint  /* wxID_PRINT*/,
    IdPrintSetup  /* wxID_PRINT_SETUP*/,
    IdPreview  /* wxID_PREVIEW*/,
    IdAbout  /* wxID_ABOUT*/,
    IdHelpContents  /* wxID_HELP_CONTENTS*/,
    IdHelpCommands  /* wxID_HELP_COMMANDS*/,
    IdHelpProcedures  /* wxID_HELP_PROCEDURES*/,
    IdHelpContext  /* wxID_HELP_CONTEXT*/,
    IdCloseAll  /* wxID_CLOSE_ALL*/,
    IdCut  /* wxID_CUT*/ = 5030,
    IdCopy  /* wxID_COPY*/,
    IdPaste  /* wxID_PASTE*/,
    IdClear  /* wxID_CLEAR*/,
    IdFind  /* wxID_FIND*/,
    IdDuplicate  /* wxID_DUPLICATE*/,
    IdSelectall  /* wxID_SELECTALL*/,

    IdFile1  /* wxID_FILE1*/ = 5050,
    IdFile2  /* wxID_FILE2*/,
    IdFile3  /* wxID_FILE3*/,
    IdFile4  /* wxID_FILE4*/,
    IdFile5  /* wxID_FILE5*/,
    IdFile6  /* wxID_FILE6*/,
    IdFile7  /* wxID_FILE7*/,
    IdFile8  /* wxID_FILE8*/,
    IdFile9  /* wxID_FILE9*/,

    // Standard button IDs
    IdOk  /* wxID_OK*/ = 5100,
    IdCancel  /* wxID_CANCEL*/,
    IdApply  /* wxID_APPLY*/,
    IdYes  /* wxID_YES*/,
    IdNo  /* wxID_NO*/,
    IdStatic  /* wxID_STATIC*/,
    IdForward  /* wxID_FORWARD*/,
    IdBackward  /* wxID_BACKWARD*/,
    IdDefault  /* wxID_DEFAULT*/,
    IdMore  /* wxID_MORE*/,
    IdSetup  /* wxID_SETUP*/,
    IdReset  /* wxID_RESET*/,
    IdContextHelp  /* wxID_CONTEXT_HELP*/,
    IdYestoall  /* wxID_YESTOALL*/,
    IdNotoall  /* wxID_NOTOALL*/,
    IdAbort  /* wxID_ABORT*/,
    IdRetry  /* wxID_RETRY*/,
    IdIgnore  /* wxID_IGNORE*/,

    // System menu IDs (used by wxUniv):
    IdSystemMenu  /* wxID_SYSTEM_MENU*/ = 5200,
    IdCloseFrame  /* wxID_CLOSE_FRAME*/,
    IdMoveFrame  /* wxID_MOVE_FRAME*/,
    IdResizeFrame  /* wxID_RESIZE_FRAME*/,
    IdMaximizeFrame  /* wxID_MAXIMIZE_FRAME*/,
    IdIconizeFrame  /* wxID_ICONIZE_FRAME*/,
    IdRestoreFrame  /* wxID_RESTORE_FRAME*/,

    // IDs used by generic file dialog (13 consecutive starting from this value)
    IdFiledlgg  /* wxID_FILEDLGG*/ = 5900,

    IdHighest  /* wxID_HIGHEST*/ = 5999
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, StdIds);

enum MessageBoxFlags
{
  MbYes = wxYES,
  MbOk = wxOK,
  MbNo = wxNO,
  MbYesNo = wxYES_NO,
  MbCancel = wxCANCEL,
  MbYesDefault = wxYES_DEFAULT,
  MbNoDefault = wxNO_DEFAULT,
  MbIconExclamation = wxICON_EXCLAMATION,
  MbIconHand = wxICON_HAND,
  MbIconWarning = wxICON_WARNING,
  MbIconError = wxICON_ERROR,
  MbIconQuestion = wxICON_QUESTION,
  MbIconInformation = wxICON_INFORMATION,
  MbIconStop = wxICON_STOP,
  MbIconAsterix = wxICON_ASTERISK,
  MbIconMask = wxICON_MASK
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, MessageBoxFlags);

ACDK_DECL_CLASS(LayoutConstraints);
ACDK_DECL_CLASS(Caret);
ACDK_DECL_CLASS(Window);


/**
  see wxWindow
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.19 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Window
: extends EvtHandler
{
  ACDK_WITH_METAINFO(Window)
public:
  inline foreign Window(wxWindow* wxobj, bool owns = false) 
   : EvtHandler(wxobj, owns) 
  { 
    initDispatch();
  }
  inline foreign Window(const wxWindow& wxobj, bool owns = false) 
   : EvtHandler(wxobj, owns) 
  { 
    initDispatch();
  }
  ACDK_WX_STD_WX_ACCESSOR(Window)
  
    
  Window(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize());
  void initDispatch();
  
  inline int getId() { return getWx()->wxWindow::GetId(); }
  //inline void SetId(int id);
  inline void setId(int id) { getWx()->SetId(id); }
  //wxString GetName() const;
  inline RString getName() const { return WXS2S(getWx()->GetName()); }
  //void SetName(const wxString& name);
  inline void setName(IN(RString)  name) { getWx()->SetName(S2WXS(name)); }

  inline bool close(bool force = false) { return getWx()->wxWindow::Close(force); }
  //virtual bool Destroy();
  inline bool destroy() { return getWx()->Destroy(); }
        // delete all children of this window, returns TRUE if ok
    //bool DestroyChildren();
  inline bool destroyChildren() { return getWx()->DestroyChildren(); }

  //void DragAcceptFiles(bool accept);
  inline void dragAcceptFiles(bool accept) 
  { 
#if defined(ACDK_OS_WIN32)
    getWx()->DragAcceptFiles(accept); 
#endif
   }

        // is the window being deleted?
    //bool IsBeingDeleted() const { return m_isBeingDeleted; }
  inline bool isBeingDeleted() const { return getWx()->IsBeingDeleted(); }

  inline void setTitle(IN(RString) title) { getWx()->wxWindow::SetTitle(S2WXS(title)); }
  inline RString getTitle() { return WXS2S(getWx()->wxWindow::GetTitle()); }
  inline void raise() { getWx()->wxWindow::Raise(); }
  inline void lower() { getWx()->wxWindow::Lower(); }
  inline bool show(bool showit) { return getWx()->wxWindow::Show(showit); }
  inline bool enable( bool doenable = true) { return getWx()->wxWindow::Enable(doenable); }
  
  inline virtual void SetFocusFromKbd() { getWx()->wxWindow::SetFocusFromKbd(); }

//  virtual long GetWindowStyleFlag() const { return m_windowStyle; }
  virtual int getWindowStyleFlag() const { return getWx()->wxWindow::GetWindowStyleFlag(); }

        // just some (somewhat shorter) synonims
  
//  void SetWindowStyle(long style) { SetWindowStyleFlag(style); }
  void setWindowStyle(int style) { getWx()->wxWindow::SetWindowStyle(style); }

//  long GetWindowStyle() const { return GetWindowStyleFlag(); }
  int getWindowStyle() const { return getWx()->wxWindow::GetWindowStyle(); }

//    bool HasFlag(int flag) const { return (m_windowStyle & flag) != 0; }
  bool hasFlag(int flag) { return getWx()->wxWindow::HasFlag(flag); }
//    virtual bool IsRetained() const { return HasFlag(wxRETAINED); }
  bool isRetained() const { return getWx()->wxWindow::IsRetained(); }

        // extra style: the less often used style bits which can't be set with
        // SetWindowStyleFlag()
//    virtual void SetExtraStyle(long exStyle) { m_exStyle = exStyle; }
  void setExtraStyle(int exStyle) { getWx()->wxWindow::SetExtraStyle(exStyle); }
//    long GetExtraStyle() const { return m_exStyle; }
  int getExtraStyle() const { return getWx()->wxWindow::GetExtraStyle(); }

        // make the window modal (all other windows unresponsive)
  void makeModal(bool modal = true) { getWx()->wxWindow::MakeModal(modal); }

//    virtual void SetThemeEnabled(bool enableTheme) { m_themeEnabled = enableTheme; }
  void setThemeEnabled(bool enableTheme) { getWx()->wxWindow::SetThemeEnabled(enableTheme); }
//    virtual bool GetThemeEnabled() const { return m_themeEnabled; }
  bool getThemeEnabled() const { return getWx()->wxWindow::GetThemeEnabled(); }

    // focus and keyboard handling
    // ---------------------------

        // set focus to this window
//    virtual void SetFocus() = 0;
  void setFocus() { getWx()->wxWindow::SetFocus(); }

        // set focus to this window as the result of a keyboard action
//    virtual void SetFocusFromKbd() { SetFocus(); }
  void setFocusFromKbd() { getWx()->wxWindow::SetFocusFromKbd(); }

        // return the window which currently has the focus or NULL
  static RWindow FindFocus() { RETURN_WXPTR2CLS(Window, wxWindow::FindFocus()); }

        // can this window have focus?
//    virtual bool AcceptsFocus() const { return IsShown() && IsEnabled(); }
  bool acceptsFocus() const { return getWx()->wxWindow::AcceptsFocus(); }

        // can this window be given focus by keyboard navigation? if not, the
        // only way to give it focus (provided it accepts it at all) is to
        // click it
//    virtual bool AcceptsFocusFromKeyboard() const { return AcceptsFocus(); }
  bool acceptsFocusFromKeyboard() const { return getWx()->wxWindow::AcceptsFocusFromKeyboard(); }

        // NB: these methods really don't belong here but with the current
        //     class hierarchy there is no other place for them :-(

        // get the default child of this parent, i.e. the one which is
        // activated by pressing <Enter>
  //virtual wxWindow *GetDefaultItem() const { return NULL; }
  RWindow getDefaultItem() const { RETURN_WXPTR2CLS(Window, getWx()->wxWindow::GetDefaultItem()); }

        // set this child as default, return the old default
    //virtual wxWindow *SetDefaultItem(wxWindow * child)
  RWindow setDefaultItem(IN(RWindow)child) { RETURN_WXPTR2CLS(Window, getWx()->wxWindow::SetDefaultItem(child->getWx())); }
        

        // set this child as temporary default
    //virtual void SetTmpDefaultItem(wxWindow * win) { }
  void setTmpDefaultItem(IN(RWindow)win) { getWx()->wxWindow::SetTmpDefaultItem(win->getWx()); }

    // parent/children relations
    // -------------------------

        // get the list of children
    // ### TODO const wxWindowList& GetChildren() const { return m_children; }
    //wxWindowList& GetChildren();
  inline RWindowArray getChildren() 
  { 
    wxWindowList& list = getWx()->GetChildren(); //wxList
    RWindowArray wa = new WindowArray(list.GetCount());
    int i = 0;
    for ( wxWindowList::Node *node = list.GetFirst(); node; node = node->GetNext(), ++i )
    {
      wa[i] = WXPTR2CLS(Window, node->GetData());
      //wa[i] = new Window(node->GetData(), false);
    }
    return wa;
  }
  
        // get the parent or the parent of the parent
    //wxWindow *GetParent() const { return m_parent; }
  RWindow getParent() const { RETURN_WXPTR2CLS(Window, getWx()->wxWindow::GetParent()); }

  //inline wxWindow *GetGrandParent() const;
  RWindow getGrandParent() const { RETURN_WXPTR2CLS(Window, getWx()->wxWindow::GetGrandParent()); }

        // is this window a top level one?
    //virtual bool IsTopLevel() const;
  bool isTopLevel() const { return getWx()->wxWindow::IsTopLevel(); }

        // it doesn't really change parent, use ReParent() instead
    //void SetParent( wxWindowBase *parent ) { m_parent = (wxWindow *)parent; }
  void setParent(IN(RWindow)parent) { getWx()->wxWindow::SetParent(parent->getWx()); }
        // change the real parent of this window, return TRUE if the parent
        // was changed, FALSE otherwise (error or newParent == oldParent)
    //virtual bool Reparent( wxWindowBase *newParent );
  bool reparent(IN(RWindow)newParent) { return getWx()->wxWindow::Reparent(newParent->getWx()); }

        // implementation mostly
    //virtual void AddChild(wxWindow *child);
  void addChild(IN(RWindow)child) { getWx()->wxWindow::AddChild(child->getWx()); }
    //virtual void RemoveChild(wxWindow *child);
  void removeChild(IN(RWindow)child) { getWx()->wxWindow::RemoveChild(child->getWx()); }

    // looking for windows
    // -------------------

        // find window among the descendants of this one either by id or by
        // name (return NULL if not found)
    //wxWindow *FindWindow( long id );
  RWindow findWindow(int id) { RETURN_WXPTR2CLS(Window, getWx()->wxWindow::FindWindow(id)); }
    //wxWindow *FindWindow( const wxString& name );
  RWindow findWindow(IN(RString) name) { RETURN_WXPTR2CLS(Window, getWx()->wxWindow::FindWindow(S2WXS(name))); }

        // Find a window among any window (all return NULL if not found)
  static RWindow findWindowById( int id, IN(RWindow) parent = Nil) { RETURN_WXPTR2CLS(Window, wxWindow::FindWindowById(id, CLS2WXPTR(parent))); }
  static RWindow findWindowByName(IN(RString) name, IN(RWindow) parent = Nil) 
  { 
    RETURN_WXPTR2CLS(Window, wxWindow::FindWindowByName(S2WXS(name), CLS2WXPTR(parent))); 
  }
  static RWindow findWindowByLabel(IN(RString) label, IN(RWindow) parent = Nil)
  {
    RETURN_WXPTR2CLS(Window, wxWindow::FindWindowByLabel(S2WXS(label), CLS2WXPTR(parent))); 
  }


    // event handler stuff
    // -------------------

        // get the current event handler
    //wxEvtHandler *GetEventHandler() const { return m_eventHandler; }
  REvtHandler getEventHandler() const { RETURN_WXPTR2CLS(EvtHandler, getWx()->wxWindow::GetEventHandler()); }

        // replace the event handler (allows to completely subclass the
        // window)
    //void SetEventHandler( wxEvtHandler *handler ) { m_eventHandler = handler; }
  void setEventHandler(IN(REvtHandler) handler) { getWx()->wxWindow::SetEventHandler(CLS2WXPTR(handler)); }

        // push/pop event handler: allows to chain a custom event handler to
        // alreasy existing ones
    //void PushEventHandler( wxEvtHandler *handler );
  void pushEventHandler(IN(REvtHandler) handler) { getWx()->wxWindow::PushEventHandler(CLS2WXPTR(handler)); }
    //wxEvtHandler *PopEventHandler( bool deleteHandler);
  REvtHandler popEventHandler(bool deleteHandler = false) { RETURN_WXPTR2CLS(EvtHandler, getWx()->wxWindow::PopEventHandler(deleteHandler)); }

        // find the given handler in the event handler chain and remove (but
        // not delete) it from the event handler chain, return TRUE if it was
        // found and FALSE otherwise (this also results in an assert failure so
        // this function should only be called when the handler is supposed to
        // be there)
    //bool RemoveEventHandler(wxEvtHandler *handler);
  bool removeEventHandler(IN(REvtHandler) handler) { return getWx()->wxWindow::RemoveEventHandler(CLS2WXPTR(handler)); }

    // validators
    // ----------

    //virtual void SetValidator( const wxValidator &validator );
  void setValidator(IN(RValidator) validator);

    //virtual wxValidator *GetValidator() { return m_windowValidator; }
  RValidator getValidator();


  //void GetPosition(int x, int y) const;
  inline void getPosition(OUT(int) x, OUT(int) y) const { getWx()->GetPosition(&x, &y); }
  //wxPoint GetPosition() const;
  inline RPoint getPosition() const { return WXVAL2CLS(Point, getWx()->GetPosition()); }
  //wxRect GetRect() const;
  inline RRect getRect() const { return WXVAL2CLS(Rect, getWx()->GetRect()); }

  //inline void GetSize(int* width, int* height) const;
  inline void getSize(OUT(int) width, OUT(int) height) const { getWx()->GetSize(&width, &height); }
  //wxSize GetSize() const;
  inline RSize getSize() const { return WXVAL2CLS(Size, getWx()->GetSize()); }
  
  //wxSize GetClientSize() const;
  inline RSize getClientSize() const { return WXVAL2CLS(Size, getWx()->GetClientSize()); }


  //void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  inline void setSize(int x, int y, int width, int height, int sizeFlags = SizeAuto) { getWx()->SetSize(x, y, width, height, sizeFlags); }
  //void SetSize(const wxRect& rect);
  inline void setSize(IN(RRect) rect) { getWx()->SetSize(CLS2WXREF(rect)); }
  //void SetSize(int width, int height);
  inline void setSize(int width, int height) { getWx()->SetSize(width, height); }
  //void SetSize(const wxSize& size);
  inline void setSize(IN(RSize) size) { getWx()->SetSize(CLS2WXREF(size)); }
  
  //void Fit();
  inline void fit() { getWx()->Fit(); }
  //void FitInside();
  inline void fitInside() { getWx()->FitInside(); }
  
  //wxSize GetBestSize() const;
  inline RSize getBestSize() const { return WXVAL2CLS(Size, getWx()->GetBestSize()); }
  
  
  // dialog oriented functions
    // -------------------------

        // validate the correctness of input, return TRUE if ok
    //virtual bool Validate();
  bool validate() { return getWx()->wxWindow::Validate(); }

        // transfer data between internal and GUI representations
    //virtual bool TransferDataToWindow();
  bool transferDataToWindow() { return getWx()->wxWindow::TransferDataToWindow(); }
    //virtual bool TransferDataFromWindow();
  bool transferDataFromWindow() { return getWx()->wxWindow::TransferDataFromWindow(); }

    //virtual void InitDialog();
  void initDialog() { getWx()->wxWindow::InitDialog(); }

#if wxUSE_ACCEL
    // accelerators
    // ------------
    //virtual void SetAcceleratorTable( const wxAcceleratorTable& accel ) { m_acceleratorTable = accel; }
  // ### TODO virtual void setAcceleratorTable(IN(RAcceleratorTable)accel) { getWx()->wxWindow::SetAcceleratorTable(CLS2WXREF(accel)); }
    //wxAcceleratorTable *GetAcceleratorTable() { return &m_acceleratorTable; }
  // ### TODO RAcceleratorTable getAcceleratorTable() { RETURN_WXPTR2CLS(AcceleratorTable, getWx()->wxWindow::GetAcceleratorTable()); }
#endif // wxUSE_ACCEL

    // dialog units translations
    // -------------------------

    //wxPoint ConvertPixelsToDialog( const wxPoint& pt );
  RPoint convertPixelsToDialog(IN(RPoint)pt) { return WXREF2CLS(getWx()->wxWindow::ConvertPixelsToDialog(CLS2WXREF(pt))); }
    //wxPoint ConvertDialogToPixels( const wxPoint& pt );
  RPoint  convertDialogToPixels(IN(RPoint)pt) { return WXREF2CLS(getWx()->wxWindow::ConvertDialogToPixels(CLS2WXREF(pt))); }
   //wxSize ConvertPixelsToDialog( const wxSize& sz )
  RSize convertPixelsToDialog(IN(RSize)sz) { return WXREF2CLS(getWx()->wxWindow::ConvertPixelsToDialog(CLS2WXREF(sz))); }
  //wxSize ConvertDialogToPixels( const wxSize& sz )
  RSize convertDialogToPixels(IN(RSize)sz) { return WXREF2CLS(getWx()->wxWindow::ConvertDialogToPixels(CLS2WXREF(sz))); }
    

    // mouse functions
    // ---------------

        // move the mouse to the specified position
    //virtual void WarpPointer(int x, int y) = 0;
  inline void warpPointer(int x, int y) { getWx()->wxWindow::WarpPointer(x, y); }

        // start or end mouse capture, these functions maintain the stack of
        // windows having captured the mouse and after calling ReleaseMouse()
        // the mouse is not released but returns to the window which had had
        // captured it previously (if any)
    //void CaptureMouse();
  inline void captureMouse() { getWx()->wxWindow::CaptureMouse(); }
    //void ReleaseMouse();
  inline void releaseMouse() { getWx()->wxWindow::ReleaseMouse(); }

        // get the window which currently captures the mouse or NULL
  static RWindow getCapture() { return WXPTR2CLS(Window, wxWindow::GetCapture()); }

        // does this window have the capture?
    //virtual bool HasCapture() const { return (wxWindow *)this == GetCapture(); }
  inline bool hasCapture() const { return getWx()->wxWindow::HasCapture(); }

    // painting the window
    // -------------------

        // mark the specified rectangle (or the whole window) as "dirty" so it
        // will be repainted
    //virtual void Refresh( bool eraseBackground, const wxRect *rect) = 0;
  inline void refresh(bool eraseBackground = true, IN(RRect) rect = Nil) 
  { 
    if (rect != Nil)
    {
      wxRect trect = CLS2WXREF(rect);
      getWx()->wxWindow::Refresh(eraseBackground, &trect); 
    } 
    else
    getWx()->wxWindow::Refresh(eraseBackground, 0); 
  }

        // a less awkward wrapper for Refresh
    //void RefreshRect(const wxRect& rect) { Refresh(TRUE, &rect); }
  inline void refreshRect(IN(RRect) rect) { getWx()->wxWindow::RefreshRect(CLS2WXREF(rect)); }

        // repaint all invalid areas of the window immediately
    //virtual void Update() { }
  inline void update() { getWx()->wxWindow::Update(); }

        // clear the window entirely
    //virtual void Clear() = 0;
  inline void clear() 
  { 

#if ACDK_CHECK_WX_VERSION(2, 5)
    getWx()->wxWindow::ClearBackground(); 
#else
    getWx()->wxWindow::Clear(); 
#endif
  }

        // freeze the window: don't redraw it until it is thawed
    //virtual void Freeze() { }
  inline void freeze() { getWx()->wxWindow::Freeze(); }

        // thaw the window: redraw it after it had been frozen
    //virtual void Thaw() { }
  inline void thaw() { getWx()->wxWindow::Thaw(); }

        // adjust DC for drawing on this window
  //void PrepareDC( wxDC & dc) { }
  inline void prepareDC(IN(RDC) dc) { getWx()->PrepareDC(CLS2WXREF(dc)); }
  

        // the update region of the window contains the areas which must be
        // repainted by the program
  //const wxRegion& GetUpdateRegion() const { return m_updateRegion; }
  inline RRegion getUpdateRegion() const { return WXVAL2CLS(Region, getWx()->GetUpdateRegion()); }

  // get the update rectangleregion bounding box in client coords
    //wxRect GetUpdateClientRect() const;
  inline RRect getUpdateClientRect() const { return WXREF2CLS(getWx()->wxWindow::GetUpdateClientRect()); }

        // these functions verify whether the given point/rectangle belongs to
        // (or at least intersects with) the update region
    //bool IsExposed( int x, int y ) const;
  inline bool isExposed(int x, int y) { return getWx()->wxWindow::IsExposed(x, y); }
    //bool IsExposed( int x, int y, int w, int h ) const;
  inline bool isExposed(int x, int y, int w, int h) { return getWx()->wxWindow::IsExposed(x, y, w, h); }

    //bool IsExposed( const wxPoint& pt ) const { return IsExposed(pt.x, pt.y); }
  inline bool isExposed(IN(RPoint) pt) { return getWx()->wxWindow::IsExposed(CLS2WXREF(pt)); }
    //bool IsExposed( const wxRect& rect ) const { return IsExposed(rect.x, rect.y, rect.width, rect.height); }
  inline bool isExposed(IN(RRect) rect) { return getWx()->wxWindow::IsExposed(CLS2WXREF(rect)); }

    // colours, fonts and cursors
    // --------------------------

        // set/retrieve the window colours (system defaults are used by
        // default): Set functions return TRUE if colour was changed
    //virtual bool SetBackgroundColour( const wxColour &colour );
  inline bool setBackgroundColour(IN(RColour) colour) { return getWx()->wxWindow::SetBackgroundColour(CLS2WXREF(colour)); }
    //virtual bool SetForegroundColour( const wxColour &colour );
  inline bool setForegroundColour(IN(RColour) colour) { return getWx()->wxWindow::SetForegroundColour(CLS2WXREF(colour)); }

  inline bool setBackgroundColour(IN(RString) colour) { return getWx()->wxWindow::SetBackgroundColour(S2WXS(colour)); }
    //virtual bool SetForegroundColour( const wxColour &colour );
  inline bool setForegroundColour(IN(RString) colour) { return getWx()->wxWindow::SetForegroundColour(S2WXS(colour)); }

    //wxColour GetBackgroundColour() const { return m_backgroundColour; }
  inline RColour getBackgroundColour() const { return new Colour(getWx()->wxWindow::GetBackgroundColour()); }
    //wxColour GetForegroundColour() const { return m_foregroundColour; }
  inline RColour getForegroundColour() const { return new Colour(getWx()->wxWindow::GetForegroundColour()); }
        // set/retrieve the cursor for this window (SetCursor() returns TRUE
        // if the cursor was really changed)
    //virtual bool SetCursor( const wxCursor &cursor );
  inline virtual bool setCursor(IN(RCursor) cursor) { return getWx()->SetCursor(CLS2WXREF(cursor)); }
    //const wxCursor& GetCursor() const { return m_cursor; }
  inline RCursor getCursor() const { return WXREF2CLS(getWx()->GetCursor()); }
  
         // set/retrieve the font for the window (SetFont() returns TRUE if the
        // font really changed)
    //virtual bool SetFont( const wxFont &font ) = 0;
  inline bool setFont(IN(RFont) font) { return getWx()->SetFont(CLS2WXREF(font)); }
    //const wxFont& GetFont() const { return m_font; }
  //inline RFont getFont() const { return WXREF2CLS(getWx()->GetFont()); }
    //wxFont& GetFont() { return m_font; }
  inline RFont getFont() { return WXREF2CLS(getWx()->GetFont()); }

#if wxUSE_CARET
        // associate a caret with the window
    //void SetCaret(wxCaret *caret);
  inline void setCaret(IN(RCaret) caret);
        // get the current caret (may be NULL)
    //wxCaret *GetCaret() const { return m_caret; }
  inline RCaret getCaret() const;
#endif // wxUSE_CARET


        // get the (average) character size for the current font
    //virtual int GetCharHeight() const;
  inline int getCharHeight() const { return getWx()->wxWindow::GetCharHeight(); }
    //virtual int GetCharWidth() const;
  inline int getCharWidth() const { return getWx()->wxWindow::GetCharWidth(); }
/* ### TODO
        // get the width/height/... of the text using current or specified
        // font
    virtual void GetTextExtent(const wxString& string, int *x, int *y, int *descent = (int *) NULL,
                               int *externalLeading = (int *) NULL,
                               const wxFont *theFont = (const wxFont *) NULL)
                               const = 0;
*/
    // client <-> screen coords
    // ------------------------

        // translate to/from screen/client coordinates (pointers may be NULL)
    //void ClientToScreen( int *x, int *y ) const { DoClientToScreen(x, y); }
  inline void clientToScreen(OUT(int) x, OUT(int) y) { getWx()->wxWindow::ClientToScreen(&x, &y); }
  //void ScreenToClient( int *x, int *y ) const { DoScreenToClient(x, y); }
  inline void screenToClient(OUT(int) x, OUT(int) y) { getWx()->wxWindow::ScreenToClient(&x, &y); }

        // wxPoint interface to do the same thing
    //wxPoint ClientToScreen(const wxPoint& pt) const
  inline RPoint clientToScreen(IN(RPoint) pt) { return WXREF2CLS(getWx()->wxWindow::ClientToScreen(CLS2WXREF(pt))); }
  //wxPoint ScreenToClient(const wxPoint& pt) const
  inline RPoint  screenToClient(IN(RPoint) pt) { return WXREF2CLS(getWx()->wxWindow::ScreenToClient(CLS2WXREF(pt))); }
        // test where the given (in client coords) point lies
   //wxHitTest HitTest(int x, int y) const { return DoHitTest(x, y); }
  inline HitTest hitTest(int x, int y) { return HitTest(getWx()->wxWindow::HitTest(x, y)); }

  //HitTest HitTest(const wxPoint& pt) const  return DoHitTest(pt.x, pt.y); }
  inline HitTest hitTest(IN(RPoint) pt) { return HitTest(getWx()->wxWindow::HitTest(CLS2WXREF(pt))); }

    // misc
    // ----

    // get the window border style: uses the current style and falls back to
    // the default style for this class otherwise (see GetDefaultBorder())
    //Border GetBorder() const;
  inline Border getBorder() const { return Border(getWx()->wxWindow::GetBorder()); }

    //void UpdateWindowUI();
  inline void updateWindowUI() { getWx()->wxWindow::UpdateWindowUI(); }

#if wxUSE_MENUS
    //bool PopupMenu( wxMenu *menu, const wxPoint& pos ) { return DoPopupMenu(menu, pos.x, pos.y); }
  inline bool popupMenu(IN(RMenu) menu, IN(RPoint) pos) { return getWx()->wxWindow::PopupMenu(CLS2WXPTR(menu), CLS2WXREF(pos)); }
    //bool PopupMenu( wxMenu *menu, int x, int y ) { return DoPopupMenu(menu, x, y); }
  inline bool popupMenu(IN(RMenu) menu, int x, int y) { return getWx()->wxWindow::PopupMenu(CLS2WXPTR(menu), x, y); }
#endif // wxUSE_MENUS

    // scrollbars
    // ----------

        // does the window have the scrollbar for this orientation?
    //bool HasScrollbar(int orient) const;
  inline bool hasScrollbar(int orient) { return getWx()->wxWindow::HasScrollbar(orient); }
    

        // configure the window scrollbars
    //virtual void SetScrollbar( int orient, int pos, int thumbvisible, int range, bool refresh);
  inline void setScrollbar(int orient, int pos, int thumbvisible, int range, bool refresh = true) { getWx()->wxWindow::SetScrollbar(orient, pos, thumbvisible, range, refresh); }
  //void SetScrollPos( int orient, int pos, bool refresh) = 0;
  inline void setScrollPos(int orient, int pos, bool refresh = true) { getWx()->wxWindow::SetScrollPos(orient, pos, refresh); }
    //int GetScrollPos( int orient ) const = 0;
  inline int getScrollPos(int orient) { return getWx()->wxWindow::GetScrollPos(orient); }
    //int GetScrollThumb( int orient ) const = 0;
  inline int getScrollThumb(int orient) { return getWx()->wxWindow::GetScrollThumb(orient); }
    //int GetScrollRange( int orient ) const = 0;
  inline int getScrollRange(int orient) { return getWx()->wxWindow::GetScrollRange(orient); }

        // scroll window to the specified position
    //virtual void ScrollWindow( int dx, int dy, const wxRect* rect) = 0;
  inline void scrollWindow(int dx, int dy, IN(RRect) rect = Nil) 
  { 
    if (rect != Nil)
    {
      wxRect tr = CLS2WXREF(rect);
      getWx()->wxWindow::ScrollWindow(dx, dy, &tr); 
    }
    else
      getWx()->wxWindow::ScrollWindow(dx, dy, 0); 
  }

        // scrolls window by line/page: note that not all controls support this
        //
        // return TRUE if the position changed, FALSE otherwise
    //virtual bool ScrollLines(int lines) { return FALSE; }
  inline bool scrollLines(int lines) { return getWx()->wxWindow::ScrollLines(lines); }
    //virtual bool ScrollPages(int pages) { return FALSE; }
  inline bool scrollPages(int pages) { return getWx()->wxWindow::ScrollPages(pages); }

        // convenient wrappers for ScrollLines/Pages
    //bool LineUp() { return ScrollLines(-1); }
  inline bool lineUp() { return getWx()->wxWindow::LineUp(); }
    //bool LineDown() { return ScrollLines(1); }
  inline bool lineDown() { return getWx()->wxWindow::LineDown(); }
    //bool PageUp() { return ScrollPages(-1); }
  inline bool pageUp() { return getWx()->wxWindow::PageUp(); }
    //bool PageDown() { return ScrollPages(1); }
  inline bool pageDown() { return getWx()->wxWindow::PageDown(); }

    // context-sensitive help
    // ----------------------

    // these are the convenience functions wrapping wxHelpProvider methods

#if wxUSE_HELP
        // associate this help text with this window
    //void SetHelpText(const wxString& text);
  inline void setHelpText(IN(RString)  text) { getWx()->wxWindow::SetHelpText(S2WXS(text)); }
        // associate this help text with all windows with the same id as this
        // one
    //void SetHelpTextForId(const wxString& text);
  inline void setHelpTextForId(IN(RString)  text) { getWx()->wxWindow::SetHelpTextForId(S2WXS(text)); }
        // get the help string associated with this window (may be empty)
    //wxString GetHelpText() const;
  inline RString getHelpText() const { return WXS2S(getWx()->wxWindow::GetHelpText()); }
#endif // wxUSE_HELP

    // tooltips
    // --------

#if wxUSE_TOOLTIPS
        // the easiest way to set a tooltip for a window is to use this method
    //void SetToolTip( const wxString &tip );
  inline void setToolTip(IN(RString)  tip) { getWx()->wxWindow::SetToolTip(S2WXS(tip)); }
        // attach a tooltip to the window
    //void SetToolTip( wxToolTip *tip ) { DoSetToolTip(tip); }
  //### TODO inline void setToolTip(IN(RToolTip) tip) { getWx()->wxWindow::SetToolTip(CLS2WXPTR(tip)); }
        // get the associated tooltip or NULL if none
    //wxToolTip* GetToolTip() const { return m_tooltip; }
  //### TODO inline RToolTip getToolTip() const { RETURN_WXPTR2CLS(ToolTip, getWx()->wxWindow::GetToolTip()); }
#endif // wxUSE_TOOLTIPS

    // drag and drop
    // -------------
        // set/retrieve the drop target associated with this window (may be
        // NULL; it's owned by the window and will be deleted by it)
    //virtual void SetDropTarget( wxDropTarget *dropTarget ) = 0;
  inline void setDropTarget(IN(RDropTarget) dropTarget) { getWx()->wxWindow::SetDropTarget(CLS2WXPTR(dropTarget)); }
    //virtual wxDropTarget *GetDropTarget() const { return m_dropTarget; }
  inline RDropTarget getDropTarget() const { RETURN_WXPTR2CLS(DropTarget, getWx()->wxWindow::GetDropTarget()); }

    // constraints and sizers
    // ----------------------
        // set the constraints for this window or retrieve them (may be NULL)
    //void SetConstraints( wxLayoutConstraints *constraints );
  inline void setConstraints(IN(RLayoutConstraints) constraints);
    //wxLayoutConstraints *GetConstraints() const { return m_constraints; }
  inline RLayoutConstraints getConstraints() const;

        // implementation only
    //void UnsetConstraints(wxLayoutConstraints *c);
  inline void unsetConstraints(IN(RLayoutConstraints) c);
    //wxWindowList *GetConstraintsInvolvedIn() const { return m_constraintsInvolvedIn; }
  // ### TODO inline RWindowList getConstraintsInvolvedIn() const { RETURN_WXPTR2CLS(WindowList, getWx()->wxWindow::GetConstraintsInvolvedIn()); }
    //void AddConstraintReference(wxWindowBase *otherWin);
  inline void addConstraintReference(IN(RWindow) otherWin) { getWx()->wxWindow::AddConstraintReference(CLS2WXPTR(otherWin)); }
    //void RemoveConstraintReference(wxWindowBase *otherWin);
  inline void removeConstraintReference(IN(RWindow) otherWin) { getWx()->wxWindow::RemoveConstraintReference(CLS2WXPTR(otherWin)); }
    //void DeleteRelatedConstraints();
  inline void deleteRelatedConstraints() { getWx()->wxWindow::DeleteRelatedConstraints(); }
    //void ResetConstraints();
  inline void resetConstraints() { getWx()->wxWindow::ResetConstraints(); }

        // these methods may be overriden for special layout algorithms
    //virtual void SetConstraintSizes(bool recurse);
  inline void setConstraintSizes(bool recurse) { getWx()->wxWindow::SetConstraintSizes(recurse); }
    //virtual bool LayoutPhase1(int *noChanges);
  inline bool layoutPhase1(OUT(int) noChanges) { return getWx()->wxWindow::LayoutPhase1(&noChanges); }
    //virtual bool LayoutPhase2(int *noChanges);
  inline bool layoutPhase2(OUT(int) noChanges) { return getWx()->wxWindow::LayoutPhase2(&noChanges); }
    //virtual bool DoPhase(int phase);
  inline bool doPhase(int phase) { return getWx()->wxWindow::DoPhase(phase); }

        // these methods are virtual but normally won't be overridden
    //virtual void SetSizeConstraint(int x, int y, int w, int h);
  inline void setSizeConstraint(int x, int y, int w, int h) { getWx()->wxWindow::SetSizeConstraint(x, y, w, h); }
    //virtual void MoveConstraint(int x, int y);
  inline void moveConstraint(int x, int y) { getWx()->wxWindow::MoveConstraint(x, y); }
    //virtual void GetSizeConstraint(int *w, int *h) const ;
  inline void getSizeConstraint(OUT(int) w, OUT(int) h) { getWx()->wxWindow::GetSizeConstraint(&w, &h); }
    //virtual void GetClientSizeConstraint(int *w, int *h) const ;
  inline void getClientSizeConstraint(OUT(int) w, OUT(int) h) { getWx()->wxWindow::GetClientSizeConstraint(&w, &h); }
    //virtual void GetPositionConstraint(int *x, int *y) const ;
  inline void getPositionConstraint(OUT(int) x, OUT(int) y) { getWx()->wxWindow::GetPositionConstraint(&x, &y); }
  
  //void Centre(int direction = wxBOTH);
  inline void centre(int direction = OrientBoth) { getWx()->Centre(direction); }
  //inline void CentreOnParent(int direction = wxBOTH);
  inline void centreOnParent(int direction = OrientBoth) { getWx()->CentreOnParent(direction); }
  
  //void CentreOnScreen(int direction = wxBOTH);
  inline void centreOnScreen(int direction = OrientBoth) { getWx()->CentreOnScreen(direction); }

        // when using constraints or sizers, it makes sense to update
        // children positions automatically whenever the window is resized
        // - this is done if autoLayout is on
    //void SetAutoLayout( bool autoLayout ) { m_autoLayout = autoLayout; }
  inline void setAutoLayout(bool autoLayout) { getWx()->wxWindow::SetAutoLayout(autoLayout); }
    //bool GetAutoLayout() const { return m_autoLayout; }
  inline bool getAutoLayout() const { return getWx()->wxWindow::GetAutoLayout(); }

        // lay out the window and its children
    //virtual bool Layout();
  inline bool layout() { return getWx()->wxWindow::Layout(); }

        // sizers
    //void SetSizer(wxSizer *sizer, bool deleteOld );
  inline void setSizer(IN(RSizer) sizer, bool deleteOld = true);
    //void SetSizerAndFit( wxSizer *sizer, bool deleteOld );
  inline void setSizerAndFit(IN(RSizer) sizer, bool deleteOld = true);

    //wxSizer *GetSizer() const { return m_windowSizer; }
  inline RSizer getSizer() const;

    // Track if this window is a member of a sizer
    //void SetContainingSizer(wxSizer* sizer) { m_containingSizer = sizer; }
  inline void setContainingSizer(IN(RSizer) sizer);
    //wxSizer *GetContainingSizer() const { return m_containingSizer; }
  inline RSizer getContainingSizer() const;

    // backward compatibility
    // ----------------------

  /**
    Displays a Message Box
    @param style combination of MessageBoxFlags flags
  */
  static int messageBox(IN(RString) message, IN(RString) caption = "Message", int style = MbOk, IN(RWindow) parent = Nil, int x = -1, int y = -1);
};

inline void 
Menu::setInvokingWindow(IN(RWindow) win) { getWx()->SetInvokingWindow(CLS2WXPTR(win)); }
inline RWindow 
Menu::getInvokingWindow() const { RETURN_WXPTR2CLS(Window, getWx()->GetInvokingWindow()); }

#define ACDK_WXWINDOW_FORWARD_FUNCS \
inline void SetTitle(const wxString& title) { _forward->setTitle(WXS2S(title)); } 


foreign class wxWindowFwd 
: public wxWindow
{
  Window* _forward;
  bool _owns;
public:
  wxWindowFwd(Window* forward, wxWindow* parent, int id, const wxPoint& point, const wxSize& size);
  wxWindowFwd();
  ~wxWindowFwd()
  {

  }
  Object* getObject() { return _forward; }
  ACDK_WXWINDOW_FORWARD_FUNCS
  
  
  void onEvent(wxEvent& event);
  
};

inline
wxWindowFwd::wxWindowFwd(Window* forward, wxWindow* parent, int id, const wxPoint& point, const wxSize& size) 
    : wxWindow(parent, id, point, size) 
    , _forward(forward)
    , _owns(true)
{
  _forward = forward;
}

inline
Window::Window(IN(RWindow) parent, int id, IN(RPoint) pos, IN(RSize) size)
    : EvtHandler(new wxWindowFwd(this, parent == Nil ? (wxWindow*)0 : parent->getWx(), id, pos->toWx(), size->toWx()))
  {
    if (parent != Nil)
      ownsWxObject(false);
  initDispatch();
    
  }




} // wx
} // acdk

#include "App.h"

#endif //acdk_wx_Window_h
