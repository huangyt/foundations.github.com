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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/TextCtrl.h,v 1.16 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_TextCtrl_h
#define acdk_wx_TextCtrl_h

#include "Event.h"
#include "Control.h"

namespace acdk {
namespace wx {

ACDK_DECL_CLASS(TextAttr);

/**
  see wxTextAttr
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC TextAttr
: extends acdk::lang::Object //WxStruct<::wxTextAttr>
{
  ACDK_WITH_METAINFO(TextAttr)
  wxTextAttr _wxObject;
public:
  //typedef WxStruct<::wxTextAttr> Super;
  //ACDK_WX_STD_MEMBERS(TextAttr, Super)
  //TextAttr() : Super(new wxTextAttr(), true) {}
  TextAttr() {}
  foreign TextAttr(const wxTextAttr& other) : _wxObject(other) {}
  foreign operator wxTextAttr () const { return _wxObject; }
  foreign const wxTextAttr& toWx() const { return _wxObject; }
  /* 
  // ctors
    wxTextAttr() { }
    wxTextAttr(const wxColour& colText,
               const wxColour& colBack = wxNullColour,
               const wxFont& font = wxNullFont)
        : m_colText(colText), m_colBack(colBack), m_font(font) { }

    // setters
    void SetTextColour(const wxColour& colText) { m_colText = colText; }
    void SetBackgroundColour(const wxColour& colBack) { m_colBack = colBack; }
    void SetFont(const wxFont& font) { m_font = font; }

    // accessors
    bool HasTextColour() const { return m_colText.Ok(); }
    bool HasBackgroundColour() const { return m_colBack.Ok(); }
    bool HasFont() const { return m_font.Ok(); }

    // setters
    const wxColour& GetTextColour() const { return m_colText; }
    const wxColour& GetBackgroundColour() const { return m_colBack; }
    const wxFont& GetFont() const { return m_font; }

    // returns false if we have any attributes set, true otherwise
    bool IsDefault() const
    {
        return !HasTextColour() && !HasBackgroundColour() && !HasFont();
    }

    // return the attribute having the valid font and colours: it uses the
    // attributes set in attr and falls back first to attrDefault and then to
    // the text control font/colours for those attributes which are not set
    static wxTextAttr Combine(const wxTextAttr& attr,
                              const wxTextAttr& attrDef,
                              const wxTextCtrlBase *text);

private:
    wxColour m_colText,
             m_colBack;
    wxFont   m_font;
    */
};

/**
  see TextCtrl
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum TextCtrlFlags
{
  TeNoVscroll     = wxTE_NO_VSCROLL    ,  // wxTE_NO_VSCROLL     0x0002
  TeAutoScroll    = wxTE_AUTO_SCROLL   ,  // wxTE_AUTO_SCROLL    0x0008

  TeReadonly       = wxTE_READONLY      ,  // wxTE_READONLY       0x0010
  TeMultiline      = wxTE_MULTILINE     ,  // wxTE_MULTILINE      0x0020
  TeProcessTab    = wxTE_PROCESS_TAB   ,  // wxTE_PROCESS_TAB    0x0040

// alignment flags
  TeLeft           = wxTE_LEFT          ,  // wxTE_LEFT           0x0000                    // 0x0000
  TeCenter         = wxTE_CENTER        ,  // wxTE_CENTER         wxALIGN_CENTER_HORIZONTAL // 0x0100
  TeRight          = wxTE_RIGHT         ,  // wxTE_RIGHT          wxALIGN_RIGHT             // 0x0200
  TeCentre         = wxTE_CENTRE        ,  // wxTE_CENTRE         wxTE_CENTER

// this style means to use RICHEDIT control and does something only under wxMSW
// and Win32 and is silently ignored under all other platforms
  TeRich           = wxTE_RICH          ,  // wxTE_RICH           0x0080

  TeProcessEnter  = wxTE_PROCESS_ENTER ,  // wxTE_PROCESS_ENTER  0x0400
  TePassword       = wxTE_PASSWORD      ,  // wxTE_PASSWORD       0x0800

// automatically detect the URLs and generate the events when mouse is
// moved/clicked over an URL
//
// this is for Win32 richedit controls only so far
  TeAutoUrl       = wxTE_AUTO_URL      ,  // wxTE_AUTO_URL       0x1000

// by default, the Windows text control doesn't show the selection when it
// doesn't have focus - use this style to force it to always show it
  TeNohidesel      = wxTE_NOHIDESEL     ,  // wxTE_NOHIDESEL      0x2000

// use wxHSCROLL to not wrap text at all, wxTE_LINEWRAP to wrap it at any
// position and wxTE_WORDWRAP to wrap at words boundary
  TeDontwrap       = wxTE_DONTWRAP      ,  // wxTE_DONTWRAP       wxHSCROLL
  TeLinewrap       = wxTE_LINEWRAP      ,  // wxTE_LINEWRAP       0x4000
  TeWordwrap       = wxTE_WORDWRAP      ,  // wxTE_WORDWRAP       0x0000  // it's just == !wxHSCROLL

// force using RichEdit version 2.0 or 3.0 instead of 1.0 (default) for
// wxTE_RICH controls - can be used together with or instead of wxTE_RICH
  TeRich2          = wxTE_RICH2         ,  // wxTE_RICH2          0x8000


};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, TextCtrlFlags);


ACDK_DECL_CLASS(TextCtrl);

/**
  see wxTextCtrl
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/02/05 10:45:35 $
  //wxEVT_COMMAND_TEXT_UPDATED 
*/
class ACDK_WX_PUBLIC TextCtrl
: extends Control
{
  ACDK_WITH_METAINFO(TextCtrl)
public:
  ACDK_WX_STD_MEMBERS(TextCtrl, Control)
  TextCtrl() : Control(new wxTextCtrl()) {}
  

  TextCtrl(IN(RWindow) parent, int id, IN(RString) value = "", IN(RPoint) pos = Point::defaultPosition(),
           IN(RSize) size = Size::defaultSize(), int style = 0
            //const wxValidator& validator = wxDefaultValidator
            )
    : Control(new wxTextCtrl(CLS2WXPTR(parent), id, S2WXS(value), CLS2WXREF(pos), CLS2WXREF(size), style), 
              parent == Nil)
  {
  }
  RString getValue() const { return WXS2S(getWx()->GetValue()); }
  void setValue(IN(RString) value) { getWx()->SetValue(S2WXS(value)); }
  RString getRange(int from, int to) const { return WXS2S(getWx()->GetRange(from, to)); }
  int getLineLength(int lineNo) const { return getWx()->GetLineLength(lineNo); }
  RString getLineText(int lineNo) const { return WXS2S(getWx()->GetLineText(lineNo)); }
  int getNumberOfLines() const { return getWx()->GetNumberOfLines(); }
  bool isModified() const { return getWx()->IsModified(); }
  bool isEditable() const { return getWx()->IsEditable(); }
  void getSelection(OUT(int) from, OUT(int) to) const { getWx()->GetSelection((long*)&from, (long*)to); }
    // operations
    // ----------

  void clear() { getWx()->Clear(); }
  void replace(int from, int to, IN(RString) value)  { getWx()->Replace(from, to, S2WXS(value)); }
//  virtual void Remove(long from, long to);
  void remove(int from, int to) { getWx()->Remove(from, to); }

    // load the controls contents from the file
//    bool LoadFile(const wxString& file);
  bool loadFile(IN(RString) file) { return getWx()->LoadFile(S2WXS(file)); }
  bool saveFile(IN(RString) file) { return getWx()->SaveFile(S2WXS(file)); }

    // clears the dirty flag
//    void DiscardEdits();
  void discardEdits() { getWx()->DiscardEdits(); }

//    void SetMaxLength(unsigned long len);
  void setMaxLength(int len) { getWx()->SetMaxLength(len); }

    // writing text inserts it at the current position, appending always
    // inserts it at the end
//    void WriteText(const wxString& text);
  void writeText(IN(RString) text) { getWx()->WriteText(S2WXS(text)); }
//    void AppendText(const wxString& text);
  void appendText(IN(RString) text) { getWx()->AppendText(S2WXS(text)); }

//  bool EmulateKeyPress(const wxKeyEvent& event);
  bool emulateKeyPress(IN(RKeyEvent)event) 
  { 
#if defined(ACDK_OS_WIN32)
      return getWx()->EmulateKeyPress(event->toWx()); 
#endif //defined(ACDK_OS_WIN32)
      return false;
  }





    // apply text attribute to the range of text (only works with richedit
    // controls)
//    bool SetStyle(long start, long end, const wxTextAttr& style);
  bool setStyle(int start, int end, IN(RTextAttr)style) 
  { 
#if wxUSE_RICHEDIT
    return getWx()->SetStyle(start, end, style->toWx()); 
#else
    return false;
#endif
  }
//    bool SetDefaultStyle(const wxTextAttr& style);
  bool setDefaultStyle(IN(RTextAttr)style) 
  { 
#if wxUSE_RICHEDIT
    return getWx()->SetDefaultStyle(style->toWx()); 
#else
    return false;
#endif
  }


    // translate between the position (which is just an index in the text ctrl
    // considering all its contents as a single strings) and (x, y) coordinates
    // which represent column and line.
    //long XYToPosition(long x, long y) const;

//    bool PositionToXY(long pos, long *x, long *y) const;
  bool positionToXY(int pos, OUT(int) x, OUT(int) y) { return getWx()->PositionToXY(pos, (long*)&x, (long*)&y); }

//    void ShowPosition(long pos);
    void showPosition(int pos) { getWx()->ShowPosition(pos); }


    // Clipboard operations
//    void Copy();
    void copy() { getWx()->Copy(); }

    
//    void Cut();
    void cut() { getWx()->Cut(); }
//    void Paste();
    void paste() { getWx()->Paste(); }

//    bool CanCopy() const;
    bool canCopy() { return getWx()->CanCopy(); }


//    bool CanCut() const;
    bool canCut() const { return getWx()->CanCut(); }
//    bool CanPaste() const;
    bool canPaste() const { return getWx()->CanPaste(); }

    // Undo/redo
//    void Undo();
    void undo() { getWx()->Undo(); }
//    void Redo();
    void redo() { getWx()->Redo(); }

//    bool CanUndo() const;
    bool canUndo() const { return getWx()->CanUndo(); }
//    bool CanRedo() const;
    bool canRedo() const { return getWx()->CanRedo(); }

    // Insertion point
//    void SetInsertionPoint(long pos);
    void setInsertionPoint(int pos) { getWx()->SetInsertionPoint(pos); }
//    void SetInsertionPointEnd();
    void setInsertionPointEnd() { getWx()->SetInsertionPointEnd(); }
//    long GetInsertionPoint() const;
    int getInsertionPoint() const { return getWx()->GetInsertionPoint(); }
//    long GetLastPosition() const;
    int getLastPosition() const { return getWx()->GetLastPosition(); }
//    void SetSelection(long from, long to);
    void setSelection(int from, int to) { getWx()->SetSelection(from, to); }
//    void SetEditable(bool editable);
    void setEditable(bool editable) { getWx()->SetEditable(editable); }

};


} // wx
} // acdk

#endif //acdk_wx_TextCtrl_h
