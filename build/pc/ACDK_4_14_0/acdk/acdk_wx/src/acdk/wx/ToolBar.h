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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ToolBar.h,v 1.9 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_ToolBar_h
#define acdk_wx_ToolBar_h

#include "Control.h"
#include "Bitmap.h"

namespace acdk {
namespace wx {

enum ItemKind;

/**
  see ToolBar
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum ToolBarStyle
{
// wxToolBar style flags
  TbHorizontal     = wxTB_HORIZONTAL    ,  // wxTB_HORIZONTAL     wxHORIZONTAL    // == 0x0004
  TbVertical       = wxTB_VERTICAL      ,  // wxTB_VERTICAL       wxVERTICAL      // == 0x0008
  Tb3dbuttons      = wxTB_3DBUTTONS     ,  // wxTB_3DBUTTONS      0x0010
  TbFlat           = wxTB_FLAT          ,  // wxTB_FLAT           0x0020          // supported only under Win98+/GTK
  TbDockable       = wxTB_DOCKABLE      ,  // wxTB_DOCKABLE       0x0040          // use native docking under GTK
  TbNoicons        = wxTB_NOICONS       ,  // wxTB_NOICONS        0x0080          // don't show the icons
  TbText           = wxTB_TEXT          ,  // wxTB_TEXT           0x0100          // show the text
  TbNodivider      = wxTB_NODIVIDER     ,  // wxTB_NODIVIDER      0x0200          // don't show the divider (Windows)
  TbNoalign        = wxTB_NOALIGN         // wxTB_NOALIGN        0x0400          // no automatic alignment (Windows)
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, ToolBarStyle);

/**
  see ToolBar
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:35 $
*/
enum ToolBarToolStyle
{
    ToolStyleButton    = wxTOOL_STYLE_BUTTON   ,  // wxTOOL_STYLE_BUTTON    = 1,
    ToolStyleSeparator = wxTOOL_STYLE_SEPARATOR,  // wxTOOL_STYLE_SEPARATOR = 2,
    ToolStyleControl = wxTOOL_STYLE_CONTROL  // wxTOOL_STYLE_CONTROL
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, ToolBarToolStyle);

foreign
class WrappedObject
: public wxObject
{
private:
  RObject _data;
public:
  WrappedObject() {}
  WrappedObject(IN(RObject) data) : _data(data) {}
  RObject GetData() { return _data; }
  void SetData(IN(RObject) data) { _data = data; }
};


ACDK_DECL_CLASS(ToolBarToolBase);
ACDK_DECL_CLASS(ToolBar);
/**
  see wxToolBarToolBase
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC ToolBarToolBase
: extends WxObject
{
  ACDK_WITH_METAINFO(ToolBarToolBase)
 
public:
  // wxToolBar

  ACDK_WX_STD_MEMBERS(ToolBarToolBase, WxObject)
  //ACDK_WX_STD_WX_ACCESSOR(ToolBarToolBase);

  //void wxToolBarToolBase(wxToolBarBase *tbar = (wxToolBarBase *)NULL, int id = wxID_SEPARATOR, const wxString& label = wxEmptyString, const wxBitmap& bmpNormal = wxNullBitmap, const wxBitmap& bmpDisabled = wxNullBitmap, wxItemKind kind = wxITEM_NORMAL, wxObject *clientData = (wxObject *) NULL, const wxString& shortHelpString = wxEmptyString, const wxString& longHelpString = wxEmptyString);
  ToolBarToolBase(IN(RToolBar) tbar = Nil, int id = -1, IN(RString)  label = "", IN(RBitmap) bmpNormal = Bitmap::nullBitmap(), 
                                IN(RBitmap) bmpDisabled = Bitmap::nullBitmap(), ItemKind kind = ItemNormal, 
                                IN(RObject)  clientData = Nil, IN(RString)  shortHelpString = "", IN(RString)  longHelpString = "");  
  
    // accessors
    // ---------

    // general
    //int GetId() const { return m_id; }
  inline int getId() const { return getWx()->GetId(); }

    //wxControl *GetControl() const
  inline RControl getControl() const { RETURN_WXPTR2CLS(Control, getWx()->GetControl()); }
    
    //wxToolBarBase *GetToolBar() const { return m_tbar; }
  inline RToolBar getToolBar() const;// { RETURN_WXPTR2CLS(ToolBar, (wxToolBar*)getWx()->GetToolBar()); }

    // style
    //bool IsButton() const { return m_toolStyle == wxTOOL_STYLE_BUTTON; }
  inline bool isButton() const { return getWx()->IsButton(); }
    //bool IsControl() const { return m_toolStyle == wxTOOL_STYLE_CONTROL; }
  inline bool isControl() const { return getWx()->IsControl(); }
    //bool IsSeparator() const { return m_toolStyle == wxTOOL_STYLE_SEPARATOR; }
  inline bool isSeparator() const { return getWx()->IsSeparator(); }
    //int GetStyle() const { return m_toolStyle; }
  inline int getStyle() const { return getWx()->GetStyle(); }
    //wxItemKind GetKind() const
  inline ItemKind getKind() const { return (ItemKind)getWx()->GetKind(); }
    
    // state
    //bool IsEnabled() const { return m_enabled; }
  inline bool isEnabled() const { return getWx()->IsEnabled(); }
    //bool IsToggled() const { return m_toggled; }
  inline bool isToggled() const { return getWx()->IsToggled(); }
    //bool CanBeToggled() const
  inline bool canBeToggled() const { return getWx()->CanBeToggled(); }
    // attributes
    //const wxBitmap& GetNormalBitmap() const { return m_bmpNormal; }
  inline RBitmap getNormalBitmap() const { return WXVAL2CLS(Bitmap, getWx()->GetNormalBitmap()); }
  //const wxBitmap& GetDisabledBitmap() const { return m_bmpDisabled; }
  inline RBitmap getDisabledBitmap() const { return WXVAL2CLS(Bitmap, getWx()->GetDisabledBitmap()); }

    //const wxBitmap& GetBitmap() const
    inline RBitmap getBitmap() const { return WXVAL2CLS(Bitmap, getWx()->GetBitmap()); }
    //wxString GetLabel() const { return m_label; }
    inline RString getLabel() const { return WXS2S(getWx()->GetLabel()); }
    //wxString GetShortHelp() const { return m_shortHelpString; }
    inline RString getShortHelp() const { return WXS2S(getWx()->GetShortHelp()); }
    //wxString GetLongHelp() const { return m_longHelpString; }
    inline RString getLongHelp() const { return WXS2S(getWx()->GetLongHelp()); }

    //wxObject *GetClientData() const
    // ### @todo use RObject no wxObject
    inline RObject getClientData() const 
    { 
      wxObject* wobj = getWx()->GetClientData();
      if (wobj == 0)
        return Nil;
      WrappedObject* wo = dynamic_cast<WrappedObject*>(wobj);
      if (wo == 0)
        return Nil;
      return wo->GetData();
    }
    // modifiers: return TRUE if the state really changed
    //bool Enable(bool enable);
    inline bool enable(bool enable) { return getWx()->Enable(enable); }
    //bool Toggle(bool toggle);
    inline bool toggle(bool toggle) { return getWx()->Toggle(toggle); }
    //bool SetToggle(bool toggle);
    inline bool setToggle(bool toggle) { return getWx()->SetToggle(toggle); }
    //bool SetShortHelp(const wxString& help);
    inline bool setShortHelp(IN(RString)  help) { return getWx()->SetShortHelp(S2WXS(help)); }
    //bool SetLongHelp(const wxString& help);
    inline bool setLongHelp(IN(RString)  help) { return getWx()->SetLongHelp(S2WXS(help)); }

    //void Toggle() { Toggle(!IsToggled()); }
    inline void toggle() { getWx()->Toggle(); }

    //void SetNormalBitmap(const wxBitmap& bmp) { m_bmpNormal = bmp; }
    inline void setNormalBitmap(IN(RBitmap) bmp) { getWx()->SetNormalBitmap(CLS2WXREF(bmp)); }
    //void SetDisabledBitmap(const wxBitmap& bmp) { m_bmpDisabled = bmp; }
    inline void setDisabledBitmap(IN(RBitmap) bmp) { getWx()->SetDisabledBitmap(CLS2WXREF(bmp)); }

    //virtual void SetLabel(const wxString& label) { m_label = label; }
    inline virtual void setLabel(IN(RString)  label) { getWx()->SetLabel(S2WXS(label)); }

    //void SetClientData(wxObject *clientData);
    inline void setClientData(IN(RObject)  clientData) 
    { 
      if (clientData == Nil)
        getWx()->SetClientData(0); 
      else
        getWx()->SetClientData(new WrappedObject(clientData));
    }

    // add tool to/remove it from a toolbar
    //virtual void Detach() { m_tbar = (wxToolBarBase *)NULL; }
    inline virtual void detach() { getWx()->Detach(); }
    //virtual void Attach(wxToolBarBase *tbar) { m_tbar = tbar; }
    inline virtual void attach(IN(RToolBar) tbar);// { getWx()->Attach(CLS2WXPTR(tbar)); }
};

// wxToolBarBase
ACDK_DECL_CLASS(ToolBarBase);

ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC ToolBarBase
: extends Control
{
  ACDK_WITH_METAINFO(ToolBarBase)
public:
  // wxToolBar
  ACDK_WX_STD_MEMBERS(ToolBarBase, Control)
  inline RToolBarToolBase addTool(int id, IN(RString)  label, IN(RBitmap) bitmap, IN(RBitmap) bmpDisabled, ItemKind kind = ItemNormal, 
                              IN(RString)  shortHelp = "", IN(RString)  longHelp = "", IN(RObject)  data = Nil) 
  { 
    wxObject* wxdata = 0;
    if (data != Nil)
      wxdata = new WrappedObject(data);
    RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->AddTool(id, S2WXS(label), CLS2WXREF(bitmap), CLS2WXREF(bmpDisabled), (wxItemKind)kind, S2WXS(shortHelp), S2WXS(longHelp), wxdata)); 
  }
  
    // the most common AddTool() version
  //wxToolBarToolBase *AddTool(int id, const wxString& label, const wxBitmap& bitmap, const wxString& shortHelp = wxEmptyString, wxItemKind kind = ItemNormal)
    
  inline RToolBarToolBase addTool(int id, IN(RString)  label, IN(RBitmap) bitmap, IN(RString)  shortHelp = "", ItemKind kind = ItemNormal) { RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->AddTool(id, S2WXS(label), CLS2WXREF(bitmap), S2WXS(shortHelp), (wxItemKind)kind)); }
    // add a check tool, i.e. a tool which can be toggled
  //wxToolBarToolBase *AddCheckTool(int id, const wxString& label, const wxBitmap& bitmap, const wxBitmap& bmpDisabled = wxNullBitmap, const wxString& shortHelp = wxEmptyString, const wxString& longHelp = wxEmptyString, wxObject *data = NULL)
  inline RToolBarToolBase addCheckTool(int id, IN(RString)  label, IN(RBitmap) bitmap, IN(RBitmap) bmpDisabled = Bitmap::nullBitmap(), 
                                       IN(RString)  shortHelp = "", IN(RString)  longHelp = "", IN(RObject)  data = Nil) 
  { 
    wxObject* wxdata = 0;
    if (data != Nil)
      wxdata = new WrappedObject(data);
    RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->AddCheckTool(id, S2WXS(label), CLS2WXREF(bitmap), CLS2WXREF(bmpDisabled), S2WXS(shortHelp), S2WXS(longHelp), wxdata)); 
  }
        
  // add a radio tool, i.e. a tool which can be toggled and releases any
  // other toggled radio tools in the same group when it happens
  //wxToolBarToolBase *AddRadioTool(int id, const wxString& label, const wxBitmap& bitmap, const wxBitmap& bmpDisabled = wxNullBitmap, const wxString& shortHelp = wxEmptyString, const wxString& longHelp = wxEmptyString, wxObject *data = NULL)
  
  inline RToolBarToolBase addRadioTool(int id, IN(RString)  label, IN(RBitmap) bitmap, IN(RBitmap) bmpDisabled = Bitmap::nullBitmap(), 
                                       IN(RString)  shortHelp = "", IN(RString)  longHelp = "", IN(RObject)  data = Nil) 
  { 
    wxObject* wxdata = 0;
    if (data != Nil)
      wxdata = new WrappedObject(data);
    RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->AddRadioTool(id, S2WXS(label), CLS2WXREF(bitmap), CLS2WXREF(bmpDisabled), S2WXS(shortHelp), S2WXS(longHelp), wxdata)); 
  }

    // insert the new tool at the given position, if pos == GetToolsCount(), it
    // is equivalent to AddTool()
  //virtual wxToolBarToolBase *InsertTool(size_t pos, int id, const wxString& label, const wxBitmap& bitmap, const wxBitmap& bmpDisabled = wxNullBitmap, wxItemKind kind = wxITEM_NORMAL, const wxString& shortHelp = wxEmptyString, const wxString& longHelp = wxEmptyString, wxObject *clientData = NULL);
  inline virtual RToolBarToolBase insertTool(int pos, int id, IN(RString)  label, IN(RBitmap) bitmap, IN(RBitmap) bmpDisabled = Bitmap::nullBitmap(), 
                                             ItemKind kind = ItemNormal, IN(RString)  shortHelp = "", IN(RString)  longHelp = "", IN(RObject)  clientData = Nil) 
  { 
    wxObject* wxdata = 0;
    if (clientData != Nil)
      wxdata = new WrappedObject(clientData);
    RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->InsertTool(pos, id, S2WXS(label), CLS2WXREF(bitmap), CLS2WXREF(bmpDisabled), (wxItemKind)kind, S2WXS(shortHelp), S2WXS(longHelp), wxdata)); 
  }

    // add an arbitrary control to the toolbar, return TRUE if ok (notice that
    // the control will be deleted by the toolbar and that it will also adjust
    // its position/size)
    //
    // NB: the control should have toolbar as its parent
  //virtual wxToolBarToolBase *AddControl(wxControl *control);
  inline virtual RToolBarToolBase addControl(IN(RControl) control) { RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->AddControl(CLS2WXPTR(control))); }
  //virtual wxToolBarToolBase *InsertControl(size_t pos, wxControl *control);
  inline virtual RToolBarToolBase insertControl(int pos, IN(RControl) control) { RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->InsertControl(pos, CLS2WXPTR(control))); }
    
    // get the control with the given id or return NULL
    //virtual wxControl *FindControl( int id );
  inline virtual RControl findControl(int id) { RETURN_WXPTR2CLS(Control, getWx()->FindControl(id)); }

    // add a separator to the toolbar
    //virtual wxToolBarToolBase *AddSeparator();
  inline virtual RToolBarToolBase addSeparator() { RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->AddSeparator()); }
    //virtual wxToolBarToolBase *InsertSeparator(size_t pos);
  inline virtual RToolBarToolBase insertSeparator(int pos) { RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->InsertSeparator(pos)); }

    // remove the tool from the toolbar: the caller is responsible for actually
    // deleting the pointer
    //virtual wxToolBarToolBase *RemoveTool(int id);
  inline virtual RToolBarToolBase removeTool(int id) { RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->RemoveTool(id)); }

    // delete tool either by index or by position
    //virtual bool DeleteToolByPos(size_t pos);
  inline virtual bool deleteToolByPos(int pos) { return getWx()->DeleteToolByPos(pos); }
    //virtual bool DeleteTool(int id);
  inline virtual bool deleteTool(int id) { return getWx()->DeleteTool(id); }

    // delete all tools
    //virtual void ClearTools();
  inline virtual void clearTools() { getWx()->ClearTools(); }

    // must be called after all buttons have been created to finish toolbar
    // initialisation
    //virtual bool Realize();
  inline virtual bool realize() { return getWx()->Realize(); }

    // tools state
    // -----------

    //virtual void EnableTool(int id, bool enable);
  inline virtual void enableTool(int id, bool enable) { getWx()->EnableTool(id, enable); }
    //virtual void ToggleTool(int id, bool toggle);
  inline virtual void toggleTool(int id, bool toggle) { getWx()->ToggleTool(id, toggle); }

    // Set this to be togglable (or not)
    //virtual void SetToggle(int id, bool toggle);
  inline virtual void setToggle(int id, bool toggle) { getWx()->SetToggle(id, toggle); }

    // set/get tools client data (not for controls)
    //virtual wxObject *GetToolClientData(int id) const;
  inline virtual RObject getToolClientData(int id) const 
  { 
    wxObject* wobj = getWx()->GetToolClientData(id);
      if (wobj == 0)
        return Nil;
      WrappedObject* wo = dynamic_cast<WrappedObject*>(wobj);
      if (wo == 0)
        return Nil;
      return wo->GetData();
  }
    //virtual void SetToolClientData(int id, wxObject *clientData);
  inline virtual void setToolClientData(int id, IN(RObject)  clientData) 
  { 
     wxObject* wxdata = 0;
    if (clientData != Nil)
      wxdata = new WrappedObject(clientData);
    getWx()->SetToolClientData(id, wxdata); 
  }

    // return TRUE if the tool is toggled
    //virtual bool GetToolState(int id) const;
  inline virtual bool getToolState(int id) const { return getWx()->GetToolState(id); }

    //virtual bool GetToolEnabled(int id) const;
  inline virtual bool getToolEnabled(int id) const { return getWx()->GetToolEnabled(id); }

    //virtual void SetToolShortHelp(int id, const wxString& helpString);
  inline virtual void setToolShortHelp(int id, IN(RString)  helpString) { getWx()->SetToolShortHelp(id, S2WXS(helpString)); }
    //virtual wxString GetToolShortHelp(int id) const;
  inline virtual RString getToolShortHelp(int id) const { return WXS2S(getWx()->GetToolShortHelp(id)); }
    //virtual void SetToolLongHelp(int id, const wxString& helpString);
  inline virtual void setToolLongHelp(int id, IN(RString)  helpString) { getWx()->SetToolLongHelp(id, S2WXS(helpString)); }
    //virtual wxString GetToolLongHelp(int id) const;
  inline virtual RString getToolLongHelp(int id) const { return WXS2S(getWx()->GetToolLongHelp(id)); }

    // margins/packing/separation
    // --------------------------

    //virtual void SetMargins(int x, int y);
  inline virtual void setMargins(int x, int y) { getWx()->SetMargins(x, y); }
    //void SetMargins(const wxSize& size);
  inline void setMargins(IN(RSize) size) 
  { 
#if defined(__WXMSW__)
      getWx()->SetMargins(CLS2WXREF(size)); 
#endif
  }
    //virtual void SetToolPacking(int packing);
  inline virtual void setToolPacking(int packing) { getWx()->SetToolPacking(packing); }
    //virtual void SetToolSeparation(int separation);
  inline virtual void setToolSeparation(int separation) { getWx()->SetToolSeparation(separation); }
    
    //virtual wxSize GetToolMargins() const { return wxSize(m_xMargin, m_yMargin); }
  inline virtual RSize getToolMargins() const { return WXVAL2CLS(Size, getWx()->GetToolMargins()); }
    //virtual int GetToolPacking() const { return m_toolPacking; }
  inline virtual int getToolPacking() const { return getWx()->GetToolPacking(); }
    //virtual int GetToolSeparation() const { return m_toolSeparation; }
  inline virtual int getToolSeparation() const { return getWx()->GetToolSeparation(); }

    // toolbar geometry
    // ----------------

    // set the number of toolbar rows
    //virtual void SetRows(int nRows);
  inline virtual void setRows(int nRows) { getWx()->SetRows(nRows); }

    // the toolbar can wrap - limit the number of columns or rows it may take
    //void SetMaxRowsCols(int rows, int cols);
  inline void setMaxRowsCols(int rows, int cols) { getWx()->SetMaxRowsCols(rows, cols); }
    //int GetMaxRows() const { return m_maxRows; }
  inline int getMaxRows() const { return getWx()->GetMaxRows(); }
    //int GetMaxCols() const { return m_maxCols; }
  inline int getMaxCols() const { return getWx()->GetMaxCols(); }

    // get/set the size of the bitmaps used by the toolbar: should be called
    // before adding any tools to the toolbar
    //virtual void SetToolBitmapSize(const wxSize& size) { m_defaultWidth = size.x; m_defaultHeight = size.y; };
  inline virtual void setToolBitmapSize(IN(RSize) size) { getWx()->SetToolBitmapSize(CLS2WXREF(size)); }
    //virtual wxSize GetToolBitmapSize() const { return wxSize(m_defaultWidth, m_defaultHeight); }
  inline virtual RSize getToolBitmapSize() const { return WXVAL2CLS(Size, getWx()->GetToolBitmapSize()); }

    // the button size in some implementations is bigger than the bitmap size:
    // get the total button size (by default the same as bitmap size)
    //virtual wxSize GetToolSize() const { return GetToolBitmapSize(); } 
  inline virtual RSize getToolSize() const { return WXVAL2CLS(Size, getWx()->GetToolSize()); }

    // returns a (non separator) tool containing the point (x, y) or NULL if
    // there is no tool at this point (corrdinates are client)
    //virtual wxToolBarToolBase *FindToolForPosition(wxCoord x, wxCoord y) const = 0;
  inline virtual RToolBarToolBase findToolForPosition(int x, int y) const { RETURN_WXPTR2CLS(ToolBarToolBase, getWx()->FindToolForPosition(x, y)); }

    // return TRUE if this is a vertical toolbar, otherwise FALSE
    //bool IsVertical() const { return HasFlag(wxTB_VERTICAL); }
  inline bool isVertical() const { return getWx()->IsVertical(); }

};

ACDK_DECL_CLASS(ToolBar);
/**
  see wxToolBar
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.9 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC ToolBar
: extends ToolBarBase
{
  ACDK_WITH_METAINFO(ToolBar)
public:
  // wxToolBar
  ACDK_WX_STD_MEMBERS(ToolBar, ToolBarBase)
  //void ToolBar(wxWindow *parent, wxWindowID id, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize, long style = wxNO_BORDER | wxTB_HORIZONTAL, const wxString& name = wxToolBarNameStr)
    
  ToolBar(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), int style = BorderNone | TbHorizontal) 
  : ToolBarBase(new wxToolBar(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style), parent == Nil)
  {
  }
};

inline void 
ToolBarToolBase::attach(IN(RToolBar) tbar) { getWx()->Attach(CLS2WXPTR(tbar)); }
inline RToolBar 
ToolBarToolBase::getToolBar() const { RETURN_WXPTR2CLS(ToolBar, (wxToolBar*)getWx()->GetToolBar()); }

inline
ToolBarToolBase::ToolBarToolBase(IN(RToolBar) tbar, int id, IN(RString)  label, IN(RBitmap) bmpNormal, 
                                IN(RBitmap) bmpDisabled, ItemKind kind, 
                                IN(RObject)  clientData, IN(RString)  shortHelpString, IN(RString)  longHelpString) 
  : WxObject(new wxToolBarToolBase(CLS2WXPTR(tbar), id, S2WXS(label), CLS2WXREF(bmpNormal), CLS2WXREF(bmpDisabled), (wxItemKind)kind, 
                                   0/*clientData == Nil ? 0 : new wxAcdkClientData(clientData)*/, S2WXS(shortHelpString), S2WXS(longHelpString)))
  { }

} // wx
} // acdk

#endif //acdk_wx_ToolBar_h
