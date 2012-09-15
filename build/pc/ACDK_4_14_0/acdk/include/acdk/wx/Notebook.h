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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Notebook.h,v 1.8 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_Notebook_h
#define acdk_wx_Notebook_h

#include "Window.h"
#include "Validator.h"
#include "Bitmap.h"
#include "Control.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(NotebookEvent);
/**
  see wxNotebookEvent
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC NotebookEvent
: extends NotifyEvent
{
  ACDK_WITH_METAINFO(NotebookEvent)
public:
  //wxNotebookEvent
  ACDK_WX_STD_EVENT_MEMBERS(NotebookEvent, NotifyEvent)
  NotebookEvent(int eventType = EvtNull, int id = 0, int sel = -1, int oldSel = -1)
  : NotifyEvent(new wxNotebookEvent((wxEventType)eventType, id, sel, oldSel))
  {
  }
  //int GetSelection() const { return m_nSel; }
  inline int getSelection() const { return getWx()->GetSelection(); }
  //void SetSelection(int nSel) { m_nSel = nSel; }
  inline void setSelection(int nSel) { getWx()->SetSelection(nSel); }
  // the page that was selected before the change (-1 if none)
  //int GetOldSelection() const { return m_nOldSel; }
  inline int getOldSelection() const { return getWx()->GetOldSelection(); }
  //void SetOldSelection(int nOldSel) { m_nOldSel = nOldSel; }
  inline void setOldSelection(int nOldSel) { getWx()->SetOldSelection(nOldSel); }

  static int EvtCommandNotebookPageChanging;
  static int EvtCommandNotebookPageChanged;
};


ACDK_DECL_CLASS(Notebook);
/**
  see wxNotebook
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.8 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC Notebook
: extends Control
{
  ACDK_WITH_METAINFO(Notebook)
public:
  // wxNotebook
  // wxNotebookEvent
  ACDK_WX_STD_MEMBERS(Notebook, Control)
  Notebook(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(), int style = 0) 
  : Control(new wxNotebook(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style), parent == Nil)
  {}
  
  //int GetPageCount() const { return m_pages.GetCount(); }
  inline int getPageCount() const { return getWx()->GetPageCount(); }
  
  // get the panel which represents the given page
  //wxWindow *GetPage(int nPage) { return m_pages[nPage]; }
  inline RWindow getPage(int nPage) { RETURN_WXPTR2CLS(Window, getWx()->GetPage(nPage)); }
  
  // get the currently selected page
  //virtual int GetSelection() const = 0;
  inline virtual int getSelection() const { return getWx()->GetSelection(); }
  
  // set/get the title of a page
  //virtual bool SetPageText(int nPage, const wxString& strText) = 0;
  inline virtual bool setPageText(int nPage, IN(RString)  strText) { return getWx()->SetPageText(nPage, S2WXS(strText)); }
  //virtual wxString GetPageText(int nPage) const = 0;
  inline virtual RString getPageText(int nPage) const { return WXS2S(getWx()->GetPageText(nPage)); }
  
  // image list stuff: each page may have an image associated with it (all
  // images belong to the same image list)
  //virtual void SetImageList(wxImageList* imageList);
  inline virtual void setImageList(IN(RImageList) imageList) { getWx()->SetImageList(CLS2WXPTR_L(imageList)); }
  
  // as SetImageList() but we will delete the image list ourselves
  //void AssignImageList(wxImageList* imageList);
  inline void assignImageList(IN(RImageList) imageList) { getWx()->AssignImageList(CLS2WXPTR_L(imageList)); }
  
  // get pointer (may be NULL) to the associated image list
  //wxImageList* GetImageList() const { return m_imageList; }
  inline RImageList getImageList() const { RETURN_WXPTR2CLS(ImageList, getWx()->GetImageList()); }
  
  // sets/returns item's image index in the current image list
  //virtual int GetPageImage(int nPage) const = 0;
  inline virtual int getPageImage(int nPage) const { return getWx()->GetPageImage(nPage); }
  //virtual bool SetPageImage(int nPage, int nImage) = 0;
  inline virtual bool setPageImage(int nPage, int nImage) { return getWx()->SetPageImage(nPage, nImage); }
  
  // get the number of rows for a control with wxNB_MULTILINE style (not all
  // versions support it - they will always return 1 then)
  //virtual int GetRowCount() const { return 1; }
  inline virtual int getRowCount() const { return getWx()->GetRowCount(); }
  
  // set the size (the same for all pages)
  //virtual void SetPageSize(const wxSize& size) = 0;
  inline virtual void setPageSize(IN(RSize) size) { getWx()->SetPageSize(CLS2WXREF(size)); }
  
  // set the padding between tabs (in pixels)
  //virtual void SetPadding(const wxSize& padding) = 0;
  inline virtual void setPadding(IN(RSize) padding) { getWx()->SetPadding(CLS2WXREF(padding)); }
  
  // set the size of the tabs for wxNB_FIXEDWIDTH controls
  //virtual void SetTabSize(const wxSize& sz) = 0;
  inline virtual void setTabSize(IN(RSize) sz) { getWx()->SetTabSize(CLS2WXREF(sz)); }
  
  // calculate the size of the notebook from the size of its page
  //virtual wxSize CalcSizeFromPage(const wxSize& sizePage);
  inline virtual RSize calcSizeFromPage(IN(RSize) sizePage) { return WXVAL2CLS(Size, getWx()->CalcSizeFromPage(CLS2WXREF(sizePage))); }
  
  // operations
  // ----------
  
  // remove one page from the notebook and delete it
  //virtual bool DeletePage(int nPage);
  inline virtual bool deletePage(int nPage) { return getWx()->DeletePage(nPage); }
  
  // remove one page from the notebook, without deleting it
  //virtual bool RemovePage(int nPage) { return DoRemovePage(nPage) != NULL; }
  inline virtual bool removePage(int nPage) { return getWx()->RemovePage(nPage); }
  
  
  // remove all pages and delete them
  //virtual bool DeleteAllPages() { WX_CLEAR_ARRAY(m_pages); return TRUE; }
  inline virtual bool deleteAllPages() { return getWx()->DeleteAllPages(); }
  
  // adds a new page to the notebook (it will be deleted by the notebook,
  // don't delete it yourself) and make it the current one if bSelect
  //virtual bool AddPage(wxWindow *pPage, const wxString& strText, bool bSelect = FALSE, int imageId = -1)
    
  inline virtual bool addPage(IN(RWindow) pPage, IN(RString)  strText, bool bSelect = false, int imageId = -1) { return getWx()->AddPage(CLS2WXPTR(pPage), S2WXS(strText), bSelect, imageId); }
    // the same as AddPage(), but adds the page at the specified position
    //virtual bool InsertPage(int nPage, wxWindow *pPage, const wxString& strText, bool bSelect = FALSE, int imageId = -1) = 0;
  inline virtual bool insertPage(int nPage, IN(RWindow) pPage, IN(RString)  strText, bool bSelect = false, int imageId = -1) { return getWx()->InsertPage(nPage, CLS2WXPTR(pPage), S2WXS(strText), bSelect, imageId); }
  
  // set the currently selected page, return the index of the previously
  // selected one (or -1 on error)
  //
  // NB: this function will _not_ generate wxEVT_NOTEBOOK_PAGE_xxx events
  //virtual int SetSelection(int nPage) = 0;
  inline virtual int setSelection(int nPage) { return getWx()->SetSelection(nPage); }
  
  // cycle thru the tabs
  //void AdvanceSelection(bool forward = TRUE)
  inline void advanceSelection(bool forward = true) { getWx()->AdvanceSelection(forward); }
};


} // wx
} // acdk

#endif //acdk_wx_Notebook_h
