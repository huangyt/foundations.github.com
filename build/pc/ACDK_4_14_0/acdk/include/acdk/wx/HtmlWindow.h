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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/HtmlWindow.h,v 1.4 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_HtmlWindow_h
#define acdk_wx_HtmlWindow_h

#include "ScrolledWindow.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(HtmlWindow);

/**
  see wxHtmlWindow
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.4 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC HtmlWindow
: extends ScrolledWindow
{
  ACDK_WITH_METAINFO(HtmlWindow)
public:
  // wxHtmlWindow
  // wxScrolledWindow
  ACDK_WX_STD_MEMBERS(HtmlWindow, ScrolledWindow)

  HtmlWindow(IN(RWindow) parent, int id, IN(RPoint) pos = Point::defaultPosition(), IN(RSize) size = Size::defaultSize(),  int style = 0, IN(RString) name = "htmlWindow")
  : ScrolledWindow(new wxHtmlWindow(CLS2WXPTR(parent), id, CLS2WXREF(pos), CLS2WXREF(size), style, S2WXS(name)), parent == Nil)
  {
  }
   // Set HTML page and display it. !! source is HTML document itself,
    // it is NOT address/filename of HTML document. If you want to
    // specify document location, use LoadPage() istead
    // Return value : FALSE if an error occured, TRUE otherwise
  //bool SetPage(const wxString& source);
  inline bool setPage(IN(RString)  source) { return getWx()->SetPage(S2WXS(source)); }
  
  // Append to current page
  //bool AppendToPage(const wxString& source);
  inline bool appendToPage(IN(RString)  source) { return getWx()->AppendToPage(S2WXS(source)); }
  
  // Load HTML page from given location. Location can be either
  // a) /usr/wxGTK2/docs/html/wx.htm
  // b) http://www.somewhere.uk/document.htm
  // c) ftp://ftp.somesite.cz/pub/something.htm
  // In case there is no prefix (http:,ftp:), the method
  // will try to find it itself (1. local file, then http or ftp)
  // After the page is loaded, the method calls SetPage() to display it.
  // Note : you can also use path relative to previously loaded page
  // Return value : same as SetPage
  //virtual bool LoadPage(const wxString& location);
  inline virtual bool loadPage(IN(RString)  location) { return getWx()->LoadPage(S2WXS(location)); }
  
  // Loads HTML page from file
  //bool LoadFile(const wxFileName& filename);
  inline bool loadFile(IN(RString) filename) 
  { 
    wxFileName wxfn(S2WXS(filename));
    return getWx()->LoadFile(wxfn); 
  }
  
  // Returns full location of opened page
  //wxString GetOpenedPage() const {return m_OpenedPage;}
  inline RString getOpenedPage() const { return WXS2S(getWx()->GetOpenedPage()); }
  // Returns anchor within opened page
  //wxString GetOpenedAnchor() const {return m_OpenedAnchor;}
  inline RString getOpenedAnchor() const { return WXS2S(getWx()->GetOpenedAnchor()); }
  // Returns <TITLE> of opened page or empty string otherwise
  //wxString GetOpenedPageTitle() const {return m_OpenedPageTitle;}
  inline RString getOpenedPageTitle() const { return WXS2S(getWx()->GetOpenedPageTitle()); }
  
  // Sets frame in which page title will  be displayed. Format is format of
  // frame title, e.g. "HtmlHelp : %s". It must contain exactly one %s
  //void SetRelatedFrame(wxFrame* frame, const wxString& format);
  inline void setRelatedFrame(IN(RFrame) frame, IN(RString)  format) { getWx()->SetRelatedFrame(CLS2WXPTR(frame), S2WXS(format)); }
  //wxFrame* GetRelatedFrame() const {return m_RelatedFrame;}
  inline RFrame getRelatedFrame() const { RETURN_WXPTR2CLS(Frame, getWx()->GetRelatedFrame()); }
  
  // After(!) calling SetRelatedFrame, this sets statusbar slot where messages
  // will be displayed. Default is -1 = no messages.
  //void SetRelatedStatusBar(int bar);
  inline void setRelatedStatusBar(int bar) { getWx()->SetRelatedStatusBar(bar); }
  
  // Sets fonts to be used when displaying HTML page.
  //void SetFonts(wxString normal_face, wxString fixed_face, const int *sizes = NULL);
  //@todo inline void setFonts(IN(RString)  normal_face, IN(RString)  fixed_face, int sizes = Nil) { getWx()->SetFonts(S2WXS(normal_face), S2WXS(fixed_face), sizes); }
  
  // Sets space between text and window borders.
  //void SetBorders(int b) {m_Borders = b;}
  inline void setBorders(int b) { getWx()->SetBorders(b); }
  
  // Saves custom settings into cfg config. it will use the path 'path'
  // if given, otherwise it will save info into currently selected path.
  // saved values : things set by SetFonts, SetBorders.
  //@todo virtual void ReadCustomization(wxConfigBase *cfg, wxString path = wxEmptyString);
  // ...
  //@todo virtual void WriteCustomization(wxConfigBase *cfg, wxString path = wxEmptyString);
  
  // Goes to previous/next page (in browsing history)
  // Returns TRUE if successful, FALSE otherwise
  //bool HistoryBack();
  inline bool historyBack() { return getWx()->HistoryBack(); }
  //bool HistoryForward();
  inline bool historyForward() { return getWx()->HistoryForward(); }
  //bool HistoryCanBack();
  inline bool historyCanBack() { return getWx()->HistoryCanBack(); }
  //bool HistoryCanForward();
  inline bool historyCanForward() { return getWx()->HistoryCanForward(); }
  // Resets history
  //void HistoryClear();
  inline void historyClear() { getWx()->HistoryClear(); }
  
  // Returns pointer to conteiners/cells structure.
  // It should be used ONLY when printing
  //@todo wxHtmlContainerCell* GetInternalRepresentation() const {return m_Cell;}
  
  // Adds input filter
  //@todo static void AddFilter(wxHtmlFilter *filter);
  
  // Returns a pointer to the parser.
  //@todo wxHtmlWinParser *GetParser() const { return m_Parser; }

};


} // wx
} // acdk

#endif //acdk_wx_HtmlWindow_h
