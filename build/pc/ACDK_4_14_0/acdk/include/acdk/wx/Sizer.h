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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Sizer.h,v 1.12 2005/03/11 11:11:50 kommer Exp $

#ifndef acdk_wx_Sizer_h
#define acdk_wx_Sizer_h

#include "Window.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(Sizer);

/**
  see wxSizer
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/03/11 11:11:50 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC Sizer
: extends WxObject
{
  ACDK_WITH_METAINFO(Sizer)
public:
  ACDK_WX_STD_MEMBERS(Sizer, WxObject)
  
  //virtual void Add( wxWindow *window, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void add(IN(RWindow) window, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Add(CLS2WXPTR(window), option, flag, border, CLS2WXPTR(userData)); }
    //virtual void Add( wxSizer *sizer, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void add(IN(RSizer) sizer, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Add(CLS2WXPTR(sizer), option, flag, border, CLS2WXPTR(userData)); }
    //virtual void Add( int width, int height, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void add(int width, int height, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Add(width, height, option, flag, border, CLS2WXPTR(userData)); }

    //virtual void Insert( int before, wxWindow *window, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void insert(int before, IN(RWindow) window, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Insert(before, CLS2WXPTR(window), option, flag, border, CLS2WXPTR(userData)); }
    //virtual void Insert( int before, wxSizer *sizer, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void insert(int before, IN(RSizer) sizer, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Insert(before, CLS2WXPTR(sizer), option, flag, border, CLS2WXPTR(userData)); }
    //virtual void Insert( int before, int width, int height, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void insert(int before, int width, int height, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Insert(before, width, height, option, flag, border, CLS2WXPTR(userData)); }

    //virtual void Prepend( wxWindow *window, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void prepend(IN(RWindow) window, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Prepend(CLS2WXPTR(window), option, flag, border, CLS2WXPTR(userData)); }
    //virtual void Prepend( wxSizer *sizer, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void prepend(IN(RSizer) sizer, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Prepend(CLS2WXPTR(sizer), option, flag, border, CLS2WXPTR(userData)); }
    //virtual void Prepend( int width, int height, int option = 0, int flag = 0, int border = 0, wxObject* userData = NULL );
  inline virtual void prepend(int width, int height, int option = 0, int flag = 0, int border = 0, IN(RWxObject)  userData = Nil) { getWx()->Prepend(width, height, option, flag, border, CLS2WXPTR(userData)); }

    //virtual bool Remove( wxWindow *window );
  inline virtual bool detach(IN(RWindow) window) { return getWx()->Detach(CLS2WXPTR(window)); }
    //virtual bool Remove( wxSizer *sizer );
  inline virtual bool remove(IN(RSizer) sizer) { return getWx()->Remove(CLS2WXPTR(sizer)); }
    //virtual bool Remove( int pos );
  inline virtual bool remove(int pos) { return getWx()->Remove(pos); }
    
    //virtual void Clear( bool delete_windows=FALSE );
  inline virtual void clear(bool delete_windows = false) { getWx()->Clear(delete_windows); }
    //virtual void DeleteWindows();
  inline virtual void deleteWindows() { getWx()->DeleteWindows(); }

    //void SetMinSize( int width, int height )        { DoSetMinSize( width, height ); }
  inline void setMinSize(int width, int height) { getWx()->SetMinSize(width, height); }
    //void SetMinSize( wxSize size )        { DoSetMinSize( size.x, size.y ); }
  inline void setMinSize(IN(RSize) size) { getWx()->SetMinSize(CLS2WXREF(size)); }

    /* Searches recursively */
    //bool SetItemMinSize( wxWindow *window, int width, int height )        { return DoSetItemMinSize( window, width, height ); }
  inline bool setItemMinSize(IN(RWindow) window, int width, int height) { return getWx()->SetItemMinSize(CLS2WXPTR(window), width, height); }
    //bool SetItemMinSize( wxWindow *window, wxSize size )        { return DoSetItemMinSize( window, size.x, size.y ); }
  inline bool setItemMinSize(IN(RWindow) window, IN(RSize) size) { return getWx()->SetItemMinSize(CLS2WXPTR(window), CLS2WXREF(size)); }

    /* Searches recursively */
    //bool SetItemMinSize( wxSizer *sizer, int width, int height )        { return DoSetItemMinSize( sizer, width, height ); }
  inline bool setItemMinSize(IN(RSizer) sizer, int width, int height) { return getWx()->SetItemMinSize(CLS2WXPTR(sizer), width, height); }
    //bool SetItemMinSize( wxSizer *sizer, wxSize size )        { return DoSetItemMinSize( sizer, size.x, size.y ); }
  inline bool setItemMinSize(IN(RSizer) sizer, IN(RSize) size) { return getWx()->SetItemMinSize(CLS2WXPTR(sizer), CLS2WXREF(size)); }

    //bool SetItemMinSize( int pos, int width, int height )        { return DoSetItemMinSize( pos, width, height ); }
  inline bool setItemMinSize(int pos, int width, int height) { return getWx()->SetItemMinSize(pos, width, height); }
    //bool SetItemMinSize( int pos, wxSize size )        { return DoSetItemMinSize( pos, size.x, size.y ); }
  inline bool setItemMinSize(int pos, IN(RSize) size) { return getWx()->SetItemMinSize(pos, CLS2WXREF(size)); }

    //wxSize GetSize();
  inline RSize getSize() { return WXVAL2CLS(Size, getWx()->GetSize()); }
  
    //wxPoint GetPosition();
  inline RPoint getPosition() { return WXVAL2CLS(Point, getWx()->GetPosition()); }
  

    /* Calculate the minimal size or return m_minSize if bigger. */
    //wxSize GetMinSize();
  inline RSize getMinSize() { return WXVAL2CLS(Size, getWx()->GetMinSize()); }
  

    //virtual void RecalcSizes() = 0;
  inline virtual void recalcSizes() { getWx()->RecalcSizes(); }
    //virtual wxSize CalcMin() = 0;
  inline virtual RSize calcMin() { return WXVAL2CLS(Size, getWx()->CalcMin()); }
  

    //virtual void Layout();
  inline virtual void layout() { getWx()->Layout(); }

  //wxSize Fit( wxWindow *window );
  inline RSize fit(IN(RWindow) window) { return WXVAL2CLS(Size, getWx()->Fit(CLS2WXPTR(window))); }
    //void FitInside( wxWindow *window );
  inline void fitInside(IN(RWindow) window) { getWx()->FitInside(CLS2WXPTR(window)); }
    //void SetSizeHints( wxWindow *window );
  inline void setSizeHints(IN(RWindow) window) { getWx()->SetSizeHints(CLS2WXPTR(window)); }
    //void SetVirtualSizeHints( wxWindow *window );
  inline void setVirtualSizeHints(IN(RWindow) window) { getWx()->SetVirtualSizeHints(CLS2WXPTR(window)); }

  //### TODO wxList& GetChildren();

    //void SetDimension( int x, int y, int width, int height );
  inline void setDimension(int x, int y, int width, int height) { getWx()->SetDimension(x, y, width, height); }

    // Manage whether individual windows or sub-sizers are considered
    // in the layout calculations or not.
    //void Show( wxWindow *window, bool show = TRUE );
  inline void show(IN(RWindow) window, bool show = true) { getWx()->Show(CLS2WXPTR(window), show); }
    //void Hide( wxWindow *window );
  inline void hide(IN(RWindow) window) { getWx()->Hide(CLS2WXPTR(window)); }
    //void Show( wxSizer *sizer, bool show = TRUE );
  inline void show(IN(RSizer) sizer, bool show = true) { getWx()->Show(CLS2WXPTR(sizer), show); }
    //void Hide( wxSizer *sizer );
  inline void hide(IN(RSizer) sizer) { getWx()->Hide(CLS2WXPTR(sizer)); }

    //bool IsShown( wxWindow *window );
  inline bool isShown(IN(RWindow) window) { return getWx()->IsShown(CLS2WXPTR(window)); }
    //bool IsShown( wxSizer *sizer );
  inline bool isShown(IN(RSizer) sizer) { return getWx()->IsShown(CLS2WXPTR(sizer)); }
    
    // Recursively call wxWindow::Show () on all sizer items.
    //void ShowItems (bool show);
  inline void showItems(bool show) { getWx()->ShowItems(show); }
};

inline 
void 
Window::setSizer(IN(RSizer) sizer, bool deleteOld) 
{ 
  getWx()->wxWindow::SetSizer(CLS2WXPTR(sizer), deleteOld); 
}
inline 
void 
Window::setSizerAndFit(IN(RSizer) sizer, bool deleteOld) 
{ 
  getWx()->wxWindow::SetSizerAndFit(CLS2WXPTR(sizer), deleteOld); 
}

inline RSizer 
Window::getSizer() const 
{ 
  RETURN_WXPTR2CLS(Sizer, getWx()->wxWindow::GetSizer()); 
}

inline 
void 
Window::setContainingSizer(IN(RSizer) sizer) 
{ 
  getWx()->wxWindow::SetContainingSizer(CLS2WXPTR(sizer)); 
}

inline 
RSizer 
Window::getContainingSizer() const 
{ 
  RETURN_WXPTR2CLS(Sizer, getWx()->wxWindow::GetContainingSizer()); 
}

ACDK_DECL_CLASS(GridSizer);

/**
  see wxGridSizer
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.12 $
  @date $Date: 2005/03/11 11:11:50 $
*/
class ACDK_WX_PUBLIC GridSizer
: extends Sizer
{
  ACDK_WITH_METAINFO(GridSizer)
public:
  ACDK_WX_STD_MEMBERS(GridSizer, Sizer)
  
  GridSizer( int rows, int cols, int vgap, int hgap ) : Sizer(new wxGridSizer(rows, cols, vgap, hgap)) {}
  GridSizer( int cols, int vgap = 0, int hgap = 0 ) : Sizer(new wxGridSizer(cols, vgap, hgap)) {}
    //void RecalcSizes();
  inline void recalcSizes() { getWx()->RecalcSizes(); }
  //wxSize CalcMin();
  inline RSize calcMin() { return WXVAL2CLS(Size, getWx()->CalcMin()); }
  

    //void SetCols( int cols )    { m_cols = cols; }
  inline void setCols(int cols) { getWx()->SetCols(cols); }
    //void SetRows( int rows )    { m_rows = rows; }
  inline void setRows(int rows) { getWx()->SetRows(rows); }
    //void SetVGap( int gap )     { m_vgap = gap; }
  inline void setVGap(int gap) { getWx()->SetVGap(gap); }
    //void SetHGap( int gap )     { m_hgap = gap; }
  inline void setHGap(int gap) { getWx()->SetHGap(gap); }
    //int GetCols()               { return m_cols; }
  inline int getCols() { return getWx()->GetCols(); }
    //int GetRows()               { return m_rows; }
  inline int getRows() { return getWx()->GetRows(); }
    //int GetVGap()               { return m_vgap; }
  inline int getVGap() { return getWx()->GetVGap(); }
    //int GetHGap()               { return m_hgap; }
  inline int getHGap() { return getWx()->GetHGap(); }

};


} // wx
} // acdk

#endif //acdk_wx_Sizer_h
