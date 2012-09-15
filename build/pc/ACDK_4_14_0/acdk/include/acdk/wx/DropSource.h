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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/DropSource.h,v 1.7 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_DropSource_h
#define acdk_wx_DropSource_h

#include "DataObjectSimple.h"
#include "DropTarget.h"

namespace acdk {
namespace wx {

enum DragResult;

/**
  see wxDropSource
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum DragPermission
{
    DragCopyOnly    = wxDrag_CopyOnly   ,  // wxDrag_CopyOnly    = 0, // allow only copying
    DragAllowMove   = wxDrag_AllowMove  ,  // wxDrag_AllowMove   = 1, // allow moving (copying is always allowed)
    DragDefaultMove = wxDrag_DefaultMove  // wxDrag_DefaultMove = 3  // the default operation is move, not copy
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, DragPermission);

class ACDK_WX_PUBLIC DropSource;


foreign 
class ACDK_WX_PUBLIC wxDropSourceFwd
: public wxDropSource
, public AcdkForwarder<DropSource>
{
public:
#if defined(ACDK_OS_WIN32)
  wxDropSourceFwd(wxWindow *win = NULL,
                 const wxCursor &cursorCopy = wxNullCursor,
                 const wxCursor &cursorMove = wxNullCursor,
                 const wxCursor &cursorStop = wxNullCursor)
  : wxDropSource(win, cursorCopy, cursorMove, cursorStop)
  {
  }
  wxDropSourceFwd(wxDataObject& data,
                 wxWindow *win = NULL,
                 const wxCursor &cursorCopy = wxNullCursor,
                 const wxCursor &cursorMove = wxNullCursor,
                 const wxCursor &cursorStop = wxNullCursor)
  : wxDropSource(data, win, cursorCopy, cursorMove, cursorStop)
  {
  }
#else
  wxDropSourceFwd(wxWindow *win = NULL,
                 const wxIcon &cursorCopy = wxNullIcon,
                 const wxIcon &cursorMove = wxNullIcon,
                 const wxIcon &cursorStop = wxNullIcon)
  : wxDropSource(win, cursorCopy, cursorMove, cursorStop)
  {
  }
  wxDropSourceFwd(wxDataObject& data,
                 wxWindow *win = NULL,
                 const wxIcon &cursorCopy = wxNullIcon,
                 const wxIcon &cursorMove = wxNullIcon,
                 const wxIcon &cursorStop = wxNullIcon)
  : wxDropSource(data, win, cursorCopy, cursorMove, cursorStop)
  {
  }
#endif
  
    // overridable: you may give some custom UI feedback during d&d operation
    // in this function (it's called on each mouse move, so it shouldn't be
    // too slow). Just return false if you want default feedback.
  virtual bool GiveFeedback(wxDragResult effect);
};

typedef WxNonCopyStruct<wxDropSource> DropSourceSuper;


ACDK_DECL_CLASS(DropSource);

/**
  see wxDropSource
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.7 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC DropSource
: extends DropSourceSuper
{
  ACDK_WITH_METAINFO(DropSource)
public:
  ACDK_WX_STD_MEMBERS(DropSource, DropSourceSuper)
  
 //void wxDropSource(wxWindow *win = NULL, const wxCursor &cursorCopy = wxNullCursor, const wxCursor &cursorMove = wxNullCursor, const wxCursor &cursorStop = wxNullCursor);
  inline DropSource(IN(RWindow) win = Nil
      /*, IN(RCursor) cursorCopy = Cursor::getNullCursor(), IN(RCursor) cursorMove = Cursor::getNullCursor(), IN(RCursor) cursorStop = Cursor::getNullCursor()*/) 
  : DropSourceSuper(new wxDropSourceFwd(CLS2WXPTR(win)/*, CLS2WXREF(cursorCopy), CLS2WXREF(cursorMove), CLS2WXREF(cursorStop)*/))
  { 
    dynamic_cast<wxDropSourceFwd*>(getWx())->setOwningForward(this);
  }
  //void wxDropSource(wxDataObject& data, wxWindow *win = NULL, const wxCursor &cursorCopy = wxNullCursor, const wxCursor &cursorMove = wxNullCursor, const wxCursor &cursorStop = wxNullCursor);
  inline DropSource(IN(RDataObject) data, IN(RWindow) win = Nil/*, IN(RCursor) cursorCopy = Cursor::getNullCursor(), IN(RCursor) cursorMove = Cursor::getNullCursor(), IN(RCursor) cursorStop = Cursor::getNullCursor()*/) 
  : DropSourceSuper(new wxDropSourceFwd(CLS2WXREF(data), CLS2WXPTR(win)/*, CLS2WXREF(cursorCopy), CLS2WXREF(cursorMove), CLS2WXREF(cursorStop)*/))
  { 
    dynamic_cast<wxDropSourceFwd*>(getWx())->setOwningForward(this);
  }
  
    
    // do it (call this in response to a mouse button press, for example)
    // params: if bAllowMove is false, data can be only copied
    //virtual wxDragResult DoDragDrop(int flags = wxDrag_CopyOnly);
  inline virtual DragResult doDragDrop(int flags = DragCopyOnly) { return DragResult(getWx()->wxDropSource::DoDragDrop(flags)); }

    // overridable: you may give some custom UI feedback during d&d operation
    // in this function (it's called on each mouse move, so it shouldn't be
    // too slow). Just return false if you want default feedback.
  //virtual bool GiveFeedback(wxDragResult effect);
  inline virtual bool giveFeedback(DragResult effect) { return getWx()->wxDropSource::GiveFeedback(wxDragResult(effect)); }

};

inline
bool 
wxDropSourceFwd::GiveFeedback(wxDragResult effect)
{
  return _forward->giveFeedback(DragResult(effect)); 
}

} // wx
} // acdk

#endif //acdk_wx_DropSource_h
