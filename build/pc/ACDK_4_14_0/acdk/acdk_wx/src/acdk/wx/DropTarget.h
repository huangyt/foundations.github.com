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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/DropTarget.h,v 1.6 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_DropTarget_h
#define acdk_wx_DropTarget_h

#include "DataObject.h"


namespace acdk {
namespace wx {


class ACDK_WX_PUBLIC DropTarget;


/**
  see wxDragResult
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum DragResult
{
    DragError = wxDragError,  // wxDragError,    // error prevented the d&d operation from completing
    DragNone = wxDragNone,  // wxDragNone,     // drag target didn't accept the data
    DragCopy = wxDragCopy,  // wxDragCopy,     // the data was successfully copied
    DragMove = wxDragMove,  // wxDragMove,     // the data was successfully moved
    DragCancel    = wxDragCancel     // wxDragCancel    // the operation was cancelled by user (not an error)
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, DragResult);

foreign 
class ACDK_WX_PUBLIC wxDropTargetFwd
: public wxDropTarget
, public AcdkForwarder<DropTarget>
{
public:
  wxDropTargetFwd(wxDataObject *dataObject = NULL)
  : wxDropTarget(dataObject)
  {
  }
  
    
  // these functions are called when data is moved over position (x, y) and
    // may return either wxDragCopy, wxDragMove or wxDragNone depending on
    // what would happen if the data were dropped here.
    //
    // the last parameter is what would happen by default and is determined by
    // the platform-specific logic (for example, under Windows it's wxDragCopy
    // if Ctrl key is pressed and wxDragMove otherwise) except that it will
    // always be wxDragNone if the carried data is in an unsupported format.

    // called when the mouse enters the window (only once until OnLeave())
  virtual wxDragResult OnEnter(wxCoord x, wxCoord y, wxDragResult def);
  
    // called when the mouse moves in the window - shouldn't take long to
    // execute or otherwise mouse movement would be too slow
  virtual wxDragResult OnDragOver(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y), wxDragResult def);

    // called when mouse leaves the window: might be used to remove the
    // feedback which was given in OnEnter()
  virtual void OnLeave();

    // this function is called when data is dropped at position (x, y) - if it
    // returns TRUE, OnData() will be called immediately afterwards which will
    // allow to retrieve the data dropped.
  virtual bool OnDrop(wxCoord x, wxCoord y);

    // called after OnDrop() returns TRUE: you will usually just call
    // GetData() from here and, probably, also refresh something to update the
    // new data and, finally, return the code indicating how did the operation
    // complete (returning default value in case of success and wxDragError on
    // failure is usually ok)
  virtual wxDragResult OnData(wxCoord x, wxCoord y, wxDragResult def);

    // may be called *only* from inside OnData() and will fill m_dataObject
    // with the data from the drop source if it returns TRUE
  virtual bool GetData();
};

typedef WxNonCopyStruct<wxDropTarget> DropTargetSuper;


ACDK_DECL_CLASS(DropTarget);

/**
  see wxDragResult
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC DropTarget
: extends DropTargetSuper
{
  ACDK_WITH_METAINFO(DropTarget)
public:
  ACDK_WX_STD_MEMBERS(DropTarget, DropTargetSuper)
  
 //void wxDropTarget(wxWindow *win = NULL, const wxCursor &cursorCopy = wxNullCursor, const wxCursor &cursorMove = wxNullCursor, const wxCursor &cursorStop = wxNullCursor);
  inline DropTarget(IN(RDataObject) dataObject = Nil) 
  : DropTargetSuper(new wxDropTargetFwd(CLS2WXPTR(dataObject)))
  { 
    dynamic_cast<wxDropTargetFwd*>(getWx())->setOwningForward(this);
  }
  // these functions are called when data is moved over position (x, y) and
    // may return either wxDragCopy, wxDragMove or wxDragNone depending on
    // what would happen if the data were dropped here.
    //
    // the last parameter is what would happen by default and is determined by
    // the platform-specific logic (for example, under Windows it's wxDragCopy
    // if Ctrl key is pressed and wxDragMove otherwise) except that it will
    // always be wxDragNone if the carried data is in an unsupported format.

    // called when the mouse enters the window (only once until OnLeave())
  //virtual wxDragResult OnEnter(wxCoord x, wxCoord y, wxDragResult def);
  inline virtual DragResult onEnter(int x, int y, DragResult def) { return DragResult(getWx()->wxDropTarget::OnEnter(x, y, wxDragResult(def))); }
  
    // called when the mouse moves in the window - shouldn't take long to
    // execute or otherwise mouse movement would be too slow
  //virtual wxDragResult OnDragOver(wxCoord x, wxCoord y, wxDragResult def);
  inline virtual DragResult onDragOver(int x, int y, DragResult def) { return DragResult(getWx()->wxDropTarget::OnDragOver(x, y, wxDragResult(def))); }

    // called when mouse leaves the window: might be used to remove the
    // feedback which was given in OnEnter()
  //virtual void OnLeave();
  inline virtual void onLeave() { getWx()->wxDropTarget::OnLeave(); }

    // this function is called when data is dropped at position (x, y) - if it
    // returns TRUE, OnData() will be called immediately afterwards which will
    // allow to retrieve the data dropped.
  //virtual bool OnDrop(wxCoord x, wxCoord y);
  inline virtual bool onDrop(int x, int y) { return getWx()->wxDropTarget::OnDrop(x, y); }

    // called after OnDrop() returns TRUE: you will usually just call
    // GetData() from here and, probably, also refresh something to update the
    // new data and, finally, return the code indicating how did the operation
    // complete (returning default value in case of success and wxDragError on
    // failure is usually ok)
  //virtual wxDragResult OnData(wxCoord x, wxCoord y, wxDragResult def);
  inline virtual DragResult onData(int x, int y, DragResult def) 
  { 
    return DragResult(getWx()->OnData(x, y, wxDragResult(def))); 
  }

    // may be called *only* from inside OnData() and will fill m_dataObject
    // with the data from the drop source if it returns TRUE
  //virtual bool GetData();
  inline virtual bool getData() { return getWx()->wxDropTarget::GetData(); }
  //void SetDataObject(wxDataObject* data);
  inline void setDataObject(IN(RDataObject) data) { getWx()->wxDropTarget::SetDataObject(CLS2WXPTR(data)); }
};


inline
wxDragResult 
wxDropTargetFwd::OnEnter(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onEnter(x, y, DragResult(def)));
}
  
inline
wxDragResult 
wxDropTargetFwd::OnDragOver(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onDragOver(x, y, DragResult(def)));
}

inline
void 
wxDropTargetFwd::OnLeave()
{
  _forward->onLeave();
}

inline
bool 
wxDropTargetFwd::OnDrop(wxCoord x, wxCoord y)
{
  return _forward->onDrop(x, y);
}

inline
wxDragResult 
wxDropTargetFwd::OnData(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onData(x, y, DragResult(def)));
}

inline
bool 
wxDropTargetFwd::GetData()
{
  return _forward->getData();
}

} // wx
} // acdk

#endif //acdk_wx_DropTarget_h
