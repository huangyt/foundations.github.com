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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/TextDropTarget.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_TextDropTarget_h
#define acdk_wx_TextDropTarget_h

#include "DropTarget.h"

namespace acdk {
namespace wx {


class ACDK_WX_PUBLIC TextDropTarget;

enum DragResult;

foreign 
class ACDK_WX_PUBLIC wxTextDropTargetFwd
: public wxTextDropTarget
, public AcdkForwarder<TextDropTarget>
{
public:
  wxTextDropTargetFwd()
  : wxTextDropTarget()
  {
  }
  virtual wxDragResult OnEnter(wxCoord x, wxCoord y, wxDragResult def);
  virtual wxDragResult OnDragOver(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y), wxDragResult def);
  virtual void OnLeave();
  virtual bool OnDrop(wxCoord x, wxCoord y);
  virtual wxDragResult OnData(wxCoord x, wxCoord y, wxDragResult def);
  virtual bool GetData();
  
  virtual bool OnDropText(wxCoord x, wxCoord y, const wxString& text);
};

typedef WxNonCopyStruct<wxTextDropTarget> TextDropTargetSuper;


ACDK_DECL_CLASS(TextDropTarget);

/**
  see wxTextDropTarget
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC TextDropTarget
: extends DropTarget
{
  ACDK_WITH_METAINFO(TextDropTarget)
public:
  ACDK_WX_STD_MEMBERS(TextDropTarget, DropTarget)
  
 //void wxTextDropTarget(wxWindow *win = NULL, const wxCursor &cursorCopy = wxNullCursor, const wxCursor &cursorMove = wxNullCursor, const wxCursor &cursorStop = wxNullCursor);
  inline TextDropTarget() 
  : DropTarget(new wxTextDropTargetFwd())
  { 
    dynamic_cast<wxTextDropTargetFwd*>(getWx())->setOwningForward(this);
  }
  inline virtual DragResult onEnter(int x, int y, DragResult def) { return DragResult(getWx()->wxTextDropTarget::OnEnter(x, y, wxDragResult(def))); }
  inline virtual DragResult onDragOver(int x, int y, DragResult def) { return DragResult(getWx()->wxTextDropTarget::OnDragOver(x, y, wxDragResult(def))); }
  inline virtual void onLeave() { getWx()->wxTextDropTarget::OnLeave(); }
  inline virtual bool onDrop(int x, int y) { return getWx()->wxTextDropTarget::OnDrop(x, y); }
  inline virtual DragResult onData(int x, int y, DragResult def) 
  { 
    return DragResult(getWx()->wxTextDropTarget::OnData(x, y, wxDragResult(def))); 
  }
  inline virtual bool getData() { return getWx()->wxTextDropTarget::GetData(); }

  //virtual bool OnDropText(wxCoord x, wxCoord y, const wxString& text);
  inline virtual bool onDropText(int x, int y, IN(RString)  text) 
  { 
    return false;
    //return getWx()->wxTextDropTarget::OnDropText(x, y, S2WXS(text)); 
  }
};

inline
wxDragResult 
wxTextDropTargetFwd::OnEnter(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onEnter(x, y, DragResult(def)));
}
  
inline
wxDragResult 
wxTextDropTargetFwd::OnDragOver(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onDragOver(x, y, DragResult(def)));
}

inline
void 
wxTextDropTargetFwd::OnLeave()
{
  _forward->onLeave();
}

inline
bool 
wxTextDropTargetFwd::OnDrop(wxCoord x, wxCoord y)
{
  return _forward->onDrop(x, y);
}

inline
wxDragResult 
wxTextDropTargetFwd::OnData(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onData(x, y, DragResult(def)));
}

inline
bool 
wxTextDropTargetFwd::GetData()
{
  return _forward->getData();
}

inline 
bool 
wxTextDropTargetFwd::OnDropText(wxCoord x, wxCoord y, const wxString& text)
{
  return _forward->onDropText(x, y, WXS2S(text));
}

} // wx
} // acdk

#endif //acdk_wx_TextDropTarget_h
