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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/FileDropTarget.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_FileDropTarget_h
#define acdk_wx_FileDropTarget_h

#include "DropTarget.h"

namespace acdk {
namespace wx {


class ACDK_WX_PUBLIC FileDropTarget;

enum DragResult;

foreign 
class ACDK_WX_PUBLIC wxFileDropTargetFwd
: public wxFileDropTarget
, public AcdkForwarder<FileDropTarget>
{
public:
  wxFileDropTargetFwd()
  : wxFileDropTarget()
  {
  }
  virtual wxDragResult OnEnter(wxCoord x, wxCoord y, wxDragResult def);
  virtual wxDragResult OnDragOver(wxCoord x, wxCoord y, wxDragResult def);
  virtual void OnLeave();
  virtual bool OnDrop(wxCoord x, wxCoord y);
  virtual wxDragResult OnData(wxCoord x, wxCoord y, wxDragResult def);
  virtual bool GetData();
  virtual bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames);
  
};


ACDK_DECL_CLASS(FileDropTarget);
/**
  see wxFileDropTarget
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC FileDropTarget
: extends DropTarget
{
  ACDK_WITH_METAINFO(FileDropTarget)
public:
  ACDK_WX_STD_MEMBERS(FileDropTarget, DropTarget)
  
 
  inline FileDropTarget() 
  : DropTarget(new wxFileDropTargetFwd())
  { 
    wxFileDropTarget* wxt = getWx();
    dynamic_cast<wxFileDropTargetFwd*>(wxt)->setOwningForward(this);
  }
  inline virtual DragResult onData(int x, int y, DragResult def) 
  { 
    return DragResult(getWx()->wxFileDropTarget::OnData(x, y, wxDragResult(def))); 
  }
  inline virtual DragResult onEnter(int x, int y, DragResult def) 
  { 
    return DragResult(getWx()->wxFileDropTarget::OnEnter(x, y, wxDragResult(def))); 
  }
  inline virtual DragResult onDragOver(int x, int y, DragResult def) 
  { 
    return DragResult(getWx()->wxFileDropTarget::OnDragOver(x, y, wxDragResult(def))); 
  }
  inline virtual void onLeave() 
  { 
    getWx()->wxFileDropTarget::OnLeave(); 
  }
  inline virtual bool onDrop(int x, int y) 
  { 
    return getWx()->wxFileDropTarget::OnDrop(x, y); 
  }
  inline virtual bool getData() 
  { 
    return getWx()->wxFileDropTarget::GetData(); 
  }
  
  //virtual bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames);
  inline virtual bool onDropFiles(int x, int y, IN(RStringArray) filenames) 
  { 
    return false;
    /*
    int count = filenames->length();
    wxArrayString wxfilenames;
    for (int i = 0; i < count; ++i)
      wxfilenames.Add(S2WXS(filenames[i]));
    return getWx()->OnDropFiles(x, y, wxfilenames); 
    */
  }
};


inline
wxDragResult 
wxFileDropTargetFwd::OnEnter(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onEnter(x, y, DragResult(def)));
}
  
inline
wxDragResult 
wxFileDropTargetFwd::OnDragOver(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onDragOver(x, y, DragResult(def)));
}

inline
void 
wxFileDropTargetFwd::OnLeave()
{
  _forward->onLeave();
}

inline
bool 
wxFileDropTargetFwd::OnDrop(wxCoord x, wxCoord y)
{
  return _forward->onDrop(x, y);
}

inline
wxDragResult 
wxFileDropTargetFwd::OnData(wxCoord x, wxCoord y, wxDragResult def)
{
  return wxDragResult(_forward->onData(x, y, DragResult(def)));
}

inline
bool 
wxFileDropTargetFwd::GetData()
{
  return _forward->getData();
}

inline 
bool 
wxFileDropTargetFwd::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames)
{
  int count = filenames.GetCount();
  RStringArray sa = new StringArray(count);
  for (int i = 0; i < count; ++i)
    sa[i] = WXS2S(filenames[i]);
  return _forward->onDropFiles(x, y, sa);
}

} // wx
} // acdk

#endif //acdk_wx_FileDropTarget_h
