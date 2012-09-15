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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/Clipboard.h,v 1.3 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_Clipboard_h
#define acdk_wx_Clipboard_h

#include "WxObject.h"
#include "DataObject.h"

namespace acdk {
namespace wx {


ACDK_DECL_CLASS(Clipboard);

/**
  see wxClipboard
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC Clipboard
: extends WxObject
{
  ACDK_WITH_METAINFO(Clipboard)
public:
  
  ACDK_WX_STD_VAL_MEMBERS(Clipboard, WxObject)
  Clipboard() : WxObject(new wxClipboard()) {}
  // open the clipboard before Add/SetData() and GetData()
  //virtual bool Open();
  inline virtual bool open() { return getWx()->Open(); }
  
  // close the clipboard after Add/SetData() and GetData()
  //virtual void Close();
  inline virtual void close() { getWx()->Close(); }
  
  // query whether the clipboard is opened
  //virtual bool IsOpened() const;
  inline virtual bool isOpened() const { return getWx()->IsOpened(); }
  
  // add to the clipboard data
  //
  // NB: the clipboard owns the pointer and will delete it, so data must be
  //     allocated on the heap
  //virtual bool AddData( wxDataObject *data );
  inline virtual bool addData(IN(RDataObject) data) { return getWx()->AddData(CLS2WXPTR(data)); }
  
  // set the clipboard data, this is the same as Clear() followed by
  // AddData()
  //virtual bool SetData( wxDataObject *data );
  inline virtual bool setData(IN(RDataObject) data) { return getWx()->SetData(CLS2WXPTR(data)); }
  
  // ask if data in correct format is available
  //virtual bool IsSupported( const wxDataFormat& format );
  inline virtual bool isSupported(IN(RDataFormat) format) { return getWx()->IsSupported(CLS2WXREF(format)); }
  
  // fill data with data on the clipboard (if available)
  //virtual bool GetData( wxDataObject& data );
  inline virtual bool getData(INOUT(RDataObject) data) 
  { 
    return getWx()->GetData(*data->getWx()); 
  }
  
  // clears wxTheClipboard and the system's clipboard if possible
  //virtual void Clear();
  inline virtual void clear() { getWx()->Clear(); }
  
  // flushes the clipboard: this means that the data which is currently on
  // clipboard will stay available even after the application exits (possibly
  //// eating memory), otherwise the clipboard will be emptied on exit
  //virtual bool Flush() { return FALSE; }
  inline virtual bool flush() { return getWx()->Flush(); }
  
  // X11 has two clipboards which get selected by this call. Empty on MSW.
  //virtual void UsePrimarySelection( bool primary = FALSE ) { }
  inline virtual void usePrimarySelection(bool primary = false) { getWx()->UsePrimarySelection(primary); }


};


} // wx
} // acdk

#endif //acdk_wx_Clipboard_h
