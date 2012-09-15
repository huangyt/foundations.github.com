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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/FileDataObject.h,v 1.3 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_FileDataObject_h
#define acdk_wx_FileDataObject_h

#include "DataObjectSimple.h"


namespace acdk {
namespace wx {

enum DataObjectDirection;

ACDK_DECL_CLASS(FileDataObject);

/**
  see wxFileDataObject
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC FileDataObject
: extends DataObjectSimple
{
  ACDK_WITH_METAINFO(FileDataObject)
public:
  ACDK_WX_STD_MEMBERS(FileDataObject, DataObjectSimple)
  FileDataObject() : DataObjectSimple(new wxFileDataObject()) {}  
  //virtual void AddFile(const wxString& file);
  inline void addFile(IN(RString)  file) { getWx()->AddFile(S2WXS(file)); }
  //const wxArrayString& GetFilenames() const;
  inline RStringArray getFilenames() const 
  { 
    const wxArrayString& wxret = getWx()->GetFilenames();
    int count = wxret.GetCount();
    RStringArray ret = new StringArray(count);
    for (int i = 0; i < count; ++i)
      ret[i] = WXS2S(wxret[i]);
    return ret; 
  }
};


} // wx
} // acdk

#endif //acdk_wx_FileDataObject_h
