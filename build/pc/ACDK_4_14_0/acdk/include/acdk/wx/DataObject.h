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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/DataObject.h,v 1.6 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_DataObject_h
#define acdk_wx_DataObject_h

#include "WxObject.h"
#include "DataFormat.h"


namespace acdk {
namespace wx {

typedef WxNonCopyStruct<wxDataObject> DataObjectSuper;

ACDK_DECL_CLASS(DataObject);


/**
  see wxDataObject
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum DataObjectDirection
{
  DataObjectGet = 0x01,    // format is supported by GetDataHere()
  DataObjectSet  = 0x02,    // format is supported by SetData()
  DataObjectBoth = 0x03     // format is supported by both (unused currently)
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, DataObjectDirection);

/**
  see wxDataObject
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_WX_PUBLIC DataObject
: extends DataObjectSuper
{
  ACDK_WITH_METAINFO(DataObject)
public:
  ACDK_WX_STD_MEMBERS(DataObject, DataObjectSuper)
  //DataObject() : WxObject(new wxDataObject()) {}
  //virtual wxDataFormat GetPreferredFormat(Direction dir = Get) const;
  inline virtual RDataFormat getPreferredFormat(DataObjectDirection dir = DataObjectGet) const 
  { 
    //return WXVAL2CLS(DataFormat, getWx()->GetPreferredFormat(wxDataObject::Direction(dir)));
    return WXVAL2CLS(DataFormat, (int)(wxDataFormat::NativeFormat)getWx()->GetPreferredFormat(wxDataObject::Direction(dir))); 
  }
  
  // get the number of formats we support
  //virtual size_t GetFormatCount(Direction dir = Get) const;
  inline virtual int getFormatCount(DataObjectDirection dir = DataObjectGet) const { return getWx()->GetFormatCount(wxDataObject::Direction(dir)); }
  
  // return all formats in the provided array (of size GetFormatCount())
  //virtual void GetAllFormats(wxDataFormat *formats, Direction dir = Get) const;
  inline virtual RDataFormatArray getAllFormats(DataObjectDirection dir = DataObjectGet) const 
  { 
    int count = getFormatCount();
    wxDataFormat* wxFormats = new wxDataFormat[count];
    getWx()->GetAllFormats(wxFormats, wxDataObject::Direction(dir)); 
    RDataFormatArray ret = new DataFormatArray(count);
    for (int i = 0; i < count; ++i)
      ret[i] = new DataFormat((int)(wxDataFormat::NativeFormat)wxFormats[i]);
    return ret;
  }
  
  // get the (total) size of data for the given format
  //virtual size_t GetDataSize(const wxDataFormat& format) const;
  inline virtual int getDataSize(IN(RDataFormat) format) const { return getWx()->GetDataSize(CLS2WXREF(format)); }
  
  // copy raw data (in the specified format) to the provided buffer, return
  // TRUE if data copied successfully, FALSE otherwise
  //virtual bool GetDataHere(const wxDataFormat& format, void *buf) const;
  inline virtual bool getDataHere(IN(RDataFormat) format, OUT(RbyteArray) buf) const 
  { 
    int count = getDataSize(format);
    buf = new byteArray(count);
    return getWx()->GetDataHere(CLS2WXREF(format), buf->data()); 
  }
  
  // get data from the buffer of specified length (in the given format),
  // return TRUE if the data was read successfully, FALSE otherwise
  //virtual bool SetData(const wxDataFormat& format, size_t len, const void * buf);
  inline virtual bool setData(IN(RDataFormat) format, IN(RbyteArray) data) 
  { 
    return getWx()->SetData(CLS2WXREF(format), data->length(), data->data()); 
  }
    
    // returns TRUE if this format is supported
    //bool IsSupported(const wxDataFormat& format, Direction dir = Get) const;
  inline bool isSupported(IN(RDataFormat) format, DataObjectDirection dir = DataObjectGet) const 
  { return getWx()->IsSupported(CLS2WXREF(format), wxDataObject::Direction(dir)); }
};


} // wx
} // acdk

#endif //acdk_wx_DataObject_h
