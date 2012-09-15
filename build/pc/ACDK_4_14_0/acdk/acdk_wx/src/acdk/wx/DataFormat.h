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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/DataFormat.h,v 1.6 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_DataFormat_h
#define acdk_wx_DataFormat_h

#include "WxObject.h"
#include "DataFormat.h"


namespace acdk {
namespace wx {

/**
  see wxDataFormat
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
enum DataFormatId
{
    DfInvalid = wxDF_INVALID,  // wxDF_INVALID =          0,
    DfText = wxDF_TEXT,  // wxDF_TEXT =             1,  /* CF_TEXT */
    DfBitmap = wxDF_BITMAP,  // wxDF_BITMAP =           2,  /* CF_BITMAP */
    DfMetafile = wxDF_METAFILE,  // wxDF_METAFILE =         3,  /* CF_METAFILEPICT */
    DfSylk = wxDF_SYLK,  // wxDF_SYLK =             4,
    DfDif = wxDF_DIF,  // wxDF_DIF =              5,
    DfTiff = wxDF_TIFF,  // wxDF_TIFF =             6,
    DfOemtext = wxDF_OEMTEXT,  // wxDF_OEMTEXT =          7,  /* CF_OEMTEXT */
    DfDib = wxDF_DIB,  // wxDF_DIB =              8,  /* CF_DIB */
    DfPalette = wxDF_PALETTE,  // wxDF_PALETTE =          9,
    DfPendata = wxDF_PENDATA,  // wxDF_PENDATA =          10,
    DfRiff = wxDF_RIFF,  // wxDF_RIFF =             11,
    DfWave = wxDF_WAVE,  // wxDF_WAVE =             12,
    DfUnicodetext = wxDF_UNICODETEXT,  // wxDF_UNICODETEXT =      13,
    DfEnhmetafile = wxDF_ENHMETAFILE,  // wxDF_ENHMETAFILE =      14,
    DfFilename = wxDF_FILENAME,  // wxDF_FILENAME =         15, /* CF_HDROP */
    DfLocale = wxDF_LOCALE,  // wxDF_LOCALE =           16,
    DfPrivate = wxDF_PRIVATE,  // wxDF_PRIVATE =          20,
    DfHtml = wxDF_HTML,  // wxDF_HTML =             30, /* Note: does not correspond to CF_ constant */
    DfMax = wxDF_MAX  // wxDF_MAX
};
ACDK_DEF_LIB_ENUM(ACDK_WX_PUBLIC, DataFormatId);

typedef WxValStruct<wxDataFormat> DataFormatSuper;

ACDK_DECL_CLASS(DataFormat);

/**
  see wxDataFormat
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.6 $
  @date $Date: 2005/02/05 10:45:34 $
*/
class ACDK_WX_PUBLIC DataFormat
: extends DataFormatSuper
{
  ACDK_WITH_METAINFO(DataFormat)
public:
  //ACDK_WX_STD_VAL_MEMBERS(DataFormat, DataFormatSuper)
  //void wxDataFormat(int format = DfInvalid) { m_format = format; }
  inline DataFormat(int format = DfInvalid) : DataFormatSuper(wxDataFormat((wxDataFormatId)format)) {}
  //void wxDataFormat(const wxString& format) { SetId(format); }
  inline DataFormat(IN(RString) format) : DataFormatSuper(wxDataFormat(S2WXS(format))) {}

  bool equals(IN(RDataFormat) other)
  {
    return *getWx() == *(other->getWx());
  }
  bool equals(IN(RObject) other)
  {
    if (instanceof(other, DataFormat) == false)
      return false;
    return equals(RDataFormat(other));
  }
  
  
  // explicit and implicit conversions to NativeFormat which is one of
  // standard data types (implicit conversion is useful for preserving the
  // compatibility with old code)
  //int GetFormatId() const { return m_format; }
  /**
     GTKreturn value is from type GdkAtom
   */
  inline int getFormatId() const 
  { 
    return (int)getWx()->GetFormatId(); 
  }
  
  // this works with standard as well as custom ids
  //void SetType(int format) { m_format = format; }
  inline void setType(int format) { getWx()->SetType((wxDataFormatId)format); }
  //int GetType() const { return m_format; }
  inline int getType() const { return getWx()->GetType(); }
  
  // string ids are used for custom types - this SetId() must be used for
  // application-specific formats
  //wxString GetId() const;
  inline RString getId() const { return WXS2S(getWx()->GetId()); }
  //void SetId(const wxChar *format);
  inline void setId(IN(RString) format) { getWx()->SetId(S2WXS(format)); }
  
  // returns TRUE if the format is one of those defined in wxDataFormatId
  //bool IsStandard() const { return m_format > 0 && m_format < wxDF_MAX; }
  inline bool isStandard() const 
  { 
#if defined(ACDK_OS_WIN32)
    return getWx()->IsStandard(); 
#else
    return false;
#endif
  }
  static RDataFormat getInvalid() { return new DataFormat(DfInvalid); }
};

inline RDataFormat fromWx(const wxDataFormat& wx) 
{ 
  return new DataFormat((int)(wxDataFormat::NativeFormat)wx); 
}

} // wx
} // acdk

#endif //acdk_wx_DataFormat_h
