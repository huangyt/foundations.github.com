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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/TextDataObject.h,v 1.3 2005/02/05 10:45:35 kommer Exp $

#ifndef acdk_wx_TextDataObject_h
#define acdk_wx_TextDataObject_h

#include "DataObjectSimple.h"


namespace acdk {
namespace wx {

enum DataObjectDirection;

ACDK_DECL_CLASS(TextDataObject);
/**
  see wxTextDataObject
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.3 $
  @date $Date: 2005/02/05 10:45:35 $
*/
class ACDK_WX_PUBLIC TextDataObject
: extends DataObjectSimple
{
  ACDK_WITH_METAINFO(TextDataObject)
public:
  ACDK_WX_STD_MEMBERS(TextDataObject, DataObjectSimple)
  
  TextDataObject(IN(RString) str = "") : DataObjectSimple(new wxTextDataObject(S2WXS(str))) {}  
  //virtual int GetTextLength() const;
  inline int getTextLength() const { return getWx()->GetTextLength(); }
  //wxString GetText() const;
  inline RString getText() const { return WXS2S(getWx()->GetText()); }
  //void SetText(const wxString& strText);
  inline void setText(IN(RString)  strText) { getWx()->SetText(S2WXS(strText)); }
};


} // wx
} // acdk

#endif //acdk_wx_TextDataObject_h
