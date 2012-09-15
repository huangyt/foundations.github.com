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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/ClientData.h,v 1.3 2005/02/05 10:45:34 kommer Exp $

#ifndef acdk_wx_ClientData_h
#define acdk_wx_ClientData_h

#include "WxObject.h"

namespace acdk {
namespace wx {


typedef WxNonCopyStruct<wxClientData> ClientDataSuper;

foreign
class wxAcdkClientData
: public wxClientData
{
private:
  RObject _data;
public:
  wxAcdkClientData() {}
  wxAcdkClientData(IN(RObject) data) : _data(data) {}
  RObject GetData() { return _data; }
  void SetData(IN(RObject) data) { _data = data; }
};

/* not needed
ACDK_DECL_CLASS(ClientData);

class ACDK_WX_PUBLIC ClientData
: extends WxObject
{
  ACDK_WITH_METAINFO(ClientData)
public:
  ACDK_WX_STD_VAL_MEMBERS(ClientData, WxObject)
  ClientData() : WxObject(new wxClientData(), true) {}
};
*/

} // wx
} // acdk

#endif //acdk_wx_ClientData_h
