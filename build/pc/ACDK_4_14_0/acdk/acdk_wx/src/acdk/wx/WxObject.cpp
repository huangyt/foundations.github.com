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
// $Header: /cvsroot/acdk/acdk/acdk_wx/src/acdk/wx/WxObject.cpp,v 1.12 2005/02/05 10:45:35 kommer Exp $

#include "Config.h"
#include <acdk/lang/System.h>
#include "WxObject.h"
#include "Window.h"
#include <map>

namespace acdk {
namespace wx {



void 
WxObject::_assignShadow(wxObject* wxObject, bool own)
{
  wxEvtHandler* wxHandler = dynamic_cast<wxEvtHandler*>(wxObject);
  if (wxHandler == 0)
    return;
  
  wxClientData* cldata = wxHandler->GetClientObject();
  if (cldata != 0)
    return;
  AcdkShadowClientDataObject* acshdw = new AcdkShadowClientDataObject(this);
  wxHandler->SetClientObject(acshdw);
}

Object* 
WxObject::_getShadow()
{
   wxObject* wxObj = getWx();
  if (wxObj == 0)
    return 0;
  wxEvtHandler* wxHandler = dynamic_cast<wxEvtHandler*>(wxObj);
  if (wxHandler == 0)
    return 0;
  wxClientData* cldata = wxHandler->GetClientObject();
  if (cldata == 0)
    return 0;
  AcdkShadowClientDataObject* acshdw = dynamic_cast<AcdkShadowClientDataObject*>(cldata);
  if (acshdw == 0)
    return this;
  return &acshdw->shadow;
}

WxObject::WxObject(wxObject* obj, bool owns) 
: _wxObject(obj)
, _forward(0)
, _ownsWxObject(owns)  
{
  WXDOUT((void*)this << " WxObject(wxObject*): " << (void*)_wxObject 
	 << "; " << getClazzInfo()->name << "; own=" << _ownsWxObject);
  _assignShadow(obj, owns);
}

WxObject::WxObject(const wxObject& obj, bool owns) 
: _wxObject(const_cast<wxObject*>(&obj))
, _forward(0)
, _ownsWxObject(owns)  
{
  WXDOUT((void*)this << " WxObject(wxObject&): " << (void*)_wxObject 
	 << "; " << getClazzInfo()->name << "; own=" << _ownsWxObject);
  _assignShadow((wxObject*)&obj, owns);
}

WxObject::~WxObject()
{
  WXDOUT((void*)this << " ~WxObject: " << (void*)_wxObject 
	 << "; " << getClazzInfo()->name << "; own=" << _ownsWxObject);
  if (_ownsWxObject == true)
  {
    
    delete _wxObject;
  }
}

typedef std::map<const wxClassInfo*, InstanceCreator> InstanceCreatorMap;
InstanceCreatorMap _creatorMap;

RegisterWxCreator::RegisterWxCreator(InstanceCreator creator, const wxClassInfo* wxClassInfo)
{
  _creatorMap[wxClassInfo] = creator;
}

InstanceCreator getCreator(const wxClassInfo* wxclass)
{
  InstanceCreatorMap::iterator it = _creatorMap.find(wxclass);
  if (it == _creatorMap.end())
    return 0;
  return it->second;
}



acdk::lang::Object* 
WxObject::_cast(const acdk::lang::dmi::ClazzInfo* ci)
{
  acdk::lang::Object* obj = Object::_cast(ci);
  if (obj != 0)
    return obj;
  obj = _getShadow();
  if (obj != 0)
    return obj;
  wxObject* wxObj = getWx();
  if (wxObj == 0)
    return 0;
  
  wxEvtHandler* wxHandler = dynamic_cast<wxEvtHandler*>(wxObj);
  if (wxHandler != 0)
  {
    wxClientData* cldata = wxHandler->GetClientObject();
    void* userData = wxHandler->GetClientData();
    wxHandler->SetClientData((void*)this);
    cldata = wxHandler->GetClientObject();
    userData = wxHandler->GetClientData();
  }

  //if (wxObj->GetRefData() == 0)
  //  return 0;
  const wxClassInfo* wxci = wxObj->GetClassInfo();
  if (wxci == 0)
    return 0;
  InstanceCreator icreator = getCreator(wxci);
  Object* _castedObject = 0;
  if (icreator != 0)
  {
    _castedObject = icreator();
  } 
  else
  {
    RString wxClassName = WXCPTR2S(wxci->GetClassName());
    if (wxClassName->startsWith("wx") == false)
      return 0;
    RString aClassName = /*"acdk/wx/" +*/ wxClassName->substr(2);
    _castedObject = (RWxObject)Class::forName(aClassName)->newInstance();
  }
  return _castedObject;
  /* hmm doesn't work really
  if (_castedObject == 0)
    return 0;
  
  ownsWxObject(false);
  _castedObject->_wxObject = _wxObject;
  _castedObject->_forward = _forward;
  */
  
  //Window::messageBox("WxObject::_cast: " + wxClassName + "; " + ci->name);
  return _castedObject;
}

} // wx
} // acdk


