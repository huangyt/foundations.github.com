// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/IUnknown.cpp,v 1.8 2005/02/05 10:45:38 kommer Exp $


#include "IUnknown.h"
#include <acdk/lang/NullPointerException.h>

namespace acdkx {
namespace com {

IUnknown::IUnknown(::IUnknown* iface/* = 0*/, bool copy/* = true*/)
: _iface(iface)
{
  if (_iface != 0 && copy == true)
    _iface->AddRef();
}

IUnknown::~IUnknown()
{
  
}


RIUnknown 
IUnknown::queryInterface(REFIID riid)
{
  if (_iface == 0)
    THROW0(NullPointerException);
  ::IUnknown* i;
  _iface->QueryInterface(riid, (void**)&i);
  return new IUnknown(i, false);
}

//virtual 
HRESULT STDMETHODCALLTYPE
IUnknown::QueryInterface(REFIID riid,void** ppvObject)
{
  if (_iface == 0)
    THROW0(NullPointerException);
  return _iface->QueryInterface(riid, ppvObject);
}

//virtual 
ULONG STDMETHODCALLTYPE
IUnknown::AddRef()
{
  addRef();
  return refCount();
}

//virtual 
ULONG STDMETHODCALLTYPE
IUnknown::Release()
{
  releaseRef();
  return refCount();
}

} // namespace com 
} // namespace acdkx 





