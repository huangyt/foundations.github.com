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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/AbstractCoInterface.h,v 1.7 2005/02/05 10:45:38 kommer Exp $


#ifndef acdkx_com_AbstractCoInterface_h
#define acdkx_com_AbstractCoInterface_h

#include "IUnknown.h"

namespace acdkx {
namespace com {



/**
  Base class for implementing a Com Interface
  @see IUnknown
*/
class ACDKX_COM_PUBLIC AbstractCoInterface
: extends ::acdk::lang::Object
, public ::IUnknown
{
public:
  virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid,void** ppvObject)
  {
    if (ppvObject == 0)
      return E_INVALIDARG;
    if (riid == IID_IUnknown)
    {
      *ppvObject = (::IUnknown*)this;
      AddRef();
      return S_OK;
    }
    *ppvObject = NULL;
    return E_NOINTERFACE;
  }
  virtual ULONG STDMETHODCALLTYPE AddRef()
  {
    addRef();
    return refCount();
  }
  virtual ULONG STDMETHODCALLTYPE Release()
  {
    releaseRef();
    return refCount();
  }
};

#define ACDK_STD_ACOINTERFACE_RC() \
  virtual ULONG STDMETHODCALLTYPE AddRef() { return AbstractCoInterface::AddRef(); } \
  virtual ULONG STDMETHODCALLTYPE Release() { return AbstractCoInterface::Release(); } 

} // namespace com 
} // namespace acdkx 



#endif //acdkx_com_AbstractCoInterface_h

