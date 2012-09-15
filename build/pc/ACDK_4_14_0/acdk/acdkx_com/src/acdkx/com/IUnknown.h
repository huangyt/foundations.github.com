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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/IUnknown.h,v 1.13 2005/02/05 10:45:38 kommer Exp $


#ifndef acdkx_com_IUnknown_h
#define acdkx_com_IUnknown_h



#include <acdk.h>
#include "Config.h"
#include "CoSys.h"


template <class I>
class IHolder
{
  I* _iface;
public:
  IHolder(I* iface = 0) 
  : _iface(iface)
  {
  }
  IHolder(const IHolder<I>& other)
  : _iface(other._iface)
  {
    if (_iface != 0)
      _iface->AddRef();
  }
  ~IHolder()
  {
    if (_iface != 0)
      _iface->Release();
  }
  bool operator!=(void* ptr) const { return _iface != ptr; }
  bool operator==(void* ptr) const { return _iface == ptr; }
  
  operator void**() { return (void**)&_iface; }
  operator I**() { return &_iface; }
  operator I*&() { return _iface; }
  I& operator *() { return *_iface; }
  I*& operator&() { return _iface; }
  I* operator->() { return _iface; }
  I*& i() { return _iface; }
  I*& iptr() { return _iface; }
  
  
};


#define ACDK_COMIFACE(cls) typedef IHolder<cls> R##cls

template <class OI, class I> IHolder<OI> iface_cast(I* iface)
{
  if (iface == 0)
    return 0;
  IHolder<OI> ret;
  iface->QueryInterface(__uuidof(OI), ret);
  return ret;
}
template <class OI, class I> IHolder<OI> iface_cast(IHolder<I>& iface)
{
  return iface_cast<OI>(iface.i());
}


ACDK_COMIFACE(IUnknown);

namespace acdkx {
namespace com {




ACDK_DECL_CLASS(IUnknown);
/**
  This is a wrapper to a foreign Interface

  @see AbstractCoInterface
*/
class ACDKX_COM_PUBLIC IUnknown 
: public ::acdk::lang::Object
, public ::IUnknown
{
  ::RIUnknown _iface;
public:
  IUnknown(::IUnknown* iface = 0, bool copy = true);
  ~IUnknown();
  RIUnknown queryInterface(REFIID riid);
  // from ::IUnknown
  virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid,void** ppvObject);
  virtual ULONG STDMETHODCALLTYPE AddRef();
  virtual ULONG STDMETHODCALLTYPE Release();
};

#define ACDK_STD_IUNKNOWN_RC() \
  virtual ULONG STDMETHODCALLTYPE AddRef() { return IUnknown::AddRef(); } \
  virtual ULONG STDMETHODCALLTYPE Release() { return IUnknown::Release(); } 

#define ACDK_STD_IUNKNOWN_QI() \
  virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid,void** ppvObject) \
{ return ::acdkx::com::IUnknown::QueryInterface(riid, ppvObject); }


/**
  Base class for implementing a Com Interface
*/
class ACDKX_COM_PUBLIC IBase
{

};

} // namespace com 
} // namespace acdkx 



#endif //acdkx_com_IUnknown_h

