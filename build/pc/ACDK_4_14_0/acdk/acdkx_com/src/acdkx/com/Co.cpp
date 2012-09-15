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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/Co.cpp,v 1.16 2005/02/05 10:45:38 kommer Exp $



#include "Co.h"
#include "AcdkObject.h"
#include "RegisterServer.h"
#include <acdk/lang/System.h>


// {A77FD131-C517-4770-886B-10818BEB0963}
const GUID CLSID_acdk_lang_Object = 
{ 0xa77fd131, 0xc517, 0x4770, 
  { 0x88, 0x6b, 0x10, 0x81, 0x8b, 0xeb, 0x9, 0x63 } 
};
const GUID CLSID_AcdkObject = CLSID_acdk_lang_Object;
const IID IID_IAcdkObject = CLSID_AcdkObject;


HINSTANCE ghInstance = NULL;

class AcdkObjectFactory
: extends ::acdkx::com::AbstractCoInterface
, public ::IClassFactory
{
  int _lock;
  typedef ::acdkx::com::AbstractCoInterface Super;
public:
  AcdkObjectFactory()
  : _lock(0)
  {
  }
  HRESULT STDMETHODCALLTYPE CreateInstance(IUnknown* pUnkOuter, REFIID riid, void** pout)
  {
    if (riid == IID_IUnknown || riid == CLSID_acdk_lang_Object)
    {
      ::acdkx::com::RAcdkObject obj = new ::acdkx::com::AcdkObject();
      obj->AddRef();
      *pout = (::IUnknown*)(::IDispatch*)(&obj);
      return S_OK;
    }
    return E_NOINTERFACE;
  }
  HRESULT STDMETHODCALLTYPE LockServer(BOOL fLock)
  {
    if (fLock)
      ++_lock;
    else
      --_lock;
    return S_OK;
  }
    // from ::IUnknown
  virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid,void** ppvObject)
  {
    if (riid == IID_IClassFactory)
    {
       AddRef();
       *ppvObject = (::IClassFactory*)this;
       return S_OK;
    }
    return Super::QueryInterface(riid, ppvObject);
  }
  virtual ULONG STDMETHODCALLTYPE AddRef()
  {
    return Super::AddRef();
  }
  virtual ULONG STDMETHODCALLTYPE Release()
  {
    return Super::Release();
  }
};

int acdk_dll_main(RStringArray args)
{
  return 0;
}



extern "C" STDAPI  ACDKX_COM_PUBLIC DllGetClassObject(REFCLSID rclsid, REFIID riid, void** ppv)
{    
  System::initAsSharedLibrary();
  //DebugBreak();
  if (CLSID_acdk_lang_Object != rclsid)        
    return ResultFromScode(E_FAIL);    
  
  AcdkObjectFactory* acf = new AcdkObjectFactory();    
  if (0 == acf)        
    return ResultFromScode(E_OUTOFMEMORY);    
  acf->addRef();

  HRESULT hr = acf->QueryInterface(riid, ppv);    
  if (FAILED(hr))
	acf->releaseRef();
  return hr;    
}


extern "C" HRESULT __stdcall DllCanUnloadNow()
{
  return S_OK;
}


extern "C" HRESULT __stdcall DllRegisterServer()
{
  //DebugBreak();
  TCHAR dllPath[MAX_PATH];
  GetModuleFileName(ghInstance, dllPath, MAX_PATH);
  return RegisterServer(dllPath, CLSID_acdk_lang_Object, _T("ACDK COM interface"), _T("Acdk.Object"), _T("Acdk.AcdkObject.1"), NULL);
}


extern "C" BOOL __stdcall DllMain(HINSTANCE hinstance, DWORD reason, void* pv)
{
  if (DLL_PROCESS_ATTACH == reason)
  {
    ghInstance = hinstance;
    DllRegisterServer();
  }
  return 1;
}

extern "C" HRESULT __stdcall DllUnregisterServer()
{
  //DebugBreak();
  //return UnregisterServer(CLSID_acdk_lang_Object, "Acdk.Object", "Acdk.Object.1");
  return S_OK;
}


namespace acdkx {
namespace com {

namespace {
struct InitCo
{
  InitCo()
  {
    CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
  }
  ~InitCo()
  {
    CoUninitialize();
  }
};
InitCo co;

} // anon namespace






RIUnknown 
Co::createInstance(REFCLSID clsid)
{
  ::IUnknown* iunk;
  ACDK_CECKCOMCALL(CoCreateInstance(clsid, NULL, CLSCTX_INPROC_SERVER, IID_IUnknown, (void**)&iunk));
  return new IUnknown(iunk, false);
}

} // namespace com 
} // namespace acdkx 





