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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/ActiveScript.cpp,v 1.8 2005/02/05 10:45:38 kommer Exp $



#include "ActiveScript.h"
#include "CoException.h"
#include "AcdkObject.h"

#include <acdk/cfgscript/Props.h>

//#include <ACTIVSCP.H>
//#include <comcat.h>
/*
class CActiveScriptSite :   public IActiveScriptSite,
                             public IActiveScriptSiteWindow {
     LONG m_cRef;
     IDispatch *m_pdispshell;
 public:
     CActiveScriptSite(IDispatch *pshell) 
         : m_cRef(0), m_pdispshell(pshell)     {
         if (m_pdispshell)
             m_pdispshell->AddRef();
     }
 
     ~CActiveScriptSite(void) {
         if (m_pdispshell)
             m_pdispshell->Release();
     }
 
 // IUnknown methods
     STDMETHODIMP QueryInterface(REFIID riid, void **ppv) {
         if (riid == IID_IUnknown||riid == IID_IActiveScriptSite)
             *ppv = (IActiveScriptSite*)this;
         else if (riid == IID_IActiveScriptSiteWindow)
             *ppv = (IActiveScriptSiteWindow*)this;
         else 
             return (*ppv = 0), E_OUTOFMEMORY;
         ((IUnknown*)*ppv)->AddRef();
         return S_OK;
     }
     
     STDMETHODIMP_(ULONG) AddRef()
     { return InterlockedIncrement(&m_cRef); }
 
     STDMETHODIMP_(ULONG) Release() {
         if (InterlockedDecrement(&m_cRef))
             return m_cRef;
         delete this;
         return 0;
     }
 
 // IActiveScriptSite methods
     STDMETHODIMP GetItemInfo(LPCOLESTR pstrName, DWORD dwReturnMask,
                              IUnknown **ppiunkItem, ITypeInfo **ppti) {
         HRESULT hr = E_FAIL;
         if (dwReturnMask & SCRIPTINFO_IUNKNOWN)
             *ppiunkItem = 0;
         if (dwReturnMask & SCRIPTINFO_ITYPEINFO)
             *ppti = 0;
 // see if top-level name is "shell"
         if (wcscmp(pstrName, OLESTR("shell")) == 0) {
             if (dwReturnMask & SCRIPTINFO_IUNKNOWN)
                 if (*ppiunkItem = m_pdispshell)
                 {
                     (*ppiunkItem)->AddRef();
                     hr = S_OK;
                 }
             
             if (dwReturnMask & SCRIPTINFO_ITYPEINFO)
                 hr = LoadTypeInfoFromThisModule(CLSID_CHostShell, ppti);
 
         }
         return hr;
     }
     
     STDMETHODIMP OnScriptError(IActiveScriptError *pscripterror) {
         DWORD dwCookie;
         LONG nChar;
         ULONG nLine;
         BSTR bstr = 0;
         EXCEPINFO ei; ZeroMemory(&ei, sizeof(ei));
         pscripterror->GetSourcePosition(&dwCookie, &nLine, &nChar);
         pscripterror->GetSourceLineText(&bstr);
         pscripterror->GetExceptionInfo(&ei);
         
         OLECHAR wszOutput[1024];
         swprintf(wszOutput, OLESTR("%s\n[Line: %d] %s\n%s"),
                   ei.bstrSource, nLine, ei.bstrDescription,
                   bstr ? bstr : OLESTR(""));
 
         SysFreeString(bstr);
         SysFreeString(ei.bstrSource);
         SysFreeString(ei.bstrDescription);
         SysFreeString(ei.bstrHelpFile);
         MessageBoxW(GetDesktopWindow(),
                     wszOutput,
                     L"Error",
                     MB_SETFOREGROUND);
 
         return S_OK;
     }
     
     STDMETHODIMP GetLCID(LCID *plcid)
     { *plcid = 9; return S_OK; }
     
     STDMETHODIMP GetDocVersionString(BSTR *pbstrVersion) 
     { *pbstrVersion = SysAllocString(L""); return S_OK; }
     
     STDMETHODIMP OnScriptTerminate(const VARIANT *pvr, const EXCEPINFO *pei)
     { return S_OK; }
     
     STDMETHODIMP OnStateChange(SCRIPTSTATE ssScriptState)
     { return S_OK; }
     
     STDMETHODIMP OnEnterScript(void)
     { return S_OK; }
     
     STDMETHODIMP OnLeaveScript(void) 
     { return S_OK; }
 
 // IActiveScriptSiteWindow methods    
     STDMETHODIMP GetWindow(HWND *phwnd)
     { *phwnd = GetDesktopWindow(); return S_OK; }
 
     STDMETHODIMP EnableModeless(BOOL)
     { return S_OK; }
 };

*/


namespace acdkx {
namespace com {

#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
IActiveScriptParse* getScript(IN(RString) language)
{
#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
  IActiveScriptParse *ppas = 0;
  RString wlanguage = language->convert(CCUcs2);
  OLECHAR* wszProgID = (OLECHAR*)S2OLE(wlanguage);
  CLSID clsid;
  HRESULT hr = CLSIDFromProgID(wszProgID, &clsid);
  if (SUCCEEDED(hr))
  {
     hr = CoCreateInstance(clsid, 0, CLSCTX_ALL,
                          IID_IActiveScriptParse,
                           (void**)&ppas);
     if (SUCCEEDED(hr))
       return ppas;
  }
  THROW2(CoException, hr, "getScript");
#else
  THROW1(CoException, "ActiveScript not supported");
#endif
  return 0;
}


class ActiveScriptSite
: extends acdkx::com::IUnknown
, implements ::IActiveScriptSite
, implements ::IActiveScriptSiteWindow
  
{
  typedef acdkx::com::IUnknown Super;
  
public:
  acdk::cfgscript::RProps _globals;
  ActiveScriptSite()
    : _globals(new acdk::cfgscript::Props())
  {
  }
  virtual ULONG STDMETHODCALLTYPE AddRef() { return Super::AddRef(); }
  virtual ULONG STDMETHODCALLTYPE Release() { return Super::Release(); }
  virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid,void** ppvObject)
  {
    if (riid == IID_IUnknown)
    {
      *ppvObject = (IUnknown*)this;
    }
    else if (riid == IID_IActiveScriptSite)
    {
      *ppvObject = (IActiveScriptSite*)this;
    }
    else if (riid == IID_IActiveScriptSiteWindow)
    {
      *ppvObject = (IActiveScriptSiteWindow*)this;
    }
    else
    {
      *ppvObject = 0;
      return E_NOINTERFACE;
    }
    addRef();
    return S_OK;
  }
  virtual HRESULT STDMETHODCALLTYPE GetLCID(LCID __RPC_FAR *plcid)
  {
    return E_NOTIMPL;
  }
  virtual HRESULT STDMETHODCALLTYPE GetItemInfo( 
            LPCOLESTR pstrName,
            DWORD dwReturnMask,
            ::IUnknown __RPC_FAR *__RPC_FAR *ppiunkItem,
            ::ITypeInfo __RPC_FAR *__RPC_FAR *ppti)
  {
    if (ppiunkItem == 0)
      return E_INVALIDARG;
    *ppiunkItem = 0;
    if (ppti != 0)
      *ppti = 0;
    int _clength;
    RString key = OLE2S(pstrName);
    ::acdk::lang::dmi::RDmiObject obj = _globals->get(key);
    if (obj == Nil)
      return TYPE_E_ELEMENTNOTFOUND;
    RAcdkObject acobj = new AcdkObject(obj->getObjectVar());
    acobj->QueryInterface(IID_IUnknown, (void**)ppiunkItem);
    return S_OK;

    //
  }
  virtual HRESULT STDMETHODCALLTYPE GetDocVersionString(BSTR __RPC_FAR *pbstrVersion)
  {
    return E_NOTIMPL;
  }
  virtual HRESULT STDMETHODCALLTYPE OnScriptTerminate(const VARIANT __RPC_FAR *pvarResult, const EXCEPINFO __RPC_FAR *pexcepinfo)
  {
    return S_OK;
  }
  virtual HRESULT STDMETHODCALLTYPE OnStateChange(SCRIPTSTATE ssScriptState)
  {
    return S_OK;
  }
  virtual HRESULT STDMETHODCALLTYPE OnScriptError(IActiveScriptError __RPC_FAR *pscripterror)
  {
    DWORD sourceContext = 0;
    ULONG lineNumber = 0;
    LONG charPosition = 0;
    pscripterror->GetSourcePosition(&sourceContext, &lineNumber, &charPosition);
    StringBuffer sb("");
    sb.append((int)lineNumber);
    sb.append(":");
    sb.append((int)charPosition);
    BSTR bstr = 0;
    sb.append(":");
    pscripterror->GetSourceLineText(&bstr);
    int _clength;
    if (bstr)
    {
      sb.append(OLE2S(bstr));
      SysFreeString(bstr);
    }
    sb.append(": ");

    EXCEPINFO exInfo;
    memset(&exInfo, 0, sizeof(exInfo));
    pscripterror->GetExceptionInfo(&exInfo);
    if (exInfo.pfnDeferredFillIn != 0)
        exInfo.pfnDeferredFillIn(&exInfo);
    if (exInfo.bstrDescription)
      sb.append(W2CS(exInfo.bstrDescription));
    SysFreeString(exInfo.bstrSource);
    SysFreeString(exInfo.bstrDescription);
    SysFreeString(exInfo.bstrHelpFile);
    THROW1(Exception, sb.toString());
    return S_OK;
  }
  virtual HRESULT STDMETHODCALLTYPE OnEnterScript()
  {
    return S_OK;
  }
  virtual HRESULT STDMETHODCALLTYPE OnLeaveScript()
  {
    return S_OK;
  }
  virtual HRESULT STDMETHODCALLTYPE GetWindow(HWND *phwnd)
  {
    *phwnd = GetDesktopWindow();
    return S_OK;
  }
  virtual HRESULT STDMETHODCALLTYPE EnableModeless(BOOL fEnable)
  {
    return S_OK;
  }
};

#endif //defined(ACDKX_ORB_WITH_ACTIVESCRIPT)

ActiveScript::ActiveScript(IN(RString) language)
#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
: _interface(getScript(language))
, _activeScriptSide(new ActiveScriptSite())
{
  HRESULT hr = _interface->InitNew();
  if (FAILED(hr))
    THROW2(CoException, hr, "InitNew");
  IHolder<IActiveScript> as = iface_cast<IActiveScript>(_interface);
  as->SetScriptSite(&_activeScriptSide);
  as->SetScriptState(SCRIPTSTATE_STARTED);
#else
{
#endif //defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
}

void checkInvokeResult(HRESULT hr, EXCEPINFO& exInfo, UINT argErr);
void 
ActiveScript::parseEval(IN(RString) code)
{
#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
  //DisplayEngines();
  EXCEPINFO excepinfo;
  memset(&excepinfo, 0, sizeof(excepinfo));
  RString wcode = code->convert(CCUcs2);
  checkInvokeResult(_interface->ParseScriptText(S2OLE(wcode), NULL, NULL, NULL, 0, 0, SCRIPTTEXT_ISVISIBLE, NULL, &excepinfo), excepinfo, 0);
#endif //defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
}


void 
ActiveScript::setVar(IN(RString) name, IN(RObject) obj)
{
#if defined(ACDKX_ORB_WITH_ACTIVESCRIPT)
  _activeScriptSide->_globals->setObjectVal(name, obj);
  IHolder<IActiveScript> as = iface_cast<IActiveScript>(_interface);
  RString wname = name->convert(CCUcs2);
  HRESULT hr = as->AddNamedItem(S2OLE(wname), SCRIPTITEM_ISVISIBLE | SCRIPTITEM_ISSOURCE);
  if (FAILED(hr))
    THROW2(CoException, hr, "ActiveScript::setVar");
#endif //defined(ACDKX_ORB_WITH_ACTIVESCRIPT)

}

} // namespace com 
} // namespace acdkx 




