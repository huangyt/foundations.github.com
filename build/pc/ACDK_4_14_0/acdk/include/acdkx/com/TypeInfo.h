// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// TypeInfopyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any TypeInfommercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/TypeInfo.h,v 1.3 2003/06/19 14:37:22 kommer Exp $


#ifndef acdkx_com_TypeInfo_h
#define acdkx_com_TypeInfo_h

#include "IUnknown.h"

namespace acdkx {
namespace com {

ACDK_DECL_CLASS(TypeInfo);

class ACDKX_COM_PUBLIC TypeInfo 
: extends ::acdkx::com::IUnknown
, public ::ITypeInfo

{
  RClass _class;
public:
  TypeInfo(RClass cls)
  : IUnknown()
  , _class(cls)
  {
  }
  
  virtual HRESULT STDMETHODCALLTYPE GetTypeAttr(TYPEATTR __RPC_FAR *__RPC_FAR *ppTypeAttr);
  virtual HRESULT STDMETHODCALLTYPE GetTypeComp(ITypeComp __RPC_FAR *__RPC_FAR *ppTComp);
  virtual HRESULT STDMETHODCALLTYPE GetFuncDesc(UINT index, FUNCDESC __RPC_FAR *__RPC_FAR *ppFuncDesc);
  virtual HRESULT STDMETHODCALLTYPE GetVarDesc( UINT index, VARDESC __RPC_FAR *__RPC_FAR *ppVarDesc);
  virtual HRESULT STDMETHODCALLTYPE GetNames(MEMBERID memid, BSTR __RPC_FAR *rgBstrNames, UINT cMaxNames, UINT __RPC_FAR *pcNames);
  virtual HRESULT STDMETHODCALLTYPE GetRefTypeOfImplType(UINT index, HREFTYPE __RPC_FAR *pRefType);
  virtual HRESULT STDMETHODCALLTYPE GetImplTypeFlags(UINT index, INT __RPC_FAR *pImplTypeFlags);
  virtual HRESULT STDMETHODCALLTYPE GetIDsOfNames(LPOLESTR __RPC_FAR *rgszNames, UINT cNames, MEMBERID __RPC_FAR *pMemId);

  virtual HRESULT STDMETHODCALLTYPE Invoke(PVOID pvInstance, MEMBERID memid, WORD wFlags, DISPPARAMS __RPC_FAR *pDispParams, 
                                           VARIANT __RPC_FAR *pVarResult, EXCEPINFO __RPC_FAR *pExcepInfo, UINT __RPC_FAR *puArgErr);

  virtual HRESULT STDMETHODCALLTYPE GetDocumentation(MEMBERID memid, BSTR __RPC_FAR *pBstrName, BSTR __RPC_FAR *pBstrDocString,
                                                      DWORD __RPC_FAR *pdwHelpContext, BSTR __RPC_FAR *pBstrHelpFile);
  
  virtual HRESULT STDMETHODCALLTYPE GetDllEntry(MEMBERID memid, INVOKEKIND invKind, BSTR __RPC_FAR *pBstrDllName, 
                                                BSTR __RPC_FAR *pBstrName, WORD __RPC_FAR *pwOrdinal);
  
  virtual HRESULT STDMETHODCALLTYPE GetRefTypeInfo(HREFTYPE hRefType, ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
  
  virtual HRESULT STDMETHODCALLTYPE AddressOfMember(MEMBERID memid, INVOKEKIND invKind, PVOID __RPC_FAR *ppv);
  
  virtual HRESULT STDMETHODCALLTYPE CreateInstance(IUnknown __RPC_FAR *pUnkOuter, REFIID riid, PVOID __RPC_FAR *ppvObj);
    
  virtual HRESULT STDMETHODCALLTYPE GetMops(MEMBERID memid, BSTR __RPC_FAR *pBstrMops);
  
  virtual HRESULT STDMETHODCALLTYPE GetContainingTypeLib(ITypeLib __RPC_FAR *__RPC_FAR *ppTLib, UINT __RPC_FAR *pIndex);

  virtual void STDMETHODCALLTYPE ReleaseTypeAttr(TYPEATTR __RPC_FAR *pTypeAttr);
  virtual void STDMETHODCALLTYPE ReleaseFuncDesc(FUNCDESC __RPC_FAR *pFuncDesc);
  virtual void STDMETHODCALLTYPE ReleaseVarDesc(VARDESC __RPC_FAR *pVarDesc);
};

} // namespace com
} // namespace acdkx 



#endif //acdkx_com_TypeInfo_h

