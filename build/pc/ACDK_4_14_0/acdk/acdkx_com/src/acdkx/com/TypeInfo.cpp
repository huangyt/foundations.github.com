// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// TypeInfopyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any TypeInfommercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/TypeInfo.cpp,v 1.13 2003/06/19 14:37:22 kommer Exp $




#include "TypeInfo.h"
#include "ComObject.h"

namespace acdkx {
namespace com {


using acdk::lang::reflect::Modifier;
using namespace acdk::lang::dmi;





//virtual 
HRESULT STDMETHODCALLTYPE 
TypeInfo::GetTypeAttr(TYPEATTR __RPC_FAR *__RPC_FAR *ppTypeAttr)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}


HRESULT STDMETHODCALLTYPE 
TypeInfo::GetTypeComp(ITypeComp __RPC_FAR *__RPC_FAR *ppTComp)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}

HRESULT STDMETHODCALLTYPE 
TypeInfo::GetFuncDesc(UINT index, FUNCDESC __RPC_FAR *__RPC_FAR *ppFuncDesc)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}

HRESULT STDMETHODCALLTYPE 
TypeInfo::GetVarDesc( UINT index, VARDESC __RPC_FAR *__RPC_FAR *ppVarDesc)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}

HRESULT STDMETHODCALLTYPE 
TypeInfo::GetNames(MEMBERID memid, BSTR __RPC_FAR *rgBstrNames, UINT cMaxNames, UINT __RPC_FAR *pcNames)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}

HRESULT STDMETHODCALLTYPE 
TypeInfo::GetRefTypeOfImplType(UINT index, HREFTYPE __RPC_FAR *pRefType)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}


HRESULT STDMETHODCALLTYPE 
TypeInfo::GetImplTypeFlags(UINT index, INT __RPC_FAR *pImplTypeFlags)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}


HRESULT STDMETHODCALLTYPE 
TypeInfo::GetIDsOfNames(LPOLESTR __RPC_FAR *rgszNames, UINT cNames, MEMBERID __RPC_FAR *pMemId)
{
  FunctionSignature fsig;
  for (int i = 0; i < cNames; ++i)
  {
    int _clength;
    fsig.functionname = W2CS(rgszNames[i]);
    fsig.size = 0;
    const ClazzInfo* ci = getClazzInfo();
    const ClazzMethodInfo* cmi = findMethod(ci, fsig, false, true);
    pMemId[i] = (int) cmi;
  }
  return S_OK;
}

using ::acdk::lang::dmi::ScriptVar;
using ::acdk::lang::dmi::ScriptVarArray;


HRESULT STDMETHODCALLTYPE 
TypeInfo::Invoke(PVOID pvInstance, MEMBERID memid, WORD wFlags, DISPPARAMS __RPC_FAR *pDispParams,
                  VARIANT __RPC_FAR *pVarResult, EXCEPINFO __RPC_FAR *pExcepInfo, UINT __RPC_FAR *puArgErr)
{
  if (wFlags & DISPATCH_METHOD)
  {
    try {
      const ::acdk::lang::dmi::ClazzMethodInfo* cmi = (const  ::acdk::lang::dmi::ClazzMethodInfo*)memid;
      ScriptVarArray sa(pDispParams->cArgs);
      variants2ScriptVarArray(pDispParams->rgvarg, pDispParams->cArgs, sa, true);
      Object* obj = (Object*)pvInstance;
      ScriptVar ret;
      obj->standardDispatch(cmi->name, ret, sa, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), Nil, 
          MiPublic, obj->getClazzInfo(), cmi);
      if (pVarResult != 0)
        scriptVar2Variant(ret, *pVarResult);
      return S_OK;
    } catch (::acdk::lang::RThrowable ex) {
      
    }
  }
  return ERROR_CALL_NOT_IMPLEMENTED;
}


HRESULT STDMETHODCALLTYPE 
TypeInfo::GetDocumentation(MEMBERID memid, BSTR __RPC_FAR *pBstrName, BSTR __RPC_FAR *pBstrDocString,
                                                      DWORD __RPC_FAR *pdwHelpContext, BSTR __RPC_FAR *pBstrHelpFile)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}
  
HRESULT STDMETHODCALLTYPE 
TypeInfo::GetDllEntry(MEMBERID memid, INVOKEKIND invKind, BSTR __RPC_FAR *pBstrDllName, 
                      BSTR __RPC_FAR *pBstrName, WORD __RPC_FAR *pwOrdinal)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}
  
HRESULT STDMETHODCALLTYPE 
TypeInfo::GetRefTypeInfo(HREFTYPE hRefType, ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}
  
HRESULT STDMETHODCALLTYPE 
TypeInfo::AddressOfMember(MEMBERID memid, INVOKEKIND invKind, PVOID __RPC_FAR *ppv)
{
  
  return TYPE_E_ELEMENTNOTFOUND;
}

  
HRESULT STDMETHODCALLTYPE 
TypeInfo::CreateInstance(IUnknown __RPC_FAR *pUnkOuter, REFIID riid, PVOID __RPC_FAR *ppvObj)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}
    
HRESULT STDMETHODCALLTYPE 
TypeInfo::GetMops(MEMBERID memid, BSTR __RPC_FAR *pBstrMops)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}
  
HRESULT STDMETHODCALLTYPE 
TypeInfo::GetContainingTypeLib(ITypeLib __RPC_FAR *__RPC_FAR *ppTLib, UINT __RPC_FAR *pIndex)
{
  return ERROR_CALL_NOT_IMPLEMENTED;
}


void STDMETHODCALLTYPE 
TypeInfo::ReleaseTypeAttr(TYPEATTR __RPC_FAR *pTypeAttr)
{
  
}

void STDMETHODCALLTYPE 
TypeInfo::ReleaseFuncDesc(FUNCDESC __RPC_FAR *pFuncDesc)
{
  
}

void STDMETHODCALLTYPE 
TypeInfo::ReleaseVarDesc(VARDESC __RPC_FAR *pVarDesc)
{
  
}


} // namespace com
} // namespace acdkx 





