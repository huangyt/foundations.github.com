// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// AcdkObjectpyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any AcdkObjectmmercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/AcdkObject.h,v 1.11 2003/06/19 14:37:22 kommer Exp $


#ifndef acdkx_com_AcdkObject_h
#define acdkx_com_AcdkObject_h


#include "AbstractCoInterface.h"
#include "IAcdkObject.h"

extern  const GUID CLSID_acdk_lang_Object;

namespace acdkx {
namespace com {

ACDK_DECL_CLASS(AcdkObject);

class ACDKX_COM_PUBLIC AcdkObject 
: extends ::acdkx::com::AbstractCoInterface
, public ::IAcdkObject
, public ::ISupportErrorInfo
{
  RObject _obj;
  RClass _class;
  bool _unwrapDmiObject;
public:
  AcdkObject()
  : _unwrapDmiObject(true)
  {
  }
  AcdkObject(RObject obj)
  : _obj(obj)
  , _unwrapDmiObject(true)
  {
    _class = _obj->getClass();
  }
  RObject getObject() { return _obj; }
  /**
    Should an wrapped ACDK Object parameter be unwrapped or not
    If flag is false, the DMI interface returns ComObject's (except String)
    otherwise the type will be casted.
  */
  void unwrapAcdkObject(bool flag) { _unwrapDmiObject = flag; }
  bool unwrapAcdkObject() { return _unwrapDmiObject; }

  ACDK_STD_ACOINTERFACE_RC()

    
  virtual HRESULT STDMETHODCALLTYPE QueryInterface(REFIID riid,void** ppvObject);
  virtual HRESULT STDMETHODCALLTYPE GetTypeInfoCount( 
    /* [out] */ UINT __RPC_FAR *pctinfo);
    
    virtual HRESULT STDMETHODCALLTYPE GetTypeInfo( 
    /* [in] */ UINT iTInfo,
    /* [in] */ LCID lcid,
    /* [out] */ ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);
    
    virtual HRESULT STDMETHODCALLTYPE GetIDsOfNames( 
    /* [in] */ REFIID riid,
    /* [size_is][in] */ LPOLESTR __RPC_FAR *rgszNames,
    /* [in] */ UINT cNames,
    /* [in] */ LCID lcid,
    /* [size_is][out] */ DISPID __RPC_FAR *rgDispId);
    
    virtual /* [local] */ HRESULT STDMETHODCALLTYPE Invoke( 
    /* [in] */ DISPID dispIdMember,
    /* [in] */ REFIID riid,
    /* [in] */ LCID lcid,
    /* [in] */ WORD wFlags,
    /* [out][in] */ DISPPARAMS __RPC_FAR *pDispParams,
    /* [out] */ VARIANT __RPC_FAR *pVarResult,
    /* [out] */ EXCEPINFO __RPC_FAR *pExcepInfo,
    /* [out] */ UINT __RPC_FAR *puArgErr);
    
    virtual /* [id][vararg] */ HRESULT STDMETHODCALLTYPE New( 
    BSTR classname,
    /* [in] */ SAFEARRAY __RPC_FAR * __MIDL_0015,
    /* [retval][out] */ IDispatch __RPC_FAR *__RPC_FAR *retvalue);
    
    virtual /* [id][vararg] */ HRESULT STDMETHODCALLTYPE invoke( 
    BSTR methodname,
    /* [in] */ SAFEARRAY __RPC_FAR * args,
    /* [retval][out] */ VARIANT __RPC_FAR *retvalue);
    
    virtual /* [id] */ HRESULT STDMETHODCALLTYPE peek( 
    BSTR membername,
    /* [retval][out] */ VARIANT __RPC_FAR *retvalue);
    
    virtual /* [id] */ HRESULT STDMETHODCALLTYPE poke( 
    BSTR membername,
    /* [in] */ VARIANT value);

  virtual HRESULT STDMETHODCALLTYPE invoke_static( BSTR classname, BSTR methodname,
                                                  /* [in] */ SAFEARRAY __RPC_FAR * args,
                                                  /* [retval][out] */ VARIANT __RPC_FAR *retvalue);
        
  virtual HRESULT STDMETHODCALLTYPE peek_static(BSTR classname, BSTR membername,
                                                           /* [retval][out] */ VARIANT __RPC_FAR *retvalue);
        
  virtual HRESULT STDMETHODCALLTYPE poke_static(BSTR classname, BSTR membername, VARIANT value);
  
  /** implemented for ISupportErrorInfo */
  virtual HRESULT STDMETHODCALLTYPE InterfaceSupportsErrorInfo(REFIID riid);

protected:
  /**
    Internal implementation. 
    @throw Throwables
  */
  HRESULT new_object(IN(RString) classname, VARIANT* args, int argnum, IDispatch __RPC_FAR *__RPC_FAR *retvalue);

  /**
    Internal implementation. 
    @throw Throwables
  */
  HRESULT _invoke(IN(RString) funcname, VARIANT *params, int argcount, VARIANT* pVarResult);

  HRESULT invoke_static( BSTR classname, BSTR methodname, VARIANT* args, int argcount, VARIANT __RPC_FAR *retvalue);

  static HRESULT createErrorInfoFromException(IN(::acdk::lang::RThrowable) ex);
  /*
  

  virtual HRESULT STDMETHODCALLTYPE GetTypeInfoCount( UINT __RPC_FAR *pctinfo);
  virtual HRESULT STDMETHODCALLTYPE GetTypeInfo(UINT iTInfo, LCID lcid, ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo);

  virtual HRESULT STDMETHODCALLTYPE GetIDsOfNames(REFIID riid, LPOLESTR __RPC_FAR *rgszNames, UINT cNames,
                                                  LCID lcid, DISPID __RPC_FAR *rgDispId);
  virtual HRESULT STDMETHODCALLTYPE Invoke(DISPID dispIdMember, REFIID riid, LCID lcid, WORD wFlags, 
                                            DISPPARAMS __RPC_FAR *pDispParams, VARIANT __RPC_FAR *pVarResult,   
                                            EXCEPINFO __RPC_FAR *pExcepInfo, UINT __RPC_FAR *puArgErr);
  */
  /*
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
  */
};

} // namespace com
} // namespace acdkx 



#endif //acdkx_com_AcdkObject_h

