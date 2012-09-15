// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// AcdkObjectpyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any AcdkObjectmmercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/AcdkObject.cpp,v 1.25 2005/04/18 20:40:37 kommer Exp $




#include "AcdkObject.h"
#include <acdk/lang/Throwable.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/lang/dmi/AcdkDmiClient.h>

#include "ComDmiClient.h"

namespace acdkx {
namespace com {


using namespace acdk::lang::dmi;

#if defined(__BORLANDC__)        
using namespace ::acdk::lang::dmi;
#endif


HRESULT STDMETHODCALLTYPE 
AcdkObject::QueryInterface(REFIID riid,void** ppvObject) 
{
  if (CLSID_acdk_lang_Object == riid)
  {
    addRef();
    *ppvObject = (::IAcdkObject*)this;
    return S_OK;
  }
  if (IID_IDispatch == riid)
  {
    addRef();
    *ppvObject = (::IDispatch*)this;
    return S_OK;
  }
  if (IID_IAcdkObject == riid || CLSID_AcdkObject == riid)
  {
    addRef();
    *ppvObject = (::IDispatch*)this;
    return S_OK;
  }
  return AbstractCoInterface::QueryInterface(riid, ppvObject);
}

//virtual 
HRESULT STDMETHODCALLTYPE 
AcdkObject::InterfaceSupportsErrorInfo(REFIID riid)
{
  if (IID_IAcdkObject == riid || CLSID_AcdkObject == riid)
    return S_OK;
  return S_FALSE;
}

HRESULT STDMETHODCALLTYPE 
AcdkObject::GetTypeInfoCount( UINT __RPC_FAR *pctinfo)
{
  *pctinfo = 0;
  return S_OK;
}



HRESULT STDMETHODCALLTYPE 
AcdkObject::GetTypeInfo(UINT iTInfo, LCID lcid, ITypeInfo __RPC_FAR *__RPC_FAR *ppTInfo)
{
  if (iTInfo != 0)
    return DISP_E_BADINDEX;

  return ERROR_CALL_NOT_IMPLEMENTED;
}


HRESULT STDMETHODCALLTYPE 
AcdkObject::GetIDsOfNames(REFIID riid, LPOLESTR __RPC_FAR *rgszNames, UINT cNames,
                                                  LCID lcid, DISPID __RPC_FAR *rgDispId)

{
  HRESULT hr = S_OK;
  if (cNames < 1)
    return DISP_E_BADINDEX;
  if (cNames > 1)
    return ERROR_CALL_NOT_IMPLEMENTED;
  
  int _clength;
  RString name = OLE2S(rgszNames[0]);
  if (name->equals("New") == true)
  {
    rgDispId[0] = 1;
    return S_OK;
  }
  if (name->equals("invoke") == true || name->equals("Invoke") == true)
  {
    rgDispId[0] = 2;
    return S_OK;
  }
  if (name->equals("peek") == true || name->equals("Peek") == true)
  {
    rgDispId[0] = 3;
    return S_OK;
  }
  if (name->equals("poke") == true || name->equals("Poke") == true)
  {
    rgDispId[0] = 4;
    return S_OK;
  }
  if (name->equals("invoke_static") == true) 
  {
    rgDispId[0] = 5;
    return S_OK;
  }
  if (name->equals("peek_static") == true) 
  {
    rgDispId[0] = 6;
    return S_OK;
  }
  if (name->equals("poke_static") == true) 
  {
    rgDispId[0] = 7;
    return S_OK;
  }
  if (_obj == Nil)
    return DISP_E_UNKNOWNNAME;
  const ::acdk::lang::dmi::ClazzInfo* ci = _obj->getClazzInfo();
  const ::acdk::lang::dmi::ClazzMethodInfo* cmi = lookupMethod(ci, name, MiPublic);
  if (cmi != 0)
  {
    rgDispId[0] = (DISPID)cmi;
    return S_OK;
  }
  int flags = MiPublic;
  if (_obj != Nil)
  {
    ci = _obj->getClazzInfo();
    const ClazzFieldInfo* cfi = acdk::lang::dmi::ClazzInfo::findField(ci, name, flags);
    if (cfi != 0)
    {
      rgDispId[0] = (DISPID)cfi;
      return S_OK;
    }
  }
  return DISP_E_UNKNOWNNAME;
}




   

HRESULT 
AcdkObject::_invoke(IN(RString) funcname, VARIANT *params, int argcount, VARIANT* result)
{
  ScriptVarArray args(argcount);
  variants2ScriptVarArray(params, argcount, args, unwrapAcdkObject());
  ScriptVar erg;
  int flags = MiPublic;
  if (_obj == Nil)
    flags |= MiStatic;
  const ::acdk::lang::dmi::ClazzMethodInfo* cmi = 
      _obj->standardDispatch(funcname, erg, args, ComDmiClient::getDmiClient(), Nil, flags);
  if (cmi == 0)
    return DISP_E_BADPARAMCOUNT;
  if (result != 0 /*&& result->vt != VT_EMPTY VBScript always set to VT_EMPTY */)
    scriptVar2Variant(erg, *result);
  variants2ScriptVarArrayOut(params, argcount, args, unwrapAcdkObject());
  return S_OK;
}
  
HRESULT STDMETHODCALLTYPE 
AcdkObject::Invoke(DISPID dispIdMember, REFIID riid, LCID lcid, WORD wFlags, 
                                            DISPPARAMS __RPC_FAR *pDispParams, VARIANT __RPC_FAR *pVarResult,   
                                            EXCEPINFO __RPC_FAR *pExcepInfo, UINT __RPC_FAR *puArgErr)
{
  try {
    switch (wFlags)
    {
    case DISPATCH_METHOD:
    case 0x3: // called from Basic
      {
        if (dispIdMember == 0) {
          return DISP_E_UNKNOWNNAME; //???
        } else if (dispIdMember == 1) {// New
          if (pDispParams->cArgs < 1)
            return DISP_E_BADPARAMCOUNT;
          
          if (pDispParams->rgvarg[pDispParams->cArgs - 1].vt != VT_BSTR)
            return DISP_E_BADVARTYPE;
          
          int _clength;
          RString classname = OLE2S(pDispParams->rgvarg[pDispParams->cArgs - 1].bstrVal);
          pVarResult->vt = VT_DISPATCH;
          return new_object(classname, pDispParams->rgvarg, pDispParams->cArgs - 1, &pVarResult->pdispVal);
        } else if (dispIdMember == 2) { // invoke
          if (pDispParams->cArgs < 1)
            return DISP_E_BADPARAMCOUNT;
          int _clength;
          return _invoke( OLE2CS(pDispParams->rgvarg[pDispParams->cArgs - 1].bstrVal), pDispParams->rgvarg
            , pDispParams->cArgs - 1, pVarResult);
          
        } else if (dispIdMember == 3) { // peek
          if (pDispParams->cArgs != 1)
            return DISP_E_BADPARAMCOUNT;
          return peek(pDispParams->rgvarg[0].bstrVal, pVarResult);
          
        } else if (dispIdMember == 4) { // poke
          if (pDispParams->cArgs != 2)
            return DISP_E_BADPARAMCOUNT;
          return poke(pDispParams->rgvarg[1].bstrVal, pDispParams->rgvarg[0]);
          
        } else if (dispIdMember == 5) { // invoke_static
          if (pDispParams->cArgs < 2)
            return DISP_E_BADPARAMCOUNT;
          return invoke_static( pDispParams->rgvarg[pDispParams->cArgs - 1].bstrVal
            , pDispParams->rgvarg[pDispParams->cArgs - 2].bstrVal
            , pDispParams->rgvarg, pDispParams->cArgs - 2, pVarResult);
          
        } else if (dispIdMember == 6) { // peek_static
          if (pDispParams->cArgs != 2)
            return DISP_E_BADPARAMCOUNT;
          return peek_static(pDispParams->rgvarg[1].bstrVal, pDispParams->rgvarg[0].bstrVal, pVarResult);
          
        } else if (dispIdMember == 7) { // poke_static
          if (pDispParams->cArgs != 3)
            return DISP_E_BADPARAMCOUNT;
          return poke_static(pDispParams->rgvarg[2].bstrVal, pDispParams->rgvarg[1].bstrVal, pDispParams->rgvarg[0]);
          
        } else { // generic invokation
          ::acdk::lang::dmi::MetaInfo* mi = (::acdk::lang::dmi::MetaInfo*)dispIdMember;
          if (mi->flags & MiFieldInfo) {
            ::acdk::lang::dmi::ClazzFieldInfo* cfi = (::acdk::lang::dmi::ClazzFieldInfo*)mi;
            if (pDispParams->cArgs == 0) { // peek
              scriptVar2Variant(_obj->peek(cfi->name), *pVarResult);
              return S_OK;
            } else if (pDispParams->cArgs == 1) { // poke
              _obj->poke(cfi->name, variant2ScriptVar(pDispParams->rgvarg[0], unwrapAcdkObject()));
              return S_OK;
            } else {
              return DISP_E_BADPARAMCOUNT;
            } 
          } else if (mi->flags & MiMethodInfo)  {
            ClazzMethodInfo* cmi = (ClazzMethodInfo*)mi;
            // ### handle namedArgs
            return _invoke(cmi->name, pDispParams->rgvarg, pDispParams->cArgs, pVarResult);
          }  else  {
            return DISP_E_BADINDEX;
          }
        }
      }
    case DISPATCH_PROPERTYGET:
      {
        ::acdk::lang::dmi::ClazzFieldInfo* cfi = (::acdk::lang::dmi::ClazzFieldInfo*)dispIdMember;
        if ((cfi->flags & MiFieldInfo) == 0) 
          return DISP_E_BADINDEX;
        if (pDispParams->cArgs != 0) 
          return DISP_E_BADPARAMCOUNT;
        scriptVar2Variant(_obj->peek(cfi->name), *pVarResult);
        return S_OK;
      }
    case DISPATCH_PROPERTYPUT:
      {
        ::acdk::lang::dmi::ClazzFieldInfo* cfi = (::acdk::lang::dmi::ClazzFieldInfo*)dispIdMember;
        if (cfi == 0)
        {
          /* in case VB:
            "var = othervar"
          */
          ScriptVar sv = variant2ScriptVar(pDispParams->rgvarg[0], unwrapAcdkObject());
          if (sv.isObjectType() == false)
            return DISP_E_BADINDEX;
          _obj = sv.getObjectVar();
          return S_OK;
        }
        if ((cfi->flags & MiFieldInfo) == 0) 
          return DISP_E_BADINDEX;
        if (pDispParams->cArgs != 1) 
          return DISP_E_BADPARAMCOUNT;
        _obj->poke(cfi->name, variant2ScriptVar(pDispParams->rgvarg[0], unwrapAcdkObject()));
        return S_OK;
      }
    case DISPATCH_PROPERTYPUTREF:
      {
        //int* x = 0;
        //*x = 0;
        return ERROR_CALL_NOT_IMPLEMENTED;
      }
    default:
      return DISP_E_BADINDEX;
    }
  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}

namespace {
template <class T>
struct SafeArrayLock
{
  SAFEARRAY& _array;
	SafeArrayLock(SAFEARRAY& arr, T* data) 
  : _array(arr) 
  {
    SafeArrayAccessData(&arr, (void HUGEP* FAR*)data);
  }
  ~SafeArrayLock() 
  { 
    SafeArrayUnaccessData(&_array);
  }
};

} // anon namespace


HRESULT STDMETHODCALLTYPE 
AcdkObject::New(BSTR classname, SAFEARRAY __RPC_FAR * args, IDispatch __RPC_FAR *__RPC_FAR *retvalue)
{
  try {
    int _clength;
    RString clsname = OLE2S(classname);
    VARIANT* variants = 0;
    SafeArrayLock<VARIANT> sa(*args, variants);
    return new_object(clsname, variants, args->cbElements, retvalue);

  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}
    
HRESULT STDMETHODCALLTYPE 
AcdkObject::invoke(BSTR methodname, SAFEARRAY __RPC_FAR * args, VARIANT __RPC_FAR *retvalue)
{
  try {
    int _clength;
    VARIANT* variants = 0;
    SafeArrayLock<VARIANT> sa(*args, variants);
    return _invoke(OLE2CS(methodname), variants, args->cbElements, retvalue);
  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}
    
HRESULT STDMETHODCALLTYPE 
AcdkObject::peek(BSTR membername, VARIANT __RPC_FAR *retvalue)
{
  try {
    int _clength = 0;
    RString member = OLE2S(membername);
    ScriptVar erg = _obj->peek(member);
    if (retvalue != 0)
      scriptVar2Variant(erg, *retvalue);
    return S_OK;
  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}
    
HRESULT STDMETHODCALLTYPE 
AcdkObject::poke( BSTR membername, VARIANT value)
{
  try {
    int _clength = 0;
    RString member = OLE2S(membername);
    ScriptVar val = variant2ScriptVar(value, unwrapAcdkObject());
    _obj->poke(member, val);
    return S_OK;
  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}


HRESULT 
AcdkObject::invoke_static( BSTR classname, BSTR methodname, VARIANT* args, int argcount, VARIANT __RPC_FAR *retvalue)
{
  try {
    int _clength;
    RString clzname = OLE2S(classname);
    RString method = OLE2S(methodname);
    
    ScriptVarArray sargs(argcount);
    variants2ScriptVarArray(args, argcount, sargs, unwrapAcdkObject());
    ScriptVar erg = StdDispatch::invokeStaticMethod(clzname, method, sargs, ComDmiClient::getDmiClient(), Nil);
    variants2ScriptVarArray(args, argcount, sargs, unwrapAcdkObject());
    if (retvalue != 0)
      scriptVar2Variant(erg, *retvalue);
    return S_OK;
  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}

HRESULT STDMETHODCALLTYPE 
AcdkObject::invoke_static( BSTR classname, BSTR methodname, SAFEARRAY __RPC_FAR * args, VARIANT __RPC_FAR *retvalue)
{
  try {
    VARIANT* variants = 0;
    SafeArrayLock<VARIANT> sa(*args, variants);
    return invoke_static(classname, methodname, variants, args->cbElements, retvalue);
  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}

HRESULT STDMETHODCALLTYPE 
AcdkObject::peek_static(BSTR classname, BSTR membername, VARIANT __RPC_FAR *retvalue)
{
  try {
    int _clength;
    RString clzname = OLE2S(classname);
    RString member = OLE2S(membername);

    ScriptVar ret = StdDispatch::peek_static(clzname, member);

    if (retvalue != 0)
      scriptVar2Variant(ret, *retvalue);
    return S_OK;
  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}
        
HRESULT STDMETHODCALLTYPE 
AcdkObject::poke_static(BSTR classname, BSTR membername, VARIANT value)
{
  RString clss;
  try {
    int _clength;
    clss = OLE2S(classname);
    clss = clss->narrow();
    StdDispatch::poke_static(clss, OLE2S(membername), variant2ScriptVar(value, unwrapAcdkObject()));
    return S_OK;
  } catch (RThrowable ex) {
    return createErrorInfoFromException(ex);  
  }
}


//static 
HRESULT
AcdkObject::new_object(IN(RString) classname, VARIANT* args, int argnum, IDispatch __RPC_FAR *__RPC_FAR *retvalue)
{
  ScriptVarArray sargs(argnum);
  
  for (int i = 0; i < argnum; ++i)
  {
    sargs[argnum - (i + 1)] = variant2ScriptVar(args[i], unwrapAcdkObject());
  }
  RClass cls = Class::forName(classname);
  if (cls == Nil)
  {
    return DISP_E_TYPEMISMATCH;
  }
    /*  
  RString constructorname = classname;
  if (constructorname->indexOf('/') != -1)
    constructorname = constructorname->substr(constructorname->lastIndexOf('/') + 1);
  */
  ScriptVar ret = StdDispatch::New(classname, sargs, ComDmiClient::getDmiClient());
  AcdkObject* acobj = new AcdkObject(ret.getObjectVar());
  acobj->addRef();
  *retvalue = acobj;
  return S_OK;
  
}


//static 
HRESULT 
AcdkObject::createErrorInfoFromException(IN(RThrowable) ex)
{
  ICreateErrorInfo* cei;
  CreateErrorInfo(&cei);
  RString msg = ex->getMessage()->convert(CCUcs2);
  cei->SetDescription((wchar_t*)S2OLE(msg));
  cei->SetGUID(IID_IAcdkObject);

  acdk::io::MemWriter sw;
  acdk::io::PrintWriter out(&sw);
  ex->printStackTrace(&out);
  RString str = (new String(sw.getBuffer()))->convert(CCUcs2);
  cei->SetSource((wchar_t*)S2OLE(str));

  IErrorInfo* ei;
  cei->QueryInterface(IID_IErrorInfo, (void**)&ei);
  SetErrorInfo(0, ei);
  cei->Release();
  ei->Release();
  return E_FAIL;
}

} // namespace com
} // namespace acdkx 





