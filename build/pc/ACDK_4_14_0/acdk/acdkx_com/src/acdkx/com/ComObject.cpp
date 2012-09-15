// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// ComObjectpyright (C) 2000 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any ComObjectmmercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/ComObject.cpp,v 1.21 2005/02/20 13:56:33 kommer Exp $



#include "ComObject.h"
#include "AcdkObject.h"
#include "CoException.h"

#include <acdk/lang/reflect/InvocationTargetException.h>

namespace acdkx {
namespace com {


using namespace acdk::lang::dmi;


HRESULT 
CreateObject(IN(RString) str, IDispatch FAR* FAR* ppdisp)
{
    CLSID clsid;                  // CLSID of ActiveX object.

    LPUNKNOWN punk = NULL;        // IUnknown of ActiveX object.
    LPDISPATCH pdisp = NULL;      // IDispatch of ActiveX object.

    *ppdisp = NULL;
    RString wstr = str->convert(CCUcs2);
    HRESULT hr = CLSIDFromProgID(S2OLE(wstr), &clsid);
    if (FAILED(hr))
        goto error;

    hr = CoCreateInstance(clsid, NULL, CLSCTX_SERVER, 
                            IID_IUnknown, (void FAR* FAR*)&punk);
    if (FAILED(hr))
        goto error;

    hr = punk->QueryInterface(IID_IDispatch, (void FAR* FAR*)&pdisp);
    if (FAILED(hr))
        goto error;

    *ppdisp = pdisp;
    punk->Release();
    return NOERROR;
    
error:
    if (punk) punk->Release();
    if (pdisp) pdisp->Release();
    return hr;
}


//virtual 
acdk::lang::dmi::ScriptVar 
ComObject::New(IN(RString) classname, acdk::lang::dmi::ScriptVarArray& args)
{
  RString wstr = classname->convert(CCUcs2);
  HRESULT hr = CreateObject(S2W(wstr), &(IDispatch*)_idispatch);
  
  return acdk::lang::dmi::ScriptVar(Nil);
}


ComObject::ComObject(IN(RString) classname)
{
  RString wstr = classname->convert(CCUcs2);
  ACDK_CECKCOMCALL(CreateObject(S2W(wstr), &(IDispatch*)_idispatch));
}

//static
::acdk::lang::dmi::ScriptVar
ComObject::New(IN(RString) classname)
{
  RIDispatch rid;
  RString wstr = classname->convert(CCUcs2);
  ACDK_CECKCOMCALL(CreateObject(S2W(wstr), &(IDispatch*)rid));
  return new ComObject(rid);
}




struct DispParams
: public DISPPARAMS
{
  DispParams(ScriptVarArray& args)
    
  {
    rgvarg = 0;
    cNamedArgs = 0;
    rgdispidNamedArgs = 0;
    cArgs = args.size();
    rgvarg = new VARIANTARG[cArgs];
    scriptVarArray2Variants(args, rgvarg);
  }
  ~DispParams()
  {
    delete[] rgvarg;
  }
  void writeBackOuts(ScriptVarArray& args)
  {
    scriptVarArray2VariantsOut(args, rgvarg);
  }
};


void 
checkInvokeResult(HRESULT hr, EXCEPINFO& exInfo, UINT argErr)
{
  if (FAILED(hr) == false)
    return;
  
  if (DISP_E_EXCEPTION == hr) {
    if (exInfo.wCode != 0 || exInfo.scode != 0) {
      if (exInfo.pfnDeferredFillIn != 0)
        exInfo.pfnDeferredFillIn(&exInfo);
      StringBuffer sb;
      int _clength;
      sb.append(W2CS(exInfo.bstrDescription));
      SysFreeString(exInfo.bstrSource);
      SysFreeString(exInfo.bstrDescription);
      SysFreeString(exInfo.bstrHelpFile);
      
      THROW2(CoException, hr, sb.toString());
    }
  }
  SysFreeString(exInfo.bstrSource);
  SysFreeString(exInfo.bstrDescription);
  SysFreeString(exInfo.bstrHelpFile);
      
  THROW2(CoException, hr, "Invoke COM Object");
}


//virtual 
void 
ComObject::setMember(IN(RString) fieldname, const ::acdk::lang::dmi::ScriptVar& newval, ::acdk::lang::dmi::DmiClient& dc, int flags)
{
  DISPID dispid;
  int _clength;
  RString fname = fieldname->convert(CCUcs2);
  OLECHAR* och = S2OLE(fname);
  HRESULT hr = _idispatch->GetIDsOfNames(IID_NULL, &och, 1, LOCALE_USER_DEFAULT, &dispid);
  if (FAILED(hr)) 
  {
    throw acdk::lang::reflect::InvocationTargetException(RString("Field cannot be found: ") + fname);
  }
  ScriptVarArray args;
  args.push_back(newval);
  DispParams dparams(args);
  dparams.cNamedArgs  = 1;
  DISPID dispidNamed = DISPID_PROPERTYPUT;
  dparams.rgdispidNamedArgs = &dispidNamed;
  VARIANT ret;
  
  EXCEPINFO exInfo;
  memset(&exInfo, 0, sizeof(exInfo));

  UINT argErr = 0;
  
  hr = _idispatch->Invoke(dispid, IID_NULL, 0, DISPATCH_PROPERTYPUT, &dparams, &ret, &exInfo, &argErr);
  checkInvokeResult(hr, exInfo, argErr);  
  
}
/*
//virtual 
void 
ComObject::poke(IN(RString) fieldname, acdk::lang::dmi::ScriptVar arg)
{
  DISPID dispid;
  OLECHAR* och = S2W(fieldname);
  HRESULT hr = _idispatch->GetIDsOfNames(IID_NULL, &och, 1, LOCALE_USER_DEFAULT, &dispid);
  if (FAILED(hr)) 
  {
    throw acdk::lang::reflect::InvocationTargetException("Field cannot be found: " + fieldname);
  }
  ScriptVarArray args;
  args.push_back(arg);
  DispParams dparams(args);
  dparams.cNamedArgs  = 1;
  DISPID dispidNamed = DISPID_PROPERTYPUT;
  dparams.rgdispidNamedArgs = &dispidNamed;
  VARIANT ret;
  
  EXCEPINFO exInfo;
  memset(&exInfo, 0, sizeof(exInfo));

  UINT argErr = 0;
  
  hr = _idispatch->Invoke(dispid, IID_NULL, 0, DISPATCH_PROPERTYPUT, &dparams, &ret, &exInfo, &argErr);
  checkInvokeResult(hr, exInfo, argErr);  
}
*/

//virtual 
::acdk::lang::dmi::ScriptVar 
ComObject::getMember(IN(RString) fieldname, ::acdk::lang::dmi::DmiClient& dc, int flags, const ::acdk::lang::dmi::ClazzInfo* type_requested)
{
  DISPID dispid;
  int _clength;
  RString fname = fieldname->convert(CCUcs2);;
  OLECHAR* och = S2OLE(fname);
  HRESULT hr = _idispatch->GetIDsOfNames(IID_NULL, &och, 1, LOCALE_USER_DEFAULT, &dispid);
  if (FAILED(hr)) 
  {
    throw acdk::lang::reflect::InvocationTargetException(RString("Field cannot be found: ") + fname);
  }
  ::acdk::lang::dmi::ScriptVarArray args;
  DispParams dparams(args);
  VARIANT ret;
  
  EXCEPINFO exInfo;
  memset(&exInfo, 0, sizeof(exInfo));

  UINT argErr = 0;
  hr = _idispatch->Invoke(dispid, IID_NULL, 0, DISPATCH_PROPERTYGET, &dparams, &ret, &exInfo, &argErr);
  checkInvokeResult(hr, exInfo, argErr);
  // ### check type_requested and flags
  return variant2ScriptVar(ret, unwrapAcdkObject());
}




::acdk::lang::dmi::ClazzMethodArgInfo* ComObject_methods_GetClass__L_acdk_lang_RClass__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* ComObject_methods_GetClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};


::acdk::lang::dmi::ClazzMethodInfo ComObject_method_GetClass__L_acdk_lang_RClass_ = 
{
  MiStatic | MiPublic | MiMethodInfo,// class flags, like static, Constructor
  0,
  "GetClass", // name of method
  -1, // nameHashCode
  "",
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::RClass::clazzInfo(), // return type
  0,
  -1, // nameHashCode
  ComObject_methods_GetClass__L_acdk_lang_RClass__args, // return the arguments
  0, // args num
  ComObject_methods_GetClass__L_acdk_lang_RClass__exceptions, // the declared exceptions
  0 // address of method currently not supported
};


::acdk::lang::dmi::ClazzMethodArgInfo* ComObject_methods_getClass__L_acdk_lang_RClass__args[] = 
{
  0
};

::acdk::lang::dmi::ClazzInfo* ComObject_methods_getClass__L_acdk_lang_RClass__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodArgInfo ComObject_methods_New_LRString__LRComObject__arg_classname = 
{
  MiMethodArgInfo | MiAiIn,
  0,
  "classname",
  -1, // nameHashCode
  "",
  0, // _scopeParent
  0, // _nextScopeSibling
  RString::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo* ComObject_methods_New_LRString__LRComObject__args[] = 
{
  &ComObject_methods_New_LRString__LRComObject__arg_classname,
  0
};

::acdk::lang::dmi::ClazzInfo* ComObject_methods_New_LRString__LRComObject__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo ComObject_method_New_LRString__LRComObject_ = 
{
  MiStatic | MiPublic | MiMethodInfo,// class flags, like static, Constructor
  0,
  "New", // name of method
  -1, // nameHashCode
  "",
  0, // _scopeParent
  0, // _nextScopeSibling
  RComObject::clazzInfo(), // return type
  0, // altname
  -1, // nameHashCode
  ComObject_methods_New_LRString__LRComObject__args, // return the arguments
  0, // argsnum
  ComObject_methods_New_LRString__LRComObject__exceptions, // the declared exceptions
  0 // address of method currently not supported
};

::acdk::lang::dmi::ClazzMethodArgInfo ComObject_methods_ComObject_LRString__LRComObject__arg_classname = 
{
  MiMethodArgInfo | MiAiIn,
  0,
  "RString",
  -1, // nameHashCode
  "",
  0, // _scopeParent
  0, // _nextScopeSibling
  RString::clazzInfo()
};

::acdk::lang::dmi::ClazzMethodArgInfo* ComObject_methods_ComObject_LRString__LRComObject__args[] = 
{
  &ComObject_methods_ComObject_LRString__LRComObject__arg_classname,
  0
};

::acdk::lang::dmi::ClazzInfo* ComObject_methods_ComObject_LRString__LRComObject__exceptions[] =
{
  0
};

::acdk::lang::dmi::ClazzMethodInfo ComObject_method_ComObject_LRString__LRComObject_ = 
{
  MiStatic | MiPublic | MiMethodInfo,// class flags, like static, Constructor
  0,
  "ComObject", // name of method
  -1, // nameHashCode
  "",
  0, // _scopeParent
  0, // _nextScopeSibling
  ComObject::clazzInfo(), // return type
  0, // altname
  -1, // nameHashCode
  ComObject_methods_ComObject_LRString__LRComObject__args, // return the arguments
  0, //args num
  ComObject_methods_ComObject_LRString__LRComObject__exceptions, // the declared exceptions
  0 // address of method currently not supported
};


::acdk::lang::dmi::ClazzMethodInfo* _ComObject_methods[] = 
{
  &ComObject_method_ComObject_LRString__LRComObject_,
  &ComObject_method_GetClass__L_acdk_lang_RClass_,
  &ComObject_method_GetClass__L_acdk_lang_RClass_,
  &ComObject_method_New_LRString__LRComObject_,
  0
};

::acdk::lang::dmi::ClazzFieldInfo* _ComObject_fields[] = 
{
  0
};

::acdk::lang::dmi::ClazzSuperInfo _ComObject_super___acdk__lang__Object =
{
  MiPublic | MiSuperInfo,
  0,
  ::acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _ComObject_interfaces[] =
{
  &_ComObject_super___acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo ComObject::_clazzInfo =
{
  MiClazzInfo | MiPublic | MiResolved | MiCiWeakBind, // clazz-flags
  0,
  "ComObject", // name of class
  -1, // nameHashCode
  "acdkx/com", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &ComObject::_clazzInfo,
   0, // _firstChild
  _ComObject_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  _ComObject_fields, // pointer to Array of fields
  0, // count of Fields
  _ComObject_methods, // pointer to Array of fields
  0, // count of Fields
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ComObject::_invoke_dynamic,
  ComObject::_invoke_static, // static_dispatch
 0, // count off all collectable members in this class
  0, // user defined info
  0 // ClazzInfo* _next; 
};
static ::acdk::lang::dmi::RegisterClazzInfo _register_ComObject(ComObject::clazzInfo());

//static 
const ClazzMethodInfo* 
ComObject::_invoke_dynamic(  ::acdk::lang::Object* This_, 
                                                          IN(RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  RComObject This = dynamic_cast<ComObject*>(This_);
  DISPID dispid;
  int _clength;
  RString funcname = fname->convert(CCUcs2);
  OLECHAR* och = S2OLE(funcname);
  // ### handle namedArgs
  HRESULT hr = This->_idispatch->GetIDsOfNames(IID_NULL, &och, 1, LOCALE_USER_DEFAULT, &dispid);
  if (FAILED(hr)) 
  {
    THROW1_FQ(acdk::lang::reflect::, InvocationTargetException, RString("Member cannot be found: ") + funcname);
  }
  DispParams dparams(args);
  VARIANT vret;
  
  EXCEPINFO exInfo;
  memset(&exInfo, 0, sizeof(exInfo));

  UINT argErr = 0;
  hr = This->_idispatch->Invoke(dispid, IID_NULL, 0, DISPATCH_METHOD,
                        &dparams,
                        &vret,
                        &exInfo, &argErr);
  checkInvokeResult(hr, exInfo, argErr);
  dparams.writeBackOuts(args);
  ret = variant2ScriptVar(vret, This->unwrapAcdkObject());
  return (::acdk::lang::dmi::ClazzMethodInfo*)1;
}

//static 
const ClazzMethodInfo* 
ComObject::_invoke_static(  IN(RString) fname, 
                                          ScriptVar& ret, 
                                          ScriptVarArray& args, 
                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  if (clazzinfo == 0)
    clazzinfo = clazzInfo();
  if (methinf == 0)
    methinf = StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags);
  if (&ComObject_method_ComObject_LRString__LRComObject_ == methinf) {
    ret = (::acdk::lang::RObject)new ComObject((RString)args[0].getObjectVar());
    return methinf;
  }
  if (methinf == &ComObject_method_New_LRString__LRComObject_)
  {
    ret = (RObject)ComObject::New((RString)args[0].getObjectVar());
    return methinf;
  }
  return Object::StandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
}

/*
//virtual 
const ::acdk::lang::dmi::ClazzMethodInfo* 
ComObject::standardDispatch(const char* fname, 
                 ::acdk::lang::dmi::ScriptVar& ret, 
                 ::acdk::lang::dmi::ScriptVarArray& args, 
                 ::acdk::lang::dmi::DmiClient& dc,  IN(::acdk::lang::RStringArray) namedArgs, int flags, 
                 const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                 const ::acdk::lang::dmi::ClazzMethodInfo* methinf)

{
  DISPID dispid;
  int _clength;
  OLECHAR* och = CS2W(fname);
  // ### handle namedArgs
  HRESULT hr = _idispatch->GetIDsOfNames(IID_NULL, &och, 1, LOCALE_USER_DEFAULT, &dispid);
  if (FAILED(hr)) 
  {
    THROW1_FQ(acdk::lang::reflect::, InvocationTargetException, RString("Member cannot be found: ") + fname);
  }
  DispParams dparams(args);
  VARIANT vret;
  
  EXCEPINFO exInfo;
  memset(&exInfo, 0, sizeof(exInfo));

  UINT argErr = 0;
  hr = _idispatch->Invoke(dispid, IID_NULL, 0, DISPATCH_METHOD,
                        &dparams,
                        &vret,
                        &exInfo, &argErr);
  checkInvokeResult(hr, exInfo, argErr);
  dparams.writeBackOuts(args);
  ret = variant2ScriptVar(vret, unwrapAcdkObject());
  return (::acdk::lang::dmi::ClazzMethodInfo*)1;
}


using ::acdk::lang::dmi::StdDispatch;

//static 
const ::acdk::lang::dmi::ClazzMethodInfo* 
ComObject::StandardDispatch(const char* fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, 
                           ::acdk::lang::dmi::DmiClient& dc,  IN(::acdk::lang::RStringArray) namedArgs, int flags, 
                           const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                           const ::acdk::lang::dmi::ClazzMethodInfo* methinf
)
{
  if (methinf == 0)
    methinf = StdDispatch::lookupMethod(clazzInfo(), fname, args, namedArgs, dc, flags);
  if (&ComObject_method_ComObject_LRString__LRComObject_ == methinf) {
    ret = (::acdk::lang::RObject)new ComObject((RString)args[0].getObjectVar());
    return methinf;
  }
  if (methinf == &ComObject_method_New_LRString__LRComObject_)
  {
    ret = (RObject)ComObject::New((RString)args[0].getObjectVar());
    return methinf;
  }
  return Object::StandardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
}
*/

} // namespace com
} // namespace acdkx 





