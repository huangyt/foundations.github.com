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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/CoSys.cpp,v 1.16 2005/02/05 10:45:38 kommer Exp $



#include "CoSys.h"
#include "AcdkObject.h"
#include "ComObject.h"
#include <acdk/lang/DmiTypeConversionException.h>

namespace acdkx {
namespace com {

using ::acdk::lang::dmi::ScriptVar;
using ::acdk::lang::dmi::ScriptVarArray;

void
variants2ScriptVarArray(VARIANT* vars, int varcount, ScriptVarArray& sv, bool unwrapAcdkObject)
{
  for (int i = 0; i < varcount; ++i)
  {
    sv[varcount - (i + 1)] = variant2ScriptVar(vars[i], unwrapAcdkObject);
  }
}


void
scriptVar2Variant(acdk::lang::dmi::ScriptVar& arg, VARIANT& dparam)
{
  switch (arg.type)
  {
  case ScriptVar::BoolType:
    dparam.vt = VT_BOOL;
    dparam.boolVal = arg.getBoolVar();
    break;

  case ScriptVar::CharType:
    dparam.vt = VT_I1;
    dparam.cVal = arg.getCharVar();
    break;
  case ScriptVar::ByteType:
    dparam.vt = VT_UI1;
    dparam.bVal = arg.getByteVar();
    break;
  case ScriptVar::ShortType:
    dparam.vt = VT_I2;
    dparam.iVal = arg.getShortVar();
    break;
  case ScriptVar::IntType:
    dparam.vt = VT_I4;
    dparam.lVal = arg.getIntVar();
    break;
  case ScriptVar::IntRefType:
    dparam.vt = VT_I4 | VT_BYREF;
    dparam.plVal = (long*)&arg.getIntRef();
    break;
  case ScriptVar::FloatType:
    dparam.vt = VT_R4;
    dparam.fltVal = arg.getFloatVar();
    break;
  case ScriptVar::DoubleType:
    dparam.vt = VT_R8;
    dparam.dblVal = arg.getDoubleVar();
    break;
  case ScriptVar::ObjectType:
  {
    RObject obj = arg.getObjectVar();
    if (obj == Nil) {

    } else if (instanceof(obj, String) == true) {
      dparam.vt = VT_BSTR;
      dparam.bstrVal = String2BSTR(RString(obj));
    } else if (instanceof(obj, ComObject) == true) {
      dparam.vt = VT_DISPATCH;
      dparam.pdispVal = RComObject(obj)->getIDispatch();
      dparam.pdispVal->AddRef();
    } else if (instanceof(obj, AcdkObject) == true) {
      dparam.vt = VT_DISPATCH;
      dparam.pdispVal = &RAcdkObject(obj);
      dparam.pdispVal->AddRef();
    } else {
      AcdkObject* acobj = new AcdkObject(obj);
      acobj->AddRef();
      dparam.vt = VT_DISPATCH;
      dparam.pdispVal = acobj;
    }
    break;
  }
  case ScriptVar::ObjectRefType:
  {
    RObject obj = arg.getObjectVar();
    if (obj == Nil) 
    {
      dparam.vt = VT_DISPATCH | VT_BYREF;
      IDispatch** acobj = new IDispatch*; // ### delete this
      *acobj = 0;
      dparam.ppdispVal = acobj;
    } 
    else if (instanceof(obj, String) == true)
    {
      BSTR* bstr = new BSTR;
      *bstr = String2BSTR(RString(obj));
      dparam.vt = VT_BSTR | VT_BYREF;
      dparam.pbstrVal = bstr; 
    } 
    else
    {
      IDispatch** acobj = new IDispatch*;
      *acobj = new AcdkObject(obj);
      (*acobj)->AddRef();
      dparam.vt = VT_DISPATCH;
      dparam.ppdispVal = acobj;
    }
    break;
  }
  case ScriptVar::UnknownType:
    dparam.vt = VT_EMPTY;
    break;
  default:
    THROW1(DmiTypeConversionException, RCS("ScriptVar Type not supported: ") + (int)arg.type);
    break;
  }
}

void
scriptVar2VariantOut(acdk::lang::dmi::ScriptVar& arg, VARIANT& dparam)
{
  int _clength;
  switch (arg.type)
  {
  case ScriptVar::ObjectRefType:
    switch (dparam.vt)
    {
    case  VT_BSTR | VT_BYREF:
    {
      arg = W2S(*dparam.pbstrVal);
      break;
    } 
    case VT_DISPATCH | VT_BYREF:
    {
      IAcdkObject* iacdk = 0;
      if (*dparam.ppdispVal == 0)
      {
        arg.getObjectRef() = Nil;
      } 
      else  if (!FAILED((*dparam.ppdispVal)->QueryInterface(IID_IAcdkObject, (void FAR* FAR*)&iacdk))) 
      {
        RAcdkObject ro = dynamic_cast<AcdkObject*>(iacdk);
        arg.getObjectRef() = ro->getObject();
        iacdk->Release();
      } 
      else
      {
        arg.getObjectRef() = new ComObject(*dparam.ppdispVal);
      }
      break;
    }
    }
    break;
  }
}

ScriptVar variant2ScriptVar(const VARIANT& varg, bool unwrapAcdkObjects)
{
  switch (varg.vt) 
  {
  case VT_EMPTY: return ScriptVar();
  case VT_NULL: return Nil;

  case VT_I1: return (char)varg.bVal;
  case VT_UI1: return (byte)varg.bVal;
  case VT_UI2:
  case VT_I2: return (short)varg.iVal;

  case VT_UINT:
  case VT_INT:
  case VT_UI4: 
  case VT_I4: return (int)varg.lVal;
  case VT_I4 | VT_BYREF:
    return (int*)varg.plVal;
  case VT_I8:
  case VT_UI8:return (jlong)varg.lVal; // ???

  case VT_R4: return (float)varg.fltVal;
  case VT_R8: return (double)varg.dblVal;
  case VT_CY:
  case VT_DATE:
  case VT_BSTR:
  {
    int _clength = 0;
    return RObject(SCS(W2CS(varg.bstrVal)));
  }
  case VT_BSTR  | VT_BYREF:
  {
    RObject* tobj = new RObject();
    int _clength = 0;
    if (varg.pbstrVal)
      *tobj = &SCS(W2CS(*varg.pbstrVal));
    return tobj;
  }
  case VT_VARIANT | VT_BYREF:
    return variant2ScriptVar(*varg.pvarVal, unwrapAcdkObjects);
    break;
  case VT_DISPATCH:
  {
    IAcdkObject* iacdk = 0;
    if (unwrapAcdkObjects == false ||
        FAILED(varg.pdispVal->QueryInterface(IID_IAcdkObject, (void FAR* FAR*)&iacdk)))
      return RObject(new ComObject(varg.pdispVal));
    RAcdkObject ro = dynamic_cast<AcdkObject*>(iacdk);
    RObject ret = ro->getObject();
    iacdk->Release();
    return ret;
    break;
  }
  case VT_DISPATCH | VT_BYREF:
  {
    //varg.ppdispVal
    RObject* robj = new RObject;
    IAcdkObject* iacdk = 0;
    if (*varg.ppdispVal != 0 && !FAILED((*varg.ppdispVal)->QueryInterface(IID_IAcdkObject, (void FAR* FAR*)&iacdk)))
    {
      RAcdkObject ro = dynamic_cast<AcdkObject*>(iacdk);
      RObject ret = ro->getObject();
      *robj = ret;
    } 
    //*robj = new ComObject(*varg.ppdispVal);
    return robj;
    /*
    if (unwrapAcdkObjects == false ||
        FAILED(varg.pdispVal->QueryInterface(IID_IAcdkObject, (void FAR* FAR*)&iacdk)))
      return RObject(new ComObject(varg.pdispVal));
    */
    break;
  }
  case VT_ERROR:
    break;
  case VT_BOOL: return varg.boolVal;
  case VT_VARIANT:
  case VT_UNKNOWN:
  case VT_DECIMAL:
  case VT_RECORD:
  default : 
    THROW1(DmiTypeConversionException, RCS("variant2ScriptVar: VARIANT type not supported:") +  (int)varg.vt); 
    break;
/*
 *  VT_VOID                [T]        C style void
 *  VT_HRESULT             [T]        Standard return type
 *  VT_PTR                 [T]        pointer type
 *  VT_SAFEARRAY           [T]        (use VT_ARRAY in VARIANT)
 *  VT_CARRAY              [T]        C style array
 *  VT_USERDEFINED         [T]        user defined type
 *  VT_LPSTR               [T][P]     null terminated string
 *  VT_LPWSTR              [T][P]     wide null terminated string
 *  VT_FILETIME               [P]     FILETIME
 *  VT_BLOB                   [P]     Length prefixed bytes
 *  VT_STREAM                 [P]     Name of the stream follows
 *  VT_STORAGE                [P]     Name of the storage follows
 *  VT_STREAMED_OBJECT        [P]     Stream contains an object
 *  VT_STORED_OBJECT          [P]     Storage contains an object
 *  VT_BLOB_OBJECT            [P]     Blob contains an object 
 *  VT_CF                     [P]     Clipboard format
 *  VT_CLSID                  [P]     A Class ID
 *  VT_VECTOR                 [P]     simple counted array
 *  VT_ARRAY            [V]           SAFEARRAY*
 *  VT_BYREF            [V]           void* for local use
 *  VT_BSTR_BLOB                      Reserved for system use
 */
  }
  return ScriptVar(); // ### throw ex
}

void variant2ScriptVarOut(const VARIANT& varg, ScriptVar& sv, bool unwrapAcdkObjects)
{
  switch (varg.vt) 
  {
  case VT_BSTR  | VT_BYREF:
  {
    RString ret = (RString)sv.getObjectVar();
    if (varg.pbstrVal != 0)
    {
      SysFreeString(*varg.pbstrVal);
      *varg.pbstrVal = String2BSTR(ret);
    }
    break;
  }
  case VT_DISPATCH  | VT_BYREF:
  {
    RObject* ret = sv.getObjectRef()._ref_this();
    RAcdkObject ro = new AcdkObject(*ret);
    delete ret;
    *varg.ppdispVal = &ro;
    (*varg.ppdispVal)->AddRef();
    break;
  }
  }
}

void
variants2ScriptVarArrayOut(VARIANT* vars, int varcount, ScriptVarArray& sv, bool unwrapAcdkObject)
{
  for (int i = 0; i < varcount; ++i)
  {
    variant2ScriptVarOut(vars[i], sv[varcount - (i + 1)], unwrapAcdkObject);
  }
}

void scriptVarArray2Variants(acdk::lang::dmi::ScriptVarArray& args, VARIANT* dparams)
{
  int asize = args.size();
  for (int i = 0; i < asize; ++i)
  {
    scriptVar2Variant(args[asize - (i + 1)], dparams[i]);
  }
}  

void scriptVarArray2VariantsOut(acdk::lang::dmi::ScriptVarArray& args, VARIANT* dparams)
{
  int asize = args.size();
  for (int i = 0; i < asize; ++i)
  {
    scriptVar2VariantOut(args[asize - (i + 1)], dparams[i]);
  }
}

} // namespace com
} // namespace acdkx






