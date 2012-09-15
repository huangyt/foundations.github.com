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
// $Header: /cvsroot/acdk/acdk/acdkx_com/src/acdkx/com/CreateTypeInfo.cpp,v 1.16 2005/03/12 11:51:43 kommer Exp $


#include "CreateTypeLib.h"
#include "CoException.h"
#include "CreateTypeInfo.h"
#include <acdk/lang/reflect/Modifier.h>

namespace acdkx {
namespace com {

using acdk::lang::reflect::Modifier;
using namespace acdk::lang::dmi;

void toElemDesc(ELEMDESC& elemdesc, ClazzMethodArgInfo& mai)
{
  
}

namespace {

class ElementDescription
{
  bool _valid;
public:
  ElementDescription(ELEMDESC& elmd, ClazzMethodArgInfo& mai)
    : _valid(true)
  {
    memset(&elmd, 0, sizeof(ELEMDESC));
    
    if (mai.type == ClazzInfo::getCharClazz()) {
      elmd.tdesc.vt = VT_UI1;
      static TYPEDESC td = { 0 };
      td.vt = VT_INT;
      elmd.tdesc.lptdesc = &td;
    } else if (mai.type == ClazzInfo::getByteClazz()) {
      elmd.tdesc.vt = VT_UI1;
    } else if (mai.type == ClazzInfo::getShortClazz()) {
      elmd.tdesc.vt = VT_I2;
    } else if (mai.type == ClazzInfo::getIntClazz()) {
      elmd.tdesc.vt = VT_I4;
    } else if (mai.type == ClazzInfo::getLongClazz()) {
      elmd.tdesc.vt = VT_I4; // I8 not exists
    } else if (mai.type == ClazzInfo::getFloatClazz()) {
      elmd.tdesc.vt = VT_R4;
    } else if (mai.type == ClazzInfo::getDoubleClazz()) {
      elmd.tdesc.vt = VT_R8;
    } else if (mai.type == ClazzInfo::getBoolClazz()) {
      elmd.tdesc.vt = VT_BOOL;
    } else if (mai.type == ClazzInfo::getVoidClazz()) {
      elmd.tdesc.vt = VT_NULL;
    } else if (mai.type == String::clazzInfo()) {
      elmd.tdesc.vt =  VT_BSTR;
    } else if (mai.type->isArray() == true) {
      Object::_throwNotImplementedYet("acdkx.com.arrays");
      _valid = false;
    } else {
      elmd.tdesc.vt = VT_INT;
      static TYPEDESC itd = { 0 };
      itd.vt = VT_INT;
      elmd.tdesc.lptdesc = &itd;
      //elmd.tdesc.vt =  VT_DISPATCH;
    } 
  }    
  bool isValid() const { return _valid; }
};

class FuncDescription
: public FUNCDESC
{
  bool _valid;
public:
  FuncDescription(ClazzInfo& clzinfo, ClazzMethodInfo& cmi)
    : _valid(false)
  {
    FUNCDESC* fd = this;
    memset(fd, 0, sizeof(FUNCDESC));
   
    set(clzinfo, cmi);
     _valid = true;
  }
  ~FuncDescription()
  {
    if (lprgelemdescParam != 0)
      delete[] lprgelemdescParam;
  }
  bool isValid() const { return _valid; }
  void setParamCount(int count)
  {
    cParams = count;
    //if (cParams > 0)
    lprgelemdescParam = new ELEMDESC[cParams];
    memset(lprgelemdescParam, 0, sizeof(ELEMDESC) * cParams);
  }
  bool set(ClazzInfo& clzinfo, ClazzMethodInfo& cmi)
  {
    invkind = INVOKE_FUNC;
    funckind = FUNC_DISPATCH;
    /*
    if (cmi.flags & Modifier::STATIC)
      funckind = FUNC_DISPATCH;
    else if (cmi.flags & Modifier::ABSTRACT)
      funckind = FUNC_DISPATCH;
      */
    int argcount = 0;
    int i;
    for (i = 0; cmi.methodArgs[i] != 0; ++i)
      ++argcount;
    if ( cmi.returnType != ClazzInfo::getVoidClazz())
      ++argcount;
    setParamCount(argcount);

    for (i = 0; cmi.methodArgs[i] != 0; ++i)
    {
      if (ElementDescription(lprgelemdescParam[i], *cmi.methodArgs[i]).isValid() == false)
        return false;
      
    }
    if ( cmi.returnType != ClazzInfo::getVoidClazz())
    {

      ClazzMethodArgInfo ret = { 0, 0, "", -1, "", 0, 0, cmi.returnType };
      if (ElementDescription(lprgelemdescParam[i], ret).isValid() == false)
        return false;
    }
    static TYPEDESC tdescUser = { 0 };
    elemdescFunc.tdesc.vt = VT_HRESULT;
    elemdescFunc.tdesc.lptdesc = &tdescUser;

    return true;
  }
  void registerFunction(int idx, ClazzMethodInfo& cmi, ICreateTypeInfo& cti)
  {
    memid = idx;
    // ### debug gest
    IID IID_ISum = {0x10000001,0x0000,0x0000,{0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x01}};
	  cti.SetGuid(IID_ISum);

    /*

  // Strukturen fuer die x, y und retval-Parameter der Sum-Methode
	TYPEDESC tdescParams = { 0 };
    tdescParams.vt = VT_INT;
	ELEMDESC myParams[3] = { 0 };
	myParams[0].tdesc.vt = VT_INT;
	myParams[0].tdesc.lptdesc = &tdescParams;
	myParams[1].tdesc.vt = VT_INT;
	myParams[1].tdesc.lptdesc = &tdescParams;
	myParams[2].tdesc.vt = VT_PTR;
	myParams[2].tdesc.lptdesc = &tdescParams;
	myParams[2].paramdesc.wParamFlags = PARAMFLAG_FRETVAL|PARAMFLAG_FOUT;

	// Zusaetzliche Daten, die die Sum-Methode und deren Rueckgabewert beschreiben
	TYPEDESC tdescUser = { 0 };
    FUNCDESC FuncDesc = { 0 };
	FuncDesc.funckind = FUNC_PUREVIRTUAL;
    FuncDesc.invkind = INVOKE_FUNC;
    FuncDesc.callconv = CC_STDCALL;

	// Parameter
	FuncDesc.cParams = 3;
	FuncDesc.lprgelemdescParam = myParams;

	// HRESULT Rueckgabewert
    FuncDesc.elemdescFunc.tdesc.vt = VT_HRESULT;
    FuncDesc.elemdescFunc.tdesc.lptdesc = &tdescUser;
    ACDK_CECKCOMCALL(cti.AddFuncDesc(idx, &FuncDesc));
    */
    funckind = FUNC_PUREVIRTUAL;
    //funckind = FUNC_DISPATCH;
    callconv = CC_STDCALL;
    ACDK_CECKCOMCALL(cti.AddFuncDesc(idx, this));

    const int MaxParams = 100;
    BSTR pnames[MaxParams];
    
    int _clength;
    int i = 0;
    pnames[i++] = CS2W(cmi.name);
    for (int j = 0; cmi.methodArgs[j] != 0; ++j)
    {
      pnames[i++] = CS2W(cmi.methodArgs[j]->name);
    }
    if ( cmi.returnType != ClazzInfo::getVoidClazz())
      pnames[++i] = CS2W("returnValue");
    //ACDK_CECKCOMCALL(
    cti.SetFuncAndParamNames(idx, pnames, i);
    
  }
};

} // anon namespaced


void 
CreateTypeInfo::createClass(IN(RClass) cls)
{
  if (cls->isInterface() == true) {
    _iface->SetTypeFlags(TYPEFLAG_FOLEAUTOMATION );
  } else  {
    _iface->SetTypeFlags(TYPEFLAG_FOLEAUTOMATION );
  }
  
  /*
    Set super (interfaces) here. If super are not registered, don't add or try to add
  */
  ClazzInfo* clzinfo = cls->clazzInfo();
  int ioffset = 0;
  for (int i = 0; clzinfo->methods[i]; ++i) 
  {
    FuncDescription fd(*clzinfo, *(clzinfo->methods[i]));
    if (fd.isValid() == true)
      fd.registerFunction(ioffset++, *(clzinfo->methods[i]), *_iface);
  }

}

} // namespace com 
} // namespace acdkx 




