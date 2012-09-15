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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/StdDispatch.cpp,v 1.98 2005/04/18 15:14:27 kommer Exp $



#include <acdk.h>
#include <acdk/lang/NoSuchDmiElementException.h>
#include <acdk/lang/ParamsMismatchException.h>
#include <acdk/lang/ClassNotFoundException.h>
#include <acdk/lang/NoSuchFieldException.h>
#include <acdk/lang/NoSuchMethodException.h>
#include <acdk/lang/Number.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/DmiTypeConversionException.h>
#include "DmiNamedArg.h"
#include "ClazzInfo.h"
#include "ClazzAttributesRes.h"

namespace acdk {
namespace lang {
namespace dmi {



//virtual 
DmiClient& 
StdDispatch::getDmiClient()
{
  return AcdkDmiClient::getDmiClient();
}

//inline 
::acdk::lang::Object* 
StdDispatch::getDmiTarget(const ::acdk::lang::dmi::ClazzInfo*& ci) 
{ 
  bool bret = false;
  ::acdk::lang::Object* o = (::acdk::lang::Object*)this;
  while ((o = o->getDmiTarget(bret, ci)) && bret == true)
    ;
  return o;
}

//static 
const ClazzMethodInfo* 
StdDispatch::_invoke_notexistant(  ::acdk::lang::Object* This, 
                                                          IN(acdk::lang::RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  THROW1(UnsupportedOperationException, "No DMI function available for call");
  return 0;
}

//static 
const ClazzMethodInfo* 
StdDispatch::_invoke_dynamic(  ::acdk::lang::Object* This, 
                                                          IN(acdk::lang::RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  if (clazzinfo == 0)
    clazzinfo = This->getClazzInfo();

  if ((flags & MiIvNoWeakBind) == 0 && This != 0) 
  {
    bool __forward = false;
    ::acdk::lang::Object* __forwardPtr = This->_getObjectPtr()->getDmiTarget(__forward, clazzinfo);
    if (__forward == true)
      return __forwardPtr->standardDispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  }
  DmiClient::setCurInvokeFlags(flags); // because on circumstance the flags are not deleted 
  if (methinf == 0 || flags & MiIvViaHash)
  {
    const ClazzInfo* clazzsic = clazzinfo;
    methinf =  ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags, methinf);
    flags &= ~MiIvViaHash;
  }
  return methinf->dispatch(This, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  //return clazzinfo->dynamic_dispatch(This, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
}

//static
const ClazzMethodInfo* 
StdDispatch::_invoke_dynamic_super(  ::acdk::lang::Object* This, 
                                                          IN(acdk::lang::RString) fname, 
                                                          ScriptVar& ret, 
                                                          ScriptVarArray& args, 
                                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  /*
  if (methinf == 0)
  {
    const ClazzInfo* clazzsic = clazzinfo;
    methinf =  ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags);

    if (clazzsic != clazzinfo)
      return clazzinfo->dynamic_dispatch(This, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  }

  int i;
  for (i = 0; i < clazzinfo->getInterfacesCount(); ++i)
  {
    const ClazzInfo* ci = clazzinfo->interfaces[i]->type;
    methinf = ci->dynamic_dispatch(This, fname, ret, args, dc, namedArgs, flags, ci, methinf);
    if (methinf != 0)
      break;
  }*/
  return methinf;
}

//static 
const ClazzMethodInfo* 
StdDispatch::_invoke_static(  IN(acdk::lang::RString) fname, 
                                          ScriptVar& ret, 
                                          ScriptVarArray& args, 
                                          DmiClient& dc,
                                          IN(::acdk::lang::RStringArray) namedArgs,
                                          int flags,
                                          const ClazzInfo* clazzinfo,
                                          const ClazzMethodInfo* methinf)
{
  const ClazzInfo* clazzSic = clazzinfo;
  if (methinf == 0 || flags & MiIvViaHash)
  {
    methinf =  ::acdk::lang::dmi::StdDispatch::lookupMethod(clazzinfo, fname, args, namedArgs, dc, flags, methinf);
    flags &= ~MiIvViaHash;
  }
  if (clazzSic->isArray() == true)
  {
    clazzinfo = clazzSic;
    ASCLITERAL(GetClass);
    if (fname->equals(lit_GetClass) == true)
    {
      ret = inOf(Class::getSingeltonClass(clazzinfo));
      return methinf;
    }
  }
  if (methinf->dispatch == 0)
    THROW1(NoSuchMethodException, "Method has no DMI implementation (probably abstract class): " + clazzinfo->toTypeString() + "::" + methinf->name);
  // ### check if methinf is this
  methinf = methinf->dispatch(0, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);
  return methinf;
}

//static 
const ClazzMethodInfo* 
StdDispatch::_invoke_static_super(  IN(acdk::lang::RString) fname, 
                                          ScriptVar& ret, 
                                          ScriptVarArray& args, 
                                          DmiClient& dc,
                                                          IN(::acdk::lang::RStringArray) namedArgs,
                                                          int flags,
                                                          const ClazzInfo* clazzinfo,
                                                          const ClazzMethodInfo* methinf)
{
  /*
  int i;
  for (i = 0; i < clazzinfo->getInterfacesCount(); ++i)
  {
    const ClazzInfo* ci = clazzinfo->interfaces[i]->type;
    methinf = ci->static_dispatch(fname, ret, args, dc, namedArgs, flags, ci, methinf);
    if (methinf != 0)
      break;
  }*/
  return methinf;
  
}


//virtual 
const ClazzMethodInfo* 
StdDispatch::standardDispatch(  IN(acdk::lang::RString) fname, ScriptVar& ret, 
                                ScriptVarArray& args, 
                                DmiClient& dc,
                                IN(::acdk::lang::RStringArray) namedArgs/* = Nil*/,
                                int flags,
                                const ClazzInfo* clazzinfo,
                                const ClazzMethodInfo* methinf/* = 0*/)
{
  if (clazzinfo == 0)
    clazzinfo = getClazzInfo();
  clazzinfo = clazzinfo->loadFullClazzInfo();
  DmiClient::setCurInvokeFlags(flags);
  return clazzinfo->dynamic_dispatch((Object*)this, fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);

  /* 
    not correct, because __dmi_proxy_target should not be static
    
  ::acdk::lang::dmi::ClazzInfo* ci = getClazzInfo();
  KeyValue<StringRes, ClazzAttributeResValue> kv = ClazzAttributesRes::getAttribute((MetaInfo*)ci, "__dmi_proxy_target");
  if (kv.value.data != 0)
  {
    RObject fw = (Object*)kv.value.data;
    return fw->standardDispatch(fname, ret, args, dc, namedArgs, flags, methinf);
  }
  */
  THROW1(UnsupportedOperationException, "StdDispatch::standardDispatch");
  return 0;
}

/**
    This version of StandardDispatch is used in the generated stub */
//static 
const ClazzMethodInfo* 
StdDispatch::StandardDispatch(IN(acdk::lang::RString) fname, 
                                                         ScriptVar& ret, 
                                                         ScriptVarArray& args, 
                                                         DmiClient& dc,
                                                         IN(::acdk::lang::RStringArray) namedArgs,
                                                         int flags,
                                                         const ClazzInfo* clazzinfo,
                                                         const ClazzMethodInfo* methinf)
{
  /*
  if (clazzinfo == 0)
    clazzinfo = getClazzInfo();
    */
  clazzinfo = clazzinfo->loadFullClazzInfo();
  return clazzinfo->static_dispatch(fname, ret, args, dc, namedArgs, flags, clazzinfo, methinf);

  THROW1(UnsupportedOperationException, "StdDispatch::standardDispatch: This method is reserved for the generated stub");
  return 0;
}


//static 
const ClazzMethodInfo* 
StdDispatch::StandardDispatch(  IN(acdk::lang::RString) classname, IN(acdk::lang::RString) fname
                             , ScriptVar& ret
                             , ScriptVarArray& args 
                             , DmiClient& dc
                             , IN(::acdk::lang::RStringArray) namedArgs
                             , int flags
                             , const ClazzInfo* clazzinfo
                             , const ClazzMethodInfo* methinf/* = 0*/)
{
  
  RClass cls;
  cls = Class::forName(&classname);// method with use cstr instead of RString
  const ClazzInfo* ci =  cls->objectClazzInfo();
  if (clazzinfo == 0)
    clazzinfo = ci;
  ci = ci->loadFullClazzInfo();
  if (ci->static_dispatch != 0/* && namedArgs == Nil*/)
    return ci->static_dispatch(fname, ret, args, dc, namedArgs, flags, ci, methinf);

  const ClazzMethodInfo* mi = lookupMethod(ci, fname, args, namedArgs, dc, flags, methinf);
  if (mi->dispatch != 0)
    return mi->dispatch(0, fname, ret, args, dc, namedArgs, flags, ci, mi);
  return _invoke_notexistant(0, fname, ret, args, dc, namedArgs, flags, ci, mi);
}

void throwFieldNotFount(IN(RString) msg, const ClazzInfo* ci, const ClazzFieldInfo* fi, int formatFlags)
{
  StringBuffer sb;
  sb.append(msg);
  sb.append(MetaInfo::flagsToString(MetaInfo::accessMask(fi->flags) | MetaInfo::staticMask(fi->flags), FieldInfoExtFlags(0)));
  sb.append(" ");
  ci->toTypeString(sb, formatFlags);
  sb.append("::");
  sb.append(fi->name);
  THROW1(NoSuchFieldException, sb.toString());
}

//static 
ScriptVar 
StdDispatch::_getMember(::acdk::lang::Object* This, const ClazzInfo* clazz, const ClazzFieldInfo* field, 
                        IN(acdk::lang::RString) fieldname, DmiClient& dc, int flags)
{
  if (field->accessor != 0)
  {
    ScriptVar erg;
    field->accessor(This, fieldname, erg, dc, flags | MiReadOnly, clazz, field);
    return erg;
  }
  if ((flags & MiIvNoThrowIfNotFound) == 0)
    throwFieldNotFount("Field has no accessor function", clazz, field, dc._formatFlags);
  return ScriptVar();
}

//static 
void 
StdDispatch::_setMember(::acdk::lang::Object* This, const ScriptVar& val, const ClazzInfo* clazz, const ClazzFieldInfo* field, 
                         IN(acdk::lang::RString) fieldname, DmiClient& dc, int flags)
{
  if (field->accessor != 0)
  {
    field->accessor(This, fieldname, const_cast<ScriptVar&>(val), dc, flags & ~MiReadOnly, clazz, field);
    return;
  }
  if ((flags & MiIvNoThrowIfNotFound) == 0)
    throwFieldNotFount("Field has no accessor function", clazz, field, dc._formatFlags);
}

//static 
dmi::ScriptVar 
StdDispatch::getStaticMember(const ClazzInfo* clazz, IN(acdk::lang::RString) fieldname, DmiClient& dc, int flags
                           , const ClazzInfo* type_requested)
{
  //int mflags = Modifier_accessMask(flags) | Modifier_accessMask(flags);
  if (clazz->flags & MiCiWeakBind && (flags & MiIvNoWeakBind) == 0)
  {
    ScriptVar ret;
    ScriptVarArray args(1);
    args[0] = &fieldname;
    clazz->static_dispatch("peek_static", ret, args, dc, Nil, flags, clazz, 0);
    return ret;
  }
  const ClazzFieldInfo* field = ClazzInfo::findField(clazz, fieldname, flags);
  ScriptVar sv = _getMember(0, clazz, field, fieldname, dc, flags);
  dc.castTo(sv, type_requested);
  return sv;
  //return ScriptVar_dynConvert(sv, type_requested);
}




dmi::ScriptVar 
StdDispatch::getMember(IN(RString) fieldname, DmiClient& dc, int flags, const ClazzInfo* type_requested)
{
  

  //Object* This = this; //dynamic_cast<ObjectBase*>(this)->_cast(Object::clazzInfo());
  bool forwarded = false;
  const ClazzInfo* clazz = 0;
  Object* This = this->getDmiTarget(forwarded, clazz);
  if (clazz == 0)
    clazz = This->getClazzInfo();

  const ClazzInfo* tclazz = clazz;
  clazz = clazz->loadFullClazzInfo();
  if ((clazz->flags & MiCiWeakBind) && (flags & MiIvNoWeakBind) == 0)
  {
    ScriptVar ret;
    ScriptVarArray args(1);
    args[0] = &fieldname;
    /*
    if (flags & MiPublic)
      mflags |= MiPublic;
      */
    This->standardDispatch("peek", ret, args, dc, Nil, flags, clazz, 0);
    return ret;
  }
  const ClazzFieldInfo* fi = ClazzInfo::findField(clazz, fieldname, flags);
  ScriptVar sv  = _getMember(This, clazz, fi, fieldname, dc, flags);

  if (type_requested == 0)
    return sv;
  if (dc.typeDistance(sv, type_requested) == -1)
  {
    StringBuffer sb("Cannot convert variable: ");
    sb.append(sv.toString());
    sb.append(" to type ");
    type_requested->toTypeString(sb, dc._formatFlags);
    sb.append(" in accessing ");
    fi->toTypeString(sb, dc._formatFlags);
    if ((flags & MiIvNoThrowIfNotFound) == 0)
      THROW1(DmiTypeConversionException, sb.toString());
    return ScriptVar();
  }
  dc.castTo(sv, type_requested);
  return sv;

}


  
void 
StdDispatch::setMember(IN(RString) fieldname, const ScriptVar& newval, DmiClient& dc, int flags)
{
  bool forwarded = false;
  //const ClazzInfo* clazz = This->getClazzInfo();
  Object* This = dmi_cast<Object>((Object*)this);
  const ClazzInfo* clazz = 0;
  This = This->getDmiTarget(forwarded, clazz);
  if (clazz == 0)
    clazz = This->getClazzInfo();
  clazz = clazz->loadFullClazzInfo();


  if (getClazzInfo()->flags & MiCiWeakBind)
  {
    ScriptVar ret;
    ScriptVarArray args(2);
    args[0] = &fieldname;
    args[1] = newval;
    /*
    if (flags & MiPublic)
      mflags |= MiPublic;
      */
    This->standardDispatch("poke", ret, args, dc, Nil, flags, getClazzInfo(), 0);
    return;

  }
  
  const ClazzFieldInfo* fi = ClazzInfo::findField(clazz, fieldname, flags);
  ScriptVar sv = newval;
  dc.castTo(sv, fi->type);
  _setMember(This, sv, clazz, fi, fieldname, dc, flags);
}

SysFields 
StdDispatch::getInternalFields(int flags, const ClazzInfo* clazz)
{
  // #### TODO handle basic/ObjectArray
  if (clazz == 0)
    clazz = getClazzInfo();
  clazz = clazz->loadFullClazzInfo(true, false);
  if (clazz == 0)
    return SysFields(0);
  ClazzFieldInfoVec fds;
  clazz->getFields(fds, flags);
  SysFields sf(fds.size());
  Object* This = dynamic_cast<Object*>(this);
  DmiClient& dc = AcdkDmiClient::getDmiClient();
  for (int i = 0; i < fds.size(); ++i)
  {
    ScriptVar sv;
    const ClazzFieldInfo* fi = fds[i];
    if (fi->accessor != 0)
    {
      fi->accessor(This, fi->name, sv, dc, MiReadOnly | MiAiOut,
                                   (const ClazzInfo*)fi->_scopeParent, fi);
      // ### TODO missing here an else? 
      sf[i] = SysField::getField(fi, sv);
    }
  }
  return sf;
}

SysField 
StdDispatch::getInternalField(IN(RString) name, int flags, const ClazzInfo* ci)
{
  if (ci == 0)
    ci = getClazzInfo();
  ci = ci->loadFullClazzInfo(true, false);
  if (ci != 0)
  {
    const ClazzFieldInfo* fi = ci->findField(ci, name, flags);
    if (fi != 0 && fi->accessor != 0)
    {
      DmiClient& dc = AcdkDmiClient::getDmiClient();
      ScriptVar sv;
      fi->accessor(dynamic_cast<Object*>(this), name, sv, dc, MiReadOnly | MiAiOut, ci, fi);
      return SysField::getField(fi, sv);
    }
  } 
  {
    SysFields sysfields = getInternalFields(flags);
    SysFields::iterator it = sysfields.begin();
    SysFields::iterator end = sysfields.end();
    for (; it != end; ++it)
    {
      if ((*it).fieldInfo->getMetaInfo()->equalsName(name) == true)
        return *it;
    }
    // ### THROW
    return SysField();
  }
}


/*
//static 
const ClazzFieldInfo*
StdDispatch::lookupMember( const ClazzInfo*& clazz, IN(RString) membername, int flags)
{
  
  if (clazz == 0)
  {
    Object* o = dynamic_cast<Object*>(this);
    clazz = o->getClazzInfo();
  }
  clazz = clazz->loadFullClazzInfo();
  for (int i = 0; i < clazz->getFieldsCount(); ++i)
  {
    if (membername.equals(clazz->fields[i]->name) == true && Modifier::checkMemberAccess(flags, clazz->fields[i]->flags)
      return clazz->fields[i];
  }
  if ((MiIvDeclared & flags) == 0)
  {
    if (flags & MiIvTransientCall)
      flags |= MiProtected;
    const ClazzInfo* tclazz = clazz;
    for (int i = 0; i < tclazz->getInterfacesCount(); i++) 
    {
      clazz = tclazz->interfaces[i]->type;
      const ClazzFieldInfo*  cfi = lookupMember(clazz, *fname, flags, vec);
      if (cfi != 0)
        return cfi;

    }
  }

  StringBuffer sb("Field not found: ");
  sb.append(Modifier::toString(Modifier::accessMask(flags) | Modifier::staticMask(flags)));
  sb.append(" ");
  clazz->toTypeString(sb, false);
  sb.append("::");
  sb.append(&membername);
  THROW1(NoSuchMethodException, sb.toString());
  return SysField();
}

//static 
const ClazzFieldInfo* 
StdDispatch::lookupStaticMember( const ClazzInfo*& clazz, IN(RString) membername, int flags)
{
  int i;
  if (clazz->fields != 0)
  {
    for (i = 0; clazz->fields[i] != 0; i++) 
    {
      const ClazzFieldInfo* cfi = clazz->fields[i];
      if (membername.equals(cfi->name) == true) 
      {
        if (cfi->address != 0 && cfi->address != (void*)-1) // otherwise doesn't work with DClazzInfo
        {
          if (Modifier::checkMemberAccess(flags, cfi->flags) == true)
          {
            return cfi;
          }
        }
      }
    }
  }
  const ClazzInfo* tclazz = clazz;
  if (tclazz->interfaces != 0 && (MiIvDeclared & flags) == 0)
  {
    if (flags & MiIvTransientCall)
      flags |= MiProtected;
    const ClazzFieldInfo* cfi;
    for (i = 0; tclazz->interfaces[i]; i++) 
    {
      clazz = tclazz->interfaces[i]->type;
      cfi = lookupStaticMember(clazz, membername, flags);
      if (cfi != 0)
        return cfi;
    }
  }
  clazz = tclazz;
  return 0;
}
*/

//static 
ScriptVar 
StdDispatch::getStaticMember(const ClazzInfo* clazz, const ClazzFieldInfo* field, int flags)
{
  if (field->accessor != 0)
  {
    return _getMember(0, clazz, field, field->name, AcdkDmiClient::getDmiClient(), MiStatic);
  }

  void* address = field->address;
  const ClazzInfo* clazzInfo = field->type;

  if (clazzInfo == ClazzInfo::getBoolClazz())
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((bool*)address));
      else
        return outOf(*((bool*)address));
    }
    else
      return inOf(*((bool*)address));
  }
  if (clazzInfo == ClazzInfo::getCharClazz())
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((char*)address));
      else
        return outOf(*((char*)address));
    }
    else
      return inOf(*((char*)address));
  }
  if (clazzInfo == ClazzInfo::getByteClazz())
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((byte*)address));
      else
        return outOf(*((byte*)address));
    }
    else
      return inOf(*((byte*)address));
  }
  if (clazzInfo == ClazzInfo::getShortClazz())
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((short*)address));
      else
        return outOf(*((short*)address));
    }
    else
      return inOf(*((short*)address));
  }
  if (clazzInfo == ClazzInfo::getIntClazz())
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((int*)address));
      else
        return outOf(*((int*)address));
    }
    else
      return inOf(*((int*)address));
  }
  if (clazzInfo == ClazzInfo::getLongClazz())
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((jlong*)address));
      else
        return outOf(*((jlong*)address));
    }
    else
      return inOf(*((jlong*)address));
  }
  if (clazzInfo == ClazzInfo::getFloatClazz())
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((float*)address));
      else
        return outOf(*((float*)address));
    }
    else
      return inOf(*((float*)address));
  }
  if (clazzInfo == ClazzInfo::getDoubleClazz())
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((double*)address));
      else
        return outOf(*((double*)address));
    }
    else
      return inOf(*((double*)address));
  }
  // otherwise
  {
    if (flags & MiAiOut)
    {
      if (flags & MiAiIn)
        return inoutOf(*((RObject*)address));
      else
        return outOf(*((RObject*)address));
    }
    else
      return inOf(*((RObject*)address));
  }
  
}

//static 
void 
StdDispatch::setStaticMember(const ClazzInfo* clazz, IN(RString) fieldname, const ScriptVar& newval, DmiClient& dc, int flags)
{
  clazz = clazz->loadFullClazzInfo();
  if (clazz->flags & MiCiWeakBind)
  {
    ScriptVar ret;
    ScriptVarArray args(2);
    args[0] = &fieldname;
    args[1] = newval;
    clazz->static_dispatch("poke_static", ret, args, dc, Nil, flags, clazz, 0);
    return;
  }
  const ClazzFieldInfo* fi = ClazzInfo::findField(clazz, fieldname, flags | MiStatic);
  ScriptVar sv = newval;
  dc.castTo(sv, fi->type);
  _setMember(0, sv, clazz, fi, fieldname, dc, flags);
}

//static 
void 
StdDispatch::setStaticMember(const ClazzInfo* clazz, const ClazzFieldInfo* field, const ScriptVar& value,  DmiClient& dc, int flags)
{
  if (flags & MiCiWeakBind)
  {
    setStaticMember(clazz, ACDK_STACK_STR(field->name), value, dc, flags);
    return;
  }
  // ### todo check flags for protecttion
  if (field->accessor != 0)
  {
    _setMember(0, value, clazz, field, field->name, AcdkDmiClient::getDmiClient(), MiStatic);
    return;
  }
  const ClazzInfo* clazzInfo = field->type;
  void* address = field->address;
  if (clazzInfo == ClazzInfo::getBoolClazz()) {
    *((bool*)address) = value.getBoolVar();
    return;
  }
  if (clazzInfo == ClazzInfo::getCharClazz()) {
    *((char*)address) = value.getCharVar();
    return;
  }
  if (clazzInfo == ClazzInfo::getByteClazz()) {
    *((byte*)address) = value.getByteVar();
    return;
  }
  if (clazzInfo == ClazzInfo::getShortClazz()) {
    *((short*)address) = value.getShortVar();
    return;
  }
  if (clazzInfo == ClazzInfo::getIntClazz()) {
    *((int*)address) = value.getIntVar();
    return;
  }
  if (clazzInfo == ClazzInfo::getLongClazz()) {
    *((jlong*)address) = value.getLongVar();
    return;
  }
  if (clazzInfo == ClazzInfo::getFloatClazz()) {
    *((float*)address) = value.getFloatVar();
    return;
  }
  if (clazzInfo == ClazzInfo::getDoubleClazz()) {
    *((double*)address) = value.getDoubleVar();
    return;
  }
  *((RObject*)address) = value.getObjectVar();
}

//static
void 
StdDispatch::poke_static(IN(RString) clsname, IN(RString) member, IN(ScriptVar) val, int flags) 
{
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    THROW1(ClassNotFoundException, "Class cannot be found: " + clsname->getName());
  StdDispatch::setStaticMember(cls->objectClazzInfo(), member, val, AcdkDmiClient::getDmiClient(), flags);
}

//static 
ScriptVar 
StdDispatch::peek_static(IN(RString) clsname, IN(RString) member, int flags) 
{
  RClass cls = Class::forName(clsname);
  if (cls == Nil)
    THROW1(ClassNotFoundException, "Class cannot be found: " + clsname->getName());
  return StdDispatch::getStaticMember(cls->objectClazzInfo(), member, AcdkDmiClient::getDmiClient(), flags) ;
}


ScriptVar 
StdDispatch::New(IN(RString) classname, ScriptVarArray& args, DmiClient& dc)
{
  return New(classname, args, Nil, dc);
}

ScriptVar 
StdDispatch::New(IN(RString) classname, ScriptVarArray& args, IN(RStringArray) nargs, DmiClient& dc)
{
  RString constructorname = classname->replace('.', '/');

  if (constructorname->indexOf('/') != -1)
    constructorname = constructorname->substr(constructorname->lastIndexOf('/') + 1);
  ScriptVar ret;
  StandardDispatch(classname, constructorname
                             , ret
                             , args 
                             , dc
                             , nargs
                             , MiPublic | MiIvConstructor //| MiStatic 
                             , 0);
  return ret;
}

//static 
ScriptVar 
StdDispatch::New(IN(RString) classname, ScriptVarArray& args, IN(NamedArgs) nargs, DmiClient& dc)
{
  StringArray namedArgs(nargs.size());
  args.reserve(args.size() + nargs.size());
  for (int i = 0; i < nargs.size(); ++i)
  {
    namedArgs[i] = new String(nargs[i]._name);
    args.push_back(*nargs[i]._val);
  }
   RString constructorname = classname->replace('.', '/');
  if (constructorname->indexOf('/') != -1)
    constructorname = constructorname->substr(constructorname->lastIndexOf('/') + 1);
  ScriptVar ret;
  StandardDispatch(classname, constructorname
                             , ret
                             , args 
                             , dc
                             , &namedArgs
                             , MiPublic | MiMiConstructor //| MiStatic 
                             , 0);
  return ret;
}


//foreign static 
ScriptVar 
StdDispatch::invokeStaticMethod(IN(RString) classname, IN(RString) funcname , ScriptVarArray& args, int flags)
{
  ScriptVar ret;
  StandardDispatch(classname, funcname, ret, args, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), Nil, 
                   flags, 0);
  return ret;
}

//foreign static 
ScriptVar 
StdDispatch::invokeStaticMethod( IN(RString) classname, IN(RString) funcname
                                                           , ScriptVarArray& args
                                                           , IN(NamedArgs) nargs
                                                           , int flags
                                                           )
{
  StringArray namedArgs(nargs.size());
  args.reserve(args.size() + nargs.size());
  for (int i = 0; i < nargs.size(); ++i)
  {
    namedArgs[i] = new String(nargs[i]._name);
    args.push_back(*nargs[i]._val);
  }
  return invokeStaticMethod(classname, funcname, args, ::acdk::lang::dmi::AcdkDmiClient::getDmiClient(), &namedArgs, flags);
}


//foreign static 
ScriptVar 
StdDispatch::invokeStaticMethod( IN(RString) classname, IN(RString) funcname
                          , ScriptVarArray& args
                          , DmiClient& dc
                          , IN(::acdk::lang::RStringArray) namedArgs// = Nil
                          , int flags
                          )
{
  ScriptVar ret;
  StandardDispatch(classname, funcname, ret, args, dc, namedArgs, flags, 0, 0);
  return ret;
}
  

//foreign 
ScriptVar 
StdDispatch::invokeMethod(IN(RString) funcname, ScriptVarArray& args, int flags)
{
  ScriptVar erg;
  standardDispatch(funcname, erg, args, getDmiClient(), Nil, flags, getClazzInfo(), 0);
  return erg;
}

//foreign 
ScriptVar 
StdDispatch::invokeMethod(IN(RString) funcname, ScriptVarArray& args, IN(NamedArgs) nargs, int flags)
{
  StringArray namedArgs(nargs.size());
  args.reserve(args.size() + nargs.size());
  for (int i = 0; i < nargs.size(); ++i)
  {
    namedArgs[i] = new String(nargs[i]._name);
    args.push_back(*nargs[i]._val);
  }
  return invokeMethod(funcname, args, &namedArgs, flags);
}



//foreign 
ScriptVar 
StdDispatch::invokeMethod(IN(RString) funcname, ScriptVarArray& args
                                              , DmiClient& dc
                                              , IN(::acdk::lang::RStringArray) namedArgs  
                                              , int flags
                                              )
{
  ScriptVar erg;
  standardDispatch(funcname, erg, args, dc, namedArgs, flags, getClazzInfo(), 0);
  return erg;
}

//foreign 
ScriptVar 
StdDispatch::invokeMethod(  IN(RString) funcname, ScriptVarArray& args
                          , IN(::acdk::lang::RStringArray) namedArgs, int flags)
{
  ScriptVar erg;
  standardDispatch(funcname, erg, args, getDmiClient(), namedArgs, flags, getClazzInfo(), 0);
  return erg;
}

//foreign 
ScriptVar 
StdDispatch::peek(IN(RString) fieldname, int flags)
{
  return getMember(fieldname, getDmiClient(), flags, 0);
}

//foreign
void 
StdDispatch::poke(IN(RString) fieldname, const ScriptVar& arg, int flags)
{
  setMember(fieldname,  arg, getDmiClient(), flags);
}

inline ::acdk::lang::RString isOperator(IN(RString) fname)
{
  RString strOperator = ACDK_STACK_STR("operator");
  if (fname->startsWith(strOperator) == true)
  {
    if (acdk::lang::Character::isJavaIdentifierPart(fname->charAt(strOperator->length())) == false)
      return fname->substr(strOperator->length());
  }
  return Nil;
}

class FunctionDescriptor
{
public:
  int getParamCount() const { return 0; }
  const ClazzInfo* getParamType(int i) const { return 0; }
  int getParamFlags(int i) const { return 0; }
  bool equalsParamName(int i, IN(RString) pname) const { return false; }
  bool hasDefaultValue(int i) const { return false; }
  ScriptVar getDefaultValue(int i) const { return ScriptVar(); }
  
};

class ClazzMethodInfoDescriptor
: public FunctionDescriptor
{
public:
  const ClazzMethodInfo* _mi;
  ClazzMethodInfoDescriptor(const ClazzMethodInfo* mi) : _mi(mi) {}
  int getParamCount() const { return _mi->getArgumentCount(); }
  const ClazzInfo* getParamType(int i) const { return _mi->methodArgs[i]->type; }
  int getParamFlags(int i) const { return _mi->methodArgs[i]->flags; }
  bool equalsParamName(int i, IN(RString) pname) const
  { 
    return _mi->methodArgs[i]->equalsName(pname);
  }
  bool hasDefaultValue(int i) const
  {
    return _mi->methodArgs[i]->getDefaultArgValueFunc != 0;
  }
  ScriptVar getDefaultValue(int i) const 
  { 
    return _mi->methodArgs[i]->getDefaultArgValueFunc(_mi->methodArgs[i]); 
  }
};

class NamedArgDescription
{
protected:
  RStringArray namedargs;
public:
  NamedArgDescription(IN(RStringArray) n) : namedargs(n) {}
  bool hasNamed() const { return namedargs != Nil && namedargs->length() > 0; }
  int getNamedCount() const 
  {
    return namedargs == Nil ? 0 :  namedargs->length();
  }
  RString getNameArgName(int argCount, int idx) 
  {
    /*
    a, b, c: C, d: D
    idx = 2 (c)
    argCount = 4;
    namedArg = 2
    realidx = 0
    
    idx =  (c)
    argCount = 4;
    namedArg = 2
    realidx = 0

    */
    int realIdx = idx - (argCount - namedargs->length());
    return namedargs[realIdx];
  }
};

class ScriptVarArgDescription
: public NamedArgDescription
{
  ScriptVarArray& args;
public:
  typedef ScriptVar ElementType;
  typedef ScriptVarArray ElementContainer;
  ScriptVarArgDescription(ScriptVarArray& a, IN(RStringArray) nargs) : NamedArgDescription(nargs), args(a) {}
  int getCount() const { return args.size(); }
  bool hasValues() const { return true; }
  ScriptVar& getValue(int idx) { return args[idx]; }
  const ClazzInfo* getType(int idx) const { return args[idx].getValueClazzInfo(); }
  
  int getUnnamedCount() const 
  {
    if (hasNamed() == false)
      return getCount();
    return getCount() - namedargs->length();
  }
  
  RString getName(int idx) 
  { 
    return getNameArgName(getCount(), idx);
  }
  const ElementContainer& elementContainer() const { return args; }
  const ElementType& get(int idx) const { return args[idx]; }
  void set(int idx, const ElementType& et) { args[idx] = et; }
  void resetElement(int idx) { args[idx].reset(); }
  void resize(int newSize) { args.resize(newSize); }
  void setValue(int idx, const ScriptVar& sv) { args[idx] = sv; }
};

class MethodArgInfoArgDesc
: public NamedArgDescription
{
  ::acdk::lang::sys::core_vector<ClazzMethodArgInfo>& args;
public:
  typedef ClazzMethodArgInfo ElementType;
  typedef ::acdk::lang::sys::core_vector<ClazzMethodArgInfo> ElementContainer;
  MethodArgInfoArgDesc(::acdk::lang::sys::core_vector<ClazzMethodArgInfo>& a, IN(RStringArray) nargs) 
    : NamedArgDescription(nargs)
    , args(a)
  {}
  int getCount() const { return args.size(); }
  bool hasValues() const { return false; }
  ScriptVar& getValue(int idx) { static ScriptVar voidSv; return voidSv; }
  const ClazzInfo* getType(int idx) const { return args[idx].type; }
  bool hasNamed() const { return namedargs != Nil && namedargs->length() > 0; }
  int getUnnamedCount() const 
  {
    if (hasNamed() == false)
      return getCount();
    return getCount() - namedargs->length();
  }
  int getNamedCount() const 
  {
    return namedargs == Nil ? 0 :  namedargs->length();
  }
  RString getName(int idx) 
  { 
     return getNameArgName(getCount(), idx);
  }
  const ElementContainer& elementContainer() const { return args; }
  
  const ElementType& get(int idx) const { return args[idx]; }
  void set(int idx, const ElementType& et) { args[idx] = et; }
  void resetElement(int idx) { }
  void resize(int newSize) { args.resize(newSize); }
  // not supported, hasValues() == false
  void setValue(int idx, const ScriptVar& sv) {  }
};

class MethodArgInfoPtrArgDesc
: public NamedArgDescription
{
  ClazzMethodArgInfo** args;
  int count;
public:
  typedef ClazzMethodArgInfo ElementType;
  typedef ClazzMethodArgInfo** ElementContainer;
  MethodArgInfoPtrArgDesc(acdk::lang::dmi::ClazzMethodArgInfo** a, IN(RStringArray) nargs) 
  : NamedArgDescription(nargs)
  , args(a)
  , count(0)
  {
    if (args != 0)
    {
      for (int i = 0; args[i] != 0; ++i)
        ++count;
    }
  }
  int getCount() const { return count; }
  bool hasValues() const { return false; }
  ScriptVar& getValue(int idx) { static ScriptVar voidSv; return voidSv; }
  const ClazzInfo* getType(int idx) const { return args[idx]->type; }
  bool hasNamed() const { return namedargs != Nil && namedargs->length() > 0; }
  int getUnnamedCount() const 
  {
    if (hasNamed() == false)
      return getCount();
    return getCount() - namedargs->length();
  }
  int getNamedCount() const 
  {
    return namedargs == Nil ? 0 :  namedargs->length();
  }
  RString getName(int idx) 
  { 
     return getNameArgName(getCount(), idx);
  }
  const ElementContainer& elementContainer() const { return args; }
  
  const ElementType& get(int idx) const { return *args[idx]; }
  void set(int idx, const ElementType& et) { }
  void resetElement(int idx) { }
  void resize(int newSize) { }
  // not supported, hasValues() == false
  void setValue(int idx, const ScriptVar& sv) {  }
};


class FunctionSignatureArgDesc
: public NamedArgDescription
{
  FunctionSignature& args;
  
public:
  typedef const ClazzInfo* ElementType;
  typedef const ClazzInfo** ElementContainer;

  FunctionSignatureArgDesc(FunctionSignature& a, IN(RStringArray) nargs) 
  : NamedArgDescription(nargs)
  , args(a)
  {
  }
  int getCount() const { return args.size; }
  bool hasValues() const { return false; }
  ScriptVar& getValue(int idx) { static ScriptVar voidSv; return voidSv; }
  const ClazzInfo* getType(int idx) const { return args.args[idx]; }
  bool hasNamed() const { return namedargs != Nil && namedargs->length() > 0; }
  int getUnnamedCount() const 
  {
    if (hasNamed() == false)
      return getCount();
    return getCount() - namedargs->length();
  }
  int getNamedCount() const 
  {
    return namedargs == Nil ? 0 :  namedargs->length();
  }
  RString getName(int idx) 
  { 
     return getNameArgName(getCount(), idx);
  }
  ElementContainer& elementContainer() { return args.args; }
  
  ElementType& get(int idx) { return args.args[idx]; }
  void set(int idx, const ElementType& et) { }
  void resetElement(int idx) { }
  void resize(int newSize) { }
  // not supported, hasValues() == false
  void setValue(int idx, const ScriptVar& sv) {  }
};


                                             

template <class CMD, class ARGS>
bool
hasRestParam(const CMD& mi, ARGS& args)
{
  bool hasRest = (mi.getParamCount() > 0) &&
      ClazzInfo::isRestArg(mi.getParamType(mi.getParamCount() - 1));
  if (hasRest == false)
    return false;
  int argCount = args.getCount();
  if (argCount == 0)
    return true;
  int paramCount = mi.getParamCount();
  if (args.getType(argCount - 1) == DmiObjectArray::clazzInfo() &&
      argCount == paramCount)
    return false;
  return true;
}

template <class CMD, class ARGS>
bool
hasNamedRestParam(const CMD& mi, ARGS& args)
{
  bool hasRest = (mi.getParamCount() > 0) &&
                  ClazzInfo::isNamedRestArg(mi.getParamType(mi.getParamCount() - 1));
  if (hasRest == false)
    return false;
  int argCount = args.getCount();
  if (argCount == 0)
    return true;
  if (args.getType(argCount - 1) == DmiNamedArgArray::clazzInfo())
    return false;
  return true;
}

//static
void
StdDispatch::findFunctions(const ClazzInfo* clazz, IN(RString) funcname, int flags, ClazzMethodInfoVec& vec)
{
  if (clazz == 0)
    return;
  clazz = clazz->loadFullClazzInfo();
  int i;
  RString opmethod;
  RString opChar;
  RString fname = funcname;
  if ((opChar = isOperator(funcname)) != Nil)
  {
    opmethod = acdk::lang::reflect::Method::encodeOperatorToFuncName(opChar);
    fname = opmethod;
  }
  if (clazz->methods != 0)
  {
    
    for (i = 0; clazz->methods[i]; i++) 
    {
      const ClazzMethodInfo* cmi = clazz->methods[i]; 
      if (MetaInfo::checkMemberAccess(flags, cmi->flags) == false)
        continue;
      // ## TODO check MiIvViaAltName flag
      if (cmi->equalsName(fname) == false && cmi->equalsAltName(fname) == false)
        continue;
      vec.push_back(cmi);
    }
  }
  if (clazz->interfaces != 0 && (MiIvDeclared & flags) == 0 && (MiMiConstructor & flags) == 0)
  {
    if (flags & MiIvTransientCall)
      flags |= MiProtected;
    for (i = 0; clazz->interfaces[i]; i++) 
    {
      findFunctions(clazz->interfaces[i]->type, fname, flags, vec);
    }
  }
}



void asSignature(StringBuffer& sb, ScriptVarArray& args)
{
  for (int i = 0; i < args.size(); ++i)
  {
    if (i > 0)
      sb.append(", ");
    sb.append(args[i].getTypeInfo());
  }
}


/*
bool
MethodInfo_hasRestParam(const ClazzMethodInfo* mi)
{
  return (mi->getArgumentCount() > 0 &&
      ClazzInfo::isRestArg(mi->methodArgs[mi->getArgumentCount() - 1]->type));
}

bool
MethodInfo_hasNamedRestParam(const ClazzMethodInfo* mi)
{
  return (mi->getArgumentCount() > 0 &&
          ClazzInfo::isNamedRestArg(mi->methodArgs[mi->getArgumentCount() - 1]->type));
}*/


//static 
const ClazzMethodInfo* 
StdDispatch::_lookupMethod(const ClazzInfo*& clazz, IN(RString) fname, 
                            ScriptVarArray& args, DmiClient& dc, int flags, const ClazzMethodInfo* methinf)
{
  return _lookupMethod(clazz, fname, args, Nil, dc, flags, methinf);
  /*
   dead method
  const ClazzInfo* tclazz = clazz;
  if (flags & MiIvViaHash)
  {
    int methhash = ACDK_CAST_PTR2INT(methinf);
    const ClazzMethodInfo* cmi = lookupMethod(clazz, methhash, flags);
    if (cmi == 0 && (flags & MiIvNoThrowIfNotFound) == 0)
      ClazzMethodInfo::throwMethodNotFound(tclazz, Integer::toString(methhash), flags, dc._formatFlags);
    return cmi;
  }

  if (fname->length() == 0 || clazz == 0)
  {
    if ((flags & MiIvNoThrowIfNotFound) == 0)
      THROW1(NoSuchDmiElementException, "Class or method not not defined");
    return 0;
  }
 
  // ## TODO check MiIvViaAltName
  RString rfname = fname;
  if ((flags & MiIvConstructor) == MiIvConstructor && clazz->isArray() == true)
  {
    const ClazzInfo* eltype = clazz->userInfo; //clazz->getArrayElementType();
    if (rfname->startsWith(eltype->name) == true)
    {
      if (eltype->isBasicClazz() == true)
        rfname = "ObjectArrayBaseImpl"; // ###typo??
      else
        rfname = "ObjectArrayBaseImpl";
    }
    else if (*eltype->name == '[')
    {
      rfname = "ObjectArrayBaseImpl";
    }
  }
  ClazzMethodInfoVec methods;
  findFunctions(clazz, rfname, flags, methods);
  
  if (methods.size() == 0)
  {
    if ((flags & MiIvNoThrowIfNotFound) == 0)
      ClazzMethodInfo::throwMethodNotFound(tclazz, &fname, flags, dc._formatFlags);
    return 0;
  }
    
  int lastmatch = 0;
  //ClazzMethodInfoVec lastBestMatch;
  const ClazzMethodInfo* lastBestMatch = 0;
  ClazzMethodInfoVec::iterator it = methods.begin();
  ClazzMethodInfoVec::iterator end = methods.end();
  for (; it != end; ++it)
  {
    const ClazzMethodInfo* cmi = *it;
    int mcount = cmi->getArgumentCount();
    bool hasRestParam = MethodInfo_hasRestParam(cmi);
    bool hasNamedRestParam = MethodInfo_hasNamedRestParam(cmi);
    
    if (mcount != args.size()) 
    {
      if (hasRestParam == false && hasNamedRestParam == false)
        continue;
    }
    bool canasign = true;
    int total = 0;
    bool wasViaNamedRest = false;
    for (int j = 0; j < mcount; j++) 
    {
      if (j == mcount - 1 && (hasRestParam == true || hasNamedRestParam == true))
      {
        wasViaNamedRest = true;
        break;
      }

      int match;
      if (cmi->methodArgs[j]->flags & MiAiOut)
        match = dc.typeDistance(cmi->methodArgs[j]->type, args[j].getClazzInfo());
      else
        match = dc.typeDistance(args[j], cmi->methodArgs[j]->type);
      if (match == -1) 
      {
        canasign = false;
        break;
      }
	    total += match;
    }
    if (canasign == true) 
    {
      if (total == 0)
      {
        if (wasViaNamedRest == false)
        {
          clazz = reinterpret_cast<const ClazzInfo*>(cmi->_scopeParent); 
          return cmi;
        }
        else
          total = 1024;
      }
      if ((total < lastmatch) || (lastBestMatch  == 0)) {
        lastmatch = total;
        lastBestMatch = *it;
      }
    }
  }
  if (lastBestMatch != 0) 
  {
    clazz = reinterpret_cast<const ClazzInfo*>(lastBestMatch->_scopeParent); 
    return lastBestMatch;
  }
  StringBuffer sb(1024);
  sb.append("No matching function found: ");
  clazz->toTypeString(sb, dc._formatFlags);
  if (dc._formatFlags & TpFtAcdkType)
    sb.append("::");
  else
    sb.append(".");
  sb.append(fname);
  sb.append("(");
  asSignature(sb, args);
  sb.append(")\n Following functions are defined:\n");
  for (int i = 0; i < methods.size(); ++i)
  {
    sb.append("\t");
    const ClazzInfo* parent = (const ClazzInfo*)methods[i]->_scopeParent;
    if (clazz->isArray() == true)
      parent = clazz;
    methods[i]->toTypeString(sb, reinterpret_cast<const ClazzInfo*>(parent), dc._formatFlags);
    sb.append("\n");
  }
  if ((flags & MiIvNoThrowIfNotFound) == 0)
    THROW3(ParamsMismatchException, sb.toString(), clazz, methods);  
  return 0;
  */
}

int matchParameter(const ClazzInfo* paramType, int paramFlag, ScriptVar& sv, DmiClient& dc)
{
  if (paramFlag & MiAiOut)
    return dc.typeDistance(paramType, sv.getClazzInfo());
  else
    return dc.typeDistance(sv, paramType);
}


int matchParameter(const ClazzInfo* paramType, int paramFlag, const ClazzInfo* argType, DmiClient& dc)
{
  if (paramFlag & MiAiOut)
    return dc.typeDistance(paramType, argType);
  else
    return dc.typeDistance(argType, paramType);
}

template <class CMD, class ARGS>
int
matchParameters(const ClazzInfo* clazz, const CMD& cmi, ARGS& args, int argCount
                                              , DmiClient& dc, int flags)

{
  int matchSum = 0;
  for (int i = 0; i < argCount; ++i)
  {
    int match = -1;
    if (args.hasValues() == true)
      match = matchParameter(cmi.getParamType(i), cmi.getParamFlags(i), args.getValue(i), dc);
    else
      match = matchParameter(cmi.getParamType(i), cmi.getParamFlags(i), args.getType(i), dc);
    if (match == -1)
      return -1;
    matchSum += match;
  }
  return matchSum;
}


/**
  return weight of this matching method
  does not check if method has 
*/
template <class CMD, class ARGS>
int
matchSimpleMethod(const ClazzInfo* clazz, const CMD& cmi, ARGS& args
                                              , DmiClient& dc
                                              , int flags
                                              , OUT(RbyteArray) order
                                              )
{
  bool hasRest = hasRestParam(cmi, args);

  bool hasNamedRest = hasNamedRestParam(cmi, args);
  int argCount = args.getCount();
  int paramCount = cmi.getParamCount();
 
  if (hasNamedRest == true)
  {
    --paramCount;
  }
  if (argCount != paramCount && hasRest == false)
    return -1;
  if (hasRest == false)
    return matchParameters(clazz, cmi, args, argCount, dc, flags);

  int c = matchParameters(clazz, cmi, args, paramCount - 1, dc, flags);
  if (c == -1)
    return -1;
  c += (argCount - (paramCount - 1)) * 1024;
  order = new byteArray(argCount);
  int i;
  for (i = 0; i < paramCount - 1; ++i)
    order[i] = i;
  for (i = paramCount - 1; i < argCount; ++i)
    order[i] = -1;
  return c;
}




template <class CMD, class ARGS>
int 
matchNamedArgsMethod(const ClazzInfo* clazz, const CMD& cmi, ARGS& args
                                              , DmiClient& dc
                                              , int flags, OUT(RbyteArray) order)
{
  bool hasNamedRest = hasNamedRestParam(cmi, args);
  int argCount = args.getCount();
  int paramCount = cmi.getParamCount();

  int namedArgCount = args.getNamedCount();
  int unnamedArgCount = args.getUnnamedCount();
  int mc = matchParameters(clazz, cmi, args, unnamedArgCount, dc, flags);
  if (mc == -1)
    return -1;
  
  if (argCount != paramCount && hasNamedRest == false)
    return -1;
  
  order = new byteArray(argCount, 0xfe);
  int i;
  
  for (i = 0; i < unnamedArgCount; ++i)
    order[i] = i;

  for (i = unnamedArgCount; i < argCount; ++i)
  {
    int namedIdx = i - unnamedArgCount;
    bool foundNamed = false;
    RString narg = args.getName(i);
    for (int j = unnamedArgCount; j < paramCount; ++j)
    {
      if (cmi.equalsParamName(j, narg) == true)
      {
        int m = -1;
        if (args.hasValues() == true)
          m = matchParameter(cmi.getParamType(j), cmi.getParamFlags(j), args.getValue(i), dc);
        else
          m = matchParameter(cmi.getParamType(j), cmi.getParamFlags(j), args.getType(i), dc);
        if (m == -1)
          return -1;
        order[i] = j;
        mc += m;
        foundNamed = true;
        break;
      }
    }
    if (foundNamed == false)
    {
      if (hasNamedRest == false)
        return -1;
      break;
    }
  }
  /** try to match default parameter doesn't work currently
  for (i = 0; i < paramCount; ++i)
  {
    int fidx = order->findFirst(byte(i));
    if (fidx == -1)
    {
      if (cmi.hasDefaultValue(i) == false)
        return -1;
    }
  }
  */
  mc += (argCount - (i - 1)) * 1024;
  for (; i < argCount; ++i)
    order[i] = -1;
  return mc;
}

template <class CMD, class ARGS>
int 
matchMethod(const ClazzInfo* clazz, const CMD& cmi, ARGS& args
                                              , DmiClient& dc
                                              , int flags
                                              , OUT(RbyteArray) order)
{
  if (args.hasNamed() == false)
  {
    return matchSimpleMethod(clazz, cmi, args, dc, flags, order);
  }
  return matchNamedArgsMethod(clazz, cmi, args, dc, flags, order);
}


template <class CMD, class ARGS>
void
mapArguments(const ClazzInfo*& clazz, const CMD& cmd, ARGS& args, IN(RbyteArray) lastnewargpos)
{
  bool hasNamedRest = hasNamedRestParam(cmd, args);
  bool hasRest = hasRestParam(cmd, args);
  bool restAdded = false;
  int argsCount = args.getCount();

  if (lastnewargpos != Nil)
  {
    typename ARGS::ElementContainer argssic = args.elementContainer();
    int i;
    
    for (i = 0; i < argsCount; ++i)
    {
      int newpos = lastnewargpos[i];
      if (newpos == 0xff)
        break;
      else if (newpos == 0xfe)
      {
        /*### not working if (lastBestMatch->methodArgs[i]->getDefaultArgValueFunc != 0)
          args[i] = lastBestMatch->methodArgs[i]->getDefaultArgValueFunc(lastBestMatch->methodArgs[i]);
        else
          ; //Ooops
          */
      }
      else if (newpos != i)
      {
        args.resetElement(newpos);
        args.set(newpos, argssic[i]);
      }
    }
    if (i != argsCount)
    {
      int fixed = i;
      int calcUnmappedsize = 0;
      for (int ti = lastnewargpos->length() - 1; ti >= 0; --ti)
      {
        if (lastnewargpos[ti] != 0xff)
          break;
        ++calcUnmappedsize;
      }
      if (args.hasValues() == true)
      {

      if (hasNamedRest = true && args.hasNamed() == true)
      {
        int unmapped = argsCount - fixed;
        RDmiNamedArgArray rest = new DmiNamedArgArray(unmapped);
        int mc = 0;
        for (int naidx = 0; i < argsCount; ++i, ++naidx)
        {
          RString narg = args.getName(i);
          rest[naidx] = new DmiNamedArg(narg, new DmiObject(args.getValue(i)));
        }
        args.resize(fixed + 1);
        args.setValue(fixed, inOf(rest));
        restAdded = true;
        
      }
      else if (hasRest == true)
      {
        int unmapped = argsCount - fixed;
        RDmiObjectArray rest = new DmiObjectArray(unmapped);
        for (; i < argsCount; ++i)
        {
          rest[i - fixed] = new DmiObject(args.getValue(i));
        }
        args.resize(fixed + 1);
        args.setValue(fixed, inOf(rest));
        restAdded = true;
      }
    }
    }
  }
  if (hasNamedRest == true && restAdded == false)
  {
    RDmiNamedArgArray rest = new DmiNamedArgArray(0);
    args.resize(argsCount + 1);
    args.setValue(argsCount, inOf(rest));
  }
  if (hasRest == true && restAdded == false)
  {
    RDmiObjectArray rest = new DmiObjectArray(0);
    args.resize(argsCount + 1);
    args.setValue(argsCount, inOf(rest));
  }
}

/*
  standard lookup used by dmi
*/
//static 
const ClazzMethodInfo* 
StdDispatch::_lookupMethod( const ClazzInfo*& clazz
                                              , IN(RString) fname
                                              , ScriptVarArray& args
                                              , IN(RStringArray) namedargs
                                              , DmiClient& dc
                                              , int flags
                                              , const ClazzMethodInfo* methinf
                                              )
{
  const ClazzInfo* tclazz = clazz;
  if (flags & MiIvViaHash)
  {
    int methhash = ACDK_CAST_PTR2INT(methinf);
    const ClazzMethodInfo* cmi = lookupMethod(clazz, methhash, flags);
    if (cmi == 0 && (flags & MiIvNoThrowIfNotFound) == 0)
      ClazzMethodInfo::throwMethodNotFound(tclazz, Integer::toString(methhash), flags, dc._formatFlags);
    return cmi;
  }
  
  if (fname->length() == 0 || clazz == 0)
  {
    if ((flags & MiIvNoThrowIfNotFound) == 0)
      THROW1(NoSuchDmiElementException, "Class or method not defined");
    return 0;
  }
  RString rfname = fname;
  if ((flags & MiIvConstructor) == MiIvConstructor && clazz->isArray() == true)
  {
    const ClazzInfo* eltype = clazz->userInfo; //clazz->getArrayElementType();
    if (rfname->startsWith(eltype->name) == true)
    {
      if (eltype->isBasicClazz() == true)
        rfname = "ObjectArrayBaseImpl"; // ###typo??
      else
        rfname = "ObjectArrayBaseImpl";
    }
    else if (*eltype->name == '[')
    {
      rfname = "ObjectArrayBaseImpl";
    }
  }

  ClazzMethodInfoVec methods;
  findFunctions(clazz, rfname, flags, methods);
  if (methods.size() == 0)
  {
    if ((flags & MiIvNoThrowIfNotFound) == MiIvNoThrowIfNotFound)
      return 0;
    StringBuffer sb;
    sb.append("Method not found: ");
    MetaInfo::flagsToTypeDecl(sb, flags, dc._formatFlags);
    sb.append(" ");
    tclazz->toTypeString(sb, dc._formatFlags);
    sb.append("::");
    sb.append(fname);
    sb.append("(...)");
    THROW1(NoSuchMethodException, sb.toString());
  }
  
  int lastmatch = 0;
  const ClazzMethodInfo* lastBestMatch = 0;
  /*
    idx is original position
    newargpos[i] is new position
    if position is -1 the argument belongs to Named or unnamed rest
  */
  RbyteArray newargpos;
  RbyteArray lastnewargpos = Nil;
  ClazzMethodInfoVec::iterator it = methods.begin();
  ClazzMethodInfoVec::iterator end = methods.end();
  

  for (; it != end; ++it)
  {
    const ClazzMethodInfo* cmi = *it;
    ClazzMethodInfoDescriptor cmd(cmi);
    ScriptVarArgDescription argdesc(args, namedargs);
    int m =  matchMethod(clazz, cmd, argdesc, dc,flags, newargpos);
    if (m == -1)
      continue;
    if (m == 0 || lastmatch == 0 || lastmatch > m)
    {
      lastBestMatch = cmi;
      lastmatch = m;
      lastnewargpos = newargpos;
      if (m == 0)
        break;
    }
  }
 
  if (lastBestMatch == 0)
  {
    if ((flags & MiIvNoThrowIfNotFound) == MiIvNoThrowIfNotFound)
     return 0;
    StringBuffer sb(1024);
    sb.append("No matching function found: ");
    tclazz->toTypeString(sb, dc._formatFlags);
    sb.append("::");
    sb.append(fname);
    sb.append("(");
    asSignature(sb, args);
    sb.append(")\n Following functions are defined:\n");
    for (int i = 0; i < methods.size(); ++i)
    {
      sb.append("\t");
      const ClazzInfo* parent = (const ClazzInfo*)methods[i]->_scopeParent;
      if (tclazz->isArray() == true)
        parent = tclazz;
      methods[i]->toTypeString(sb, reinterpret_cast<const ClazzInfo*>(parent), dc._formatFlags);
      sb.append("\n");
    }
    THROW3(ParamsMismatchException, sb.toString(), tclazz, methods);  
  }
  
  ClazzMethodInfoDescriptor cmd(lastBestMatch);
  ScriptVarArgDescription argdesc(args, namedargs);
  mapArguments(clazz, cmd, argdesc, lastnewargpos);
  /*

  bool hasNamedRest = hasNamedRestParam(cmd);
  bool hasRest = hasRestParam(cmd);
  bool restAdded = false;

  if (lastmatch == 0 && hasNamedRest == false && hasRest == false)
    return lastBestMatch;
  
  if (lastnewargpos != Nil)
  {
    ScriptVarArray argssic = args;
    int i;
    for (i = 0; i < args.size(); ++i)
    {
      int newpos = lastnewargpos[i];
      int argsCount = args.size();
      if (newpos == 0xff)
        break;
      else if (newpos == 0xfe)
      {
        if (lastBestMatch->methodArgs[i]->getDefaultArgValueFunc != 0)
          args[i] = lastBestMatch->methodArgs[i]->getDefaultArgValueFunc(lastBestMatch->methodArgs[i]);
        else
          ; //Ooops
      }
      else if (newpos != i)
      {
        args[newpos].reset();
        args[newpos] = argssic[i];
      }
    }
    if (i != args.size())
    {
      int fixed = i;
      int calcUnmappedsize = 0;
      for (int ti = lastnewargpos->length() - 1; ti >= 0; --ti)
      {
        if (lastnewargpos[ti] != 0xff)
          break;
        ++calcUnmappedsize;
      }
      ClazzMethodInfoDescriptor cmd(lastBestMatch);
      if (hasNamedRestParam(cmd) == true)
      {
        int unmapped = args.size() - fixed;
        RDmiNamedArgArray rest = new DmiNamedArgArray(unmapped);
        int mc = 0;
        
        int unMappedOffset = namedargs->length() - calcUnmappedsize;
        for (int naidx = 0; i < args.size(); ++i, ++naidx)
        {
          RString narg = namedargs[unMappedOffset + naidx];
          rest[naidx] = new DmiNamedArg(narg, new DmiObject(args[i]));
        }
        args.resize(fixed + 1);
        args[fixed] = &rest;
        restAdded = true;
        
      }
      else if (hasRestParam(cmd) == true)
      {
        int unmapped = args.size() - fixed;
        RDmiObjectArray rest = new DmiObjectArray(unmapped);
        for (; i < args.size(); ++i)
        {
          rest[i - fixed] = new DmiObject(args[i]);
        }
        args.resize(fixed + 1);
        args[fixed] = &rest;
        restAdded = true;
      }
    }
  }
  if (hasNamedRest == true && restAdded == false)
  {
    RDmiNamedArgArray rest = new DmiNamedArgArray(0);
    args.resize(args.size() + 1);
    args[args.size() - 1] = &rest;
  }
  if (hasRest == true && restAdded == false)
  {
    RDmiObjectArray rest = new DmiObjectArray(0);
    args.resize(args.size() + 1);
    args[args.size() - 1] = &rest;
  }
  */
  clazz = reinterpret_cast<const ClazzInfo*>(lastBestMatch->_scopeParent); 
  
  return lastBestMatch;
  /*
    int mcount = 0;
    int nargsc = namedargs->length();
    int unamedargsc  = 0;
  
    newargpos = new byteArray(args.size(), (byte)-1);
    const ClazzMethodInfo* cmi = *it;
    mcount = cmi->getArgumentCount();
    bool hasNamedRest = MethodInfo_hasNamedRestParam(cmi);
    if (hasNamedRest == true)
      mcount -= 1;
    else if (mcount != args.size()) 
      continue;
    if (hasNamedRest == true)
      unamedargsc = mcount;
    else
      unamedargsc = mcount - nargsc;
    bool canasign = true;
    int total = 0;
    for (int j = 0; j < unamedargsc; j++) 
    {
      int match;
      if (cmi->methodArgs[j]->flags & MiAiOut)
        match = dc.typeDistance(cmi->methodArgs[j]->type, args[j].getClazzInfo());
      else
        match = dc.typeDistance(args[j], cmi->methodArgs[j]->type);
      if (match == -1) {
        canasign = false;
        break;
      }
	    total += match;
    }
    if (canasign == true) 
    {
      for (int j = unamedargsc; j < mcount; j++) // checke named values
      {
        
        bool foundp = false;
        int mapped = 0;
        //for (mapped = 0; mapped < nargsc && mapped + j < mcount; ++mapped) // seek positinal
        for (mapped = 0; mapped < nargsc && mapped + unamedargsc < mcount; ++mapped) // seek positinal
        
        {
          RString narg = namedargs[mapped];
          if (cmi->methodArgs[j]->equalsName(narg) == true)
          {
            int match = dc.typeDistance(args[unamedargsc + mapped], cmi->methodArgs[j]->type);
            if (match == -1) 
            {
              canasign = false;
              break;
            }
            newargpos[unamedargsc + mapped] = j;
            foundp = true;
            total += match;
            break;
          }
        }
        
        if (foundp == false)
        {
          canasign = false;
          break;
        }
      }
      
      if (total == 0 && canasign == true)
      {
        lastBestMatch = *it;
        lastnewargpos = newargpos;
        goto foundMethod;
      }
      if (canasign == true && (total < lastmatch) || (lastBestMatch == 0)) 
      {
        lastmatch = total;
        lastnewargpos = newargpos;
        lastBestMatch = *it;
      }
    }
  }
foundMethod:
  
  

  // reorder ScriptVar elements in order of the named values

  if (lastBestMatch != 0) 
  {
    bool hasNamedRest = MethodInfo_hasNamedRestParam(lastBestMatch);
    if (hasNamedRest == true)
    {
      int mcount = lastBestMatch->getArgumentCount();
      int fixed = mcount - 1;
      int unmapped = args.size() - fixed;
      RDmiNamedArgArray rest = new DmiNamedArgArray(unmapped);
      int mc = 0;
      for (int i = 0; i < unmapped; ++i)
      {
        RString narg = namedargs[i];
        rest[i] = new DmiNamedArg(narg, new DmiObject(args[fixed + i]));
      }
      args.resize(fixed + 1);
      args[fixed] = &rest;
      clazz = reinterpret_cast<const ClazzInfo*>(lastBestMatch->_scopeParent); 
      return lastBestMatch;
    }

    bool reorder = false;
    int i;
    int mcount = lastBestMatch->getArgumentCount();
    int nargsc = namedargs->length();
    int unamedargsc = mcount - nargsc;
    for (i = unamedargsc; i < args.size(); ++i)
    {
      if (lastnewargpos[i] != i)
      {
        reorder = true;
      }
    }
    if (reorder == false)
    {
      clazz = reinterpret_cast<const ClazzInfo*>(lastBestMatch->_scopeParent); 
      return lastBestMatch;
    }
    ScriptVarArray argssic = args;
    for (i = unamedargsc; i < args.size(); ++i)
    {
      if (lastnewargpos[i] != i && char(lastnewargpos[i]) != -1)
      {
        args[lastnewargpos[i]].reset();
        args[lastnewargpos[i]] = argssic[i];
      }
    }
    clazz = reinterpret_cast<const ClazzInfo*>(lastBestMatch->_scopeParent);
    return lastBestMatch;
  }
  if ((flags & MiIvNoThrowIfNotFound) == MiIvNoThrowIfNotFound)
    return 0;
  StringBuffer sb(1024);
  sb.append("No matching function found: ");
  clazz->toTypeString(sb, dc._formatFlags);
  sb.append("::");
  sb.append(fname);
  sb.append("(");
  asSignature(sb, args);
  sb.append(")\n Following functions are defined:\n");
  for (int i = 0; i < methods.size(); ++i)
  {
    sb.append("\t");
    const ClazzInfo* parent = (const ClazzInfo*)methods[i]->_scopeParent;
    if (clazz->isArray() == true)
      parent = clazz;
    methods[i]->toTypeString(sb, reinterpret_cast<const ClazzInfo*>(parent), dc._formatFlags);
    sb.append("\n");
  }
  THROW3(ParamsMismatchException, sb.toString(), clazz, methods);  
  return 0;
  */
}

void 
asSignature(::acdk::lang::StringBuffer& sb, ::acdk::lang::sys::core_vector<ClazzMethodArgInfo>& args)
{
  for (int i = 0; i < args.size(); ++i)
  {
    if (i > 0)
      sb.append(", ");
    args[i].toTypeString(sb, TpFtFormatStandard);
  }
}


/*
  used by Class::getMethod()
*/
//static 
const ClazzMethodInfo*  
StdDispatch::_lookupMethod(  const ClazzInfo*& clazz
                                                , IN(RString) fname
                                                , ::acdk::lang::sys::core_vector<ClazzMethodArgInfo>& args
                                                , IN(RStringArray) namedargs
                                                , DmiClient& dc
                                                , int flags
                                                )
{
  
  if (fname->length() == 0 || clazz == 0)
  {
    if (MiIvNoThrowIfNotFound & flags)
      return 0;

    THROW1(NoSuchDmiElementException, "DMI: class not defined");
  }
  
  ClazzMethodInfoVec methods;
  findFunctions(clazz, fname, flags, methods);
  if (methods.size() == 0)
  {
    if (MiIvNoThrowIfNotFound & flags)
      return 0;
    StringBuffer sb;
    sb.append("Method not found: ");
    MetaInfo::flagsToTypeDecl(sb, flags, dc._formatFlags);
    //sb.append(Modifier::toString(Modifier::accessMask(flags) | Modifier::staticMask(flags)));
    sb.append(" ");
    clazz->toTypeString(sb, dc._formatFlags);
    sb.append("::");
    sb.append(fname);
    sb.append("(...)");
    THROW1(NoSuchMethodException, sb.toString());
  }
  int lastmatch = 0;
  const ClazzMethodInfo* lastBestMatch = 0;
  int nargsc = namedargs == Nil ? 0 : namedargs->length();
  int unamedargsc  = 0;
  ClazzMethodInfoVec::iterator it = methods.begin();
  ClazzMethodInfoVec::iterator end = methods.end();
  int mcount = 0;
  
  RbyteArray newargpos;
  RbyteArray lastnewargpos = Nil;
  for (; it != end; ++it)
  {
    const ClazzMethodInfo* cmi = *it;    
    ClazzMethodInfoDescriptor cmd(cmi);
    MethodArgInfoArgDesc argdesc(args, namedargs);
    int m =  matchMethod(clazz, cmd, argdesc, dc, flags, newargpos);
    if (m == -1)
      continue;
    if (m == 0 || lastmatch == 0 || lastmatch > m)
    {
      lastBestMatch = cmi;
      lastmatch = m;
      lastnewargpos = newargpos;
      if (m == 0)
        break;
    }
    /*
    newargpos = new byteArray(args.size());
    const ClazzMethodInfo* cmi = *it;
    mcount = cmi->getArgumentCount();
    if (mcount != args.size()) 
      continue;
    unamedargsc = mcount - nargsc;
    bool canasign = true;
    int total = 0;
    for (int j = 0; j < mcount - nargsc; j++) 
    {
      int match;
      if (cmi->methodArgs[j]->flags & MiAiOut)
        match = dc.typeDistance(cmi->methodArgs[j]->type, args[j].type);
      else
        match = dc.typeDistance(args[j].type, cmi->methodArgs[j]->type);
      if (match == -1) {
        canasign = false;
        break;
      }
	    total += match;
    }
    if (canasign == true) 
    {
      for (int j = unamedargsc; j < mcount; j++) // checke named values
      {
        
        bool foundp = false;
        for (int i = 0; i < nargsc; ++i) // seek positinal
        {
          RString narg = namedargs[i];
          if (cmi->methodArgs[j]->equalsName(narg) == true)
          {
            int match = dc.typeDistance(args[unamedargsc + i].type, cmi->methodArgs[j]->type);
            if (match == -1) 
            {
              canasign = false;
              break;
            }
            newargpos[unamedargsc + i] = j;
            foundp = true;
            total += match;
          }
        }
        if (foundp == false)
        {
          canasign = false;
          break;
        }
      }
      
      if (total == 0 && canasign == true)
      {
        lastBestMatch = *it;
        lastnewargpos = newargpos;
        goto foundMethod;
      }
      if ((total < lastmatch) || (lastBestMatch == 0)) {
        lastmatch = total;
        lastnewargpos = newargpos;
        lastBestMatch = *it;
      }
    }
    */
  }
foundMethod:
  
  // reorder ScriptVar elements in order of the named values

  if (lastBestMatch != 0) 
  {
    ClazzMethodInfoDescriptor cmd(lastBestMatch);
    MethodArgInfoArgDesc argdesc(args, namedargs);

    mapArguments(clazz, cmd, argdesc, lastnewargpos);
    /*
    bool reorder = false;
    int i;
    for (i = unamedargsc; i < args.size(); ++i)
    {
      if (lastnewargpos[i] != i)
      {
        reorder = true;
      }
    }
    if (reorder == false)
    {
      clazz = reinterpret_cast<const ClazzInfo*>(lastBestMatch->_scopeParent); 
      return lastBestMatch;
    }
    ::acdk::lang::sys::core_vector<ClazzMethodArgInfo> argssic = args;
    for (i = unamedargsc; i < args.size(); ++i)
    {
      if (lastnewargpos[i] != i)
      {
        args[lastnewargpos[i]] = argssic[i];
      }
    }
    */
    clazz = reinterpret_cast<const ClazzInfo*>(lastBestMatch->_scopeParent);
    return lastBestMatch;
  }
  StringBuffer sb(1024);
  sb.append("No matching function found: ");
  clazz->toTypeString(sb, dc._formatFlags);
  sb.append("::");
  sb.append(fname);
  sb.append("(");
  asSignature(sb, args);
  sb.append(")\n Following functions are defined:\n");
  for (int i = 0; i < methods.size(); ++i)
  {
    sb.append("\t");
    const ClazzInfo* parent = (const ClazzInfo*)methods[i]->_scopeParent;
    if (clazz->isArray() == true)
      parent = clazz;
    methods[i]->toTypeString(sb, reinterpret_cast<const ClazzInfo*>(parent), dc._formatFlags); 
    sb.append("\n");
  }
  THROW3(ParamsMismatchException, sb.toString(), clazz, methods);  
  return 0;
  
}


//static 
const ClazzMethodInfo* 
StdDispatch::lookupMethod( const ClazzInfo*& clazz
                          , IN(RString) fname
                          , ScriptVarArray& args
                          , IN(RStringArray) namedArgs
                          , DmiClient& dc
                          , int flags
                          , const ClazzMethodInfo* methinf
                          )
{
  return _lookupMethod(clazz, fname, args, namedArgs, dc, flags, methinf);
  /*
  if (namedArgs == Nil)
  {

    const ClazzMethodInfo* cmi = _lookupMethod(clazz, fname, args, namedArgs, dc, flags, methinf);
    if (cmi == 0) // if _lookupMethod didn't thrown an ex, MiIvNoThrowIfNotFound is set
      return 0; 
    int mcount = cmi->getArgumentCount();
    bool hasRest = MethodInfo_hasRestParam(cmi);
    bool hasNamedRest = MethodInfo_hasNamedRestParam(cmi);
    if (hasRest == true || hasNamedRest == true)
      mcount -= 1;

    for (int j = 0; j < mcount; j++) 
    {
      dc.castTo(args[j], cmi->methodArgs[j]->type);
    }
    if (hasRest == true)
    {
      RDmiObjectArray rest = new DmiObjectArray(args.size() - mcount);

      int baseidx = 0;
      for (int i = mcount; i < args.size(); ++i, ++baseidx)
      {
        rest[baseidx] = new DmiObject(args[i]);
      }
      args.resize(mcount + 1);
      args[mcount] = &rest;
    }
    else if (hasNamedRest == true)
    {
      RDmiNamedArgArray rest = new DmiNamedArgArray(0);
      args.resize(args.size() + 1);
      args[args.size() - 1] = &rest;
    }
    return cmi;
  }
  else
    return _lookupMethod(clazz, fname, args, namedArgs, dc, flags, methinf); 
  */
}

//static 
const ClazzMethodInfo*  
StdDispatch::lookupMethod(  const ClazzInfo*& clazz
                                                , IN(::acdk::lang::RString) fname
                                                , acdk::lang::dmi::ClazzMethodArgInfo** const args
                                                , DmiClient& dc
                                                , int flags
                                                )
{
  ClazzMethodInfoVec methods;
  StdDispatch::findFunctions(clazz, fname, flags, methods);
  if (methods.size() == 0)
    return 0;
  ClazzMethodInfoVec::iterator it = methods.begin();
  ClazzMethodInfoVec::iterator end = methods.end();
  const ClazzMethodInfo* cmi = *it;
  const ClazzMethodInfo* lastBestMatch = 0;
  RbyteArray newargpos;
  RbyteArray lastnewargpos;
  int mcount = 0;
  int acount = 0;
  int lastmatch = 0;
  for (; args != 0 && args[acount] != 0; ++acount)
    ;
  bool canasign = true;
  int total = 0;

  for (; it != end; ++it)
  {
    const ClazzMethodInfo* cmi = *it;
    ClazzMethodInfoDescriptor cmd(cmi);
    MethodArgInfoPtrArgDesc argsDesc(args, Nil);
    
    int m =  matchMethod(clazz, cmd, argsDesc, dc,flags, newargpos);
    if (m == -1)
      continue;
    if (m == 0 || lastmatch == 0 || lastmatch > m)
    {
      lastBestMatch = cmi;
      lastmatch = m;
      lastnewargpos = newargpos;
      if (m == 0)
        break;
    }
    /*
    
    mcount = cmi->getArgumentCount();
    if (mcount != acount) 
      continue;
    
    for (int j = 0; j < acount; j++) 
    {
      int match;
      if (cmi->methodArgs[j]->flags & MiAiOut)
        match = dc.typeDistance(cmi->methodArgs[j]->type, args[j]->type);
      else
        match = dc.typeDistance(args[j]->type, cmi->methodArgs[j]->type);
      if (match == -1) 
      {
        canasign = false;
        break;
      }
	    total += match;
    }
    if (total == 0 && canasign == true)
    {
      lastBestMatch = *it;
      goto foundMethod;
    }
    if ((total < lastmatch) || (lastBestMatch == 0)) 
    {
      lastmatch = total;
      lastBestMatch = *it;
    }
    */
  }
foundMethod:
  if (lastBestMatch != 0) 
    return lastBestMatch;
  return lastBestMatch;
}

//static 
const ClazzMethodInfo* 
StdDispatch::lookupMethod( const ClazzInfo*& clazz, IN(RString) fname, int flags)
{
  if (clazz == 0)
    return 0;
  clazz = clazz->loadFullClazzInfo();
  int i;
  if (clazz->methods != 0)
  {
    for (i = 0; clazz->methods[i]; i++) 
    {
      const ClazzMethodInfo* cmi = clazz->methods[i]; 
      if (MetaInfo::checkMemberAccess(flags, clazz->methods[i]->flags) == false)
        continue;
      if (cmi->equalsName(fname) == true) 
        return cmi;
    }
  }
  const ClazzMethodInfo* cmi;
  if (clazz->interfaces != 0 && (MiIvDeclared & flags) == 0)
  {
    const ClazzInfo* tclazz = clazz;
    if (flags & MiIvTransientCall)
      flags |= MiProtected;
    for (i = 0; tclazz->interfaces[i] != 0; i++) 
    {
      clazz = tclazz->interfaces[i]->type;
      cmi = lookupMethod(clazz, fname, flags);
      if (cmi != 0)
        return cmi;
    }
    clazz = tclazz;
  }
  return 0;
}

//static 
const ClazzMethodInfo* 
StdDispatch::lookupMethod( const ClazzInfo*& clazz, int methodhash, int flags)
{
  int i;
  clazz = clazz->loadFullClazzInfo();
  for (i = 0; i < clazz->getMethodsCount(); ++i)
  {
    if (clazz->methods[i]->getMethodSignatureHashValue() == methodhash)
    {
      if (MetaInfo::checkMemberAccess(flags, clazz->methods[i]->flags) == false)
        return 0;
      return clazz->methods[i];
    }
  }
  if (MiIvDeclared & flags)
    return 0;
  const ClazzInfo* tclazz = clazz;
  for (i = 0; tclazz->interfaces[i] != 0; i++) 
  {
    if (flags & MiIvTransientCall)
      flags |= MiProtected;
    clazz = tclazz->interfaces[i]->type;
    const ClazzMethodInfo* merg = lookupMethod(clazz, methodhash, flags);
    if (merg != 0)
      return merg;
    clazz = tclazz;
  }
  return 0;
}

bool 
StdDispatch::isDmiOverLoaded(const ClazzInfo* ci, const ClazzMethodInfo* mi)
{
  if ((DmiClient::getCurInvokeFlags() & MiIvNoWeakBind) != 0)
    return false;
  return isDmiOverLoaded(ci, mi->name, mi, mi->methodArgs);
}

// virtual
bool 
StdDispatch::isDmiOverLoaded(const ClazzInfo* ci, IN(RString) funcname, const ClazzMethodInfo* mi, ClazzMethodArgInfo**const args)
{
  if (mi != 0)
  {
    
  }
  // #### maybe implement me
  return false;
}

RString getClassMethodDevider(int formatFlags)
{
  if (formatFlags & TpFtJavaType)
    return ".";
  if (formatFlags & TpFtLoadableClass || formatFlags & TpFtJavaSignature)
    return "/";
  return "::";

}

void throwNonPolyMethodNotFound(const ClazzInfo* clazz, IN(RString) fname, int formatFlags)
{
   StringBuffer sb;
   clazz->toTypeString(sb, formatFlags);
   sb.append(getClassMethodDevider(formatFlags));
   sb.append(fname);
   THROW1(NoSuchMethodException, "No such Method: " + sb.toString());
}

const ClazzMethodInfo* 
StdDispatch::lookupMethodNoPolymorph( const ClazzInfo*& clazz 
                                                          , IN(RString) fname
                                                          , ScriptVarArray& args
                                                          , IN(RStringArray) namedArgs
                                                          , DmiClient& dc
                                                          , int flags
                                                          , const ClazzMethodInfo* methinf
                                                          )
{
  clazz = clazz->loadFullClazzInfo();
  int i;
  for (i = 0; i < clazz->getMethodsCount(); ++i)
  {
    const ClazzMethodInfo* cmi = clazz->methods[i];
    if (cmi->altlabel != 0)
    {
      if (cmi->equalsAltName(fname) == true)
        return cmi;
    }
    if (cmi->equalsName(fname) == true)
      return cmi;
  }

  if (flags & MiIvDeclared)
  {
    if (flags & MiIvNoThrowIfNotFound)
      return 0;
    throwNonPolyMethodNotFound(clazz, fname, dc._formatFlags);
  }
  
  const ClazzInfo* tclazz = clazz;
  for (i = 0; clazz->interfaces[i]; i++) 
  {
    clazz = tclazz->interfaces[i]->type;
    const ClazzMethodInfo* ret = lookupMethodNoPolymorph(clazz, fname, args, Nil, dc, flags | MiIvNoThrowIfNotFound);
    if (ret != 0)
      return ret;
    clazz = tclazz;
  }
  if ((flags & MiIvNoThrowIfNotFound) == 0)
    throwNonPolyMethodNotFound(tclazz, fname, dc._formatFlags);
  return 0;
}

const ClazzMethodInfo* 
StdDispatch::findMethod(const ClazzInfo*& ci, const FunctionSignature& signature, bool exactMatch, int flags)  
{
  int i;
  if (ci == 0)
    return 0;
  ci = ci->loadFullClazzInfo();
  for (i = 0; i < ci->getMethodsCount(); i++) 
  {
    //### maybe used with FunctionSignatureArgDesc
    if (strcmp(ci->methods[i]->name, signature.functionname) == 0) {
      int mcount = ci->methods[i]->getArgumentCount();
      if (mcount == signature.size) 
      {
        bool canasign = true;
        for (int j = 0; j < mcount; j++) 
        {
          if (ci->methods[i]->methodArgs[j] == 0) 
          {
            canasign = false;
            break;
          }
          if (exactMatch == true) {
            if (signature.args[j] != ci->methods[i]->methodArgs[j]->type) {
              canasign = false;
              break;
            }
          } else {
            if (AcdkDmiClient::getDmiClient().typeDistance(ci->methods[i]->methodArgs[j]->type, 
                                                            signature.args[j]) == -1)
            {
              canasign = false;
              break;
            }
          }
        }
        if (canasign == true) {
          return ci->methods[i];
        }
      }
    }
  }
  const ClazzMethodInfo* merg = 0;
  if (ci->interfaces != 0 && (MiIvDeclared & flags) == 0)
  {
    const ClazzInfo* tclazz = ci;
    for (i = 0; i < tclazz->getInterfacesCount(); i++) 
    {
      if (flags & MiIvTransientCall)
        flags |= MiProtected;
      ci = tclazz->interfaces[i]->type;
      merg = findMethod(ci, signature, exactMatch, flags);
      if (merg != 0)
        return merg;
      ci = tclazz;
    }
  }
  return 0;
}

/*
  used by acdkx_orb?
  not used anymore

//static
const ClazzMethodInfo* 
StdDispatch::findMethod(const ClazzInfo*& ci, IN(RString) funcname, 
                       ArgumentExprTypes& types, int flags, bool exactMatch)  
{
  int i;
  if (ci == 0)
    return 0;
  ci = ci->loadFullClazzInfo();
  int lastDistance = -1;
  int lastmethodidx = -1;
  int curDistance = 0;
  if (ci->methods == 0)
    goto searchInSuper;
 
  for (i = 0; ci->methods[i]; i++) 
  {
    if (funcname == Nil || 
        ci->methods[i]->equalsName(funcname) == true ||
        ci->methods[i]->equalsAltName(funcname) == true
      ) 
    {
      int mcount = 0;
      if (ci->methods[i]->methodArgs != 0)
      {
        while (ci->methods[i]->methodArgs[mcount++])
          ;
        --mcount;
      }
      if (mcount > 0 &&
          (MethodInfo_hasRestParam(ci->methods[i]) == true ||
           MethodInfo_hasNamedRestParam(ci->methods[i]) == true))
      {
          ;
      }
      else if (mcount != types.size()) 
        goto searchNextMethod;
      curDistance = 0;
      bool canasign = true;
      int lastPositionalArg = 0;
      for (int j = 0; j < mcount; j++) 
      {
        if (types[j]._argName != 0)
        {
          if (ClazzInfo::isNamedRestArg(ci->methods[i]->methodArgs[j]->type) == true)
            break;
          types[j]._position = -1;
          for (int k = lastPositionalArg; k < mcount; ++k)
          {
            if (strcmp(types[j]._argName, ci->methods[i]->methodArgs[k]->name) == 0)
            {
              types[j]._position = k; 
              break;
            }
          }
          if (types[j]._position == -1) // positional arg not found
            goto searchNextMethod;
          if (exactMatch == true) 
          {
            if (types[j]._type != ci->methods[i]->methodArgs[types[j]._position]->type) 
              goto searchNextMethod;
          } 
          else 
          {
            if (AcdkDmiClient::getDmiClient().typeDistance(types[j]._type, ci->methods[i]->methodArgs[types[j]._position]->type
                                                            ) == -1)
              goto searchNextMethod;
          }
        }
        else // not a named arg
        {
          if (ci->methods[i]->methodArgs[j] == 0) 
          {
            canasign = false;
            break;
          }
          if (ClazzInfo::isRestArg(ci->methods[i]->methodArgs[j]->type) == true)
            break;
          if (exactMatch == true) 
          {
            if (types[j]._type != ci->methods[i]->methodArgs[j]->type) 
              goto searchNextMethod;
            types[j]._position = j;
          } 
          else 
          {
            int dist = AcdkDmiClient::getDmiClient().typeDistance(types[j]._type, ci->methods[i]->methodArgs[j]->type);
            if (dist == -1)
              goto searchNextMethod;
            types[j]._position = j;
            lastPositionalArg = j;
            curDistance += dist;
          }
        }
      }
      if (curDistance < lastDistance || lastDistance == -1)
      {
        lastDistance = curDistance;
        lastmethodidx = i;
      }  
    }
searchNextMethod:
    continue;
  }
  if (lastmethodidx != -1)
    return ci->methods[lastmethodidx];

searchInSuper:
  const ClazzMethodInfo* merg = 0;
  if (ci->interfaces != 0 && (MiIvDeclared & flags) == 0)
  {
    for (i = 0; ci->interfaces[i]; i++) 
    {
      if (flags & MiIvTransientCall)
        flags |= MiProtected;
      ci = ci->interfaces[i]->type;
      merg = findMethod(ci, funcname, types, flags, exactMatch);
      if (merg != 0)
        return merg;
    }
  }
  return 0;
}
*/

} // dmi
} // lang
} // acdk
