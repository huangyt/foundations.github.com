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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ClazzInfo.cpp,v 1.97 2005/05/02 23:08:49 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/ClassLoader.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Number.h>

#include <acdk/lang/NoSuchElementException.h>
#include <acdk/lang/reflect/Enumeration.h>
#include <acdk/lang/sys/core_sys_static_mutex.h>
#include <acdk/lang/Thread.h>

#include "DmiObject.h"
#include "DmiNamedArg.h"
#include "MetaInfoChildsArray.h"
#include "ClazzAttributesRes.h"


//#define LOCAL_DEBUG
#if defined(LOCAL_DEBUG)
# define DOUT(msg) do { ::acdk::lang::sys::coreout << msg << ::acdk::lang::sys::eofl; } while(false)
#else
# define DOUT(msg) do { } while(false)
#endif

namespace acdk {
namespace lang {
namespace dmi {

#ifdef ACDK_MT

typedef sys::static_mutex ClazzInfoLockMutex;
ClazzInfoLockMutex& getClazzInfoAccessLock()
{
  static ClazzInfoLockMutex _clazzInfoAccessLock;
  return _clazzInfoAccessLock;
}

//#define CLAZZINFOLOCKGUARD() TLockGuard<ClazzInfoLockMutex>  lockthis(getClazzInfoAccessLock())
#define CLAZZINFOLOCKGUARD() sys::SysStaticMutexLockGuard<ClazzInfoLockMutex> lockthis(Thread::isSingleThreaded() == false, getClazzInfoAccessLock())
#else
#define CLAZZINFOLOCKGUARD()
#endif //ACDK_MT

OUT(RMetaObjectListenerArray)
metaObjectListeners()
{
  static RMetaObjectListenerArray listener; 
  return listener;
}

OUT(RMetaObjectListenerArray)
getMetaObjectListeners()
{
  OUT(RMetaObjectListenerArray) mo = metaObjectListeners();
  if (mo == Nil)
    mo = new MetaObjectListenerArray(0);
  return mo;
}

void onRegisterMetaInfo(IN(RMetaObject) mo)
{
  OUT(RMetaObjectListenerArray) ml = getMetaObjectListeners();
  for (int i = 0; i < ml->length(); ++i)
    ml[i]->onRegister(mo);
}

void onUnRegisterMetaInfo(IN(RMetaObject) mo)
{
  OUT(RMetaObjectListenerArray) ml = getMetaObjectListeners();
  for (int i = 0; i < ml->length(); ++i)
    ml[i]->onUnregister(mo);
}


ClazzInfo* _clazzRoot = 0;

ClazzInfo* _arrayClazzRoot = 0;
UnitInfo* _unitInfoRoot = 0;
ClazzEnumInfo* _enumInfoRoot = 0;

bool
ClazzInfo_load_acdk_core_metainf()
{
  static bool acdk_core_metainfLoaded = false;
  if (acdk_core_metainfLoaded == false)
  {
    if (System::isInMain() == true)
    {
      acdk_core_metainfLoaded = true;
      ClassLoader::getSystemClassLoader()->loadMetaInfoLibrary("acdk_core");
    }
    return true;
  }
  return false;
}
void addSyntaxElem(StringBuffer& sb, IN(RString) text, int formatflags)
{
  if (formatflags & TpFtACDKSignature)
    sb.append("_");
  else
    sb.append(text);
}

void addSyntaxElem(StringBuffer& sb, const char* text, int formatflags)
{
  if (formatflags & TpFtACDKSignature)
    sb.append("_");
  else
    sb.append(text);
}


void addSpace(StringBuffer& sb, int formatflags)
{
  if (formatflags & TpFtACDKSignature)
    sb.append("_");
  else
    sb.append(" ");
}
void addNsScope(StringBuffer& sb, int formatflags)
{
  if (formatflags & TpFtACDKSignature)
    sb.append("_");
  else if (formatflags & TpFtLoadableClass || formatflags & TpFtJavaSignature)
    sb.append("/");
  else if (formatflags & TpFtJavaType)
    sb.append(".");
  else
    sb.append("::");
}

void AttributeToTypeString(const MetaInfo* mi, StringBuffer& sb, int formatflags)
{
  ClazzAttributesRes::ResTable restable = ClazzAttributesRes::getAttributes(const_cast<MetaInfo*>(mi));
  ClazzAttributesRes::ResTable::iterator it = restable.begin();
  ClazzAttributesRes::ResTable::iterator end = restable.end();
  for (; it != end; ++it)
  {
    sb << "[" << it->key << "=";
    switch(it->value.resType)
    {
    case EmptyResType:
      break;
    case CharPtrResType:
    case StringResType:
    case ObjectPtrResType:
    {
      RObject obj = ClazzAttributeResValue::castObjectPtr(it->value.resType, it->value.data);
      if (obj != Nil)
        sb << obj->toString();
      else
        sb << "<Nil>";
      break;
    }
    case ScriptVarResType:
      sb << ClazzAttributeResValue::castScriptVar(it->value.resType, it->value.data).toCode();
      break;
    default:
      sb << "<unknown>";
      break;
    }
    sb << "]\n";
  }
}


/////////////////////////////    UnitInfo    ////////////////////////////////////////

RString
UnitInfo::toTypeString(int formatflags) const
{
  StringBuffer sb;
  UnitInfo::toTypeString(sb, this->name, formatflags, false);
  return sb.toString();
}

//static
void
UnitInfo::toTypeString(StringBuffer& sb, const char* ns, int formatflags, bool withMerger)
{

  if (ns == 0 || *ns == 0)
    return;
  /*
  if (formatflags & TpFtAttributes)
    AttributeToTypeString((const MetaInfo*)this, sb, formatflags);
    */
  if (formatflags & TpFtAcdkType)
  {
    RString t = RString(ns)->replace("/", "::");
    sb.append("::");
    sb.append(t);
    if (withMerger)
      sb.append("::");
  }
  else  if (formatflags & TpFtLoadableClass || formatflags & TpFtJavaSignature)
  {
    sb.append(ns);
    if (withMerger)
      sb.append("/");
  }
  else if (formatflags & TpFtJavaType)
  {
     sb.append(RString(ns)->replace('/', '.'));
     if (withMerger)
      sb.append(".");
  }
  else if (formatflags & TpFtACDKSignature)
  {
    sb.append(RString(ns)->replace('/', '_'));
     if (withMerger)
      sb.append("_");
  }
  else
  {
    sb.append(ns);
    if (withMerger)
      sb.append("/");
  }
}

//static 
void 
ClazzInfo::splitClassAndNs(IN(RString) compName, OUT(RString) className, OUT(RString) namespaceName) 
{
  RString cp = compName->replace("::", "/")->replace('.', '/');
  int lidx = cp->lastIndexOf('/');
  if (lidx == -1)
  {
    className = cp;
    return;
  }
  className = cp->substr(lidx + 1);
  namespaceName = cp->substr(0, lidx);
}

void
ClazzInfo::splitLastElement(IN(RString) name, OUT(RString) left, OUT(RString) right)
{
  int idx = name->lastIndexOf('/');
  if (idx != -1)
  {
    left = name->substr(0, idx);
    right = name->substr(idx + 1);

  }
  else
    right = name;
}

// static
UnitInfo*
UnitInfo::create(const char* name)
{
  UnitInfo* ui = new UnitInfo();
  memset(ui, 0, sizeof(UnitInfo));
  ui->nameHashCode = -1;
  ui->flags = MiDelete | MiUnitInfo;
  ui->type = ClazzInfo::getVoidClazz();
  if (name != 0 && *name != 0)
  {
    RString left;
    RString right;

    ClazzInfo::splitLastElement(RCS(name), left, right);
    UnitInfo* parent = UnitInfo::getRoot();
    if (left != Nil)
    {
      parent = findCreateUnit(left->c_str());
      ui->ns = MetaInfo::strdup(left->c_str());
    }
    else
      ui->ns = MetaInfo::strdup("");
    ui->_scopeParent = parent;
    ui->name = MetaInfo::strdup(right->c_str());
  }
  else
  {
    ui->name = MetaInfo::strdup("");
    ui->ns = MetaInfo::strdup("");
    _unitInfoRoot = ui;
    return ui;
  }
  ui->_nextScopeSibling = ui->_scopeParent->_firstChild;
  ui->_scopeParent->_firstChild = ui->getMetaInfo();
  return ui;
}

// static
UnitInfo*
UnitInfo::findCreateUnit(const char* ns)
{
  const UnitInfo* ui = reinterpret_cast<const UnitInfo*>(MetaInfo::findMetaInfoNormalized(ACDK_STACK_STR(ns), MiUnitInfo, false));
  if (ui != 0)
    return const_cast<UnitInfo*>(ui);
  return create(ns);
}

//static
UnitInfo*
UnitInfo::getRoot()
{
  if (_unitInfoRoot != 0)
    return _unitInfoRoot;
  _unitInfoRoot = UnitInfo::create("");
  return _unitInfoRoot;
}

//static
const UnitInfo*
UnitInfo::findUnitInfo(IN(RString) name, bool tryLoad)
{
  return reinterpret_cast<const UnitInfo*>(MetaInfo::findMetaInfo(name, MiUnitInfo, tryLoad));
}

void
UnitInfo_unregister(UnitInfo* ui) // #### implement me
{

}

void
UnitInfo::dispose()
{
  UnitInfo_unregister(this);
  if (getMetaInfo()->isDelete() == false)
    return;
  getMetaInfo()->dispose();
  delete this;
}

UnitInfo*
UnitInfo::clone(bool deep)
{
  UnitInfo* ui = createMetaInfo<UnitInfo>();
  ui->getMetaInfo()->copyFrom(getMetaInfo(), deep);
  return ui;
}



/////////////////////////////    ClazzEnumValueInfo    ////////////////////////////////////////

void
ClazzEnumValueInfo::dispose()
{
  getMetaInfo()->unregisterFromParent();
  if (getMetaInfo()->isDelete() == false)
    return;
  getMetaInfo()->onDisposeMetaInfo();
  getMetaInfo()->dispose();
  delete this;
}

ClazzEnumValueInfo*
ClazzEnumValueInfo::create(ClazzEnumInfo* ei, IN(RString) name, int value)
{
  ClazzEnumValueInfo* ev = createMetaInfo<ClazzEnumValueInfo>();
  ev->flags = MiEnumValInfo | MiPublic | MiDelete | MiResolved ;
  ev->name = MetaInfo::strdup(name->c_str());
  ev->nameHashCode = -1;
  ev->value = value;
  MetaInfoChildsArray<ClazzEnumValueInfo>(ei->values).push_back(ev);
  registerEnumValueInfo(ev);
  ev->flags |=  MiRegistered;
  return ev;
}

ClazzEnumValueInfo*
ClazzEnumValueInfo::clone(bool deep)
{
  ClazzEnumValueInfo* ev = createMetaInfo<ClazzEnumValueInfo>();
  ev->getMetaInfo()->copyFrom(getMetaInfo(), deep);
  ev->value = value;
  return ev;
}

RString
ClazzEnumValueInfo::toTypeString(int format) const
{
  /*
  if (formatflags & TpFtAttributes)
    AttributeToTypeString((const MetaInfo*)this, sb, formatflags);
    */
  return name;
}

//static
void
ClazzEnumValueInfo::registerEnumValueInfo(const ClazzEnumValueInfo* enumVal)
{
  ClazzInfo_load_acdk_core_metainf();
  enumVal->getMetaInfo()->registerInParent();
}

//static
void
ClazzEnumValueInfo::unregisterEnumValueInfo(const ClazzEnumValueInfo* enumVal)
{
  if (enumVal == 0)
    return;
  enumVal->getMetaInfo()->unregisterFromParent();
}

/////////////////////////////    ClazzEnumInfo    ////////////////////////////////////////

//static
const ClazzEnumInfo*
ClazzEnumInfo::findEnum(IN(RString) enumname, IN(RString) namesp) // ### replace this with MetaInfo::findInfo
{
  ClazzInfo_load_acdk_core_metainf();

  RString ns;
  if (namesp != Nil)
  {
    ns = namesp->replace("::", "/")->replace('.', '/');
  }
  for (const ClazzEnumInfo* ei = _enumInfoRoot; ei != 0; ei = ei->_next)
  {
    if (ei->getMetaInfo()->equalsName(enumname)== true)
    {
      if (ns == Nil || ns->length() == 0 || ns->equals(ei->ns) == true)
        return ei;
    }
  }
  return 0;
}

//static
const ClazzEnumValueInfo*
ClazzEnumInfo::findEnumValue(IN(RString) enumstring, const ClazzEnumInfo** ei)
{
  RString en = enumstring->replace("::", "/")->replace('.', '/');
  int idx = en->lastIndexOf('/');
  if (idx == -1)
    return findEnumValue(en, Nil, ei);
  return findEnumValue(en->substr(idx + 1), en->substr(0, idx), ei);
}

//static
const ClazzEnumValueInfo*
ClazzEnumInfo::findEnumValue(IN(RString) enumname, IN(RString) namesp, const ClazzEnumInfo** enuminfo)
{
  ClazzInfo_load_acdk_core_metainf();
  RString ns;
  if (namesp != Nil)
  {
    ns = namesp->replace("::", "/")->replace('.', '/');
  }
  for (const ClazzEnumInfo* ei = _enumInfoRoot; ei != 0; ei = ei->_next)
  {
    for (int i = 0; ei->values[i] != 0; ++i)
    {
      if (ei->values[i]->getMetaInfo()->equalsName(enumname) == true)
      {
        if (ns == Nil || ns->equals(ei->ns) == true)
        {
          if (enuminfo != 0)
            *enuminfo = ei;
          return ei->values[i];
        }
      }
    }
  }
  return 0;
}

//static
ClazzEnumInfo*
ClazzEnumInfo::getRoot()
{
  ClazzInfo_load_acdk_core_metainf();
  return _enumInfoRoot;
}

const ClazzEnumValueInfo*
ClazzEnumInfo::getValue(IN(RString) name) const
{
  if (values == 0)
    return 0;
  for (int i = 0; values[i] != 0; ++i)
    if (values[i]->getMetaInfo()->equalsName(name) == true)
      return values[i];
  return 0;
}

bool
ClazzEnumInfo::hasName(IN(RString) name) const
{
  const ClazzEnumValueInfo* v = getValue(name);
  if (v == 0)
    return false;
  return true;
}

int
ClazzEnumInfo::getIntValue(IN(RString) name) const
{
  const ClazzEnumValueInfo* v = getValue(name);
  if (v != 0)
    return v->value;
  return -1;
}

RString
ClazzEnumInfo::toTypeString(int formatflags) const
{
  StringBuffer sb;
  toTypeString(sb, formatflags);
  return sb.toString();
}

void
ClazzEnumInfo::toTypeString(StringBuffer& sb, int formatflags) const
{
  if (formatflags & TpFtAttributes)
    AttributeToTypeString((const MetaInfo*)this, sb, formatflags);

  if (formatflags & TpFtTypeDecl || formatflags & TpFtTypeDef)
    sb << "enum ";
  // ### TODO handle formatflags
  sb.append(name);
  if (formatflags & TpFtTypeDef)
  {
    sb << "\n{";
    for (int i = 0; values[i] != 0; ++i)
    {
      if (i > 0)
        sb << ",";
      const ClazzEnumValueInfo* val = values[i];
      sb << "\n  " << val->name << " = " << val->value;
    }
    sb << "\n}\n";
  }
}


/*
void
NamedScopedMetaInfo_registerInParent(const NamedScopedMetaInfo* This) // ### into NamedScopedMetaInfo
{
  UnitInfo* pui = 0;
  if (This->_scopeParent != 0)
  {
    if (This->_scopeParent->isUnitInfo() == true)
      pui = const_cast<UnitInfo*>(reinterpret_cast<const UnitInfo*>(This->_scopeParent));
  }
  else // parent == 0
  {
    pui = UnitInfo::findCreateUnit(This->ns);
    This->_scopeParent = pui->getMetaInfo();
  }
  if (pui != 0)
  {
    This->_nextScopeSibling = pui->_firstChild;
    pui->_firstChild = This;
  }
}

//static
void
ClazzEnumInfo::registerEnumValueInfo(const ClazzEnumValueInfo* enumVal)
{
  ClazzEnumValueInfo::registerEnumValueInfo(enumVal);
}
*/

//static
void
ClazzEnumInfo::registerEnumInfo(const ClazzEnumInfo* ei)
{
  ClazzInfo_load_acdk_core_metainf();

  //ClazzInfo_callStaticInitializer(clazz);
  if (ei->flags & MiRegistered)
    return;
  ei->getMetaInfo()->registerInParent();
  ei->_next = _enumInfoRoot;
  _enumInfoRoot = const_cast<ClazzEnumInfo*>(ei);
  for (int i = 0; ei->values != 0 && ei->values[i] != 0; ++i)
  {
    ei->values[i]->_scopeParent = ei->_scopeParent;
    ClazzEnumValueInfo::registerEnumValueInfo(ei->values[i]);
  }
  const_cast<ClazzEnumInfo*>(ei)->flags |= MiRegistered;
  if (metaObjectListeners() != Nil)
    onRegisterMetaInfo(new acdk::lang::reflect::Enumeration(ei));
}

//static
void
ClazzEnumInfo::unregisterEnumInfo(const ClazzEnumInfo* enumInfo)
{
  if (enumInfo == 0)
    return;
  enumInfo->getMetaInfo()->unregisterFromParent();
  
  for (int i = 0; enumInfo->values != 0 && enumInfo->values[i] != 0; ++i)
  {
    ClazzEnumValueInfo::unregisterEnumValueInfo(enumInfo->values[i]);
  }
  
  if (metaObjectListeners() != Nil)
    onUnRegisterMetaInfo(new acdk::lang::reflect::Enumeration(enumInfo));

}

void
ClazzEnumInfo::dispose()
{
  ClazzEnumInfo::unregisterEnumInfo(this);
  if (getMetaInfo()->isDelete() == false)
    return;
  getMetaInfo()->onDisposeMetaInfo();
  getMetaInfo()->dispose();

  MetaInfoChildsArray<ClazzEnumValueInfo> vals(values);
  vals.dispose();
  delete this;
}

ClazzEnumInfo*
ClazzEnumInfo::clone(bool deep)
{
  ClazzEnumInfo* ev = createMetaInfo<ClazzEnumInfo>();

  ev->getMetaInfo()->copyFrom(getMetaInfo(), deep);
  MetaInfoChildsArray<ClazzEnumValueInfo> vals(values);
  vals.copyTo(ev->values, deep);
  return ev;
}


/////////////////////////////    ClazzFieldInfo    ////////////////////////////////////////

RString
ClazzFieldInfo::toTypeString(int formatflags) const
{
  StringBuffer sb;
  toTypeString(sb, formatflags);
  return sb.toString();
}


void
ClazzFieldInfo::toTypeString(StringBuffer& sb, int formatflags) const
{
  if (formatflags & TpFtAttributes)
    AttributeToTypeString((const MetaInfo*)this, sb, formatflags);
  MetaInfo::flagsToTypeDecl(sb, flags, formatflags);
  type->toTypeString(sb, formatflags & ~TpFtTypeDef & ~TpFtTypeDecl);
  addSpace(sb, formatflags);
  sb.append(name);
}


int
ClazzFieldInfo::getHashValue() const
{
  return MetaInfo::calcHashValue(flags) * 31 + getMetaInfo()->getNameHashCode() + type->getHashValue();
}


void
ClazzFieldInfo::dispose()
{
  if (getMetaInfo()->isDelete() == false)
    return;
  getMetaInfo()->onDisposeMetaInfo();
  getMetaInfo()->dispose();
  delete this;
}

ClazzFieldInfo*
ClazzFieldInfo::clone(bool deep)
{
  ClazzFieldInfo* ev = createMetaInfo<ClazzFieldInfo>();
  ev->getMetaInfo()->copyFrom(getMetaInfo(), deep);
  return ev;
}


/////////////////////////////    ClazzMethodArgInfo    ////////////////////////////////////////

void
ClazzMethodArgInfo::toTypeString(StringBuffer& sb, int formatflags) const
{
  if (formatflags & TpFtAttributes)
    AttributeToTypeString((const MetaInfo*)this, sb, formatflags);
  if (flags & MiAiByval)
  {
    if ((flags & MiAiIn) && (flags & MiAiOut))
    {
      if (formatflags & TpFtAcdkType)
        sb.append("BYVALINOUT(");
      else if (formatflags & TpFtACDKSignature)
        sb.append("byval_inout_");
      else
        sb.append("byval inout ");
    }
    else if (flags & MiAiIn)
    {
      if (formatflags & TpFtAcdkType)
        sb.append("BYVALIN(");
      else if (formatflags & TpFtACDKSignature)
        sb.append("byval_in_");
      else
        sb.append("byval in ");
    }
    else if (flags & MiAiOut)
    {
      if (formatflags & TpFtAcdkType)
        sb.append("BYVALOUT(");
      else if (formatflags & TpFtACDKSignature)
        sb.append("byval_out_");
      else
        sb.append("byval out ");
    }
    else
    {
      if (formatflags & TpFtAcdkType)
        sb.append("BYVAL(");
      else if (formatflags & TpFtACDKSignature)
        sb.append("byval_");
      else
        sb.append("byval ");
    }
  }
  else
  {
    if ((flags & MiAiIn) && (flags & MiAiOut))
    {
      if (formatflags & TpFtAcdkType)
        sb.append("INOUT(");
      else if (formatflags & TpFtACDKSignature)
        sb.append("inout_");
      else
        sb.append("inout ");
    }
    else if (flags & MiAiIn)
    {
      if (formatflags & TpFtAcdkType)
        sb.append("IN(");
      else if (formatflags & TpFtACDKSignature)
        sb.append("in_");
      else
        sb.append("in ");
    }
    else if (flags & MiAiOut)
    {
      if (formatflags & TpFtAcdkType)
        sb.append("OUT(");
      else if (formatflags & TpFtACDKSignature)
        sb.append("out_");
      else
        sb.append("out ");
    }
  }
  // ### hasAttribute/ getAttribute should only need const MetaInfo*
  if (type == 0)
  {
     sb.append("<type is 0x0>");
  }
  else if (formatflags & TpFtAcdkType &&
      type->isIntClazz() == true &&
      ClazzAttributesRes::hasAttribute((MetaInfo*)this, "__enumArgInfo"))
  {
    ClazzAttributeResValue attr = ClazzAttributesRes::getAttribute((MetaInfo*)this, "__enumArgInfo");
    const ClazzEnumInfo* ei = (const ClazzEnumInfo*)attr.data;
    if (ei != 0 && ei->name != 0)
      sb.append(ei->name);
    else
      type->toTypeString(sb, formatflags);
  }
  else
  {
    type->toTypeString(sb, formatflags);
  }
  if ((flags & MiAiIn) || (flags & MiAiOut) || (flags & MiAiByval))
  {
    if (formatflags & TpFtAcdkType)
      sb.append(")");
  }
  if ((formatflags & TpFtACDKSignature) == 0 && (formatflags & TpFtJavaSignature) == 0)
  {
    addSpace(sb, formatflags);
    sb.append(name);
  }
}

RString
ClazzMethodArgInfo::toTypeString(int formatflags) const
{
  StringBuffer sb;
  toTypeString(sb, formatflags);
  return sb.toString();
}

void
ClazzMethodArgInfo::dispose()
{
  if (getMetaInfo()->isDelete() == false)
    return;
  getMetaInfo()->onDisposeMetaInfo();
  getMetaInfo()->dispose();
  delete this;
}

ClazzMethodArgInfo*
ClazzMethodArgInfo::clone(bool deep)
{
  ClazzMethodArgInfo* ev = createMetaInfo<ClazzMethodArgInfo>();
  ev->getMetaInfo()->copyFrom(getMetaInfo(), deep);
  return ev;
}

/////////////////////////////    ClazzMethodInfo    ////////////////////////////////////////

RString
ClazzMethodInfo::toTypeString(const ClazzInfo* clazz, int formatflags) const
{
  StringBuffer sb;
  toTypeString(sb, clazz, formatflags);
  return sb.toString();
}

RString 
ClazzMethodInfo::toTypeString(int formatflags) const
{
  return toTypeString(reinterpret_cast<const ClazzInfo*>(_scopeParent), formatflags);
}

void
ClazzMethodInfo::toTypeString(StringBuffer& sb, const ClazzInfo* clazz, int formatflags) const
{
  if (formatflags & TpFtAttributes)
    AttributeToTypeString((const MetaInfo*)this, sb, formatflags);
  MetaInfo::flagsToTypeDecl(sb, (int)flags, formatflags);
  addSpace(sb, formatflags);
  int argfflags = formatflags & ~TpFtTypeDef & ~TpFtTypeDecl;
  if (argfflags & TpFtAcdkType && (formatflags & TpFtACDKSignature) == 0)
    argfflags |= TpFtRHPrefix;
  
  if (returnType == 0)
    sb.append("<returnType=0x0>");
  else if ((formatflags & TpFtACDKSignature) == 0)
  {
    if (formatflags & TpFtAcdkType &&
      returnType->isIntClazz() == true &&
      ClazzAttributesRes::hasAttribute((MetaInfo*)this, "__enumArgInfo"))
    {
      const ClazzEnumInfo* ei = (const ClazzEnumInfo*)ClazzAttributesRes::getAttribute((MetaInfo*)this, "__enumArgInfo").data;
      sb.append(ei->name);
    }
    else
    {
      returnType->toTypeString(sb, argfflags);
    }
    addSpace(sb, formatflags);
  }

  if (clazz != 0)
  {
    clazz->toTypeString(sb, formatflags & ~TpFtTypeDef & ~TpFtTypeDecl);
    addNsScope(sb, formatflags);
  }
  sb.append(name);
  int i = 0;
  int argcount = getArgumentCount();
  if (argcount == 0 && (formatflags & TpFtACDKSignature))
    return;
  addSyntaxElem(sb, "(", formatflags);

  for (i = 0; i < argcount; ++i)
  {
    if (i > 0)
      addSyntaxElem(sb, ", ", formatflags);
    methodArgs[i]->toTypeString(sb, argfflags);
  }
  if (formatflags & TpFtACDKSignature)
    return;

  sb.append(")");
  int excount = getExceptionsCount();
  for (i = 0; i < excount; ++i)
  {
    if (i == 0)
    {
      if (formatflags & TpFtAcdkType)
      {
        sb.append(" THROWS");
        sb.append(excount);
        sb.append("(");
      }
      else
        sb.append(" throws ");
    }
    else
      sb.append(" ,");
    exceptions[i]->toTypeString(sb, argfflags);
  }
  if (i > 0)
  {
    if (formatflags & TpFtAcdkType)
      sb.append(")");
  }
}

//static
void
ClazzMethodInfo::DefaultDispatchThrowableFunc(IN(::acdk::lang::RThrowable) ex)
{
  throw ex;
}

int
ClazzMethodInfo::getArgumentCount() const
{
  int i;
  if (methodArgs == 0)
    return 0;
  if (argumentCount > 0)
    return argumentCount;
  for (i = 0; methodArgs[i] != 0; ++i)
    ;
  return const_cast<ClazzMethodInfo*>(this)->argumentCount = i;
}

int
ClazzMethodInfo::getExceptionsCount() const
{
  if (exceptions == 0)
    return 0;
  int i;
  for (i = 0; exceptions[i] != 0; ++i)
    ;
  return i;
}

int
ClazzMethodArgInfo::getHashValue() const
{
  return MetaInfo::calcHashValue(flags) * 31 + type->getHashValue();
}

//static
int
ClazzMethodInfo::_calcHashValue(::acdk::lang::dmi::ClazzMethodArgInfo** args)
{
  if (args == 0)
    return 0;
  int ret = 0;
  for (int i = 0; args[i] != 0; ++i)
  {
    ret = ret * 31 + args[i]->getHashValue();
  }
  return ret;
}

bool equalsArgumentFlags(int f1, int f2)
{
  if (f1 == f2)
    return true;

  if ((f1 & MiAiIn) == MiAiIn && (f2 & MiAiOut) == 0)
    f1 &= ~MiAiIn;
  if ((f2 & MiAiIn) == MiAiIn && (f1 & MiAiOut) == 0)
    f2 &= ~MiAiIn;
  f1 &= ~MiAiHasDefaultInit;
  f2 &= ~MiAiHasDefaultInit;
  if (f1 == f2)
    return true;
  return false;
}

bool
ClazzMethodInfo::equals(const ClazzMethodInfo* other, int compareflags) const
{
  if ((compareflags & CompareName && compareflags & CompareArgs && compareflags & CompareFlags)
      && (compareflags & ~CompareName & ~CompareArgs & ~CompareFlags) == 0)
  {
    return getMethodSignatureHashValue() == other->getMethodSignatureHashValue();
  }
  if (other == this)
    return true;
  if (compareflags & CompareFlags)
  {
    int flag1 = flags & (MiMiConstructor | MiMiDestructor | MiStatic);
    int flag2 = other->flags & (MiMiConstructor | MiMiDestructor | MiStatic);
    if (flag1 != flag2)
      return false;
  }
  if (compareflags & CompareAccess)
  {
    int flag1 = flags & (MiPublic | MiPrivate | MiProtected);
    int flag2 = other->flags & (MiPublic | MiPrivate | MiProtected);
    if (flag1 != flag2)
      return false;
  }

  if (compareflags & CompareName)
  {
    if (getMetaInfo()->equalsName(*other->getMetaInfo()) == false)
      return false;
  }
  if (compareflags & CompareArgs)
  {
    int argcount1 = getArgumentCount();
    int argcount2 = other->getArgumentCount();
    if (argcount1 != argcount2)
      return false;
    for (int i = 0; i < argcount1; ++i)
    {
      if (methodArgs[i]->type != other->methodArgs[i]->type)
        return false;
      if (equalsArgumentFlags(methodArgs[i]->flags, other->methodArgs[i]->flags) == false)
        return false;
      if (compareflags & CompareArgNames)
      {
        if (methodArgs[i]->getMetaInfo()->equalsName(*other->methodArgs[i]->getMetaInfo()) == false)
          return false;
      }
    }
  }
  if (compareflags & CompareReturnType)
  {
    if (returnType != other->returnType)
      return false;
    int flag1 = flags & (MiMiInOut | MiMiByval);
    int flag2 = other->flags & (MiMiInOut | MiMiByval);
    if (flag1 != flag2)
      return false;
  }
  if (compareflags & CompareThrowables)
  {
    int argcount1 = getExceptionsCount();
    int argcount2 = other->getExceptionsCount();
    if (argcount1 != argcount2)
      return false;
    for (int i = 0; i < argcount1; ++i)
    {
      if (exceptions[i] != other->exceptions[i])
        return false;
    }
  }

  return true;
}

bool
ClazzMethodInfo_findVirtual(const ClazzInfo* ci, const ClazzMethodInfo* mi)
{
  int i;
  for (i = 0; i < ci->getMethodsCount(); ++i)
  {
    if (ci->methods[i]->flags & MiMiVirtual &&
        mi->equals(ci->methods[i], CompareName | CompareArgs | CompareFlags))
      return true;
  }
  for (i = 0; i < ci->getInterfacesCount(); ++i)
  {
    if (ClazzMethodInfo_findVirtual(ci->interfaces[i]->type, mi) == true)
      return true;
  }
  return false;
}

bool
ClazzMethodInfo::isVirtual(const ClazzInfo* clazz) const
{
  if (flags & MiMiVirtual)
    return true;
  if (clazz->getInterfacesCount() == 0)
    return false;
  return ClazzMethodInfo_findVirtual(clazz, this);
  for (int i = 0; i < clazz->getInterfacesCount(); ++i)
  {
    if (ClazzMethodInfo_findVirtual(clazz->interfaces[i]->type, this) == true)
      return true;
  }
  return false;
}

void
ClazzMethodInfo::_resolveParents(const ClazzInfo* ci) const
{

  _scopeParent = ci->getMetaInfo();
  int ac = getArgumentCount();
  for (int i = 0; i < ac; ++i)
    methodArgs[i]->_scopeParent = getMetaInfo();
}

int
ClazzMethodInfo::_calcMethodSignatureHashValue() const
{
  const_cast<ClazzMethodInfo*>(this)->_methodSignatureHashValue =
    MetaInfo::calcHashValue(flags) * 31 +
    getMetaInfo()->getNameHashCode() * 31 +
    _calcHashValue(methodArgs);
  return _methodSignatureHashValue;
}


void
ClazzMethodInfo::dispose()
{
  if (getMetaInfo()->isDelete() == false)
    return;
  getMetaInfo()->onDisposeMetaInfo();
  getMetaInfo()->dispose();
  MetaInfo::strdel(altlabel);

  MetaInfoChildsArray<ClazzMethodArgInfo>(methodArgs).dispose();

  MetaInfoChildsArray<ClazzInfo>(exceptions).dispose(false);

  delete this;
}

ClazzMethodInfo*
ClazzMethodInfo::clone(bool deep)
{
  ClazzMethodInfo* ev = createMetaInfo<ClazzMethodInfo>();
  ev->getMetaInfo()->copyFrom(getMetaInfo(), deep);
  ev->altlabel = MetaInfo::strdup(altlabel);
  ev->altlabelHashCode = altlabelHashCode;
  MetaInfoChildsArray<ClazzMethodArgInfo>(methodArgs).copyTo(ev->methodArgs, deep);
  MetaInfoChildsArray<ClazzInfo>(exceptions).copyTo(ev->exceptions, false);
  return ev;
}

void 
ClazzMethodInfo::_calcAltlabelHashCode() const
{
  if (altlabel == 0)
    const_cast<ClazzMethodInfo*>(this)->altlabelHashCode = 0;
  else
    const_cast<ClazzMethodInfo*>(this)->altlabelHashCode = String::calcHashCode(altlabel);
}

void
ClazzMethodInfo::addArgument(const ClazzMethodArgInfo* ai)
{
  if (getMetaInfo()->isDelete() == false)
    THROW1(DmiException, "Cannot add argument to compiled ClazzMethodInfo" + toTypeString(0, TpFtFormatStandard));
  MetaInfoChildsArray<ClazzMethodArgInfo>(methodArgs).push_back(const_cast<ClazzMethodArgInfo*>(ai));
}

void
ClazzMethodInfo::addThrowable(const ClazzInfo* ex)
{
  if (getMetaInfo()->isDelete() == false)
    THROW1(DmiException, "Cannot add throw spec to compiled ClazzMethodInfo" + toTypeString(0, TpFtFormatStandard));
  MetaInfoChildsArray<ClazzInfo>(exceptions).push_back(const_cast<ClazzInfo*>(ex));
}


void
ClazzMethodInfo::throwMethodNotFound(const ClazzInfo* clazz, IN(RString) fname, int flags, int formatFlags)
{
  StringBuffer sb;
  sb.append("Method not found: ");
  MetaInfo::flagsToTypeDecl(sb, flags, TpFtAcdkType);
  //sb.append(Modifier::toString(Modifier::accessMask(flags) | Modifier::staticMask(flags)));
  sb.append(" ");
  clazz->toTypeString(sb, formatFlags);
  sb.append("::");
  sb.append(fname);
  sb.append("(...)");
  THROW1(NoSuchMethodException, sb.toString());
}

/////////////////////////////    ClazzSuperInfo    ////////////////////////////////////////

void
ClazzSuperInfo::dispose()
{
  if (getMetaInfo()->isDelete() == false)
    return;
  getMetaInfo()->onDisposeMetaInfo();
  getMetaInfo()->dispose();
  type = 0;
  delete this;
}

ClazzSuperInfo*
ClazzSuperInfo::clone(bool deep)
{
  ClazzSuperInfo* ev = createMetaInfo<ClazzSuperInfo>();
  ev->getMetaInfo()->copyFrom(getMetaInfo(), deep);
  ev->type = type;
  return ev;
}

RString 
ClazzSuperInfo::toTypeString(int format) const
{
  // ### TODO implement me ClazzSuperInfo::toTypeString
  return "";
}


/////////////////////////////    ClazzInfo    ////////////////////////////////////////

int
ClazzInfo::_calcInterfacesCount() const
{
  if (interfaces == 0)
    return 0;
  int i = 0;
  for (; interfaces[i] != 0; ++i)
    ;
  return const_cast<ClazzInfo*>(this)->_interfacesCount = i;
}

int
ClazzInfo::_calcFieldsCount() const
{
  if (fields == 0)
    return 0;
  int i = 0;
  for (; fields[i] != 0; ++i)
    ;
  return const_cast<ClazzInfo*>(this)->_fieldsCount = i;
}

int
ClazzInfo::_calcMethodsCount() const
{
  if (methods == 0)
    return 0;
  int i = 0;
  for (; methods[i] != 0; ++i)
      ;
  return const_cast<ClazzInfo*>(this)->_methodsCount = i;
}

int
ClazzInfo::getHashValue() const
{
  if (isArray() == true && userInfo != 0)
  {
    const ClazzInfo* ci = (const ClazzInfo*)userInfo;
    return ci->getHashValue() * 31 + String::calcHashCode("Array");
  }
  return String::calcHashCode(ns) * 31 + getMetaInfo()->getNameHashCode();
}

jlong
ClazzInfo::_calcSerialVersionUID() const
{
  jlong erg = 0;
  erg = getHashValue();
  // ## TODO use intefaces, fields and methods to calc uid
  return const_cast<ClazzInfo*>(this)->_serialVersionUID = erg;
}

RString
ClazzInfo::toTypeString(int format) const
{
  StringBuffer sb;
  toTypeString(sb, format);
  return sb.toString();
}

void
ClazzInfo::toTypeString(StringBuffer& sb, int format) const
{
  if (format & TpFtAttributes)
    AttributeToTypeString((const MetaInfo*)this, sb, format);
  if (format & TpFtJavaSignature || format & TpFtLoadableClass || format & TpFtJavaSignature || format & TpFtACDKSignature)
    format |= TpFtFqName;
  if (format & TpFtTypeDef && format & TpFtAcdkType)
    format |= TpFtRHPrefix;

  if (this == ClazzInfo::getCharClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("C");
    else
      sb.append("char");
  }
  else if (this == ClazzInfo::getUcCharClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("U");
    else
      sb.append("ucchar");
  }
  else if (this == ClazzInfo::getByteClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("B");
    else
      sb.append("byte");

  }
  else if (this == ClazzInfo::getShortClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("S");
    else
      sb.append("short");

  }
  else if (this == ClazzInfo::getIntClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("I");
    else
      sb.append("int");

  }
  else if (this == ClazzInfo::getLongClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("J");
    else if (format & TpFtJavaType)
      sb.append("long");
    else
      sb.append("jlong");
  }
  else if (this == ClazzInfo::getDoubleClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("D");
    else
      sb.append("double");

  }
  else if (this == ClazzInfo::getFloatClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("F");
    else
      sb.append("float");

  }
  else if (this == ClazzInfo::getBoolClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("Z");
    else if (format & TpFtJavaType)
      sb.append("boolean");
    else
      sb.append("bool");
  }
  else if (this == ClazzInfo::getVoidClazz())
  {
    if (format & TpFtJavaSignature || format & TpFtACDKSignature)
      sb.append("V");
    else
      sb.append("void");

  }
  else
  {
    if (format & TpFtTypeDecl || format & TpFtTypeDef)
    {
      if (format & TpFtAcdkType)
        sb.append("class ");
      else
      {
        if ((this->flags & MiCiInterface) == MiCiInterface)
          sb.append("interface ");
        else
          sb.append("class ");
      }
      if (format & TpFtFqName)
      {
        UnitInfo::toTypeString(sb, this->ns, format, true);
      }
      sb.append(this->name);
      if ((format & TpFtTypeDef) == 0)
        return;

      sb.append("\n");
      if (interfaces != 0)
      {
        for (int i = 0; interfaces[i] != 0; ++i)
        {
          if (i != 0)
          {
            sb.append(", ");

          }
          else
          {
            if (format & TpFtAcdkType)
              sb.append(": ");
          }
          if (this->flags & MiCiInterface)
          {
            sb.append("extends ");
          }
          else if (interfaces[i]->flags & MiCiInterface)
          {
            sb.append("implements ");
          }
          else
            sb.append("extends ");
          interfaces[i]->type->toTypeString(sb, format & ~TpFtTypeDef & ~TpFtTypeDecl & ~TpFtRHPrefix);
          sb.append("\n");
        }
      } // end interfaces
      sb.append("{\n");
      if (fields != 0)
      {
        for (int i = 0; fields[i] != 0; ++i)
        {
          sb.append("  ");
          fields[i]->toTypeString(sb, format);
          sb.append(";\n");
        }
      } // end fields
      if (methods != 0)
      {
        for (int i = 0; methods[i] != 0; ++i)
        {
          sb.append("  ");
          methods[i]->toTypeString(sb, 0, format);
          sb.append(";\n");
        }
      } // end methods
      sb.append("}");
      if (format & TpFtAcdkType)
        sb.append(";");
      sb.append("\n");
    }
    else
    {
      if (format & TpFtJavaSignature || format & TpFtLoadableClass)
      {
        if (this->isArray() == true)
        {
          sb.append("[");
          reinterpret_cast<const ClazzInfo*>(this->userInfo)->toTypeString(sb, format);
        }
        else
        {
          UnitInfo::toTypeString(sb, this->ns, format, true);
          sb.append(this->name);
        }
      }
      else if (format & TpFtJavaType)
      {
        if (this->isArray() == true)
        {
          reinterpret_cast<const ClazzInfo*>(this->userInfo)->toTypeString(sb, format);
          sb.append("[]");
        }
        else
        {
          UnitInfo::toTypeString(sb, this->ns, format, true);
          sb.append(this->name);
        }
      }
      else
      {

        if (this->isArray() == true)
        {
          if (this->userInfo != 0)
          {
            const ClazzInfo* elci = reinterpret_cast<const ClazzInfo*>(this->userInfo);
            int subformat = format;
            if (format & TpFtRHPrefix && elci->isBasicClazz())
            {
              sb.append("R");
              subformat &= TpFtRHPrefix;
            }
            elci->toTypeString(sb, subformat);
          }
          sb.append("Array");
        }
        else
        {
          if (format & TpFtUnitName)
          {
            if (this->ns != 0 && *this->ns != 0)
              UnitInfo::toTypeString(sb, ns, format, true);
          }
          if (format & TpFtRHPrefix)
            sb.append("R");
          sb.append(this->name);
        }
      }
    }
  }
}

// static
const ClazzFieldInfo*
ClazzInfo::findField(const ClazzInfo*& clazz, IN(RString) fieldname, int flags)
{
  clazz = clazz->loadFullClazzInfo();
  for (int i = 0; i < clazz->getFieldsCount(); ++i)
  {
    const ClazzFieldInfo* fi = clazz->fields[i];
    if (fi->getMetaInfo()->equalsName(fieldname) == true && MetaInfo::checkMemberAccess(flags, fi->flags) == true)
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
      const ClazzFieldInfo*  cfi = findField(clazz, fieldname, flags | MiIvNoThrowIfNotFound);
      if (cfi != 0)
        return cfi;
      clazz = tclazz;
    }
  }
  if ((flags & MiIvNoThrowIfNotFound) == 0)
  {
    StringBuffer sb("Field not found: ");
    sb.append(MetaInfo::flagsToString(MetaInfo::accessMask(flags) | MetaInfo::staticMask(flags),
              FieldInfoExtFlags(0)));
    sb.append(" ");
    clazz->toTypeString(sb, TpFtJavaType);
    addNsScope(sb, TpFtJavaType);
    sb.append(fieldname);
    THROW1(NoSuchElementException, sb.toString());
  }
  return 0;
}

void
ClazzInfo::getFields(ClazzFieldInfoVec& fields, int flags) const
{
  const ClazzInfo* ci = loadFullClazzInfo();
  if ((flags & MiIvDeclared) == 0)
  {
    int subflags = flags;
    if (subflags & MiIvTransientCall)
      subflags |= MiProtected;
    for (int i = 0; i < ci->getInterfacesCount(); ++i)
    {
      ci->interfaces[i]->type->getFields(fields, subflags);
    }
  }
  for (int i = 0; i < ci->getFieldsCount(); ++i)
  {
    const ClazzFieldInfo* fi = ci->fields[i];
    if (MetaInfo::checkMemberAccess(flags , fi->flags) == true)
    {
      fields.push_back(fi);
    }
  }
}

//static
bool
ClazzInfo::isRestArg(const ClazzInfo* ci)
{
  return Class::getSingeltonArrayClazz(acdk::lang::dmi::DmiObject::clazzInfo()) == ci;
}
//static
bool
ClazzInfo::isNamedRestArg(const ClazzInfo* ci)
{
  return Class::getSingeltonArrayClazz(acdk::lang::dmi::DmiNamedArg::clazzInfo()) == ci;
}

const MetaInfo*
ClazzInfo::findMetaInfo(IN(RString) name, int flags) const
{
  const ClazzInfo* ci = this;
  if ((flags & MiMetaInfoTypeMask) == 0 || flags & MiFieldInfo)
  {
    const ClazzFieldInfo* fi = findField(ci, name, flags | MiIvNoThrowIfNotFound);
    if (fi != 0)
      return (const MetaInfo*)fi;
  }
  ci = this;
  const ClazzMethodInfo* mi = StdDispatch::lookupMethod(ci, name, flags | MiIvNoThrowIfNotFound);
  if (mi != 0)
    return (const MetaInfo*)mi;
  /*
    ### @todo throw exception, if not found MiIvNoThrowIfNotFound
  */
  return 0;
}


void
ClazzInfo::addMethod(const ClazzMethodInfo* mi)
{
  if (getMetaInfo()->isDelete() == false)
    THROW1(DmiException, "Cannot add method to compiled class: " + toTypeString(TpFtFormatStandard));
  MetaInfoChildsArray<ClazzMethodInfo>(methods).push_back(const_cast<ClazzMethodInfo*>(mi));
  _calcMethodsCount();

  mi->_nextScopeSibling = _firstChild;
  _firstChild = mi->getMetaInfo();
}

void
ClazzInfo::addField(const ClazzFieldInfo* fi)
{
  if (getMetaInfo()->isDelete() == false)
    THROW1(DmiException, "Cannot add field to compiled class: " + toTypeString(TpFtFormatStandard));
  MetaInfoChildsArray<ClazzFieldInfo>(fields).push_back(const_cast<ClazzFieldInfo*>(fi));
  _calcFieldsCount();
  fi->_nextSibling = _firstChild;
  _firstChild = fi->getMetaInfo();
}

void
ClazzInfo::addSuper(const ClazzSuperInfo* si)
{
  if (getMetaInfo()->isDelete() == false)
    THROW1(DmiException, "Cannot add super to compiled class: " + toTypeString(TpFtFormatStandard));
  MetaInfoChildsArray<ClazzSuperInfo>(interfaces).push_back(const_cast<ClazzSuperInfo*>(si));
  _calcInterfacesCount();
}


void
ClazzInfo::callClassInitializer() const
{
  const ClazzInfo* ci = this;
  acdk::lang::dmi::MetaObjectImpl mo((acdk::lang::dmi::MetaInfo*)ci);
  RMetaAttribute ma = mo.getMetaAttribute("__acdk_classinitfunction");
  if (ma == Nil)
    return;
  RString name = (RString)ma->value;
  name = name->replace('.', '/');
  name = name->replace("::", "/");
  int idx = name->lastIndexOf('/');
  RString cname;
  RString methodname;
  if (idx == -1)
  {
    cname = ci->toTypeString(TpFtLoadableClass | TpFtFqName);
    methodname = name;
  }
  else
  {
    cname = name->substr(0, idx);
    methodname = name->substr(idx + 1);
  }
  ScriptVarArray sa(0);
  Object::invokeStaticMethod(cname, methodname, sa, MiStatic);
}

void
ClazzInfo::callClassDeinitializer() const
{
   // ### TODO implement
}



//static
void
ClazzInfo::registerClazzInfo(const ClazzInfo* clazz)
{
  StringBuffer sb;
  if (clazz->ns != 0)
    sb << clazz->ns << "/";
  sb << clazz->name;

  const MetaInfo* mi  = MetaInfo::findMetaInfoNormalized(sb.toString());
  
  if (mi != 0 && (mi != (const MetaInfo*)clazz))
  {
    // ### TODO log to stdout, because exception will not be shown
    RString msg = SBSTR("ClazzInfo::registerClazzInfo: " << clazz->toTypeString() << " already bound by: " << mi->toString());
    printf(msg->c_str());
    THROW1(RuntimeException, msg);
  }
 
  //ClazzInfo_callStaticInitializer(clazz);
  if (clazz->flags & MiRegistered)
    return;
  DOUT("register clazz(" << (void*)clazz << "): " << clazz->name << " root->_next: " << (void*)clazz->_next);
  clazz->_next = _clazzRoot;
  _clazzRoot = const_cast<ClazzInfo*>(clazz);

  clazz->getMetaInfo()->registerInParent();

  if (clazz->flags & MiResolved)
    clazz->_resolveMemberParents();
  if (metaObjectListeners() != Nil)
    onRegisterMetaInfo(&Class::getSingeltonClass(clazz));
}


//static
void
ClazzInfo::registerArrayClazzInfo(const ClazzInfo* clazz)
{
  CLAZZINFOLOCKGUARD();
  clazz->_next = _arrayClazzRoot;
  _arrayClazzRoot = const_cast<ClazzInfo*>(clazz);
}


//static
void
ClazzInfo::unregisterClazzInfo(const ClazzInfo* clazz)
{
  //CLAZZINFOLOCKGUARD();
  DOUT("unregister(" << (void*)clazz << "): "  << clazz->name << " root=" << (void*)_clazzRoot);
  const ClazzInfo* ci = _clazzRoot;
  const ClazzInfo* tci = 0;

  for (; ci != 0; ci = ci->_next)
  {
    if (ci == clazz)
    {
      if (ci ==   _clazzRoot)
        _clazzRoot = _clazzRoot->_next;
      else
        tci->_next = ci->_next;
      break;
    }
    tci = ci;
  }
  if (metaObjectListeners() != Nil)
    onUnRegisterMetaInfo(&Class::getSingeltonClass(clazz));

  clazz->getMetaInfo()->unregisterFromParent();
}

//static
void
ClazzInfo::unregisterAllArrayClazzInfo()
{
  CLAZZINFOLOCKGUARD();
  
  for (const ClazzInfo* ci = _arrayClazzRoot; ci != 0; )
  {
    const ClazzInfo* tci = ci;
    ci = ci->_next;
    delete const_cast<ClazzInfo*>(tci);
  }
  _arrayClazzRoot = 0;
}


void
ClazzInfo_deinit()
{
  ClazzInfo::unregisterAllArrayClazzInfo();

  for (const ClazzInfo* ci = _clazzRoot; ci != 0; ci = ci->_next)
  {
    if (ci->thisClass != 0)
    {
      ci->thisClass->releaseRef();
      const_cast<ClazzInfo*>(ci)->thisClass = 0;
    }
  }

}

void
ClazzInfo::_resolveSupers(bool returnSuperIfAvail, bool throwExIfNotFound) const
{
  for (int i = 0; i < getInterfacesCount(); ++i)
  {
    if (interfaces[i]->type != 0)
     interfaces[i]->type = interfaces[i]->type->loadFullClazzInfo(returnSuperIfAvail, throwExIfNotFound);
  }
}

void
ClazzInfo::_resolveMemberParents() const
{
  if (type == 0)
    const_cast<ClazzInfo*>(this)->type = this;
  int i;
  for (i = 0; i < getFieldsCount(); ++i)
    fields[i]->_scopeParent = getMetaInfo();
  for (i = 0; i < getMethodsCount(); ++i)
    methods[i]->_resolveParents(this);
}



const ClazzInfo*
ClazzInfo::_loadFullClazzInfo(bool returnSuperIfAvail, bool throwExIfNotFound) const
{
  DOUT("LoadFullClazzInfo: " << name << ", " << isResolved());
  if (ClazzInfo_load_acdk_core_metainf())
  {
    DOUT("OK ClazzInfo_load_acdk_core_metainf");
    if (isResolved() == true)
    {
      DOUT("Clazz resolved: " << name << ", " << flags);
      _resolveMemberParents();
      _resolveSupers(returnSuperIfAvail, throwExIfNotFound);
        //System::out->println(RString("Class is resolved: ") + name);
      return this;
    }
  }
  if (isArray() == true)
  {
    const ClazzInfo* elci = (const ClazzInfo*)userInfo;
    elci->loadFullClazzInfo(returnSuperIfAvail, throwExIfNotFound);
    ClazzInfo* ci = const_cast<ClazzInfo*>(this);
    ci->fields =  ObjectArrayBaseImpl::clazzInfo()->fields;
    ci->methods =  ObjectArrayBaseImpl::clazzInfo()->methods;
    ci->_scopeParent = elci->_scopeParent;
    ci->flags |= ::acdk::lang::dmi::MiResolved;
    _resolveSupers(returnSuperIfAvail, throwExIfNotFound);
    return this;
  }
  RString libname = toTypeString(TpFtAcdkType | TpFtFqName );
  DOUT("try load MetaInfoLibrary: " << libname->c_str());
  ClassLoader::getSystemClassLoader()->loadMetaInfoLibrary(libname);
  if (isResolved() == true)
  {
    DOUT("Clazz resolved: " << name << ", " << flags);
    _resolveMemberParents();
    _resolveSupers(returnSuperIfAvail, throwExIfNotFound);
    return this;
  }
  //System::out->println(RString("Class load meta info library had no effect: ") + name);
  if (returnSuperIfAvail == true)
  {
    const ClazzInfo* ci;
    int i;
    for (i = 0; i < getInterfacesCount(); ++i)
    {
      ci = interfaces[i]->type;
      if (ci == 0)
        continue;
      ci = ci->loadFullClazzInfo(false, false);
      if (ci != 0)
        return ci;
    }
    for (i = 0; i < getInterfacesCount(); ++i)
    {
      ci = interfaces[i]->type;
      if (ci == 0)
        continue;
      ci = ci->loadFullClazzInfo(true, false);
      if (ci != 0)
        return ci;
    }
  }

  if (throwExIfNotFound == true)
    THROW1(ClassNotFoundException, "Metainfo for class not available: " + toTypeString());
  _resolveMemberParents();
  _resolveSupers(returnSuperIfAvail, throwExIfNotFound);
  return 0;
}

//static
const ClazzInfo*
ClazzInfo::findClazzInfo(IN(RString) rclassname, IN(RString) rnamespacename)
{
  // ### @todo ClazzInfo::findClazzInfo(IN(RString) rclassname, IN(RString) rnamespacename) remove this function

  CLAZZINFOLOCKGUARD();
  RString classname = rclassname;
  RString namespacename = rnamespacename;

  RString namesp = namespacename;
  RString tclassname = classname;
  RString tnamespace;
  if (classname->charAt(0) == 'L' && tclassname->length() > 2 && Character::isUpperCase(classname->charAt(1)) &&
    classname->charAt(classname->length() - 1) == ';')
  {
    tclassname = tclassname->substr(1, tclassname->length() - 1);
  }
  else if (classname->charAt(0) == '[')
  {
    // ### TODO ???
  }

  if (namesp->length() == 0 && tclassname->indexOf(':') != -1)
  {
    int lpos = tclassname->lastIndexOf(':');
    tnamespace = tclassname->substr(0, lpos);
    tclassname = tclassname->substr(lpos + 1);
  }
  //classname = tclassname;
  if (tnamespace != Nil)
    namesp = tnamespace;
  //cerr << "lookup symbol: " << (namesp ? namesp : "") << "|" << classname << endl;
  const ClazzInfo* c = 0;
  if (namesp != Nil && namesp->length() > 0)
    c = reinterpret_cast<const ClazzInfo*>(MetaInfo::findMetaInfo(namesp + "/" + tclassname, MiClazzInfo, false));
  if (c != 0)
    return c;
  
  for (c = _clazzRoot; c != 0; c = c->_next)
  {
    if (c->getMetaInfo()->equalsName(tclassname) == true)
    {
      if (namesp->length() == 0)
        return c;
      if (namesp->equals(c->ns) == true)
        return c;
    }
  }
  return 0;
}
/*
//static
const ClazzInfo*
ClazzInfo::findClazzInfo(IN(RString) name, bool tryLoad)
{
  if (name == Nil)
    return 0;
  return findClazzInfo(*name, tryLoad);
}
*/

//static
const ClazzInfo*
ClazzInfo::findClazzInfo(IN(RString) name, bool tryLoad)
{
  RString namesp;
  RString pcls = name->replace(".", "/");
  pcls = pcls->replace("::", "/");
  int pos;
  if ((pos = pcls->indexOf('/')) == 0) {
    pcls = pcls->substr(1);
  }

  const ClazzInfo* clazz = 0;
  if (pcls->charAt(0) == '[')
  {
    clazz = ClazzInfo::findArrayClazzInfo(pcls->substr(1));
  }
  else if (pcls->endsWith("Array") == true)
  {
    clazz = ClazzInfo::findArrayClazzInfo(pcls->substr(0, pcls->length() - 5));
  }
  else
  {
    if ((pos = pcls->lastIndexOf('/')) != -1)
    {
      namesp = pcls->substr(0, pos);
      pcls = pcls->substr(pos + 1);
    }
    clazz = ClazzInfo::findClazzInfo(pcls, namesp == Nil ? String::emptyString() : namesp);
  }
  if (clazz != 0)
    return clazz;
  if (tryLoad == false)
    return 0;
  RClassLoader cl = ClassLoader::getSystemClassLoader();
  if (cl->loadClassLibrary(name) == true)
    return findClazzInfo(name, false);
  return 0;
}

/*
const ClazzInfo*
ClazzInfo::findArrayClazzInfo(IN(RString) elementname)
{
  return findArrayClazzInfo(&elementname);
}
*/

const ClazzInfo*
ClazzInfo::findArrayClazzInfo(IN(RString) elementname, bool tryLoad)
{
  RString pcls = elementname;
  const ClazzInfo* clazz;
  if (elementname->charAt(0) == '[')
  {
    RString nele = elementname->substr(1);
    clazz = findArrayClazzInfo(nele, tryLoad);
  }
  else if (elementname->endsWith("Array") == true)
  {
    clazz = ClazzInfo::findArrayClazzInfo(elementname->substr(0, elementname->length() - 5), tryLoad);
  }
  else
  {
    clazz = ClazzInfo::findClazzInfo(elementname, tryLoad);
    /*
    RString namesp;
    int pos;
    if ((pos = pcls->lastIndexOf('/')) != -1) {
      namesp = pcls->substr(0, pos);
      pcls = pcls->substr(pos + 1);
    }
    clazz = ClazzInfo::findClazzInfo(pcls, namesp == Nil ? String::emptyString() : namesp, tryLoad);
    if (clazz == 0)
      return 0;
    */
  }
  const ClazzInfo* arrayclazz = ClazzInfo::findArrayClazzInfo(clazz);
  if (arrayclazz != 0)
    return arrayclazz;
  arrayclazz = Class::getSingeltonArrayClazz(clazz);
  return arrayclazz;
}


//static
const ClazzInfo*
ClazzInfo::findArrayClazzInfo(const ClazzInfo* elementtype)
{
  CLAZZINFOLOCKGUARD();

  for (const ClazzInfo* ci = _arrayClazzRoot; ci != 0; ci = ci->_next)
  {
    if (ci->userInfo == (void*)elementtype)
      return ci;
  }
  return 0;
}


bool
ClazzInfo::isArray() const
{
  return flags & dmi::MiCiArray;
}

int
ClazzInfo::_calcCollectableFieldsCount() const
{
  if (isArray() == true)
    return 1;
  int count = 0;
  // ### TODO calculate current collectable fields
  //int count = collectableFields;
  if (interfaces[0] != 0 && interfaces[0]->type  != 0)
    count += interfaces[0]->type->getCollectableFieldsCount();
  return const_cast<ClazzInfo*>(this)->_collectableFields = count;
}


//static
void
ClazzInfo::findClasses(IN(RString) ns, ::acdk::lang::sys::core_vector<const ClazzInfo*>& ret, bool recursive)
{
  for (const ClazzInfo* ci = _clazzRoot; ci != 0; ci = ci->_next)
  {
    if (ns->equals(ci->ns) == true)
    {
      ret.push_back(ci);

    } else if (recursive == true) {

      String ons(ci->ns);
      if (ons.startsWith(ns) == true)
        ret.push_back(ci);
    }
  }
}



namespace {

  bool
containsString(::acdk::lang::sys::core_vector<const char*>& vec, const char* text)
{
  ::acdk::lang::sys::core_vector<const char*>::iterator it = vec.begin();
  ::acdk::lang::sys::core_vector<const char*>::iterator end = vec.end();
  for (; it != end; ++it)
  {
    if (strcmp(*it, text) == 0)
      return true;
  }
  return false;
}

} // anon namespace

///static
void
ClazzInfo::findNamespaces(IN(RString) ns, ::acdk::lang::sys::core_vector<const char*>& ret, bool recursive)
{

  for (const ClazzInfo* ci = _clazzRoot; ci != 0; ci = ci->_next)
  {
    String ons(ci->ns);
    if (ons.startsWith(ns) == true
        && ns->equals(ci->ns) == false
        && containsString(ret, ci->ns) == false)
    {
      if (recursive == true)
      {
        ret.push_back(ci->ns);
      }
      else
      {
        if (strlen(ci->ns) > (size_t)ns->length())
        {
          const char* ptr = ci->ns + ns->length() + 1;
          if (strstr(ptr, "/") == 0)
            ret.push_back(ci->ns);
        }
      }
    }
  }
}


const ClazzSuperInfo*
ClazzInfo::findSuper(IN(RString) supername) const
{
  if (interfaces == 0)
    return 0;
  for (int i = 0; interfaces[i] != 0; ++i)
  {
    if (interfaces[i]->type->getMetaInfo()->equalsName(supername) == true)
      return interfaces[i];
    RString fqname = interfaces[i]->type->toTypeString(acdk::lang::dmi::TpFtJavaType | acdk::lang::dmi::TpFtFqName);
    if (fqname->equals(supername) == true)
      return interfaces[i];
    const ClazzSuperInfo* su = interfaces[i]->type->findSuper(supername);
    if (su != 0)
      return su;
  }
  return 0;
}
/*
const ClazzSuperInfo*
ClazzInfo::findSuper(IN(RString) supername) const
{
  return findSuper(supername);
}
*/
enum NumericType
{
  bool_type = 0,
  char_type = 1, // bad hack for standard-C-conversions char <=> int
  byte_type = 2,
  short_type = 3,
  int_type = 4,
  long_type = 5,
  float_type = 6,
  double_type = 7
};


int
getTypeNumeric(const ClazzInfo* fromType)
{
  if (fromType->isBoolClazz() == true) {
	  return bool_type;
  } else if (fromType->isCharClazz() == true) {
    return char_type; // or many many things won't work anymore
  } else if (fromType->isByteClazz() == true) {
    return byte_type;
  } else if (fromType->isShortClazz() == true) {
    return short_type;
  } else if (fromType->isIntClazz() == true) {
    return int_type;
  } else if (fromType->isLongClazz() == true) {
    return long_type;
  } else if (fromType->isFloatClazz() == true) {
    return float_type;
  } else if (fromType->isDoubleClazz() == true) {
    return double_type;
  } else {
    return -1;
  }
}

int
ClazzInfo::assignDistance(const ClazzInfo* fromType) const
{
  if (fromType == 0)
    return -1;

  const ClazzInfo* toType = this;

  if (toType == fromType)
    return 0;

  if (fromType->isBasicClazz() == true)
  {
    if (toType->isBasicClazz() == false)
      return -1;
    int st, tt;
    st = getTypeNumeric(fromType);
    if (st == -1)
      return -1;
    if (st == bool_type)
      return -1;
    tt = getTypeNumeric(toType);
    if (tt > st)
      return (tt - st);
    else
      return (256 + st - tt);
  }
  if (fromType->isArray() == true && toType->isArray() == true)
  {
    return ((const ClazzInfo*)toType->userInfo)->assignDistance((const ClazzInfo*)fromType->userInfo);
  }
  if (fromType->interfaces == 0)
    return -1;
  int dist;
  int bestdist = 100000;
  int bestiface = -1;
  for (int i = 0; fromType->interfaces[i] != 0; ++i)
  {
    dist = toType->assignDistance(fromType->interfaces[i]->type);
    if (dist == -1)
      continue;
    dist =  dist + 1;
    if (dist < bestdist)
    {
      bestdist = dist;
      bestiface = i;
    }
  }
  if (bestiface == -1)
    return -1;
  return bestdist;
}

namespace {



bool checkBasicClazzInfo(const ClazzInfo* to, const ClazzInfo* from, bool allowWeakCasts)
{
  if (to->isBoolClazz() == true&& allowWeakCasts == true)
  {
    if (from == Boolean::clazzInfo())
      return true;
    return false;
  }
  if (to->isCharClazz() && allowWeakCasts == true)
  {
    if (from == Character::clazzInfo())
      return true;
    return false;
  }
  if (to->isUcCharClazz() == true)
  {
    if (from == Character::clazzInfo())
      return true;
    if (allowWeakCasts == true)
    {
      if (from == UnicodeCharacter::clazzInfo())
        return true;
      return false;
     }
     return false;
  }
  if (to->isIntegerTypeClazz() == true)
  {
    if (from->isIntegerTypeClazz() == true)
      return true;
    if (allowWeakCasts == true)
    {
      if (from->isFloatingTypeClazz() == true)
        return true;
      if (Number::clazzInfo()->assignableFrom(from) == true)
        return true;
    }
    return false;
  }
  if (to->isFloatingTypeClazz() == true)
  {
    if (from->isNumberTypeClazz() == true)
      return true;
    if (allowWeakCasts == true)
    {
      if (Number::clazzInfo()->assignableFrom(from) == true)
        return true;
    }
    return false;
  }
  return false;
}


} // anon ns

bool
ClazzInfo::assignableFrom(const ClazzInfo* from) const
{
  if (this == 0 || from == 0)
    return false;
  if (this == from)
    return true;
  if (isBasicClazz() || from->isBasicClazz())
    return checkBasicClazzInfo(this, from, true);
  if (this->isArray() == true && from->isArray() == true)
  {
    const ClazzInfo* thisM = (const ClazzInfo*)this->userInfo;
    const ClazzInfo* fromM = (const ClazzInfo*)from->userInfo;
    return thisM->assignableFrom(fromM);
  }
  if (from->interfaces == 0)
    return false;
  for (int i = 0; from->interfaces[i]; i++)
    if (assignableFrom(from->interfaces[i]->type) == true)
      return true;

  return false;
}


const ClazzInfo*
ClazzInfo::getArrayElementType() const
{
  if (isArray() == true)
    return getElementClazzInfo()->getArrayElementType();
  return this;
}

int
ClazzInfo::getArrayDims() const
{
  if (isArray() == true)
    return getElementClazzInfo()->getArrayDims() + 1;
  return 0;
}

int
ClazzInfo::getEnumerationValue(IN(RString) key) const
{
  // ### TODO implement enumeration value
  return -1;
}

ScriptVar
ClazzInfo::createInitializedValue() const
{
  if (isBoolClazz() == true)
    return acdk::lang::dmi::ScriptVar(false);
  if (isCharClazz() == true)
    return acdk::lang::dmi::ScriptVar(char(0));
  if (isByteClazz() == true)
    return acdk::lang::dmi::ScriptVar(byte(0));
  if (isShortClazz() == true)
    return acdk::lang::dmi::ScriptVar(short(0));
  if (isIntClazz() == true)
    return acdk::lang::dmi::ScriptVar(int(0));
  if (isLongClazz() == true)
    return acdk::lang::dmi::ScriptVar(jlong(0));
  if (isFloatClazz() == true)
    return acdk::lang::dmi::ScriptVar(float(0.0));
  if (isDoubleClazz() == true)
    return acdk::lang::dmi::ScriptVar(double(0.0));
  return acdk::lang::dmi::ScriptVar(RObject());
}

//static
void
ClazzInfo::addRegisterClazzHook(ClazzInfoCallBack cicb)
{
  // ### implement or remove!
}

//static
void
ClazzInfo::removeRegisterClazzHook(ClazzInfoCallBack cicb)
{
  // ### implement?
}

//static
void
ClazzInfo::forEachClazzInfo(ClazzInfoCallBack cicb)
{
  for (const ClazzInfo* ci = _clazzRoot; ci != 0; ci = ci->_next)
  {
    if (cicb(ci, 0) == false)
      break;
  }
}

//static
void
ClazzInfo::forEachArrayClazzInfo(ClazzInfoCallBack cicb)
{

  for (const ClazzInfo* ci = _arrayClazzRoot; ci != 0; ci = ci->_next)
  {
    if (cicb(ci, 0) == false)
      break;
  }
}

//static
const ClazzInfo*
ClazzInfo::getClazzInfoRoot()
{
  return _clazzRoot;
}

//static
const ClazzInfo*
ClazzInfo::getArrayClazzInfoRoot()
{
  return _arrayClazzRoot;
}


void
NamedScopedParentMetaInfo_replaceChild(const NamedScopedParentMetaInfo* This,
                                       const NamedScopedMetaInfo* find,
                                       const NamedScopedMetaInfo* repl
                                       )
{
  if (This == 0)
    return;
  repl->_scopeParent = This;
  if (This->_firstChild == find)
  {
    repl->_nextScopeSibling = This->_firstChild->_nextScopeSibling;
    This->_firstChild = repl;
    return;
  }
  const NamedScopedMetaInfo* it = This->_firstChild;
  while (it != 0)
  {
    if (it->_nextScopeSibling == find)
    {
      it->_nextScopeSibling = repl;
      repl->_nextScopeSibling = find->_nextScopeSibling;
      return;
    }
    it = it->_nextScopeSibling;
  }
}


void UnitInfo::registerUnitInfo(UnitInfo* ui)
{
  RString uiname = ui->ns;
  if (uiname == Nil)
  {
    uiname = ui->name;
  }
  else
  {
    uiname = uiname + "/" + ui->name;
  }

  UnitInfo* fui = const_cast<UnitInfo*>(reinterpret_cast<const UnitInfo*>(MetaInfo::findMetaInfoNormalized(uiname, MiUnitInfo, false)));
  if (fui == 0)
  {
    if (ui->_scopeParent == 0)
    {
      const UnitInfo* parent = _unitInfoRoot;
      if (ui->ns != 0 && *ui->ns != 0)
        ui->_scopeParent = UnitInfo::findCreateUnit(ui->ns);
      else
        ui->_scopeParent = _unitInfoRoot;
    }
    ui->_nextScopeSibling = ui->_scopeParent->_firstChild;
    ui->_scopeParent->_firstChild = ui->getMetaInfo();
    return;
  }
  const NamedScopedMetaInfo* child = fui->_firstChild;
  while (child != 0)
  {
    const NamedScopedMetaInfo* nchild = child->_nextScopeSibling;
    child->_scopeParent = ui->getMetaInfo();
    child =  nchild;
  }
  NamedScopedParentMetaInfo_replaceChild(fui->_scopeParent->getMetaInfo(), fui->getMetaInfo(), ui->getMetaInfo());
  ui->_firstChild = fui->_firstChild;
  fui->_firstChild = 0;
  fui->_scopeParent = 0;
  fui->_nextScopeSibling = 0;
  fui->getMetaInfo()->onReplaceMetaInfo(ui->getMetaInfo());
  fui->dispose();
}

const ClazzInfo*
ClazzInfo::createArrayClazzInfo(int dims) const
{
  const ClazzInfo* ci = this;
  for (; dims > 0; --dims)
  {
    const ClazzInfo* ar = ClazzInfo::findArrayClazzInfo(ci);
    if (ar == 0)
      break;
    ci = ar;
  }
  for (; dims > 0; --dims)
  {
    ci = ::acdk::lang::Class::getUnInitializedSingeltonArrayClazz(ci);
  }
  return ci;
}

void
ClazzInfo::dispose()
{
  unregisterClazzInfo(this);
  if (getMetaInfo()->isDelete() == false)
    return;
  getMetaInfo()->onDisposeMetaInfo();
  getMetaInfo()->dispose();
  MetaInfoChildsArray<ClazzSuperInfo>(interfaces).dispose();

  MetaInfoChildsArray<ClazzFieldInfo>(fields).dispose();

  MetaInfoChildsArray<ClazzMethodInfo>(methods).dispose();

}

ClazzInfo*
ClazzInfo::clone(bool deep)
{
  ClazzInfo* ev = createMetaInfo<ClazzInfo>();
  ev->getMetaInfo()->copyFrom(getMetaInfo(), deep);

  MetaInfoChildsArray<ClazzSuperInfo>(interfaces).copyTo(ev->interfaces, deep);
  MetaInfoChildsArray<ClazzFieldInfo>(fields).copyTo(ev->fields, deep);
  MetaInfoChildsArray<ClazzMethodInfo>(methods).copyTo(ev->methods, deep);
  ev->_interfacesCount = _interfacesCount;
  ev->_fieldsCount = _fieldsCount;
  ev->_methodsCount = _methodsCount;
  ev->creator = creator;
  ev->array_creator = array_creator;
  ev->array_array_creator = array_array_creator;
  ev->dynamic_dispatch  = dynamic_dispatch;
  ev->static_dispatch = static_dispatch;
  ev->_collectableFields = _collectableFields;
  ev->userInfo = userInfo;

  return ev;
}

bool
ClazzMethodInfo_isOverloaded(const ClazzMethodInfo* mi, const ClazzInfo* ci)
{
  //  ### @todo ClazzMethodInfo_isOverloaded Implement me
  return false;
}

/////////////////////////////    RegisterUnitInfo    ////////////////////////////////////////

RegisterUnitInfo::RegisterUnitInfo(const UnitInfo* ui, InitUnitInfoFunc initfunc, DeinitUnitInfoFunc deinitfunc)
: _unitInfo(ui)
, _deinitFunc(deinitfunc)
{
  UnitInfo::registerUnitInfo(const_cast<UnitInfo*>(ui));
}

RegisterUnitInfo::~RegisterUnitInfo()
{
  if (_deinitFunc != 0)
    _deinitFunc(_unitInfo);
}



//static 
void 
MetaObject::registerListener(IN(RMetaObjectListener) listner)
{
  getMetaObjectListeners()->append(listner);
}

//static 
void 
MetaObject::unRegisterListener(IN(RMetaObjectListener) listner)
{
  OUT(RMetaObjectListenerArray) la = metaObjectListeners();
  if (la == Nil)
    return;
  for (int i = 0; i < la->length(); ++i)
  {
    if (la[i] == listner)
    {
      la->remove(i);
      break;
    }
  }
  if (la->length() == 0)
    la = Nil;
}

/////////////////////////////    basic types    ////////////////////////////////////////
// #### move into own cpp

RObject
create_char_array(int size)
{
  RcharArray a = new charArray(size);
  for (charArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = 0;
  return &a;
}

#define BasicTypeFlags MiClazzInfo | MiCiBasicType | MiResolved

ClazzInfo char_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "char", // name of class
  -1,
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &char_clazzInfo, // type
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_char_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0, //  mutable ClazzInfo* _next;
};

RegisterClazzInfo __register_char_clazzInfo(&char_clazzInfo);

RObject
create_ucchar_array(int size)
{
  RuccharArray a = new uccharArray(size);
  for (uccharArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = 0;
  return &a;
}

ClazzInfo ucchar_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "ucchar", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &ucchar_clazzInfo,
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_ucchar_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0, //  mutable ClazzInfo* _next;
};

RegisterClazzInfo __register_ucchar_clazzInfo(&ucchar_clazzInfo);



RObject
create_byte_array(int size)
{
  RbyteArray a = new byteArray(size);
  for (byteArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = 0;
  return &a;
}

ClazzInfo byte_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "byte", // name of class
  -1, // nameHashCode
   "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
   &byte_clazzInfo,
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_byte_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0, //  mutable ClazzInfo* _next;
};

RegisterClazzInfo __register_byte_clazzInfo(&byte_clazzInfo);

RObject
create_short_array(int size)
{
  RshortArray a = new shortArray(size);
  for (shortArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = 0;
  return &a;
}

ClazzInfo short_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "short", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &short_clazzInfo,
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_short_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0, //  mutable ClazzInfo* _next;
};
RegisterClazzInfo __register_short_clazzInfo(&short_clazzInfo);


RObject
create_int_array(int size)
{
  RintArray a = new intArray(size);
  for (intArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = 0;
  return &a;
}

ClazzInfo int_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "int", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &int_clazzInfo,
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_int_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0, //  mutable ClazzInfo* _next;
};
RegisterClazzInfo __register_int_clazzInfo(&int_clazzInfo);

RObject
create_long_array(int size)
{
  RlongArray a = new longArray(size);
  for (longArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = 0;
  return &a;
}

ClazzInfo long_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "jlong", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &long_clazzInfo,
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_long_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0, //  mutable ClazzInfo* _next;
};
RegisterClazzInfo __register_long_clazzInfo(&long_clazzInfo);


RObject
create_float_array(int size)
{
  RfloatArray a = new floatArray(size);
  for (floatArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = 0.0;
  return &a;
}

ClazzInfo float_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "float", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &float_clazzInfo,
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_float_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0, //  mutable ClazzInfo* _next;
};
RegisterClazzInfo __register_float_clazzInfo(&float_clazzInfo);


RObject
create_double_array(int size)
{
  RdoubleArray a = new doubleArray(size);
  for (doubleArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = 0.0;
  return &a;
}

ClazzInfo double_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "double", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &double_clazzInfo,
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_double_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0 //  mutable ClazzInfo* _next;
};
RegisterClazzInfo __register_double_clazzInfo(&double_clazzInfo);


RObject
create_bool_array(int size)
{
  RboolArray a = new boolArray(size);
  for (boolArray::iterator it = a->begin(); it < a->end(); ++it)
    *it = false;
  return &a;
}
ClazzInfo bool_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "bool", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &bool_clazzInfo,
  0, // _firstChild
  0, //ClazzSuperInfo** interfaces;
  0, // int interfacesCount;
  0, // ClazzFieldInfo** fields;
  0, // int fieldsCount;
  0, //ClazzMethodInfo** methods;
  0, //  int methodsCount;
  0, //  ObjectCreator creator;
  create_bool_array, //  ArrayCreator array_creator ;
  0, // ArrayArrayCreator array_array_creator;
  0, //  Class* thisClass;
  0,   //  jlong serialVersionUID;
  0, //  StandardDispatchFunction static_dispatch;
  0, //  int collectableFields;
  0, //  mutable void *userInfo;
  0, //  mutable UnitInfo* unitInfo;
  0 //  mutable ClazzInfo* _next;
};

RegisterClazzInfo __register_bool_clazzInfo(&bool_clazzInfo);

ClazzInfo void_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "void", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &void_clazzInfo,
  0, // _firstChild
  0, // ClazzSuperInfo** interfaces;
  0, // ClazzFieldInfo** fields;
  0, // ClazzMethodInfo** methods;
  0, // ObjectCreator creator; create-function for cloning/serializing
  0, // Class* thisClass;
  0, // jlong serialVersionUID
  0, // void *userInfo; slot for user type
  0, // UnitInfo* unitInfo
  0 // ClazzInfo* _next;
};

RegisterClazzInfo __register_void_clazzInfo(&void_clazzInfo);

ClazzInfo unknown_clazzInfo =
{
  BasicTypeFlags, // clazz-flags
  0, // MetaInfo
  "<unknown>", // name of class
  -1, // nameHashCode
  "", // the namespace
  0, // _scopeParent
  0, // _nextScopeSibling
  &unknown_clazzInfo,
  0, // _firstChild
  0, // ClazzSuperInfo** interfaces;
  0, // ClazzFieldInfo** fields;
  0, // ClazzMethodInfo** methods;
  0, // ObjectCreator creator; create-function for cloning/serializing
  0, // Class* thisClass;
  0, // jlong serialVersionUID
  0, // void *userInfo; slot for user type
  0, // UnitInfo* unitInfo
  0 // ClazzInfo* _next;
};
RegisterClazzInfo __register_unknown_clazzInfo(&unknown_clazzInfo);

//static
const ClazzInfo*
ClazzInfo::getUnknownBasicClazz()
{
  return &unknown_clazzInfo;
}

//static
const ClazzInfo*
ClazzInfo::getCharClazz()
{
  return &char_clazzInfo;
}
//static
const ClazzInfo*
ClazzInfo::getUcCharClazz()
{
  return &ucchar_clazzInfo;
}

//static
const ClazzInfo*
ClazzInfo::getByteClazz()
{
  return &byte_clazzInfo;
}


//static
const ClazzInfo*
ClazzInfo::getShortClazz()
{
  return &short_clazzInfo;
}

//static
const ClazzInfo*
ClazzInfo::getIntClazz()
{
  return &int_clazzInfo;
}

//static
const ClazzInfo*
ClazzInfo::getLongClazz()
{
  return &long_clazzInfo;
}

//static
const ClazzInfo*
ClazzInfo::getFloatClazz()
{
  return &float_clazzInfo;
}

//static
const ClazzInfo*
ClazzInfo::getDoubleClazz()
{
  return &double_clazzInfo;
}

//static
const ClazzInfo*
ClazzInfo::getBoolClazz()
{
  return &bool_clazzInfo;
}

//static
const ClazzInfo*
ClazzInfo::getVoidClazz()
{
  return &void_clazzInfo;
}

} // dmi
} // lang
} // acdk
