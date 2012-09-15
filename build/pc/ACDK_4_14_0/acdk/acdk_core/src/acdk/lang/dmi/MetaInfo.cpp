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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/MetaInfo.cpp,v 1.26 2005/04/09 19:26:52 kommer Exp $


#include <acdk.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/ClassLoader.h>
#include <acdk/lang/sys/core_system.h>
#include "ClazzAttributesRes.h"

namespace acdk {
namespace lang {
namespace dmi {

typedef sys::core_vector<MetaInfoChangeListener*> MetaInfoChangeListenerArray;
MetaInfoChangeListenerArray&
getMetaInfoChangeListener()
{
  static MetaInfoChangeListenerArray _listener;
  return _listener;
}

void 
MetaInfo::registerMetaInfoListener(MetaInfoChangeListener* listner)
{
  getMetaInfoChangeListener().push_back(listner);
}

void 
MetaInfo::unRegisterMetaInfoListener(MetaInfoChangeListener* listner)
{
  MetaInfoChangeListenerArray& listeners = getMetaInfoChangeListener();
  MetaInfoChangeListenerArray::iterator it = listeners.find(listner);
  if (it != listeners.end())
    getMetaInfoChangeListener().erase(it);
}

  /// internal helper to call MetaInfoChangeListener callbacks 
void 
MetaInfo::onNewMetaInfo()
{
  if (acdk::lang::sys::core_system::getState() == acdk::lang::sys::AfterMain)
    return;
  MetaInfoChangeListenerArray& listeners = getMetaInfoChangeListener();
  MetaInfoChangeListenerArray::iterator it = listeners.begin();
  MetaInfoChangeListenerArray::iterator end = listeners.end();
  for (; it != end; ++it)
    (*it)->onNewMetaInfo(this);
}

void 
MetaInfo::onResolvedMetaInfo()
{
  if (acdk::lang::sys::core_system::getState() == acdk::lang::sys::AfterMain)
    return;
  
  MetaInfoChangeListenerArray& listeners = getMetaInfoChangeListener();
  MetaInfoChangeListenerArray::iterator it = listeners.begin();
  MetaInfoChangeListenerArray::iterator end = listeners.end();
  for (; it != end; ++it)
    (*it)->onResolvedMetaInfo(this);
}

void 
MetaInfo::onReplaceMetaInfo(MetaInfo* newMetaInfo)
{
  if (acdk::lang::sys::core_system::getState() == acdk::lang::sys::AfterMain)
    return;
  
  MetaInfoChangeListenerArray& listeners = getMetaInfoChangeListener();
  MetaInfoChangeListenerArray::iterator it = listeners.begin();
  MetaInfoChangeListenerArray::iterator end = listeners.end();
  for (; it != end; ++it)
    (*it)->onReplaceMetaInfo(this, newMetaInfo);
}

void 
MetaInfo::onUnregisterMetaInfo()
{
  if (acdk::lang::sys::core_system::getState() == acdk::lang::sys::AfterMain)
    return;
  
  MetaInfoChangeListenerArray& listeners = getMetaInfoChangeListener();
  MetaInfoChangeListenerArray::iterator it = listeners.begin();
  MetaInfoChangeListenerArray::iterator end = listeners.end();
  for (; it != end; ++it)
    (*it)->onUnregisterMetaInfo(this);
}

void 
MetaInfo::onDisposeMetaInfo()
{
  if (acdk::lang::sys::core_system::getState() == acdk::lang::sys::AfterMain)
    return;
  
  MetaInfoChangeListenerArray& listeners = getMetaInfoChangeListener();
  MetaInfoChangeListenerArray::iterator it = listeners.begin();
  MetaInfoChangeListenerArray::iterator end = listeners.end();
  for (; it != end; ++it)
    (*it)->onDisposeMetaInfo(this);
}

//static 
void 
MetaInfo::flagsToTypeDecl(StringBuffer& sb, int flags, int renderFlags) 
{
  if (renderFlags & TpFtACDKSignature)
    return;
  if (flags & MiPublic)
    sb.append("public"); 
  if (flags & MiPrivate)
    sb.append("private"); 
  if (flags & MiProtected)
    sb.append("protected"); 
  if (flags & MiPublic || flags & MiPrivate || flags & MiProtected)
  {
    if (renderFlags & TpFtAcdkType)
      sb.append(": ");
    else
      sb.append(" ");
  }
  if (flags & MiStatic)
    sb.append("static "); 
  if ((flags & MiMiVirtual) && (renderFlags & TpFtAcdkType))
    sb.append("virtual "); 
  if (flags & MiMiOneway)
    sb.append("oneway "); 
  if (flags & MiNoDmiProxy)
    sb.append("final ");
}

RString
MetaInfo::toString(int format) const
{
  if (isClazzInfo() == true)
    return reinterpret_cast<const ClazzInfo*>(this)->toTypeString(format);
  if (isMethodInfo()  == true)
    return reinterpret_cast<const ClazzMethodInfo*>(this)->toTypeString(format);
  
  if (isEnumInfo() == true)
    return reinterpret_cast<const ClazzEnumInfo*>(this)->toTypeString(format);
  if (isUnitInfo() == true)
    return reinterpret_cast<const UnitInfo*>(this)->toTypeString(format);
  if (isEnumValInfo() == true)
    return reinterpret_cast<const ClazzEnumValueInfo*>(this)->toTypeString(format);
  if (isMethodArgInfo() == true)
    return reinterpret_cast<const ClazzMethodArgInfo*>(this)->toTypeString(format);
  if (isFieldInfo()  == true)
    return reinterpret_cast<const ClazzFieldInfo*>(this)->toTypeString(format);
  if (isSuperInfo() == true)
    return reinterpret_cast<const ClazzSuperInfo*>(this)->toTypeString(format);
  return "<unknown>";
}

/*
//static 
const MetaInfo* 
MetaInfo::findMetaInfo(IN(RString) ns, IN(RString) cname, bool tryLoad)
{
  RString nsp = ns;
  RString className = cname;
  if (nsp != Nil)
    nsp = nsp->replace("::", "/")->replace('.', '/');
  if (className->charAt(0) == 'R' && 
    Character::isUpperCase(className->charAt(1)) == true)
    className = className->substr(1);
  return findMetaInfoNormalized(nsp, className, tryLoad);
}
*/

void
splitFirstElement(IN(RString) name, OUT(RString) first, OUT(RString) rest)
{
  int idx = name->indexOf('/');
  if (idx != -1)
  {
    first = name->substr(0, idx);
    rest = name->substr(idx + 1);
    
  }
  else
    first = name;
}

inline
bool checkMetaInfoType(int searchFlag, int isFlag)
{
  return (((searchFlag & MiMetaInfoTypeMask) == 0) || 
          (searchFlag & MiMetaInfoTypeMask) & (isFlag & MiMetaInfoTypeMask))
       && MetaInfo::checkMemberAccess(searchFlag, isFlag);
}

const MetaInfo*
NamedScopedParentMetaInfo::findMetaInfo(IN(RString) name, int flags) const
{
  if (isClazzInfo() == true)
  {
    const ClazzInfo* ci = (const ClazzInfo*)this;
    return ci->findMetaInfo(name, flags);
  }

  RString baseName;
  RString subName;
  splitFirstElement(name, baseName, subName);
  
  const NamedScopedMetaInfo* smi = _firstChild;
  while (smi != 0)
  {
    if (smi->equalsName(baseName) == true)
    {
      if (subName == Nil || subName->length() == 0)
      {
        if (checkMetaInfoType(flags, smi->flags) == true)
          return smi;
      }
      else
      {
        if (smi->isUnitInfo() == true)
        {
          const MetaInfo* mi = reinterpret_cast<const NamedScopedParentMetaInfo*>(smi)->findMetaInfo(subName, flags);
          if (mi != 0)
            return mi;
        }
        if (smi->isClazzInfo() == true)
        {
          if ((flags & MiMetaInfoTypeMask) == 0 || ((flags & MiMethodInfo) || (flags & MiFieldInfo)))
          {
            const ClazzInfo* ci = reinterpret_cast<const ClazzInfo*>(smi);
            const MetaInfo* mi = ci->findMetaInfo(name, flags);
            if (mi != 0)
              return mi;
          }
        }
      }

    }
    smi = smi->_nextScopeSibling;
  }
  
  return 0;
}

void 
NamedScopedMetaInfo::registerInParent() const
{
  if (flags & MiRegistered)
    return;
  UnitInfo* pui = 0;
  if (_scopeParent != 0)
  {
    if (_scopeParent->isUnitInfo() == true)
      pui = const_cast<UnitInfo*>(reinterpret_cast<const UnitInfo*>(_scopeParent));
  }
  else // parent == 0
  {
    pui = UnitInfo::findCreateUnit(ns);
    _scopeParent = pui->getMetaInfo();
  }
  if (pui != 0)
  {
    _nextScopeSibling = pui->_firstChild;
    pui->_firstChild = this;
  }
  const_cast<NamedScopedMetaInfo*>(this)->flags |= MiRegistered;
}

void 
UnitInfo_removeChild(const UnitInfo* ui, const NamedScopedMetaInfo* mi) // move in UnitInfo
{
  const NamedScopedMetaInfo* it = ui->_firstChild;
  if (it == mi)
  {
    ui->_firstChild = mi->_nextScopeSibling;
    return;
  }
  while (it != 0)
  {
    if (it->_nextScopeSibling == mi)
    {
      it->_nextScopeSibling = mi->_nextScopeSibling;
      return;
    }
    it = it->_nextScopeSibling;
  }
}

void 
NamedScopedMetaInfo::unregisterFromParent() const
{
  if ((flags & MiRegistered) != MiRegistered)
    return;
  const_cast<NamedScopedMetaInfo*>(this)->flags &= ~MiRegistered;
  if (_scopeParent == 0)
    return;
  if (_scopeParent->isUnitInfo() == true)
  {
    UnitInfo_removeChild(reinterpret_cast<const UnitInfo*>(_scopeParent), this);
  }
  _scopeParent = 0;
}

//static 
const MetaInfo* 
MetaInfo::findMetaInfoNormalized(IN(RString) name, int flags, bool tryLoad)
{
  if (name->length() == 0 || (UnitInfo::getRoot() == 0 && tryLoad == false))
    return UnitInfo::getRoot()->getMetaInfo();
  const MetaInfo* mi = UnitInfo::getRoot()->getMetaInfo()->findMetaInfo(name, flags);
  
  if (mi != 0 || tryLoad == false)
    return mi;

  if (ClassLoader::getSystemClassLoader()->loadClassLibrary(name) == true)
    return UnitInfo::getRoot()->getMetaInfo()->findMetaInfo(name, flags);
  return 0;
}

//static 
const MetaInfo* 
MetaInfo::findMetaInfo(IN(RString) name, int flags, bool tryLoad)
{
  return findMetaInfoNormalized(name->replace("::", "/")->replace('.', '/'), flags, tryLoad);
}

//static
char* 
MetaInfo::strdup(const char* n)
{
  if (n == 0)
    return 0;
  int len = strlen(n);
  char* t = new char[len + 1];
  memcpy(t, n, len);
  t[len] = 0;
  return t;
}

// static
void 
MetaInfo::strdel(const char*& n)
{
  if (n == 0)
    return;
  char* p = const_cast<char*>(n);
  delete[] p;
  n = 0;
}

void 
MetaInfo::setString(const char*& target, const char* n) 
{
  if (target != 0 && flags & MiDelete && target != n)
    strdel(target);
  target = strdup(n);
}


RString 
NamedScopedMetaInfo::getScopedName(const char* joiner) const
{
  if (_scopeParent != 0)
    return _scopeParent->getScopedName(joiner) + joiner + name;
  return name;
}

void 
MetaInfo::copyFrom(const MetaInfo* source, bool deep)
{
  flags = source->flags | MiDelete;
  if (deep == true)
  {
    // ### TODO deep copy
    attributeRes = source->attributeRes;
  }
  else
  {
    attributeRes = source->attributeRes;
  }
}

void 
NamedMetaInfo::copyFrom(const NamedMetaInfo* source, bool deep) 
{
  MetaInfo::copyFrom((const MetaInfo*)source, deep);
  name = MetaInfo::strdup(source->name);
  nameHashCode = source->nameHashCode;
}



void 
NamedScopedMetaInfo::copyFrom(const NamedScopedMetaInfo* source, bool deep) 
{
  NamedMetaInfo::copyFrom((const NamedMetaInfo*)source, deep);
  ns = MetaInfo::strdup(source->ns);
  _scopeParent = source->_scopeParent;
}



void 
MetaInfo::dispose()
{
  if (attributeRes == 0)
    return;
  ClazzAttributesRes::deleteMetaAttributes(this);
}


void 
NamedMetaInfo::dispose()
{
  strdel(name);
  MetaInfo::dispose();
}

void 
NamedScopedMetaInfo::dispose()
{
  strdel(ns);
  NamedMetaInfo::dispose();
}

void 
TypedMetaInfo::copyFrom(const TypedMetaInfo* source, bool deep)
{
  NamedScopedMetaInfo::copyFrom((const NamedScopedMetaInfo*)source, deep);
  type = source->type;
}

void 
TypedMetaInfo::dispose()
{
  NamedScopedMetaInfo::dispose();
  type = 0;
}

#if !defined(DOXYGENONLY)

namespace {
/// @internal
struct IntToString 
{ 
  int val;
  const char* name;
};

/// @internal
#define INT2STRINGTABLEENTRY(val) { val, #val }
/// @internal
IntToString MetaInfoFlagsTable[] =
{
  INT2STRINGTABLEENTRY(MiDelete),
  INT2STRINGTABLEENTRY(MiRegistered),
  INT2STRINGTABLEENTRY(MiResolved),
  INT2STRINGTABLEENTRY(MiReadOnly),
  INT2STRINGTABLEENTRY(MiPublic),
  INT2STRINGTABLEENTRY(MiPrivate),
  INT2STRINGTABLEENTRY(MiProtected),
  INT2STRINGTABLEENTRY(MiStatic),
  INT2STRINGTABLEENTRY(MiNonStatic),
  INT2STRINGTABLEENTRY(MiClazzInfo),
  INT2STRINGTABLEENTRY(MiUnitInfo),
  INT2STRINGTABLEENTRY(MiEnumInfo),
  INT2STRINGTABLEENTRY(MiEnumValInfo),
  INT2STRINGTABLEENTRY(MiMethodInfo),
  INT2STRINGTABLEENTRY(MiMethodArgInfo),
  INT2STRINGTABLEENTRY(MiFieldInfo),
  INT2STRINGTABLEENTRY(MiSuperInfo),
  INT2STRINGTABLEENTRY(MiIsSealed),
  INT2STRINGTABLEENTRY(MiNoDmiProxy),
  { 0, 0 }
};

/// @internal
IntToString ClazzInfoExtFlagsTable[] =
{
  INT2STRINGTABLEENTRY(MiCiWeakBind),
  INT2STRINGTABLEENTRY(MiCiBasicType),
  INT2STRINGTABLEENTRY(MiCiInterface),
  INT2STRINGTABLEENTRY(MiCiAbstract),
  INT2STRINGTABLEENTRY(MiCiThrowable),
  INT2STRINGTABLEENTRY(MiCiArray),
  INT2STRINGTABLEENTRY(MiCiSerializable),
  INT2STRINGTABLEENTRY(MiCiCloneable),
 { 0, 0 }
};

/// @internal
IntToString FieldInfoExtFlagsTable[] =
{
  INT2STRINGTABLEENTRY(MiFiTransient),
  { 0, 0 }
};

/// @internal
IntToString MethodInfoExtFlagsTable[] =
{
  
  INT2STRINGTABLEENTRY(MiMiIn),
  INT2STRINGTABLEENTRY(MiMiOut),
  INT2STRINGTABLEENTRY(MiMiByval),
  INT2STRINGTABLEENTRY(MiMiVirtual),
  INT2STRINGTABLEENTRY(MiMiAbstract),
  INT2STRINGTABLEENTRY(MiMiConstructor),
  INT2STRINGTABLEENTRY(MiMiDestructor),
  INT2STRINGTABLEENTRY(MiMiOneway),
  INT2STRINGTABLEENTRY(MiMiOrgPoly),
  INT2STRINGTABLEENTRY(MiMiDmiImpl),
 { 0, 0 }
};
 
/// @internal
IntToString MethodArgInfoExtFlagsTable[] =
{
  INT2STRINGTABLEENTRY(MiAiIn),
  INT2STRINGTABLEENTRY(MiAiOut),
  INT2STRINGTABLEENTRY(MiAiByval),
  INT2STRINGTABLEENTRY(MiAiHasDefaultInit),
 { 0, 0 }
};

/// @internal
IntToString ClazzInvokeInfoTable[] =
{
  INT2STRINGTABLEENTRY(MiIvTransientCall),
  INT2STRINGTABLEENTRY(MiIvDeclared),
  INT2STRINGTABLEENTRY(MiIvViaHash),
  INT2STRINGTABLEENTRY(MiIvViaAltName),
 { 0, 0 }
};

#undef INT2STRINGTABLEENTRY

/// @internal
RString stdFlagsToString(int& flags, bool& first, int formatFlags, IntToString* table)
{
  StringBuffer sb;
  for (int i = 0; flags != 0 && table[i].name != 0; ++i)
  {
    if (flags & table[i].val)
    {
      if (first == false)
      {
        sb.append(" | ");
      }
      else
      {
        first = false;
      }
      if (formatFlags & TpFtFqName)
        sb.append("::acdk::lang::dmi::");
      sb.append(table[i].name);
      flags &= ~table[i].val;
    }
  }
 
  return sb.toString();
}

}

#endif //!defined(DOXYGENONLY)

//static 
RString 
MetaInfo::flagsToString(int flags, ClazzInfoExtFlags dummy, int formatFlags)
{
  bool first = true;
  RString erg = stdFlagsToString(flags, first, formatFlags, MetaInfoFlagsTable);
  erg = erg + stdFlagsToString(flags, first, formatFlags, ClazzInfoExtFlagsTable);
  if (flags != 0 || erg->length() == 0)
  {
    if (erg->length() != 0)
      return erg + " | 0x" + Integer::toString(flags, 16);
    return "0x" + Integer::toString(flags, 16);
  }
  return erg;
}

RString 
MetaInfo::flagsToString(int flags, FieldInfoExtFlags dummy, int formatFlags)
{
   bool first = true;
  RString erg = stdFlagsToString(flags, first, formatFlags, MetaInfoFlagsTable);
  erg = erg + stdFlagsToString(flags, first, formatFlags, FieldInfoExtFlagsTable);
  if (flags != 0 || erg->length() == 0)
  {
    if (erg->length() != 0)
      return erg + " | 0x" + Integer::toString(flags, 16);
    return "0x" + Integer::toString(flags, 16);
  }
  return erg;
}


//static 
RString 
MetaInfo::flagsToString(int flags, MethodInfoExtFlags dummy, int formatFlags)
{
  bool first = true;
  RString erg = stdFlagsToString(flags, first, formatFlags, MetaInfoFlagsTable);
  erg = erg + stdFlagsToString(flags, first, formatFlags, MethodInfoExtFlagsTable);
  if (flags != 0 || erg->length() == 0)
  {
    if (erg->length() != 0)
      return erg + " | 0x" + Integer::toString(flags, 16);
    return "0x" + Integer::toString(flags, 16);
  }
  return erg;
}

//static 
RString 
MetaInfo::flagsToString(int flags, MethodArgInfoExtFlags dummy, int formatFlags)
{
  bool first = true;
  RString erg = stdFlagsToString(flags, first, formatFlags, MetaInfoFlagsTable);
  erg = erg + stdFlagsToString(flags, first, formatFlags, MethodArgInfoExtFlagsTable);
  if (flags != 0 || erg->length() == 0)
  {
    if (erg->length() != 0)
      return erg + " | 0x" + Integer::toString(flags, 16);
    return "0x" + Integer::toString(flags, 16);
  }
  return erg;
}

//static 
RString 
MetaInfo::flagsToString(int flags, ClazzInvokeInfo dummy, int formatFlags)
{
  bool first = true;
  RString erg = stdFlagsToString(flags, first, formatFlags, MetaInfoFlagsTable);
  erg = erg + stdFlagsToString(flags, first, formatFlags, ClazzInvokeInfoTable);
  if (flags != 0 || erg->length() == 0)
  {
    if (erg->length() != 0)
      return erg + " | 0x" + Integer::toString(flags, 16);
    return "0x" + Integer::toString(flags, 16);
  }
  return erg;
}


//static 
int 
MetaInfo::calcHashValue(int flags)
{
  return flags & ~MiPublic && 
         flags & ~acdk::lang::dmi::MiPrivate &&
         flags & ~acdk::lang::dmi::MiProtected;
}

} // namespace dmi
} // namespace acdk
} // namespace lang



