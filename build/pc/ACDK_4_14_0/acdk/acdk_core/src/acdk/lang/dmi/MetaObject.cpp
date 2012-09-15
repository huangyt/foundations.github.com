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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/MetaObject.cpp,v 1.17 2005/02/05 10:44:58 kommer Exp $


#include <acdk.h>
#include "MetaObject.h"
#include "ClazzAttributesRes.h"


namespace acdk {
namespace lang {
namespace dmi {


//virtual 
RMetaAttributeArray 
MetaObject::getMetaAttributes()
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributesRes::ResTable rt = ClazzAttributesRes::getAttributes(mi);
  RMetaAttributeArray ret = new MetaAttributeArray(rt.size());
  for (int i = 0; i < rt.size(); ++i)
  {
    RObject val = rt[i].value.getAsObject();
    ret[i] = new MetaAttribute(rt[i].value.resType, rt[i].key, val);
                               
  }
  return ret;
}

//virtual 
RMetaAttributeArray 
MetaObject::getInstanceMetaAttributes(IN(RObject) obj)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributesRes::ResTable rt = ClazzAttributesRes::getInstanceAttributes(mi, &obj);
  RMetaAttributeArray ret = new MetaAttributeArray(rt.size());
  for (int i = 0; i < rt.size(); ++i)
  {
    RObject val = rt[i].value.getAsObject();
    ret[i] = new MetaAttribute(rt[i].value.resType, rt[i].key, val);
                               
  }
  return ret;
}

//virtual 
bool 
MetaObject::hasMetaAttribute(IN(RString) keyname)
{
  MetaInfo* mi = getMetaInfo();
  return ClazzAttributesRes::hasAttribute(mi, keyname);
}

//virtual 
bool 
MetaObject::hasInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname)
{
  MetaInfo* mi = getMetaInfo();
  return ClazzAttributesRes::hasInstanceAttribute(mi, &obj, keyname);
}


//virtual 
RMetaAttribute
MetaObject::getMetaAttribute(IN(RString) str)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributeResValue rv = ClazzAttributesRes::getAttribute(mi, str);
  if (rv.resType == 0)
    return Nil;
  return new MetaAttribute(rv.resType, str, rv.getAsObject());
}

RObject 
MetaObject::getObjectMetaAttribute(IN(RString) keyname)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributeResValue rv = ClazzAttributesRes::getAttribute(mi, keyname);
  if (rv.resType == 0)
    return Nil;
  return rv.getAsObject();
}

ScriptVar 
MetaObject::getScriptVarMetaAttribute(IN(RString) keyname)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributeResValue rv = ClazzAttributesRes::getAttribute(mi, keyname);
  if (rv.resType == 0)
    return ScriptVar();
  return rv.getAsScriptVar();
}

//virtual 
RMetaAttribute 
MetaObject::getInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributeResValue rv = ClazzAttributesRes::getInstanceAttribute(mi, obj, keyname);
  if (rv.resType == 0)
    return Nil;
  return new MetaAttribute(rv.resType, keyname, rv.getAsObject());
}

ScriptVar 
MetaObject::getScriptVarInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributeResValue rv = ClazzAttributesRes::getInstanceAttribute(mi, obj, keyname);
  if (rv.resType == 0)
    return ScriptVar();
  return rv.getAsScriptVar();
}

//virtual 
void 
MetaObject::setMetaAttribute(IN(RString) str, IN(RObject) obj)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributesRes::attachAttribute(mi, str, 
    ClazzAttributeResValue(ObjectPtrResType, (void*)obj.impl(), ClazzAttributeResValue::cloneObjectPtr, 
                                                                ClazzAttributeResValue::releaseObjectPtr, 
                                                                ClazzAttributeResValue::castObjectPtr,
                                                                ClazzAttributeResValue::castScriptVar));
}

void 
MetaObject::setMetaAttribute(IN(RString) keyname, const ScriptVar& attr)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributesRes::attachAttribute(mi, keyname, 
    ClazzAttributeResValue(ScriptVarResType, (void*)new ScriptVar(attr), 
                                                                ClazzAttributeResValue::cloneScriptVarPtr, 
                                                                ClazzAttributeResValue::releaseScriptVarPtr, 
                                                                ClazzAttributeResValue::castObjectPtr,
                                                                ClazzAttributeResValue::castScriptVar
                                                                ));
}

//virtual 
void 
MetaObject::setInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname, IN(RObject) attr)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributesRes::attachInstanceAttribute(mi,  keyname, obj,
    ClazzAttributeResValue(ObjectPtrResType, (void*)attr.impl(), ClazzAttributeResValue::cloneObjectPtr, 
                                                                ClazzAttributeResValue::releaseObjectPtr, 
                                                                ClazzAttributeResValue::castObjectPtr,
                                                                ClazzAttributeResValue::castScriptVar));
}

//virtual 
void 
MetaObject::setInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname, const ScriptVar& attr)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributesRes::attachInstanceAttribute(mi,  keyname, obj,
    ClazzAttributeResValue(ScriptVarResType, (void*)new ScriptVar(attr), ClazzAttributeResValue::cloneObjectPtr, 
                                                                ClazzAttributeResValue::releaseObjectPtr, 
                                                                ClazzAttributeResValue::castObjectPtr,
                                                                ClazzAttributeResValue::castScriptVar));
}

//virtual 
void 
MetaObject::deleteMetaAttribute(IN(RString) keyname)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributesRes::deleteMetaAttribute(mi, keyname);
}

//virtual 
void 
MetaObject::deleteInstanceMetaAttribute(IN(RObject) obj, IN(RString) keyname)
{
  MetaInfo* mi = getMetaInfo();
  ClazzAttributesRes::deleteInstanceMetaAttribute(mi, obj, keyname);
}

//virtual 
RString 
MetaObjectImpl::toTypeString(int format)
{
  if (_metaInfo->isClazzInfo() == true)
    return reinterpret_cast<const ClazzInfo*>(_metaInfo)->toTypeString(format);
  if (_metaInfo->isUnitInfo() == true)
    return reinterpret_cast<const UnitInfo*>(_metaInfo)->toTypeString(format);
  if (_metaInfo->isEnumInfo() == true)
    return reinterpret_cast<const ClazzEnumInfo*>(_metaInfo)->toTypeString(format);
  if (_metaInfo->isEnumValInfo() == true)
    return reinterpret_cast<const ClazzEnumValueInfo*>(_metaInfo)->toTypeString(format);
  if (_metaInfo->isMethodInfo() == true)
  {
    const ClazzMethodInfo* mci = reinterpret_cast<const ClazzMethodInfo*>(_metaInfo);
    const ClazzInfo* ci = reinterpret_cast<const ClazzInfo*>(mci->_scopeParent);
    return mci->toTypeString(ci, format);
  }
  if (_metaInfo->isMethodArgInfo() == true)
  {
    return reinterpret_cast<const ClazzMethodArgInfo*>(_metaInfo)->toTypeString(format);
  }
  if (_metaInfo->isFieldInfo() == true)
  {
  }
  return "";
}

} // dmi
} // lang
} // acdk



