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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/MetaAttribute.cpp,v 1.25 2005/02/11 11:06:24 kommer Exp $
#include <acdk.h>

#include "MetaAttribute.h"
#include "ClazzAttributesRes.h"
#include "../Integer.h"
#include "../Long.h"

namespace acdk {
namespace lang {
namespace dmi {

void*
ClazzAttributeResValue::cloneObjectPtr(int restype, void* ptr)
{
  if (ptr == 0)
    return 0;
  Object* sptr = reinterpret_cast<Object*>(ptr);
  sptr->addRef();
  return ptr;
}

void*
ClazzAttributeResValue::releaseObjectPtr(int restype, void* ptr)
{
  if (ptr == 0)
    return 0;
  Object* sptr = reinterpret_cast<Object*>(ptr);
  sptr->releaseRef();
  return 0;
}

RObject 
ClazzAttributeResValue::castObjectPtr(int restype, void *ptr)
{
  switch (restype)
  {
    case EmptyResType:
      return Nil;
    case CharPtrResType:
      return (RObject)SCS((const char*)ptr);
    case StringResType:
    case ObjectPtrResType:
      return reinterpret_cast<Object*>(ptr);
    case ScriptVarResType:
      return reinterpret_cast<ScriptVar*>(ptr)->getObjectVar();
    default:
      return Nil;
  }
}

void* 
cloneCharPtr(int restype, void* ptr)
{
  if (ptr == 0)
    return 0;
  char* sptr = reinterpret_cast<char*>(ptr);
  char* nptr = ::new char[strlen(sptr) + 1];
  strcpy(nptr, sptr);
  return (void*)nptr;
}

void* 
releaseCharPtr(int restype, void* ptr)
{
  if (ptr == 0)
    return 0;
  char* sptr = reinterpret_cast<char*>(ptr);
  delete[] sptr;
  return 0;
}

RObject
makeStringOfRes(int type, void* ptr)
{
  if (ptr == 0)
    return (RObject)String::emptyString();
  if (type == CharPtrResType)
    return &SCS((const char*)ptr);
  else if (type == StringResType)
  {
    RObject ret = (String*)ptr;
    return ret;
  }
  return Nil;
}


void* 
cloneFunctionPtr(int restype, void* ptr)
{
  return ptr;
}

void* 
releaseFunctionPtr(int restype, void* ptr)
{
  return ptr;
}

RObject 
castFunctionPtr(int restype, void *ptr)
{
#if defined(ACDK_64_BIT_PTR)
  return new Long((jlong)ptr);
#else
  return new Integer((int)ptr);
#endif
}

ScriptVar
castFunctionPtr2ScriptVar(int restype, void *ptr)
{
#if defined(ACDK_64_BIT_PTR)
  return ScriptVar(inOf(jlong(ptr)));
#else
  return ScriptVar(inOf(int(ptr)));
#endif
}

ACDK_DECL_CLASS(InstanceAttributeData);

class ACDK_CORE_PUBLIC InstanceAttributeData
: extends acdk::lang::Object
{
public:
  typedef KeyValue<Object*, ClazzAttributesRes> InstanceAttr;
  typedef acdk::lang::sys::core_vector<InstanceAttr> InstanceAttrMap;
  InstanceAttrMap _data;
  InstanceAttributeData() {}
  ~InstanceAttributeData()
  {
  }
  ClazzAttributeResValue getAttribute(Object* o, const char* ptr)
  {
    InstanceAttrMap::iterator it = _data.find(InstanceAttr(o, ClazzAttributesRes()));
    if (it == _data.end())
      return ClazzAttributeResValue();
    return it->value.get(ptr);
  }
  bool hasEntry(Object* o, const char* ptr)
  {
    InstanceAttrMap::iterator it = _data.find(InstanceAttr(o, ClazzAttributesRes()));
    if (it == _data.end())
      return false;
    return it->value.hasEntry(ptr);
  }
  ClazzAttributesRes getTable(Object* o)
  {
    InstanceAttrMap::iterator it = _data.find(InstanceAttr(o, ClazzAttributesRes()));
    if (it == _data.end())
      return ClazzAttributesRes();
    return it->value;
  }
  void put(Object* o, const StringRes& key, const ClazzAttributeResValue& val)
  {
    InstanceAttrMap::iterator it = _data.find(InstanceAttr(o, ClazzAttributesRes()));
    if (it == _data.end())
    {
      _data.push_back(InstanceAttr(o, ClazzAttributesRes()));
      it = _data.find(InstanceAttr(o, ClazzAttributesRes()));
    }
    it->value.put(key, val);
  }
  void remove(Object* o, const char* ptr)
  {
    InstanceAttrMap::iterator it = _data.find(InstanceAttr(o, ClazzAttributesRes()));
    if (it == _data.end())
      return;
    it->value.remove(ptr);
  }
  void releaseObject(Object* o)
  {
    InstanceAttrMap::iterator it = _data.find(InstanceAttr(o, ClazzAttributesRes()));
    if (it == _data.end())
      return;
    _data.erase(it, it + 1);
  }
  
};

ClazzAttributeResValue::ClazzAttributeResValue(int restype, void* d, CloneAttributeValFnc clonfnc, 
                                                                  ReleaseAttributeValFnc releasefnc, 
                                                                  CastAttributeToObjectFnc castfnc,
                                                                  CastAttributeToScriptVarFnc castsvfnc)
: resType(restype)
, data(d)
, cloneFnc(clonfnc)
, releaseFnc(releasefnc)
, castObjectFnc(castfnc)
, castScriptVarFnc(castsvfnc)
{
  if (cloneFnc != 0)
    data = cloneFnc(restype, data);
}

ClazzAttributeResValue::~ClazzAttributeResValue()
{
  if (releaseFnc)
    data = releaseFnc(resType, data);
}

ClazzAttributeResValue::ClazzAttributeResValue(const ClazzAttributeResValue& other)
: resType(other.resType)
, data(other.data)
, cloneFnc(other.cloneFnc)
, releaseFnc(other.releaseFnc)
, castObjectFnc(other.castObjectFnc)
, castScriptVarFnc(other.castScriptVarFnc)
{
   if (cloneFnc != 0)
    data = cloneFnc(resType, data);
}

ClazzAttributeResValue
ClazzAttributeResValue::makeStringRes(IN(RString) str)
{
  if (str->stringFlags() & ConstSST)
    return ClazzAttributeResValue(CharPtrResType, (void*)str->c_str(), cloneCharPtr, releaseCharPtr, makeStringOfRes);
  /*String* sptr = &str;
  sptr->addRef();
  */
  return ClazzAttributeResValue(StringResType, &str, cloneObjectPtr, releaseObjectPtr, castObjectPtr);
}

//static 
ClazzAttributeResValue 
ClazzAttributeResValue::makeObjectRes(IN(RObject) obj)
{
  return ClazzAttributeResValue(ObjectPtrResType, &obj, cloneObjectPtr, releaseObjectPtr, castObjectPtr);
}

//static 
ClazzAttributeResValue 
ClazzAttributeResValue::makeStringRes(const char* text, bool deleteRes)
{
  if (deleteRes == true)
    return ClazzAttributeResValue(CharPtrResType, (void*)text, cloneCharPtr, releaseCharPtr, makeStringOfRes);
  return ClazzAttributeResValue(CharPtrResType, (void*)text, 0, 0, makeStringOfRes);
}

//static 
ClazzAttributeResValue 
ClazzAttributeResValue::makeScriptVarRes(IN(ScriptVar) sv)
{
  ScriptVar* svptr = ::new ScriptVar(sv);
  return ClazzAttributeResValue(ScriptVarResType, (void*)svptr, cloneScriptVarPtr, releaseScriptVarPtr, 0, castScriptVar);
}

//static 
ClazzAttributeResValue 
ClazzAttributeResValue::makeFunctionPtrRes(void* callback)
{
  return ClazzAttributeResValue(FunctionPtrResType, callback, cloneFunctionPtr, releaseFunctionPtr, castFunctionPtr, castFunctionPtr2ScriptVar);
}


//static 
void* 
ClazzAttributeResValue::cloneScriptVarPtr(int restype, void* ptr)
{
  return (void*)new ScriptVar(*reinterpret_cast<ScriptVar*>(ptr));
}

//static 
void* 
ClazzAttributeResValue::releaseScriptVarPtr(int restype, void* ptr)
{
  delete reinterpret_cast<ScriptVar*>(ptr);
  return 0;
}

//static 
ScriptVar 
ClazzAttributeResValue::castScriptVar(int restype, void *ptr)
{
  if (ptr == 0)
    return ScriptVar();
  if (restype == ScriptVarResType)
    return *reinterpret_cast<ScriptVar*>(ptr);
  if (restype == ObjectPtrResType || restype == StringResType)
    return inOf(RObject(reinterpret_cast<Object*>(ptr)));
  if (restype == CharPtrResType)
    return inOf(RString(reinterpret_cast<char*>(ptr)));
  return ScriptVar();
}

RObject 
ClazzAttributeResValue::getAsObject()
{
  if (castObjectFnc != 0)
    return castObjectFnc(resType, data);
  if (castScriptVarFnc != 0)
    return castScriptVarFnc(resType, data).getObjectVar();
  return Nil;
}

ScriptVar 
ClazzAttributeResValue::getAsScriptVar()
{
  if (castScriptVarFnc != 0)
    return castScriptVarFnc(resType, data);
  if (castObjectFnc != 0)
    return inOf(castObjectFnc(resType, data));
  return ScriptVar();
}

ClazzAttributeResValue 
ClazzAttributesRes::get(const char* key)
{
  ResTable::iterator it = _entries.begin();
  ResTable::iterator end = _entries.end();
  for (; it < end; ++it)
  {
    if (strcmp((*it).key, key) == 0)
    {
      return (*it).value;
    }
  }
  return ClazzAttributeResValue();
}


ClazzAttributeResValue 
ClazzAttributesRes::getInstance(Object* obj, const char* key)
{
  ResTable::iterator it = _entries.find(ResTableEntry("__instd_acdk_dmi", ClazzAttributeResValue()));
  if (it == _entries.end())
    return ClazzAttributeResValue();
  RInstanceAttributeData o = (RInstanceAttributeData)it->value.getAsObject();
  return o->getAttribute(obj, key);
}

bool 
ClazzAttributesRes::hasEntry(const char* key)
{
  ResTable::iterator it = _entries.begin();
  ResTable::iterator end = _entries.end();
  for (; it < end; ++it)
  {
    if (strcmp((*it).key, key) == 0)
    {
      return true;
    }
  }
  return false;
}

bool 
ClazzAttributesRes::hasInstanceEntry(Object* obj, const char* key)
{
  ResTable::iterator it = _entries.find(ResTableEntry("__instd_acdk_dmi", ClazzAttributeResValue()));
  if (it == _entries.end())
    return false;
  RInstanceAttributeData o = (RInstanceAttributeData)it->value.getAsObject();
  return o->hasEntry(obj, key);
}

void 
ClazzAttributesRes::put(const StringRes& key, const ClazzAttributeResValue& val)
{
  ResTable::iterator it = _entries.begin();
  ResTable::iterator end = _entries.end();
  for (; it < end; ++it)
  {

    if ((*it).key != 0 && strcmp((*it).key, (const char*)key) == 0)
    {
      (*it).value = val;
      return;
    }
  }
  _entries.push_back(KeyValue<StringRes, ClazzAttributeResValue>(key, val));
}

void 
ClazzAttributesRes::putInstance(Object* obj, const StringRes& key, const ClazzAttributeResValue& val)
{
  ResTable::iterator it = _entries.find(ResTableEntry("__instd_acdk_dmi", ClazzAttributeResValue()));
  if (it == _entries.end())
  {
    _entries.push_back(ResTableEntry("__instd_acdk_dmi", ClazzAttributeResValue::makeObjectRes(new InstanceAttributeData())));
    it = _entries.find(ResTableEntry("__instd_acdk_dmi", ClazzAttributeResValue()));
  }
  RInstanceAttributeData o = (RInstanceAttributeData)it->value.getAsObject();
  o->put(obj, key, val);
}

void 
ClazzAttributesRes::removeAll()
{
  _entries.erase(_entries.begin(), _entries.end());
}

void 
ClazzAttributesRes::remove(const StringRes& key)
{
  ResTable::iterator it = _entries.find(ResTableEntry(key, ClazzAttributeResValue()));
  if (it == _entries.end())
    return;
  _entries.erase(it, it + 1);
}
  
void 
ClazzAttributesRes::removeInstance(Object* obj, const StringRes& key)
{
  ResTable::iterator it = _entries.find(ResTableEntry("__instd_acdk_dmi", ClazzAttributeResValue()));
  if (it == _entries.end())
    return;
  RInstanceAttributeData o = (RInstanceAttributeData)it->value.getAsObject();
  o->remove(obj, key);
}

//static 
void 
ClazzAttributesRes::attachAttribute(MetaInfo* mi, const StringRes& key, const ClazzAttributeResValue& val)
{
  if (mi->attributeRes == 0)
    mi->attributeRes = (void*)new ClazzAttributesRes();
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  ar->put(key, val);
}

//static 
void 
ClazzAttributesRes::attachInstanceAttribute(MetaInfo* mi, const StringRes& key, Object* obj, const ClazzAttributeResValue& val)
{
  if (mi->attributeRes == 0)
    mi->attributeRes = (void*)new ClazzAttributesRes();
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  ar->putInstance(obj, key, val);
}

//static 
ClazzAttributeResValue
ClazzAttributesRes::getAttribute(MetaInfo* mi, IN(RString) key)
{
  if (mi->attributeRes == 0)
    return ClazzAttributeResValue();
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  return ar->get(key->c_str());
  /*
  for (int i = 0; i < ar->_entries.size(); ++i)
  {
    const char* k = ar->_entries[i].key;
    if (strcmp(k, key->c_str()) == 0)
      return ar->_entries[i];
  }
  return KeyValue<StringRes, ClazzAttributeResValue> (0, ClazzAttributeResValue());
  */
}

//static 
bool 
ClazzAttributesRes::hasAttribute(MetaInfo* mi, IN(RString) key)
{
  if (mi->attributeRes == 0)
    return false;
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  return ar->hasEntry(key->c_str());
}

//static 
ClazzAttributeResValue
ClazzAttributesRes::getInstanceAttribute(MetaInfo* mi, Object* obj, IN(RString) key)
{
  if (mi->attributeRes == 0)
    return ClazzAttributeResValue();
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  return ar->getInstance(obj, key->c_str());
}

//static 
bool 
ClazzAttributesRes::hasInstanceAttribute(MetaInfo* mi, Object* obj, IN(RString) key)
{
  if (mi->attributeRes == 0)
    return false;
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  return ar->hasInstanceEntry(obj, key->c_str());
}


//static 
ClazzAttributesRes::ResTable 
ClazzAttributesRes::getAttributes(MetaInfo* mi)
{
  if (mi->attributeRes == 0)
    return ResTable(0);
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  return ar->_entries;
}

//static 
void 
ClazzAttributesRes::deleteMetaAttributes(MetaInfo* mi)
{
  if (mi->attributeRes == 0)
    return;
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  ar->removeAll();
}

//static 
void 
ClazzAttributesRes::deleteMetaAttribute(MetaInfo* mi, IN(RString) key)
{
  if (mi->attributeRes == 0)
    return;
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  ar->remove(key->c_str());
}

//static 
void 
ClazzAttributesRes::deleteInstanceMetaAttribute(MetaInfo* mi, Object* o, IN(RString) key)
{
  if (mi->attributeRes == 0)
    return;
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  ar->removeInstance(o, key->c_str());

}

//static 
ClazzAttributesRes::ResTable 
ClazzAttributesRes::getInstanceAttributes(MetaInfo* mi, Object* o)
{
  if (mi->attributeRes == 0)
    return ResTable(0);
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  ClazzAttributeResValue carv = ar->get("__instd_acdk_dmi");
  if (carv.data == 0)
    return ResTable(0);
  RInstanceAttributeData iad = (RInstanceAttributeData)carv.getAsObject();
  ClazzAttributesRes catr = iad->getTable(o);
  return catr._entries;
}

//static 
void 
ClazzAttributesRes::releaseInstanceData2(MetaInfo* mi, Object* o)
{
  ClazzAttributesRes* ar = (ClazzAttributesRes*)mi->attributeRes;
  ClazzAttributeResValue carv = ar->get("__instd_acdk_dmi");
  if (carv.data == 0)
    return;
  RInstanceAttributeData iad = (RInstanceAttributeData)carv.getAsObject();
  iad->releaseObject(o);
}

} // dmi
} // lang
} // acdk





