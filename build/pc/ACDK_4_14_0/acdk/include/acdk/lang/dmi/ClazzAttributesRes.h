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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ClazzAttributesRes.h,v 1.20 2005/04/09 19:26:51 kommer Exp $

#ifndef acdk_lang_dmi_ClazzAttributesRes_h
#define acdk_lang_dmi_ClazzAttributesRes_h

#include <acdk.h>
#include "DmiObject.h"


namespace acdk {
namespace lang {
namespace dmi {

#if !defined(DOXYGENONLY)
/// @internal
typedef void* (*CloneAttributeValFnc)(int type, void* data);
/// @internal
typedef void* (*ReleaseAttributeValFnc)(int type, void* data);
/// @internal
typedef RObject (*CastAttributeToObjectFnc)(int type, void* data);
/// @internal
typedef ScriptVar (*CastAttributeToScriptVarFnc)(int type, void* data);



/**
  Handles simple string
  @internal
*/
struct ACDK_CORE_PUBLIC StringRes
{
  const char* _data; 

  bool _deleteRes;
  StringRes()
  : _data("")
  , _deleteRes(false)
  {
  }
  StringRes(IN(RString) str)
  : _data(0)
  , _deleteRes(false)
  {
    if ((str->stringFlags() & ConstSST) && str->isCharacterClass(CCAscii))
    {
      _data = (const char*)str->byte_begin();
    }
    else
    {
      _data = new char[str->length() + 1];
      strcpy(const_cast<char*>(_data), str->c_str()); // ### Unicode FIXIT
      _deleteRes = true;
    }
  }
  StringRes(const char* data, bool delres = false)
  : _data(data)
  , _deleteRes(delres)
  {
  }
  StringRes(const StringRes& other)
  : _data(other._data)
  , _deleteRes(other._deleteRes)
  {
    if (other._deleteRes == true && other._data != 0)
    {
      _data = new char[strlen(other._data) + 1];
      strcpy(const_cast<char*>(_data), other._data);
      _deleteRes = true;
    }
  }
  ~StringRes()
  {
    if (_deleteRes == true && _data != 0)
      delete[] const_cast<char*>(_data);
  }
  operator RString () const
  {
    if (_deleteRes == false)
      return RCS(_data);
    return new String(_data, NormalSST | CCAscii);
  }
  operator const char* () const { return _data; }
  bool operator==(const StringRes& other) const
  {
    return strcmp(_data, other._data) == 0;
  }
  bool operator<(const StringRes& other) const
  {
    return strcmp(_data, other._data) < 0;
  }
};

#endif //!defined(DOXYGENONLY)

/**
  determine the type held by an attribute
*/
enum AttributeResType
{
  /**
    Value does not contain a value
  */
  EmptyResType = 0x0,
  /**
    data is const char*
  */
  CharPtrResType = 0x1,
  /** 
    data is a String*
  */
  StringResType = 0x2,
  /**
    Contains a Object pointer
  */
  ObjectPtrResType = 0x3,
  /**
    Contains a ScriptVar
  */
  ScriptVarResType = 0x4,

  /**
    Contains a static function pointer
  */
  FunctionPtrResType = 0x5,

  /// unknown/user res types
  UserResType = 0xF
};

ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, AttributeResType);

  
/*
  A opaque Attribute value used to store in 
  ClazzAttributesRes
*/
struct ACDK_CORE_PUBLIC ClazzAttributeResValue
{
  /// See AttributeResType
  int resType;
  void* data;
  CloneAttributeValFnc cloneFnc;
  ReleaseAttributeValFnc releaseFnc;
  CastAttributeToObjectFnc castObjectFnc;
  CastAttributeToScriptVarFnc castScriptVarFnc;
  ClazzAttributeResValue(int restype = EmptyResType, void* d = 0, CloneAttributeValFnc clonfnc = 0, 
                                                                  ReleaseAttributeValFnc releasefnc = 0, 
                                                                  CastAttributeToObjectFnc castfnc = 0,
                                                                  CastAttributeToScriptVarFnc castsvfnc = 0);
  ClazzAttributeResValue(const ClazzAttributeResValue& other);
  ~ClazzAttributeResValue();
  RObject getAsObject();
  ScriptVar getAsScriptVar();
  static ClazzAttributeResValue makeStringRes(IN(RString) str);
  static ClazzAttributeResValue makeStringRes(const char* text, bool deleteRes);
  static ClazzAttributeResValue makeObjectRes(IN(RObject) obj);
  static ClazzAttributeResValue makeFunctionPtrRes(void* callback);
  static void* cloneObjectPtr(int restype, void* ptr);
  static void* releaseObjectPtr(int restype, void* ptr);
  static RObject castObjectPtr(int restype, void *ptr);
  static ClazzAttributeResValue makeScriptVarRes(IN(ScriptVar) str);
  static void* cloneScriptVarPtr(int restype, void* ptr);
  static void* releaseScriptVarPtr(int restype, void* ptr);
  static ScriptVar castScriptVar(int restype, void *ptr);
};

#if !defined(DOXYGENONLY)
/**
  @internal
*/
template <class K, class V>
struct KeyValue
{
  K key;
  V value;
  KeyValue() {}
  KeyValue(const K&k , const V& v)
  : key(k)
  , value(v)
  {
  }
  bool operator==(const KeyValue<K, V>& other) const
  {
    return key == other.key;
  }
};
#endif //!defined(DOXYGENONLY)

/**
  Used to store attributes at ClazzInfo FieldInfo etc.
  User normally don't operate on these structs, but
  uses the reflection API to query attributes MetaAttribute
*/
struct ACDK_CORE_PUBLIC ClazzAttributesRes
{
  typedef KeyValue<StringRes, ClazzAttributeResValue> ResTableEntry;
  typedef acdk::lang::sys::core_vector<ResTableEntry> ResTable;
  
  ResTable _entries;
  ClazzAttributeResValue get(const char* key);
  ClazzAttributeResValue getInstance(Object* obj, const char* key);
  bool hasEntry(const char* key);
  bool hasInstanceEntry(Object* obj, const char* key);

  void put(const StringRes& key, const ClazzAttributeResValue& val = ClazzAttributeResValue());
  void putInstance(Object* obj, const StringRes& key, const ClazzAttributeResValue& val = ClazzAttributeResValue());
  void remove(const StringRes& key);
  void removeAll();
  void removeInstance(Object* obj, const StringRes& key);
  static void attachAttribute(MetaInfo* mi, const StringRes& key, const ClazzAttributeResValue& val = ClazzAttributeResValue());
  static void attachInstanceAttribute(MetaInfo* mi, const StringRes& key, Object* obj, const ClazzAttributeResValue& val = ClazzAttributeResValue());
  
  static ClazzAttributeResValue getAttribute(MetaInfo* mi, IN(RString) key);
  static bool hasAttribute(MetaInfo* mi, IN(RString) key);
  static ClazzAttributeResValue getInstanceAttribute(MetaInfo* mi, Object* obj, IN(RString) key);
  static bool hasInstanceAttribute(MetaInfo* mi, Object* obj, IN(RString) key);
  static ResTable getAttributes(MetaInfo* mi);
  static ResTable getInstanceAttributes(MetaInfo* mi, Object* o);
  static void deleteMetaAttributes(MetaInfo* mi);
  static void deleteMetaAttribute(MetaInfo* mi, IN(RString) key);
  static void deleteInstanceMetaAttribute(MetaInfo* mi, Object* o, IN(RString) key);
  static inline void releaseInstanceData(MetaInfo* mi, Object* o)
  {
    if (mi == 0 || mi->attributeRes == 0)
      return;
    releaseInstanceData2(mi, o);
  }
  static void releaseInstanceData2(MetaInfo* mi, Object* o);
};


} // dmi
} // lang
} // acdk



#endif //acdk_lang_dmi_ClazzAttributesRes_h

