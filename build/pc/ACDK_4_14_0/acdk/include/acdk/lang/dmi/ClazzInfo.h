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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/ClazzInfo.h,v 1.61 2005/04/18 14:22:27 kommer Exp $

#ifndef acdk_lang_dmi_ClazzInfo_h
#define acdk_lang_dmi_ClazzInfo_h

#include "MetaInfo.h"
#include "ScriptVar.h"

namespace acdk {
namespace lang {

typedef RObject (*ObjectCreator)();
typedef RObject (*ArrayCreator)(int length);
typedef RObject (*ArrayArrayCreator)(int length, int width);

ACDK_DECL_THROWABLE(Throwable, Object);
// throws deserailized exection
typedef void (*DispatchThrowableFunc)(IN(RThrowable) ex); 

class Class;
class StringBuffer;

namespace dmi {

#if ACDK_CHECK_GCC_VERSION(4, 0)
class ClazzInfo;
class ClazzEnumInfo;
#else
class ACDK_CORE_PUBLIC ClazzInfo;
class ACDK_CORE_PUBLIC ClazzEnumInfo;
#endif

/**
  All classes in one namespace are in one unit.
  castable to NamedScopedMetaInfo
*/
foreign class ACDK_CORE_PUBLIC UnitInfo
{
public:
  /** @see acdk::lang::dmi::MetaInfoFlags */
  int flags;
  /** @see acdk::lang::dmi::AttributesRes */
  void* attributeRes;
  
  /** name of unit */
  const char* name;
  int nameHashCode;

  const char* ns;
  mutable const UnitInfo* _scopeParent;

  mutable const NamedScopedMetaInfo* _nextScopeSibling;
  /**
    this is a dummy type and always void type
  */
  const ClazzInfo* type;
  
  /** first element in unit child Chain */
  mutable const NamedScopedMetaInfo* _firstChild;

  
  RString toTypeString(int formatflags) const;
  static UnitInfo* getRoot();
  static void toTypeString(StringBuffer& sb, const char* ns, int formatflags, bool withMerger);
  /**
    find unit info
    Asumed names are already in normalized for (acdk/lang reflect)
    @param unitName may Nil
    @param tryLoad use ClassLoader to find UnitInfo
  */
  static const UnitInfo* findUnitInfo(IN(RString) name, bool tryLoad = false);
  static UnitInfo* create(const char* name);
  static UnitInfo* findCreateUnit(const char* ns);
  static MetaInfo* findMetaInfo(IN(RString) elementName);
  static void registerUnitInfo(UnitInfo* ui);
  /**
    dispose this structure and owning
    Delete ClazzInfo if flags has MiDelete
  */
  void dispose();
  /**
    Make a dynamic clone of this structor
    @param deep if true make also a clone
            of child structs
  */
  UnitInfo* clone(bool deep = true);
  inline bool equalsName(IN(acdk::lang::RString) n) const { return getMetaInfo()->equalsName(n); }
  inline const NamedScopedParentMetaInfo* getMetaInfo() const { return reinterpret_cast<const NamedScopedParentMetaInfo*>(this); }
  inline NamedScopedParentMetaInfo* getMetaInfo() { return reinterpret_cast<NamedScopedParentMetaInfo*>(this); }
  
  
  
};

/**
  stores an enumeration value
*/
foreign class ACDK_CORE_PUBLIC ClazzEnumValueInfo
{
public:
  int flags;
  void* attributeRes;
  const char* name;
  int nameHashCode;
  const char* ns;
  /** 
    reference to parent Unit, not to ClazzEnum
  */
  mutable const NamedScopedMetaInfo* _scopeParent;
  
  mutable const NamedScopedMetaInfo* _nextSibling;

  const ClazzEnumInfo* parent;
  /** the integer value represented by this clazzenum */
  int value;
  /**
    dispose this structure and owning
    Delete ClazzInfo if flags has MiDelete
  */
  void dispose();
  /**
    Make a dynamic clone of this structor
    @param deep if true make also a clone
            of child structs
  */
  ClazzEnumValueInfo* clone(bool deep = true);
  /**
    creates new (deletable) ClazzEnumValueInfo
  */
  static ClazzEnumValueInfo* create(ClazzEnumInfo* ei, IN(RString) name, int value);
  RString toTypeString(int format) const;
  inline bool equalsName(IN(acdk::lang::RString) n) const { return getMetaInfo()->equalsName(n); }

  inline void registerEnumValueInfo() const { registerEnumValueInfo(this); }
  /**
    intern method to register EnumValueInfo
  */
  static void registerEnumValueInfo(const ClazzEnumValueInfo* enumVal);
  static void unregisterEnumValueInfo(const ClazzEnumValueInfo* enumVal);
  inline const NamedScopedMetaInfo* getMetaInfo() const { return reinterpret_cast<const NamedScopedMetaInfo*>(this); }
  inline NamedScopedMetaInfo* getMetaInfo() { return reinterpret_cast<NamedScopedMetaInfo*>(this); }
};

/** 
  contains information about an enumeration
  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @date $Date: 2005/04/18 14:22:27 $
*/
foreign class ACDK_CORE_PUBLIC ClazzEnumInfo
{
public:
   /** see acdk::lang::dmi::MetaInfoFlags */
  int flags;
  /** @see acdk::lang::dmi::AttributesRes */
  void* attributeRes;

  /** the label of the field */
  const char* name;
  int nameHashCode;
  const char* ns;
  /** namespace where the Enumeration is defined */
  mutable const NamedScopedMetaInfo* _scopeParent;
  mutable const NamedScopedMetaInfo* _nextSibling;
  
  /**
    0 terminated list of value definitions of this enumeration
  */
  ClazzEnumValueInfo** values;
  /** 
    next registerd ClazzEnumInfo
  */
  mutable ClazzEnumInfo* _next;

  inline bool isResolved() const { return MetaInfo::isResolved(flags); }
  
  /**
    find enumeration type with given name
    @param enumname name of enumeration
    @param namesp namespace of enumeration, Nil if any namespace
    @return 0 if enumeration cannot be found
  */
  static const ClazzEnumInfo* findEnum(IN(RString) enumname, IN(RString) namesp);
  /**
    find enumeration value with given name
    @param enumname name of enumeration value
    @param namesp namespace of enumeration, Nil if any namespace
    @param optional poiner to ClazzEnumInfo, which owns the enumeration ifno
    @return 0 if enumeration cannot be found
  */
  static const ClazzEnumValueInfo* findEnumValue(IN(RString) enumname, IN(RString) namesp, const ClazzEnumInfo** ei = 0);
  static const ClazzEnumValueInfo* findEnumValue(IN(RString) enumstring, const ClazzEnumInfo** ei = 0);
  /**
    return the root of all registered ClazzEnumInfo
  */
  static ClazzEnumInfo* getRoot();
  bool hasName(IN(RString) name) const;
  /** return -1 if name is not known by this enumeration */
  int getIntValue(IN(RString) name) const;
  const ClazzEnumValueInfo* getValue(IN(RString) name) const;
  RString toTypeString(int formatflags) const;
  void toTypeString(StringBuffer& sb, int formatflags) const;
  /**
    intern method to register this EnumInfo
  */
  inline void registerEnumInfo() const { registerEnumInfo(this); }
  /**
    intern method to register EnumInfo
  */
  static void registerEnumInfo(const ClazzEnumInfo* clazz);
  /**
    intern method to unregister EnumInfo
  */
  static void unregisterEnumInfo(const ClazzEnumInfo* clazz);
  
  /**
    dispose this structure and owning
    Delete ClazzInfo if flags has MiDelete
  */
  void dispose();
  /**
    Make a dynamic clone of this structor
    @param deep if true make also a clone
            of child structs
  */
  ClazzEnumInfo* clone(bool deep = true);
  inline bool equalsName(IN(acdk::lang::RString) n) const { return getMetaInfo()->equalsName(n); }
  inline const NamedScopedMetaInfo* getMetaInfo() const { return reinterpret_cast<const NamedScopedMetaInfo*>(this); }
  inline NamedScopedMetaInfo* getMetaInfo() { return reinterpret_cast<NamedScopedMetaInfo*>(this); }
};

/** 
  contains information of an field of a parsed class 
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @date $Date: 2005/04/18 14:22:27 $
*/

foreign class ACDK_CORE_PUBLIC ClazzFieldInfo
{
public:
   /** see acdk::lang::reflect::Modifier */
  int flags;
  /** @see acdk::lang::dmi::AttributesRes */
  void* attributeRes;

  /* the label of the field */
  const char* name;
  int nameHashCode;
  const char* ns;

  mutable const NamedScopedMetaInfo* _scopeParent;
  mutable const NamedScopedMetaInfo* _nextSibling;

  /** the type of current field */
  const ClazzInfo* type;
  /**
    Accessor to read/write the field.
    @see FieldAccessorFunction
  */
  FieldAccessorFunction accessor;

  /** in case of static members the address of fields*/
  void* address; // ### TODO check if really needed
  
 
  inline bool isTransient() const { return flags & MiFiTransient; }
  RString toTypeString(int formatflags) const;
  void toTypeString(StringBuffer& sb, int formatflags) const;
  int getHashValue() const;
  inline bool equalsName(IN(acdk::lang::RString) n) const { return getMetaInfo()->equalsName(n); }
  
  /**
    dispose this structure and owning
    Delete ClazzInfo if flags has MiDelete
  */
  
  void dispose();
  /**
    Make a dynamic clone of this structor
    @param deep if true make also a clone
            of child structs
  */
  ClazzFieldInfo* clone(bool deep = true);
  inline const TypedMetaInfo* getMetaInfo() const { return reinterpret_cast<const TypedMetaInfo*>(this); }
  inline TypedMetaInfo* getMetaInfo() { return reinterpret_cast<TypedMetaInfo*>(this); }
};

typedef ::acdk::lang::sys::core_vector<const ClazzFieldInfo*> ClazzFieldInfoVec;

class ClazzMethodArgInfo;
typedef ScriptVar (*GetDefaultArgValueFunc)(const ClazzMethodArgInfo* ai);

/** 
  contains information of an argument of an method
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @date $Date: 2005/04/18 14:22:27 $
*/

foreign class ACDK_CORE_PUBLIC ClazzMethodArgInfo
{
public:
  /** @see acdk::lang::dmi::MetaInfoFlags */
  int flags;
  /** @see acdk::lang::dmi::AttributesRes */
  void* attributeRes;

  /** label of the argument */
  const char* name; 
  int nameHashCode;
  const char* ns;
  /** normally ClazzInfo */
  mutable const NamedScopedMetaInfo* _scopeParent;
  mutable const NamedScopedMetaInfo* _nextSibling;

  /** type of the argument */
  const ClazzInfo* type; 
 
  GetDefaultArgValueFunc getDefaultArgValueFunc;
  /**
    render as ACDK source
    @param sb where to append
  */
  void toTypeString(StringBuffer& sb, int formatflags) const;
  RString toTypeString(int formatflags) const;
  int getHashValue() const;
  inline bool equalsName(IN(acdk::lang::RString) n) const { return getMetaInfo()->equalsName(n); }
  /**
    dispose this structure and owning
    Delete ClazzInfo if flags has MiDelete
  */
  void dispose();
  /**
    Make a dynamic clone of this structor
    @param deep if true make also a clone
            of child structs
  */
  ClazzMethodArgInfo* clone(bool deep = true);
  inline const TypedMetaInfo* getMetaInfo() const { return reinterpret_cast<const TypedMetaInfo*>(this); }
  inline TypedMetaInfo* getMetaInfo() { return reinterpret_cast<TypedMetaInfo*>(this); }
};

typedef ::acdk::lang::sys::core_vector<const ClazzMethodArgInfo*> ClazzMethodArgInfoVec;

/**
  Used to compare two methods
*/
foreign 
enum ClazzMethodCompareFlags
{
  /** return false if names are not equal */
  CompareName       = 0x01,
  /** return false if the types of the arguments are not equal */
  CompareArgs       = 0x02,
  /** return false if names of arguments are not equal */
  CompareArgNames   = 0x04,
  /** return false if return types are not equal */
  CompareReturnType = 0x08,
  /* return false if throwables of method are not equal */
  CompareThrowables = 0x10,
  /** return false if flags except access rights are not equal */
  CompareFlags      = 0x20,
  /** return false if methods have not equal access rights */
  CompareAccess     = 0x40,
  
  CompareDefault = CompareName | CompareArgs | CompareFlags
};

/** 
  contains information of a method
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @date $Date: 2005/04/18 14:22:27 $
*/

foreign class ACDK_CORE_PUBLIC ClazzMethodInfo
{
public: 
  /**
    @see acdk::lang::dmi::MetaInfoFlags
  */
  int flags;
  /** @see acdk::lang::dmi::AttributesRes */
  void* attributeRes;
  /** method name */
  const char* name;
  int nameHashCode;
  const char* ns;
  mutable const NamedScopedMetaInfo* _scopeParent;
  mutable const NamedScopedMetaInfo* _nextScopeSibling;

  // alias for type
  const ClazzInfo* returnType;
  
  /** alternative method name */
  const char* altlabel;
  int altlabelHashCode;
  /** type of the return value */
  
  /** list of arguments from the left to the right */
  ClazzMethodArgInfo** methodArgs;

  int argumentCount;
  /** the exceptions the method may throws */
  ClazzInfo** exceptions;
  /**
    dispatching function
  */
  DynamicDispatchFunction dispatch;
  /** 
    points to a method, which dispatch the exception orginated by this method, 
    which means throws best matching Exception.
    Method always throws an exception.
    Used in case a scripting implementator trows an untyped exception 
    this can mapped to a known type.
  */
  DispatchThrowableFunc dispatchThrowable;
  
  /**
    calculate the hash value of this
    method for fast dispatching
    don't use this value directly, but use the 
    getMethodSignatureHashValue() function
  */
  int _methodSignatureHashValue;
  
  /**
    returns a hash value of the method signature
    with 
    - flags (except access right flags)
    - name of method
    - argument flags and types
  */
  inline int getMethodSignatureHashValue() const
  {
    if (_methodSignatureHashValue != 0)
      return _methodSignatureHashValue;
    return _calcMethodSignatureHashValue();
  }
  /**
    return the number of expected arguments of this method
  */
  int getArgumentCount() const;
  /** returns the number of declared method throws exceptions */
  int getExceptionsCount() const;
  /**
    check if two methods are equal
    @param compareflags refer to ClazzMethodCompareFlags for possible flags
  */
  bool equals(const ClazzMethodInfo* other, int compareflags) const;
  inline bool equalsName(IN(acdk::lang::RString) n) const { return getMetaInfo()->equalsName(n); }
  inline bool equalsAltName(IN(acdk::lang::RString) n) const;
  inline int getAltNameHashCode() const
  {
    if (altlabelHashCode != -1)
      return altlabelHashCode;
    _calcAltlabelHashCode();
    return altlabelHashCode;
  }
  void _calcAltlabelHashCode() const;
  /**
    return if this method is virtual
    either if this method flags has MiMiVirtual
    or if the same method signature in base classes are
    virtual.
    @param clazz the class owns this method
  */
  bool isVirtual(const ClazzInfo* clazz) const;
  bool isVirtual() const 
  { 
    if (_scopeParent->isClazzInfo() == false)
      return false;
    return isVirtual((const ClazzInfo*)_scopeParent); 
  }
  /**
    render as ACDK source
    @param sb where to append
    @param clazz. if clazz != 0 void ClassName::FuncName(Param1 p) throw(Exception)
                  will be printed. otherwise without ClassName
  */
  void toTypeString(StringBuffer& sb, const ClazzInfo* clazz, int formatflags) const;
  
  RString toTypeString(const ClazzInfo* clazz, int formatflags) const;
  RString toTypeString(int formatflags = TpFtFormatStandard) const;
  static void DefaultDispatchThrowableFunc(IN(::acdk::lang::RThrowable) ex);
  /**
    dispose this structure and owning
    Delete ClazzInfo if flags has MiDelete
  */
  void dispose();
  /**
    Make a dynamic clone of this structor
    @param deep if true make also a clone
            of child structs
  */
  ClazzMethodInfo* clone(bool deep = true);
  /**
    add argument to this method.
    Only valid if this method is dynamic
  */
  void addArgument(const ClazzMethodArgInfo* ai);
  /**
    add throwable spec to this method.
    Only valid if this method is dynamic
  */
  void addThrowable(const ClazzInfo* ex); 
  static void throwMethodNotFound(const ClazzInfo* clazz, IN(acdk::lang::RString) fname, int flags, int formatFlags);

//private methods
  static int _calcHashValue(::acdk::lang::dmi::ClazzMethodArgInfo** args);
  int _calcMethodSignatureHashValue() const;
  void _resolveParents(const ClazzInfo* ci) const;
  inline const NamedScopedParentMetaInfo* getMetaInfo() const { return reinterpret_cast<const NamedScopedParentMetaInfo*>(this); }
  inline NamedScopedParentMetaInfo* getMetaInfo() { return reinterpret_cast<NamedScopedParentMetaInfo*>(this); }
};

typedef ::acdk::lang::sys::core_vector<const ClazzMethodInfo*> ClazzMethodInfoVec;

/** 
  contains information of a Super class
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @date $Date: 2005/04/18 14:22:27 $
*/

foreign class ACDK_CORE_PUBLIC ClazzSuperInfo
{
public:
  /**
    @see acdk::lang::dmi::MetaInfoFlags
  */
  int flags;
  /** @see acdk::lang::dmi::AttributesRes */
  void* attributeRes;

  /** super type */
  const ClazzInfo* type;
  /**
    dispose this structure and owning
    Delete ClazzInfo if flags has MiDelete
  */
  void dispose();
  /**
    Make a dynamic clone of this structor
    @param deep if true make also a clone
            of child structs
  */
  ClazzSuperInfo* clone(bool deep = true);
  inline const MetaInfo* getMetaInfo() const { return reinterpret_cast<const MetaInfo*>(this); }
  inline MetaInfo* getMetaInfo() { return reinterpret_cast<MetaInfo*>(this); }
  RString toTypeString(int format = TpFtFormatStandard) const;
};
typedef ::acdk::lang::sys::core_vector<const ClazzSuperInfo*> ClazzSuperInfoVec;

/**
  Used to hook register ClazzInfo
  or to iterate through all ClazzInfos
*/
typedef bool (*ClazzInfoCallBack)(const ClazzInfo* ci, int flags);
/**
  used to cast from object to a interface
  this callback normally calls void* ClassName::_castToInterfacePtr(Object* obj);
*/
typedef void* (*CastToInterfacePtrFunc)(Object* obj);

/** 
  contains meta information of a ACDK class
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @date $Date: 2005/04/18 14:22:27 $
*/

foreign class ACDK_CORE_PUBLIC ClazzInfo 
{
public:
  /**
    @see acdk::lang::dmi::MetaInfoFlags
    aka flags
  */
  int flags;
  /** @see acdk::lang::dmi::AttributesRes */
  void* attributeRes;
  /** name of class */
  const char* name;
  int nameHashCode;

  const char* ns;
  /** namespace of class */
  mutable const NamedScopedMetaInfo* _scopeParent;
  mutable const NamedScopedMetaInfo* _nextScopeSibling;

  /** type == this */
  const ClazzInfo* type;
  /** list of methods, members (and later maybe nested types) */
  mutable const NamedScopedMetaInfo* _firstChild;

  /** an 0 terminated array of super + interfaces */
  ClazzSuperInfo** interfaces;
  /** 
    count of current super / interfaces 
    don't access this value directly, but
    use the getIntefacesCount() method
  */
  int _interfacesCount;
  /** all found fields */
  ClazzFieldInfo** fields;
  /** 
    count of current declared fields 
    don't access this field directly, but use the 
    getFieldsCount() method
  */
  int _fieldsCount;
  /** all found methods including Constructors */
  ClazzMethodInfo** methods;
  /** 
    count of current declared methods including constructors 
    don't access this field directly but use the
    getMethodsCount() function
  */
  int _methodsCount;
  /** the creator-functions */
  ObjectCreator creator;
  
   /** function creates an Array instance of given class */
  ArrayCreator array_creator ;
  /** function creates a 2 dimension Array instance of given class */
  ArrayArrayCreator array_array_creator;

  /** singelton for Class instance */
  Class* thisClass; 
  /** 
    precompiled hashvalue for serialization 
    dont access this value directly, but use
    the getSerialVersionUID() method
  */
  jlong _serialVersionUID;

  /**
    used to invoke dynamic methods
    not used if method provides dispatch 
  */
  DynamicDispatchFunction dynamic_dispatch;
  /**
    used to invoke static methods
    not used if method provides dispatch 
  */
  StandardDispatchFunction static_dispatch;
  
  /** 
    number of collectable Fields (aka RObject's) in this class 
    dont use this value directly but use
    the getCollectableFieldsCount() method
  */
  int _collectableFields;
  /** used for Array type */
  mutable ClazzInfo *userInfo;
  
  /** 
    used to cast a object to a interface pointer
    with correct vtable
  */
  CastToInterfacePtrFunc _castToInterfacePtr;
  
  /**  next in chain */
  mutable ClazzInfo* _next; 

  inline int getInterfacesCount() const
  {
    if (_interfacesCount != 0)
      return _interfacesCount;
    return _calcInterfacesCount();
  }
  /**
    return the number of fields declared by this class
  */
  inline int getFieldsCount() const
  {
    if (_fieldsCount != 0)
      return _fieldsCount;
    return _calcFieldsCount();
  }
  /**
    return the number of methods (including constructors) 
    declared by this class
  */
  int getMethodsCount() const
  {
    if (_methodsCount != 0)
      return _methodsCount;
    return _calcMethodsCount();
  }
  /**
    get a hash value using namespace and name
    of this class
  */
  int getHashValue() const;
  inline bool equalsName(IN(acdk::lang::RString) n) const { return getMetaInfo()->equalsName(n); }
  
  /**
    dispose this structure and owning
    Delete ClazzInfo if flags has MiDelete
  */
  void dispose();
  /**
    Make a dynamic clone of this structor
    @param deep if true make also a clone
            of child structs
  */
  ClazzInfo* clone(bool deep = true);
  /**
    get an id for this class, describing all
    fields and methods
  */
  jlong getSerialVersionUID() const
  {
    if (_serialVersionUID != 0)
      return _serialVersionUID;
    return _calcSerialVersionUID();
  }
  /** 
    make ClazzInfo public available 
    API: ACDK
    normally only used by metacompiler generated code
  */
  /**
    Checks if one clazz is asignable to another
    @param this is the target (right side of assignment)
    @param from is the source (left side of assignment)
    @return true if type of same class or this is an interface 
            implemented by from

    \htmlonly
    <source>
      Number::clazzInfo()->assignableFrom(Integer::clazzInfo()); // -> returns true
      Integer::clazzInfo()->assignableFrom(Number::clazzInfo()); // -> returns false
      // but not working with basic types:
      ClazzInfo::getLongClazz()->assignableFrom(ClazzInfo::getIntClazz()); // return false
    </source>
    To get full test of assignable use DmiClient::typeDistance() instead
  */
  bool assignableFrom(const ClazzInfo* from) const;
  /**
    return -1 if fromType cannot be assigned to this type
    return 0 if fromType is this type
    return > 0 if type can be assigned
    greater values represents more distance between the types
  */
  int assignDistance(const ClazzInfo* fromType) const;
  static const ClazzInfo* getUnknownBasicClazz();
  static const ClazzInfo* getCharClazz();
  static const ClazzInfo* getUcCharClazz();
  static const ClazzInfo* getByteClazz();
  static const ClazzInfo* getShortClazz();
  static const ClazzInfo* getIntClazz();
  static const ClazzInfo* getLongClazz();
  static const ClazzInfo* getFloatClazz();
  static const ClazzInfo* getDoubleClazz();
  static const ClazzInfo* getBoolClazz();
  static const ClazzInfo* getVoidClazz();
  
  bool isBoolClazz() const { return this == getBoolClazz(); }
  bool isCharClazz() const { return this == getCharClazz(); }
  bool isUcCharClazz() const { return this == getUcCharClazz(); }
  bool isByteClazz() const { return this == getByteClazz(); }
  bool isShortClazz() const { return this == getShortClazz(); }
  bool isIntClazz() const { return this == getIntClazz() /*|| isEnumeration() == true*/; }
  bool isLongClazz() const { return this == getLongClazz(); }
  bool isFloatClazz() const { return this == getFloatClazz(); }
  bool isDoubleClazz() const { return this == getDoubleClazz(); }
  bool isVoidClazz() const {   return this == getVoidClazz(); }

#ifdef ACDK_SUPPORT_ANSI_SPECIALIZATION
  template <class T> template_static const ClazzInfo* getBasicTypeClazz(T t);
#if 0
  template <> template_static const ClazzInfo* getBasicTypeClazz<char>(char ) { return getCharClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<byte>(byte ) { return getByteClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<short>(short ) { return getShortClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<int>(int ) { return getIntClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<jlong>(jlong ) { return getLongClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<bool>(bool ) { return getBoolClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<float>(float ) { return getFloatClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<double>(double ) { return getDoubleClazz(); }
#endif //0
#else
  template <class T> template_static const ClazzInfo* getBasicTypeClazz(T ) { return getUnknownBasicClazz(); }
  template <char> template_static const ClazzInfo* getBasicTypeClazz(char ) { return getCharClazz(); }
  template <ucchar> template_static const ClazzInfo* getBasicTypeClazz(ucchar) { return getUcCharClazz(); }
  template <byte> template_static const ClazzInfo* getBasicTypeClazz(byte ) { return getByteClazz(); }
  template <short> template_static const ClazzInfo* getBasicTypeClazz(short ) { return getShortClazz(); }
  template <int> template_static const ClazzInfo* getBasicTypeClazz(int ) { return getIntClazz(); }
  template <jlong> template_static const ClazzInfo* getBasicTypeClazz(jlong ) { return getLongClazz(); }
  template <bool> template_static const ClazzInfo* getBasicTypeClazz(bool ) { return getBoolClazz(); }
#if !defined(__SUNPRO_CC)
  template <float> template_static const ClazzInfo* getBasicTypeClazz(float ) { return getFloatClazz(); }
  template <double> template_static const ClazzInfo* getBasicTypeClazz(double ) { return getDoubleClazz(); }
#endif //defined(__SUNPRO_CC)
#endif
  
  /**
    load full clazzInfo for DMI-functionality
    @param returnSuperIfAvail if true method return the next super clazzinfo with metainfo 
           it try to find metainfo first in the direct super classes, than in deep first order
    @param throwExIfNotFound if true, function throws exception, if metainfo cannot be found
    @return ClazzInfo can be found. Depends on the parameters
  */
  inline const ClazzInfo* loadFullClazzInfo(bool returnSuperIfAvail = true, bool throwExIfNotFound = true) const { return isResolved() ? this : _loadFullClazzInfo(returnSuperIfAvail, throwExIfNotFound); }
  /**
    internal function to load external metainfo shared library
  */
  const ClazzInfo* _loadFullClazzInfo(bool returnSuperIfAvail, bool throwExIfNotFound) const;
  /**
    internal function to load extended metainfo for interfaces of this ClazzInfo
  */
  void  _resolveSupers(bool returnSuperIfAvail, bool throwExIfNotFound) const;
  /**
    return true if full metainfo is available.
    use loadFullClazzInfo to load full metainfo
  */
  inline bool isResolved() const { return MetaInfo::isResolved(flags); }
  inline void registerClazzInfo() const { if ((flags & MiRegistered) == 0) registerClazzInfo(this); }

  static void registerClazzInfo(const ClazzInfo* clazz);
  /** 
    make ClazzInfo for Array public available 
    API: ACDK
    normally only used internally
  */
  static void registerArrayClazzInfo(const ClazzInfo* clazz);
  
  /** 
    unregister the ClazzInfo
    API: ACDK
    Use this if Classes are not longer availabe, i.e. a shared library will unloaded
  */
  static void unregisterClazzInfo(const ClazzInfo* clazz);

  /**
    remove all temporary created ClazzInfo for Arrays
    API: ACDK
    normally only used internally
  */
  static void unregisterAllArrayClazzInfo();
  /**
    try to find class on given name and namespace
    This method doesn't try to load the class from a library
  */
  static const ClazzInfo* findClazzInfo(IN(RString) classname, IN(RString) namesp);
  
  /**
    try to find the class on given name
    @param fqclassname fully qualified class acdk.lang.StringBuffer or acdk/lang/StringBuffer
    @param tryLoad if true, use the systems class loader to load given class.
  */
  static const ClazzInfo* findClazzInfo(const String& fqclassname, bool tryLoad = true);
  /**
    try to find the class on given name.
    The fqclass may has the form with . :: or / as component divider
    @param fqclassname fully qualified class acdk.lang.StringBuffer or acdk/lang/StringBuffer
    @param tryLoad if true, use the systems class loader to load given class.
  */
  static const ClazzInfo* findClazzInfo(IN(RString) fqclassname, bool tryLoad = true);
  /**
    Try to find the class on given name and return 0 if not found.
    This method doesn't try to load libraries
    @param fqclassname fully qualified class acdk.lang.StringBuffer or acdk/lang/StringBuffer
  */
  static const ClazzInfo* findArrayClazzInfo(const String& elementname);
  /**
    Try to find the class on given name and return 0 if not found.
    The elementname must be normalized with / component divider
    This method ties also to load libraries
    @param fqclassname fully qualified class acdk.lang.StringBuffer or acdk/lang/StringBuffer
    @param tryLoad if true, use the systems class loader to load given class.
  */
  static const ClazzInfo* findArrayClazzInfo(IN(RString) elementname, bool tryLoad = true);
  /**
    Try to find the class on given name and return 0 if not found.
    This method doesn't try to load libraries
    @param fqclassname fully qualified class acdk.lang.StringBuffer or acdk/lang/StringBuffer
  */
  static const ClazzInfo* findArrayClazzInfo(const ClazzInfo* elementtype);
  /**
    helper class to split a given classname into class an namespace part.
    @param compName is a fully qualified class name in the form 
            my::package::MyClass, my.package.MyClass or my/package/MyClass.
    @param className returns the class name
    @param namespaceName returns the namespace in the form my/package
  */
  static void splitClassAndNs(IN(RString) compName, OUT(RString) className, OUT(RString) namespaceName);
  /**
    helper class returns the left last element of a given component 
    @param name has to be be a component identifier in the form my/package/subpackage
    @param left return for example my/package. left will be unset (Nil) if name has no components
    @param right return for example namespaceName. right contains the name if name has no components
  */
  static void splitLastElement(IN(RString) name, OUT(RString) left, OUT(RString) right);
  /**
    find a field with given name and given flags
    @param fieldname name of the field
    @param flags flag which as to match in normal case PUBLIC and/or STATIC, IsDeclared, TRANSIENT
    @return found member otherwise throw NoSuchElementException
    @throw NoSuchElementException if not found
  */
  static const ClazzFieldInfo* findField(const ClazzInfo*& clazz, IN(RString) fieldname, int flags);
  
  /**
    return 0 if first interface is not an normal class (not interface)
  */
  inline const ClazzSuperInfo* getSuper() const
  {
    if (getInterfacesCount() == 0 || interfaces[0]->type->flags & MiCiInterface)
      return 0;
    return interfaces[0];
  }
  /**
    find the super class by given name 
    Expect java style type name
    @return 0 if not found
  */
  const ClazzSuperInfo* findSuper(IN(RString) supername) const;
  /**
    find the super class by given name 
    Expect java style type name
    @return 0 if not found
  */
  //const ClazzSuperInfo* findSuper(IN(RString) supername) const;
  /**
    find the super class by given name 
    Expect java style type name
    @return 0 if not found
  */
  inline const ClazzInfo* findSuperClazz(IN(RString) supername) const
  {
    const ClazzSuperInfo* su = findSuper(supername);
    if (su == 0)
      return 0;
    return su->type;
  }
  /** 
    Find member metainfo.
    returns field or function or Super
  */
  const MetaInfo* findMetaInfo(IN(RString) name, int flags) const;
  /**
    returns true if this ClazzInfo is a basic type
  */
  bool isBasicClazz() const { return flags & MiCiBasicType; }
  /**
    return true if this ClazzInfo is an Array
  */
  bool isArray() const;
  /**
    return true if byte, short, int or jlong
  */
  inline bool isIntegerTypeClazz() const
  {
    return isByteClazz() == true ||
         isShortClazz() == true ||
         isIntClazz() == true ||
         isLongClazz() == true;
  }
  /**
    return true if float or double
  */
  inline bool isFloatingTypeClazz() const
  {
    return isFloatClazz() == true || isDoubleClazz();
  }
  /**
    return true if byte, short, int, jlong, float or double
  */
  inline bool isNumberTypeClazz() const { return isIntegerTypeClazz() || isFloatingTypeClazz(); }
  /**
    return true if char or uc2char
  */
  inline bool isCharacterTypeClazz() const { return isUcCharClazz() || isCharClazz(); }
  /**
    return true if this ClazzInfo is an enumeration
  */
  static bool isRestArg(const ClazzInfo* ci);
  static bool isNamedRestArg(const ClazzInfo* ci);
  /**
    returns the enumeration integer value for
    the given enumeration identifier
    return -1 if this is not an enumeration.
  */
  int getEnumerationValue(IN(RString) key) const;
  /** 
    returns sum of all collectableFields in hierarchie 
    use Object::collectableFieldsCount();
  */
  int getCollectableFieldsCount() const
  {
    if (_collectableFields != 0)
      return _collectableFields;
    return _calcCollectableFieldsCount();
  }
  /**
    return the field definitions
    @param flags combination of Modifier flags
  */
  void getFields(ClazzFieldInfoVec& fields, int flags) const;
 

  /**
    prints this type as Type name: ::acdk::lang::StringBuffer or ::acdk::lang::RStringBuffer
    @param format combination of flags  TypeNameFormat
  */
  void toTypeString(StringBuffer& sb, int format = TpFtFormatStandard) const;
  RString toTypeString(int format = TpFtFormatStandard) const;
  /**
    find all classes in given namespace. 
    
    @param ns the namespace where to start the search
              use "" to find from root
    @param ret where to put the find ClazzInfo into.
               the core_vector append method will be used.
    @param recursive search recursially in child namespaces
  */
  static void findClasses(IN(RString) ns, ::acdk::lang::sys::core_vector<const ClazzInfo*>& ret, bool recursive = false);
  /**
    find all namespaces in given namespace. 
    
    @param ns the namespace where to start the search
              use "" to find from root
    @param ret where to put the namespaces into.
               the core_vector append method will be used.
    @param recursive search recursially in child namespaces
  */
  static void findNamespaces(IN(RString) ns, ::acdk::lang::sys::core_vector<const char*>& ret, bool recursive = false);
  /** 
    cicb will called every time a ClazzInfo will be registered (flag == 1)
    or unregistered (flag == 2).
    return value cicb will be ignored
  */
  static void addRegisterClazzHook(ClazzInfoCallBack cicb);
  /**
    removed previously added with addRegisterClazzHook
  */
  static void removeRegisterClazzHook(ClazzInfoCallBack cicb);
  /**
    Call cicb for each registered ClazzInfo.
    If cicb returns false the listining ends
  */
  static void forEachClazzInfo(ClazzInfoCallBack cicb);
  /**
    Call cicb for each registered ClazzInfo.
    If cicb returns false the listining ends
  */
  static void forEachArrayClazzInfo(ClazzInfoCallBack cicb);
  /**
    get Internal root of ClazzInfo
  */
  static const ClazzInfo* getClazzInfoRoot();
  /**
    get Internal root of ClazzInfo Arrays
  */
  static const ClazzInfo* getArrayClazzInfoRoot();
  /**
    create an ArrayClazzInfo on this element type
  */
  const ClazzInfo* createArrayClazzInfo(int dims) const;
  /**
    this has to be an array.
    @return the ClazzInfo hold by this array as element
  */
  const ClazzInfo* getElementClazzInfo() const { return (const ClazzInfo*)userInfo; }
  /**
    return the deepest non-array type element
    if this is not array, return this
  */
  const ClazzInfo* getArrayElementType() const;
  /**
    return the number of dimensions of this array
    returns 0 if this ClazzInfo is not an array
  */
  int getArrayDims() const;
  /**
    returns an initialized ScriptVar
    of this type
  */
  ScriptVar createInitializedValue() const;
  /**
    call the method(s) registered at attribute "__acdk_classinitfunction"
    via DMI
    @see acdk::tools::mc::ClassInitAttribute
  */
  void callClassInitializer() const;
  /**
    call the method(s) registered at attribute "__acdk_classdeinitfunction"
    via DMI
    @see acdk::tools::mc::ClassInitAttribute
  */
  void callClassDeinitializer() const;
  /**
    Add a method to this clazz.
    only valid if dynamically allocated
  */
  void addMethod(const ClazzMethodInfo* mi);
  /**
    Add a field to this clazz.
    only valid if dynamically allocated
  */
  void addField(const ClazzFieldInfo* fi);
  /**
    Add a super info to this clazz.
    only valid if dynamically allocated
  */
  void addSuper(const ClazzSuperInfo* si);
  inline const NamedScopedParentMetaInfo* getMetaInfo() const { return reinterpret_cast<const NamedScopedParentMetaInfo*>(this); }
  inline NamedScopedParentMetaInfo* getMetaInfo() { return reinterpret_cast<NamedScopedParentMetaInfo*>(this); }

// private methods
  int _calcInterfacesCount() const;
  int _calcMethodsCount() const;
  int _calcFieldsCount() const;
  jlong _calcSerialVersionUID() const;
  int _calcCollectableFieldsCount() const;
  /**
    resolve parent links for members
  */
  void _resolveMemberParents() const;
};



typedef ::acdk::lang::sys::core_vector<const ClazzInfo*> ClazzInfoVec;



#ifdef ACDK_SUPPORT_ANSI_SPECIALIZATION
  template <class T> template_static const ClazzInfo* getBasicTypeClazz(T t) 
  { 
    return ClazzInfo::getUnknownBasicClazz(); 
  }
  template <> template_static const ClazzInfo* getBasicTypeClazz<char>(char ) { return ClazzInfo::getCharClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<uc2char>(uc2char ) { return ClazzInfo::getUcCharClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<byte>(byte ) { return ClazzInfo::getByteClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<short>(short ) { return ClazzInfo::getShortClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<int>(int ) { return ClazzInfo::getIntClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<jlong>(jlong ) { return ClazzInfo::getLongClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<bool>(bool ) { return ClazzInfo::getBoolClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<float>(float ) { return ClazzInfo::getFloatClazz(); }
  template <> template_static const ClazzInfo* getBasicTypeClazz<double>(double ) { return ClazzInfo::getDoubleClazz(); }
  /**
    return the ClazzInfo from given type
  */
template <class T> 
inline
//template_static 
const ClazzInfo* 
ClazzInfo::getBasicTypeClazz(T t) 
  { 
    //return getUnknownBasicClazz(); 
    return ::acdk::lang::dmi::getBasicTypeClazz(t);
  }

#endif

/// @internal
template <class T> inline  int  get0TerminatedArraySize(T t) 
{
  int i = -1;
  while (t[++i])
    ;
  return i;
}


/**
  for query functions in ::acdk::lang::Class a 
  minimal definition of a method signature.
  API: ACDK<br>
  @see ClazzMethodInfo
  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @date $Date: 2005/04/18 14:22:27 $
*/
foreign class ACDK_CORE_PUBLIC FunctionSignature
{
public:
  /**
    the name of the function/method 
  */
  const char* functionname;
  
  /**
    number of arguments in args
  */
  int size;
  /**
    array of argument types
  */
  const ClazzInfo** args;
};


#if !defined(DOXYGENONLY)
typedef void (*InitFunc)();
typedef void (*DeinitFunc)();

/** 
  just a little helper for using in genated Metainformation.
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.61 $
  @date $Date: 2005/04/18 14:22:27 $
  @internal
*/

foreign class ACDK_CORE_PUBLIC RegisterClazzInfo
{
  const ClazzInfo* _clazz;
  DeinitFunc _deinitFunc;
public: 
  RegisterClazzInfo(const ClazzInfo* clazz, InitFunc initfunc = 0, DeinitFunc deinitfunc = 0)
  : _clazz(clazz)
  , _deinitFunc(deinitfunc)
  {
    ClazzInfo::registerClazzInfo(clazz);
    if (initfunc != 0)
      initfunc();
  }
  ~RegisterClazzInfo()
  {
    if (_deinitFunc != 0)
      _deinitFunc();
    ClazzInfo::unregisterClazzInfo(_clazz);
  }
};

/**
  Helper class to register enumeration info at load
  time
  @internal
*/
foreign class ACDK_CORE_PUBLIC RegisterEnumInfo
{
  const ClazzEnumInfo* _ei;
  DeinitFunc _deinitFunc;
public: 
  RegisterEnumInfo(const ClazzEnumInfo* ei, InitFunc initfunc = 0, DeinitFunc deinitfunc = 0)
  : _ei(ei)
  , _deinitFunc(deinitfunc)
  {
    ClazzEnumInfo::registerEnumInfo(_ei);
    if (initfunc != 0)
      initfunc();
  }
  ~RegisterEnumInfo()
  {
    if (_deinitFunc != 0)
      _deinitFunc();
    ClazzEnumInfo::unregisterEnumInfo(_ei);
  }
};
/// @internal
typedef void (*InitUnitInfoFunc)(const UnitInfo* ui);
/// @internal
typedef void (*DeinitUnitInfoFunc)(const UnitInfo* ui);

/// @internal
foreign class ACDK_CORE_PUBLIC RegisterUnitInfo
{
  const UnitInfo* _unitInfo;
  DeinitUnitInfoFunc _deinitFunc;
public: 
  RegisterUnitInfo(const UnitInfo* ui, InitUnitInfoFunc initfunc = 0, DeinitUnitInfoFunc deinitfunc = 0);
  ~RegisterUnitInfo();
};

#endif //!defined(DOXYGENONLY)

/** 
  defines an Attribute for a Unit
  Will be evaluated by acdkmc
  @ingroup acdkmetainfo
  @bug not yet implemented
  @see gw_ref[acdk_hb_mi_attributes]
*/
#define ACDK_UNITATTRIBUTE(text)

/** 
  defines an Attribute for a Class
  Will be evaluated by acdkmc.
  @ingroup acdkmetainfo
  @see gw_ref[acdk_hb_mi_attributes]
*/
#define ACDK_CLASSATTRIBUTE(text)

/** 
  defines an Attribute for Super definitions
  Will be evaluated by acdkmc
  @bug not yet implemented
  @ingroup acdkmetainfo
  @see gw_ref[acdk_hb_mi_attributes]
*/
#define ACDK_SUPERATTRIBUTE(text)

/** defines an Attribute for a Method
    Will be evaluated by acdkmc
    @ingroup acdkmetainfo
    @see gw_ref[acdk_hb_mi_attributes]
*/
#define ACDK_METHODATTRIBUTE(text)

/** defines an Attribute for a Method
    Will be evaluated by acdkmc
    @ingroup acdkmetainfo
    @see gw_ref[acdk_hb_mi_attributes]
*/
#define ACDK_FIELDATTRIBUTE(text)


/** defines an Attribute for a Paramter
    Will be evaluated by acdkmc
    @ingroup acdkmetainfo
    @see gw_ref[acdk_hb_mi_attributes]
*/
#define ACDK_PARAMATTRIBUTE(text)

/** 
  defines a unit
  text should be the namespace
  in following form:
  namespace: acdk::util::logging
  text: acdk_util_logging
  @ingroup acdkmetainfo
*/
#define ACDK_DECL_UNIT(text) \
extern ::acdk::lang::dmi::UnitInfo text##_unitInfo;

} // namespace dmi
} // namespace acdk
} // namespace lang


#endif //acdk_lang_dmi_ClazzInfo_h
