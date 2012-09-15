// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_DClazzInfo_h
#define acdk_aci_DClazzInfo_h

#include <acdk.h>
#include "Config.h"
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/DmiNamedArg.h>
#include "./vm/Executable.h"

namespace acdk {
namespace aci {


ACDK_DECL_CLASS(ClazzTypes);
ACDK_DECL_CLASS(DClazzInfo);
ACDK_DECL_CLASS(Compiler);


ACDK_DECL_INTERFACE(SemanticElem);
class ACDK_ACI_PUBLIC SemanticElem
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(SemanticElem)
public:
  /**
    return the name of this Element
    For example:
    Class -> FqClassName
    Variable -> varname
    Function -> functionanme
  */
  virtual RString getName() = 0;
  /**
    in subscription, look for a sub sem addressed via subscription
  */
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op) = 0;
  /**
    returns the Type of this expression
  */
  virtual RDClazzInfo getType() = 0;
  virtual int& getFlags() = 0;
  /**
    returns the class, which owns this element
    @return Nil if this element is not owned by a class
  */
  virtual RDClazzInfo getOwnerClass() { return Nil; }
};

class ACDK_ACI_PUBLIC ClazzTypes
: extends acdk::lang::Object
{

protected:
  bool _delete;
  ClazzTypes(bool del) : _delete(del) {}
  static char* strdup(IN(RString) str);
  static void strdel(const char*& ptr);
};

ACDK_DECL_CLASS(DUnitInfo);

class ACDK_ACI_PUBLIC DUnitInfo
: extends ClazzTypes
, implements SemanticElem
{
  ACDK_WITH_METAINFO(DUnitInfo)
protected:
  acdk::lang::dmi::UnitInfo* _unitInfo;
public:
  DUnitInfo(IN(RString) unitname);
  foreign DUnitInfo(acdk::lang::dmi::UnitInfo* unitInfo)
  : ClazzTypes(false)
  , _unitInfo(unitInfo)
  {
  }
  ~DUnitInfo();
  int& getFlags() { return _unitInfo->flags; }
  foreign acdk::lang::dmi::UnitInfo* getUnitInfo() { return _unitInfo; }
  RString toString() { return "namespace " + getName(); }
  RString getName() { return _unitInfo->name; }
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);
  virtual RDClazzInfo getType() { return Nil; }
};

ACDK_DECL_CLASS(DClazzFieldInfo);

class ACDK_ACI_PUBLIC DClazzFieldInfo
: extends ClazzTypes
, implements SemanticElem
{
  //DECL_ACDK_DEFAULT_METACLASS(SemanticElem)
  ACDK_WITH_METAINFO(DClazzFieldInfo)
public:
  
  acdk::lang::dmi::ClazzFieldInfo* _fieldInfo;
  const acdk::lang::dmi::ClazzInfo* _clazzInfo;
  RDClazzInfo _type;
public:

  DClazzFieldInfo(const acdk::lang::dmi::ClazzInfo* clazzInfo, acdk::lang::dmi::ClazzFieldInfo* fieldInfo);
  DClazzFieldInfo(int flags, IN(RString) name, IN(RDClazzInfo) type, const acdk::lang::dmi::ClazzInfo* clazzInfo);
  ~DClazzFieldInfo();
  /**
    returns field declaration
  */
  virtual RString toString();
  acdk::lang::dmi::ClazzFieldInfo* getFieldInfo() { return _fieldInfo; }

  virtual RString getName() { return _fieldInfo->name; }
  int& getFlags() { return _fieldInfo->flags; }
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);
  virtual RDClazzInfo getType();
  
  bool isStatic() { return acdk::lang::dmi::MetaInfo::isStatic(_fieldInfo->flags); }
  const acdk::lang::dmi::ClazzInfo* getOwnerClazz() { return _clazzInfo; }
  RString getOwnerClassName();
  RDClazzInfo getOwnerClass();
};



ACDK_DECL_CLASS(DClazzMethodArgInfo);
class ACDK_ACI_PUBLIC DClazzMethodArgInfo
: extends ClazzTypes
{
  
  const acdk::lang::dmi::ClazzInfo* _clazzInfo;
  const acdk::lang::dmi::ClazzMethodInfo* _methodInfo;
  acdk::lang::dmi::ClazzMethodArgInfo* _methodArgInfo;
  RDClazzInfo _type;
public:
  DClazzMethodArgInfo(const acdk::lang::dmi::ClazzInfo* clazzInfo, const acdk::lang::dmi::ClazzMethodInfo* methodInfo,
                   IN(RString) name, IN(RDClazzInfo) type, int flags = 0);
  ~DClazzMethodArgInfo();
  acdk::lang::dmi::ClazzMethodArgInfo* getMethodArgInfo() { return _methodArgInfo; }
  RDClazzInfo getType() { return _type; }
  RString getName() { return _methodInfo->name; }
  int& getFlags() { return _methodArgInfo->flags; }
};

ACDK_DECL_CLASS(DClazzMethodInfo);

class ACDK_ACI_PUBLIC DClazzMethodInfo
: extends ClazzTypes
, implements SemanticElem
{
  //DECL_ACDK_DEFAULT_METACLASS(SemanticElem)
  ACDK_WITH_METAINFO(DClazzMethodInfo)

  acdk::lang::dmi::ClazzMethodInfo* _methodInfo;
  const acdk::lang::dmi::ClazzInfo* _clazzInfo;
  RDClazzInfo _retType;
  RDClazzMethodArgInfoArray _args;
  acdk::aci::vm::RExecutableArray _code;
public:
  DClazzMethodInfo(int flags, IN(RString) name, const acdk::lang::dmi::ClazzInfo* clazzInfo);
  DClazzMethodInfo(const acdk::lang::dmi::ClazzInfo* clazzInfo, const acdk::lang::dmi::ClazzMethodInfo* methodInfo);
  ~DClazzMethodInfo();
  /**
    return method declarations
  */
  virtual RString toString();
  virtual RString getName() { return _methodInfo->name; }
  void setName(IN(RString) name);
  virtual RString getCallName() { return _methodInfo->altlabel == 0 ? _methodInfo->name : _methodInfo->altlabel; }
  int& getFlags() { return _methodInfo->flags; }
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);
  virtual RDClazzInfo getType();
  RDClazzInfo getOwnerClass();
  const acdk::lang::dmi::ClazzInfo* getOwnerClazz() { return _clazzInfo; }
  bool isStatic() { return _methodInfo->flags & acdk::lang::dmi::MiStatic; }
  bool isConstructor() { return _methodInfo->flags & acdk::lang::dmi::MiMiConstructor; }
  /**
    clones the arguments from other methodinfo
  */
  void cloneArgs(IN(RDClazzMethodInfo) other);
  acdk::lang::dmi::ClazzMethodInfo* getMethodInfo() { return _methodInfo; }
  RDClazzInfo getReturnType() { return _retType; }
  
  void setReturnType(IN(RDClazzInfo) ci);
  void addParameter(IN(RString) name, IN(RDClazzInfo) ci, int flags = 0);
  acdk::aci::vm::RExecutableArray getCode();
  /** add _code as attribute to current method under the key __aci_code */
  void makeCodeAttribute();
  foreign virtual RObject clone(acdk::lang::sys::Allocator* alc);
  foreign RObject clone() { return clone(allocator()); }
  /**
    return true if return type and arguments has the same types
  */
  bool hasCompatibleSignature(const acdk::lang::dmi::ClazzMethodInfo* other);
  void setAltName(int counter);
  
};

ACDK_DECL_CLASS(DClazzSuperInfo);

class ACDK_ACI_PUBLIC DClazzSuperInfo
: extends ClazzTypes
, implements SemanticElem
{
  DECL_ACDK_DEFAULT_METACLASS(SemanticElem)
public:
  const acdk::lang::dmi::ClazzInfo* _clazzInfo;
  acdk::lang::dmi::ClazzSuperInfo* _superInfo;
  DClazzSuperInfo(const acdk::lang::dmi::ClazzInfo* ci, acdk::lang::dmi::ClazzSuperInfo* si)
  : ClazzTypes(false)
  , _clazzInfo(ci)
  , _superInfo(si)
  {
  }
  //DClazzSuperInfo(const acdk::lang::dmi::ClazzInfo* ci, IN(RString) superName);
  DClazzSuperInfo(const acdk::lang::dmi::ClazzInfo* ci, IN(RDClazzInfo) superclazz);
  ~DClazzSuperInfo();
  int& getFlags() { return _superInfo->flags; }
  virtual RString getName() { return _superInfo->type->name; }
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op)
  {
    return Nil;
  }
  virtual RDClazzInfo getType();
};

/**
  Dynamic wrapper version of acdk::lang::dmi::ClazzInfo
*/
class ACDK_ACI_PUBLIC DClazzInfo
: extends ClazzTypes
, implements SemanticElem
{
  //DECL_ACDK_DEFAULT_METACLASS(SemanticElem)
  ACDK_WITH_METAINFO(DClazzInfo)
protected:
  acdk::lang::dmi::ClazzInfo* _clazzInfo;
  RDClazzFieldInfoArray _fields;
  RDClazzMethodInfoArray _methods;
  RDClazzSuperInfoArray _interfaces;
  bool _registered;
protected:
  /**
    use static getInstance(const acdk::lang::dmi::ClazzInfo* clazzInfo) instead
  */
  foreign DClazzInfo(const acdk::lang::dmi::ClazzInfo* clazzInfo);
  DClazzInfo(IN(RClass) cls);
public:

  
  DClazzInfo(IN(RString) classname = Nil);
  static RDClazzInfo getInstance(const acdk::lang::dmi::ClazzInfo* clazzInfo);
  static RDClazzInfo getInstance(IN(RClass) cls) { return getInstance(cls->objectClazzInfo()); }
  
  ~DClazzInfo();
  //void addSuper(IN(RString) str);
  void addSuper(IN(RDClazzInfo) cls);
  void addSuper(const acdk::lang::dmi::ClazzInfo* clazzInfo);
  RDClazzFieldInfo addField(int flags, IN(RString) name, IN(RDClazzInfo) type);
  
  RDClazzMethodInfo createMethod(IN(RString) name);
  foreign acdk::lang::dmi::ClazzInfo* getImplClazzInfo() { return _clazzInfo; }
  bool equals(IN(RDClazzInfo) other) { return getImplClazzInfo() == other->getImplClazzInfo(); }
  virtual RString getName();
  int& getFlags() { return _clazzInfo->flags; }
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);
  /**
    @param name function name, may also be Nil if first function with given 
                signature should be returned
    @param args arguments with optional name.
                In case of named args, the ArgumentExprType::_position will be 
                set with correct positional parameter.
    @param flags standard DMI flags (PUBLIC, STATIC, etc.)

    @return Nil if method cannot be found
  */
  RDClazzMethodInfo findFunction(IN(RString) name, acdk::lang::dmi::ArgumentExprTypes& args, int flags);
  virtual RDClazzInfo getType() { return this; }
  
  virtual void setName(IN(RString) name);
  virtual RString getNamespace();
  virtual void setNamespace(IN(RString) name);
  void registerClazzInfo();
  void deregisterClazzInfo();
  foreign static RString getFqClassName(const acdk::lang::dmi::ClazzInfo* ci);
  foreign static void printClazzInfo(IN(acdk::io::RPrintWriter) out, const acdk::lang::dmi::ClazzInfo* ci);
  bool isNumber()
  {
    return _clazzInfo->isByteClazz() || _clazzInfo->isShortClazz() || _clazzInfo->isIntClazz() || _clazzInfo->isLongClazz() ||
           _clazzInfo->isFloatClazz() || _clazzInfo->isDoubleClazz();
  }
  bool isIntegerNumber()
  {
    return _clazzInfo->isByteClazz() || _clazzInfo->isShortClazz() || _clazzInfo->isIntClazz() || _clazzInfo->isLongClazz();
  }
  bool isBoolean() { return _clazzInfo->isBoolClazz(); }
  bool isVoid() { return _clazzInfo == acdk::lang::dmi::ClazzInfo::getVoidClazz(); }
  bool isArray() { return _clazzInfo->isArray(); }
  bool isAny() { return _clazzInfo == acdk::lang::dmi::DmiObject::clazzInfo(); }
  bool isThrowable() { return (_clazzInfo->flags & acdk::lang::dmi::MiCiThrowable) != 0; }
  RDClazzInfo getArrayElementType() { return new DClazzInfo(_clazzInfo->getElementClazzInfo()); }
  /**
    only use for AAL defined classes
  */
  bool hasAnyConstructor();
  /**
    return the super classes/interfaces
  */
  RDClazzInfoArray getSuperClasses();
  RDClazzInfo getSuperClass();
  /**
    return the resulting expression type if both arguments are connected with
    an number calculation
  */
  static RDClazzInfo selectResultingAlgExpression(const acdk::lang::dmi::ClazzInfo* lh, const acdk::lang::dmi::ClazzInfo* rh);

  static bool isRestArg(const acdk::lang::dmi::ClazzInfo* ci) { return acdk::lang::dmi::DmiObjectArray::clazzInfo() == ci; }
  static bool isNamedRestArg(const acdk::lang::dmi::ClazzInfo* ci) {  return acdk::lang::dmi::DmiNamedArgArray::clazzInfo() == ci; }
  /**
    return the first operator() function
    @param flags STATIC or not
    @return Nil if non found
  */
  RDClazzMethodInfo getDefunMethod(int flags);
  /**
    find an assign compatible method (with same signature) as other method
    in this class with given method name
    @return Nil if non found
  */
  RDClazzMethodInfo findCompatibleMethod(IN(RString) name, IN(RDClazzMethodInfo) other);
  acdk::lang::dmi::ScriptVar getInitializeValue();
  /**
    Serialize support
  */
  virtual void writeObject(IN(acdk::io::RObjectWriter) out, IN(RClass) cls);
  /**
    Serialize support
  */
  virtual void readObject(IN(acdk::io::RObjectReader) in);
  
  /**
    returns/create the ClassInitializer method
    static ClassName() {}
  */
  RDClazzMethodInfo getClassInitializerMethod(bool create = true);
  /**
    register the given DClazzMethodInfo as class initializer
    method
  */
  void setClassInitializerMethod(IN(RDClazzMethodInfo) mi);
  /**
    after parsing class, set the altname of the ClazzMethodInfo
  */
  void setMethodAltNames();
};

inline 
RString 
DClazzFieldInfo::getOwnerClassName() 
{ 
  return DClazzInfo::getFqClassName(_clazzInfo); 
}



ACDK_DECL_CLASS(CastedObject);
class ACDK_ACI_PUBLIC CastedObject
: public acdk::lang::Object
{
  RObject _target;
  const acdk::lang::dmi::ClazzInfo* _castedClazzInfo;
  
public:
  CastedObject(IN(RObject) targetObject, const acdk::lang::dmi::ClazzInfo* castedClazzInfo)
  : _target(targetObject)
  , _castedClazzInfo(castedClazzInfo)
  {
  }
  virtual Object* getDmiTarget(OUT(bool) forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci)
  {
    forwarded = true;
    ci = _castedClazzInfo;
    return _target;
  }
};

} // aci
} // acdk


#endif //acdk_aci_DClazzInfo_h

