// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 




#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/dmi/ClazzAttributesRes.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/UnicodeCharacter.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>
#include <acdk/io/BinaryDataWriter.h>
#include <acdk/io/BinaryDataReader.h>

#include <acdk/io/PrintWriter.h>

#include "DClazzInfo.h"
#include "SymbolTable.h"
#include "Compiler.h"
#include "ast/AciCodeAttributeData.h"
#include "ast/Terminal.h"

#include "aci.h"

namespace acdk {
namespace aci {

using namespace acdk::lang::dmi;


template <class T>
class StaticArray
{
  T**& _ptr;
public:
  StaticArray(T**& p, int initsize = 10) 
    : _ptr(p) 
  {
    if (_ptr == 0)
    {
      _ptr = ::new T*[initsize];
      memset(_ptr, 0, sizeof(T*) * initsize);
      _ptr[1] = (T*)initsize;
    }
  }
  int end()
  {
    for (int i = 0; true; ++i)
      if (_ptr[i] == 0)
        return i;
    return -1;
  }
  void resize(int oldsize, int newsize)
  {
    T** np = ::new T*[newsize];
    memset(np, 0, sizeof(T*) * newsize);
    if (_ptr != 0)
    {
      memcpy(np, _ptr, sizeof(T*) * oldsize);
      delete [] _ptr;
    }
    _ptr = np;
  }
  void destroy()
  {
    if (_ptr != 0)
    {
      delete [] _ptr;
      _ptr = 0;
    }
  }

  void push_back(T* p)
  {
    int e = end();
    int size = (int)_ptr[e + 1];
    if (e + 2 > size)
      resize(size, size * 2);
    _ptr[e] = p;
    _ptr[e + 1] = 0;
    _ptr[e + 2] = (T*)(size * 2);
  }
};




char* 
ClazzTypes::strdup(IN(RString) str)
{
  RString astr = str->convert(CCUtf8);//CCAscii
  int size = astr->length() + 1;
  char* ret = ::new char[size];
  memset(ret, 0, size);
  memcpy(ret, astr->c_str(), size - 1);
  return ret;
}

void 
ClazzTypes::strdel(const char*& ptr)
{
  char* delptr = const_cast<char*>(ptr);
  delete [] delptr;
  ptr = 0;

}

/*      --------------------- DUnitInfo  --------------------------*/

DUnitInfo::DUnitInfo(IN(RString) unitname)
: ClazzTypes(true)
, _unitInfo(new acdk::lang::dmi::UnitInfo())
{
  memset(_unitInfo, 0, sizeof(acdk::lang::dmi::UnitInfo));
  if (unitname != Nil)
    _unitInfo->name = ClazzTypes::strdup(unitname);
}

DUnitInfo::~DUnitInfo()
{
  if (_delete == false)
    return;
  if (_unitInfo->name != 0)
    ClazzTypes::strdel(_unitInfo->name);
  if (_unitInfo->attributeRes != 0)
    acdk::lang::dmi::ClazzAttributesRes::deleteMetaAttributes((acdk::lang::dmi::MetaInfo*)_unitInfo);
  delete _unitInfo;
  _unitInfo = 0;

}

RSemanticElem 
DUnitInfo::findSubSem(IN(RString) str, IN(RString) op)
{
  return Nil;
}

/*      --------------------- DClazzFieldInfo  --------------------------*/

DClazzFieldInfo::DClazzFieldInfo(int flags, IN(RString) name, IN(RDClazzInfo) type, const acdk::lang::dmi::ClazzInfo* clazzInfo)
: ClazzTypes(true)
, _fieldInfo(new acdk::lang::dmi::ClazzFieldInfo())
, _clazzInfo(clazzInfo)
, _type(type)
{
  memset(_fieldInfo, 0, sizeof(acdk::lang::dmi::ClazzFieldInfo));
  _fieldInfo->flags = flags;
  if (name != Nil)
    _fieldInfo->name = ClazzTypes::strdup(name);
  _fieldInfo->type = type->getImplClazzInfo();
}

DClazzFieldInfo::DClazzFieldInfo(const acdk::lang::dmi::ClazzInfo* clazzInfo, acdk::lang::dmi::ClazzFieldInfo* fieldInfo)
: ClazzTypes(false)
, _fieldInfo(fieldInfo)
, _clazzInfo(clazzInfo)
, _type(DClazzInfo::getInstance(fieldInfo->type))
{
}

DClazzFieldInfo::~DClazzFieldInfo()
{
  if (_delete == false)
    return;
  if (_fieldInfo->name != 0)
    ClazzTypes::strdel(_fieldInfo->name);
  if (_fieldInfo->attributeRes != 0)
    acdk::lang::dmi::ClazzAttributesRes::deleteMetaAttributes((acdk::lang::dmi::MetaInfo*)_fieldInfo);
  delete _fieldInfo;
  _fieldInfo = 0;
}

RDClazzInfo 
DClazzFieldInfo::getOwnerClass()
{
  return DClazzInfo::getInstance(_clazzInfo);
}


/*      --------------------- DClazzMethodArgInfo  --------------------------*/

DClazzMethodArgInfo::DClazzMethodArgInfo(const acdk::lang::dmi::ClazzInfo* clazzInfo, const acdk::lang::dmi::ClazzMethodInfo* methodInfo,
                                          IN(RString) name, IN(RDClazzInfo) type, int flags)
: ClazzTypes(true)
, _clazzInfo(clazzInfo)
, _methodInfo(methodInfo)
, _methodArgInfo(new acdk::lang::dmi::ClazzMethodArgInfo())
, _type(type)
{
  memset(_methodArgInfo, 0, sizeof(acdk::lang::dmi::ClazzMethodArgInfo));
  _methodArgInfo->flags = flags;
  if (name != Nil)
    _methodArgInfo->name = ClazzTypes::strdup(name);
  if (type != Nil)
    _methodArgInfo->type = type->getImplClazzInfo();
}

DClazzMethodArgInfo::~DClazzMethodArgInfo()
{
  if (_delete == false)
    return;
  if (_methodArgInfo->name != 0)
    ClazzTypes::strdel(_methodArgInfo->name);
  if (_methodArgInfo->attributeRes != 0)
    acdk::lang::dmi::ClazzAttributesRes::deleteMetaAttributes((acdk::lang::dmi::MetaInfo*)_methodArgInfo);
  delete _methodArgInfo;
  _methodArgInfo = 0;
}

/*      --------------------- DClazzMethodInfo  --------------------------*/

DClazzMethodInfo::DClazzMethodInfo(int flags, IN(RString) name, const acdk::lang::dmi::ClazzInfo* clazzInfo)
: ClazzTypes(true)
, _methodInfo(new acdk::lang::dmi::ClazzMethodInfo())
, _clazzInfo(clazzInfo)
, _args(new DClazzMethodArgInfoArray(0))
{
  memset(const_cast<acdk::lang::dmi::ClazzMethodInfo*>(_methodInfo), 0, sizeof(acdk::lang::dmi::ClazzMethodInfo));
  const_cast<acdk::lang::dmi::ClazzMethodInfo*>(_methodInfo)->flags = flags;
  _methodInfo->_scopeParent = clazzInfo->getMetaInfo();
  if (name != Nil)
  {
    const_cast<acdk::lang::dmi::ClazzMethodInfo*>(_methodInfo)->name = ClazzTypes::strdup(name);
    if (clazzInfo != 0 && name->equals(clazzInfo->name) == true)
      const_cast<acdk::lang::dmi::ClazzMethodInfo*>(_methodInfo)->flags |= MiMiConstructor;
  }

}

DClazzMethodInfo::DClazzMethodInfo(const acdk::lang::dmi::ClazzInfo* clazzInfo, const acdk::lang::dmi::ClazzMethodInfo* methodInfo)
: ClazzTypes(false)
, _methodInfo(const_cast<acdk::lang::dmi::ClazzMethodInfo*>(methodInfo))
, _clazzInfo(clazzInfo)
, _retType(DClazzInfo::getInstance(methodInfo->returnType))
, _args(Nil)
{
  // ### initialize _args 
}


DClazzMethodInfo::~DClazzMethodInfo()
{
  if (_delete == false)
    return;
  if (_methodInfo->name != 0)
    ClazzTypes::strdel(const_cast<acdk::lang::dmi::ClazzMethodInfo*>(_methodInfo)->name);
  StaticArray<ClazzMethodArgInfo> args(_methodInfo->methodArgs);
  args.destroy();
  if (_methodInfo->attributeRes != 0)
    acdk::lang::dmi::ClazzAttributesRes::deleteMetaAttributes((acdk::lang::dmi::MetaInfo*)_methodInfo);
  delete const_cast<acdk::lang::dmi::ClazzMethodInfo*>(_methodInfo);
  _methodInfo = 0;
}

RDClazzInfo 
DClazzMethodInfo::getOwnerClass() 
{ 
  return DClazzInfo::getInstance(_clazzInfo); 
}

RSemanticElem 
DClazzMethodInfo::findSubSem(IN(RString) str, IN(RString) op) 
{ 
  return getReturnType()->findSubSem(str, op); 
}

RDClazzInfo 
DClazzMethodInfo::getType() 
{ 
  return getReturnType(); 
}

void 
DClazzMethodInfo::setReturnType(IN(RDClazzInfo) td)
{
  _retType = td;
  const_cast<acdk::lang::dmi::ClazzMethodInfo*>(_methodInfo)->returnType = td->getType()->getImplClazzInfo();
}


void
DClazzMethodInfo::setName(IN(RString) name)
{
  if (_delete == false)
    THROW1(Exception, "Cannot not modify this DClazzMethodInfo: " + toString());
  if (_methodInfo->name != 0)
    ClazzTypes::strdel(const_cast<acdk::lang::dmi::ClazzMethodInfo*>(_methodInfo)->name);
  _methodInfo->name = ClazzTypes::strdup(name);
}

void 
DClazzMethodInfo::addParameter(IN(RString) name, IN(RDClazzInfo) ci, int flags)
{
  RDClazzMethodArgInfo ai = new DClazzMethodArgInfo(_clazzInfo, _methodInfo, name, ci, flags);
  _args->append(ai);
  StaticArray<ClazzMethodArgInfo> args(_methodInfo->methodArgs);
  args.push_back(ai->getMethodArgInfo());
}

RExecutableArray 
DClazzMethodInfo::getCode() 
{
  if (_code == Nil)
    _code = new ExecutableArray(0);
  return _code;
}

void 
DClazzMethodInfo::makeCodeAttribute()
{
  getCode();
  acdk::lang::dmi::MetaObjectImpl mo((acdk::lang::dmi::MetaInfo*)_methodInfo);
  if (mo.getMetaAttribute("__aci_code") != Nil)
    return;
  mo.setMetaAttribute("__aci_code", RObject(new AciCodeAttributeData(Nil, _code))); // ### @todo save optional also AstNode to ClazzMethod
}


void 
DClazzMethodInfo::cloneArgs(IN(RDClazzMethodInfo) other)
{
  if (other->_args != Nil)
  {
    for (int i = 0; i < other->_args->length(); ++i)
      addParameter(other->_args[i]->getName(), other->_args[i]->getType(), other->_args[i]->getFlags());
  }
  else if (other->_methodInfo->methodArgs != 0)
  {
    for (int i = 0; other->_methodInfo->methodArgs[i] != 0; ++i)
      addParameter(SCS(other->_methodInfo->methodArgs[i]->name), 
                  DClazzInfo::getInstance(other->_methodInfo->methodArgs[i]->type), 
                  other->_methodInfo->methodArgs[i]->flags);
  }
  if (other->_retType != Nil)
    setReturnType(other->_retType);
  else 
    setReturnType(DClazzInfo::getInstance(other->_methodInfo->returnType));

}

//virtual 
RString 
DClazzMethodInfo::toString()
{
  StringBuffer sb;
  _methodInfo->toTypeString(sb, _clazzInfo, TpFtJavaType | TpFtFqName | TpFtTypeDef);
  return sb.toString();
}

//virtual 
RObject 
DClazzMethodInfo::clone(acdk::lang::sys::Allocator* alc)
{
  RDClazzMethodInfo erg = new (alc) DClazzMethodInfo( getFlags(), getName(), _clazzInfo);
  erg->cloneArgs(this);
  return &erg;
}

bool 
DClazzMethodInfo::hasCompatibleSignature(const acdk::lang::dmi::ClazzMethodInfo* other)
{
  if (_methodInfo->returnType != other->returnType)
    return false;
  if (_methodInfo->methodArgs == 0 && other->methodArgs == 0)
    return true;
  if (_methodInfo->methodArgs == 0 || other->methodArgs == 0)
    return false;
  for (int i = 0; _methodInfo->methodArgs[i] != 0; ++i)
  {
    if (other->methodArgs[i] == 0)
      return false;
    if (other->methodArgs[i]->type != _methodInfo->methodArgs[i]->type)
      return false;
  }
  return true;
}

// duplicated code in acdk/tools/mc/ClassInfo.cpp
void
getAltMethodPrefix(StringBuffer& sb, int i)
{
   if (i < 10)
    sb.append(i);
  else if (i < 26 + 10)
    sb.append(char('a' + i - 10));
  else if (i < 2 * 26 + 10)
    sb.append(char('A' + i - (10 + 26)));
  else
  {
    int next = i / (2 * 26 + 10);
    getAltMethodPrefix(sb, next);
    int rest = i % (2 * 26 + 10);
    getAltMethodPrefix(sb, rest);
  }
}

RString getAltMethodPrefix(int i)
{
  StringBuffer sb;
  getAltMethodPrefix(sb, i);
 
  sb.append("_");
  return sb.toString();
}

void 
DClazzMethodInfo::setAltName(int counter)
{
  if (_methodInfo->altlabel != 0)
    return;
  RString altname = "_" + getAltMethodPrefix(counter) + getName();
  _methodInfo->altlabel = strdup(altname);
}

/*      --------------------- DClazzFieldInfo  --------------------------*/



RString 
DClazzFieldInfo::toString()
{
  return _fieldInfo->toTypeString(TpFtJavaType | TpFtFqName | TpFtTypeDef);
}

RDClazzInfo 
DClazzFieldInfo::getType() 
{ 
  return _type; 
}

RSemanticElem 
DClazzFieldInfo::findSubSem(IN(RString) str, IN(RString) op) 
{ 
  return _type->findSubSem(str, op); 
}



/*      --------------------- DClazzSuperInfo  --------------------------*/
/*
DClazzSuperInfo::DClazzSuperInfo(const acdk::lang::dmi::ClazzInfo* ci, IN(RString) superName)
: ClazzTypes(true)
, _clazzInfo(ci)
, _superInfo(0)
{
  const ClazzInfo* sci = ClazzInfo::findClazzInfo(superName);
  _superInfo = new ClazzSuperInfo();
  memset(_superInfo, 0, sizeof(ClazzSuperInfo));
  _superInfo->type = sci;
}
*/

DClazzSuperInfo::DClazzSuperInfo(const acdk::lang::dmi::ClazzInfo* ci, IN(RDClazzInfo) superclazz)
: ClazzTypes(true)
, _clazzInfo(ci)
, _superInfo()
{
  _superInfo = new ClazzSuperInfo();
  memset(_superInfo, 0, sizeof(ClazzSuperInfo));
  _superInfo->flags |= MiPublic;
  _superInfo->type =  superclazz->getImplClazzInfo();
}

DClazzSuperInfo::~DClazzSuperInfo()
{
  if (_delete == false || _superInfo == 0)
    return;
  delete _superInfo;
}

RDClazzInfo 
DClazzSuperInfo::getType() 
{ 
  return DClazzInfo::getInstance(_superInfo->type); 
}

/*      --------------------- DClazzInfo  --------------------------*/

DClazzInfo::DClazzInfo(IN(RString) classname)
: ClazzTypes(true)
, _clazzInfo(new acdk::lang::dmi::ClazzInfo())
, _fields(new DClazzFieldInfoArray(0))
, _methods(new DClazzMethodInfoArray(0))
, _interfaces(new DClazzSuperInfoArray(0))
, _registered(false)
{
  memset(_clazzInfo, 0, sizeof(acdk::lang::dmi::ClazzInfo));
  if (classname != Nil)
    _clazzInfo->name = ClazzTypes::strdup(classname);
  StaticArray<ClazzSuperInfo> si(_clazzInfo->interfaces);
  StaticArray<ClazzFieldInfo> fi(_clazzInfo->fields);
  StaticArray<ClazzMethodInfo> mi(_clazzInfo->methods);
  
  ClazzAttributesRes::attachAttribute((MetaInfo*)_clazzInfo, "__aci_DClazzInfoPtr", ClazzAttributeResValue(UserResType, this));
  _clazzInfo->flags |= MiResolved;
}



DClazzInfo::DClazzInfo(const acdk::lang::dmi::ClazzInfo* clazzInfo)
: ClazzTypes(false)
, _clazzInfo(const_cast<acdk::lang::dmi::ClazzInfo*>(clazzInfo))
, _fields(new DClazzFieldInfoArray(0))
, _methods(new DClazzMethodInfoArray(0))
, _interfaces(new DClazzSuperInfoArray(0))
, _registered(false)
{
}

DClazzInfo::DClazzInfo(IN(RClass) cls)
: ClazzTypes(false)
, _clazzInfo(const_cast<acdk::lang::dmi::ClazzInfo*>(cls->objectClazzInfo()))
, _fields(new DClazzFieldInfoArray(0))
, _methods(new DClazzMethodInfoArray(0))
, _interfaces(new DClazzSuperInfoArray(0))
, _registered(false)
{
}

//static 
RDClazzInfo 
DClazzInfo::getInstance(const acdk::lang::dmi::ClazzInfo* clazzInfo)
{
  ClazzAttributeResValue carv = ClazzAttributesRes::getAttribute((MetaInfo*)clazzInfo, "__aci_DClazzInfoPtr");
  if (carv.data != 0)
    return (DClazzInfo*)carv.data;
  return new DClazzInfo(clazzInfo);
}

DClazzInfo::~DClazzInfo()
{
  if (_delete == false)
    return;
  deregisterClazzInfo();
  if (_clazzInfo->name != 0)
    ClazzTypes::strdel(_clazzInfo->name);
  StaticArray<ClazzSuperInfo> si(_clazzInfo->interfaces);
  si.destroy();
  StaticArray<ClazzFieldInfo> fi(_clazzInfo->fields);
  fi.destroy();
  StaticArray<ClazzMethodInfo> mi(_clazzInfo->methods);
  mi.destroy();

  if (_clazzInfo->attributeRes != 0)
    acdk::lang::dmi::ClazzAttributesRes::deleteMetaAttributes((acdk::lang::dmi::MetaInfo*)_clazzInfo);
  delete _clazzInfo;  
  _clazzInfo = 0;

}

void 
DClazzInfo::setName(IN(RString) name)
{
  if (_clazzInfo->name != 0)
    ClazzTypes::strdel(_clazzInfo->name);
  if (name != Nil)
    _clazzInfo->name = ClazzTypes::strdup(name);
}
/*
void 
DClazzInfo::addSuper(IN(RString) str)
{
  RDClazzSuperInfo csi = new DClazzSuperInfo(_clazzInfo, str);

  StaticArray<ClazzSuperInfo> args(_clazzInfo->interfaces);
  args.push_back(csi->_superInfo);
  _interfaces->append(csi);
}
*/

void 
DClazzInfo::addSuper(IN(RDClazzInfo) cls)
{
  RDClazzSuperInfo csi = new DClazzSuperInfo(_clazzInfo, cls);
  StaticArray<ClazzSuperInfo> args(_clazzInfo->interfaces);
  args.push_back(csi->_superInfo);
  _interfaces->append(csi);
}

  
RDClazzFieldInfo 
DClazzInfo::addField(int flags, IN(RString) name, IN(RDClazzInfo) type)
{
  RDClazzFieldInfo tfi = new DClazzFieldInfo(flags, name, type->getType(), _clazzInfo);
  StaticArray<ClazzFieldInfo> fi(_clazzInfo->fields);
  ClazzFieldInfo* cfi = tfi->getFieldInfo();
  fi.push_back(cfi);
  _fields->append(tfi);
  if (flags & MiStatic)
  {
    acdk::lang::dmi::RDmiObject obj = new acdk::lang::dmi::DmiObject(ScriptVar::getInitialized(cfi->type));
    acdk::lang::dmi::MetaObjectImpl moi((acdk::lang::dmi::MetaInfo*)cfi);
    moi.setMetaAttribute("__aal_field", (RObject)obj);
    cfi->address = obj->getDataPtr();
  }
  return tfi;
}

RDClazzMethodInfo 
DClazzInfo::createMethod(IN(RString) name)
{
  RDClazzMethodInfo mi = new DClazzMethodInfo(0, name, _clazzInfo);
  _methods->append(mi);
  StaticArray<ClazzMethodInfo> mis(_clazzInfo->methods);
  mis.push_back(const_cast<acdk::lang::dmi::ClazzMethodInfo*>(mi->getMethodInfo()));
  return mi;
}

RString 
DClazzInfo::getName()
{
  if (_clazzInfo != 0 && _clazzInfo->name != 0)
    return getFqClassName(_clazzInfo);
  return "";
}

//foreign 
//static 
RString 
DClazzInfo::getFqClassName(const acdk::lang::dmi::ClazzInfo* ci) // ## TODO use ClazzInfo feature
{
  if (ci == 0)
    return "";
  if (*ci->name == '[')
  {
    return "[" + getFqClassName(reinterpret_cast<const acdk::lang::dmi::ClazzInfo*>(ci->userInfo));
  } 
  StringBuffer sb;
  if (ci->ns != 0 && strlen(ci->ns) > 0)
  {
    sb.append(ci->ns);
    sb.append("/");
  }
  sb.append(ci->name);
  return sb.toString()->replace('/', '.');
}

RString 
DClazzInfo::getNamespace()
{
  if (_clazzInfo != 0 && _clazzInfo->ns != 0)
    return RCS(_clazzInfo->ns);
  return "";
}

//virtual 
void 
DClazzInfo::setNamespace(IN(RString) name)
{
  if (_clazzInfo->ns != 0)
    ClazzTypes::strdel(_clazzInfo->ns);
  if (name != Nil)
    _clazzInfo->ns = ClazzTypes::strdup(name);
}

const ClazzInfo* getWrappedClazz(const ClazzInfo* ci) // ### maybe into ClazzInfo
{
  if (ci->isBasicClazz() == false)
    return ci;
  if (ci == ClazzInfo::getBoolClazz())
    return Boolean::clazzInfo();
  if (ci == ClazzInfo::getCharClazz())
    return Character::clazzInfo();
  if (ci == ClazzInfo::getUcCharClazz())
    return UnicodeCharacter::clazzInfo();
  if (ci == ClazzInfo::getByteClazz())
    return Byte::clazzInfo();
  if (ci == ClazzInfo::getShortClazz())
    return Short::clazzInfo();
  if (ci == ClazzInfo::getIntClazz())
    return Integer::clazzInfo();
  if (ci == ClazzInfo::getLongClazz())
    return Long::clazzInfo();
  if (ci == ClazzInfo::getFloatClazz())
    return Float::clazzInfo();
  if (ci == ClazzInfo::getDoubleClazz())
    return Double::clazzInfo();
  // ### error
  return ci;
}



RSemanticElem 
DClazzInfo::findSubSem(IN(RString) str, IN(RString) op)
{
  const acdk::lang::dmi::ClazzInfo* ci = getImplClazzInfo();
  ci = getWrappedClazz(ci);
  const acdk::lang::dmi::ClazzInfo* cierg = ci;
  const acdk::lang::dmi::ClazzFieldInfo* cfi = ci->findField(cierg, str, MiIvNoThrowIfNotFound);
  if (cfi != 0)
  {
    return new DClazzFieldInfo(cierg, const_cast<acdk::lang::dmi::ClazzFieldInfo*>(cfi));
  }
  cierg = ci;
  // no polymorphic
  const acdk::lang::dmi::ClazzMethodInfo* cmi = lookupMethod(cierg, str, MiIvNoThrowIfNotFound);
  if (cmi != 0)
  {
    return new DClazzMethodInfo(cierg, cmi);
  }
  //return Nil;

  
    
  const acdk::lang::dmi::ClazzInfo* super = ci->findSuperClazz(str);
  if (super != 0)
  {
    // asume super constructor name
    const acdk::lang::dmi::ClazzMethodInfo* cmi = lookupMethod(cierg, super->name, MiIvNoThrowIfNotFound);
    if (cmi != 0)
    {
      return new DClazzMethodInfo(cierg, cmi);
    }
  }
  return Nil;
}



RDClazzMethodInfo 
DClazzInfo::findFunction(IN(RString) name, ArgumentExprTypes& args, int flags)
{
  const char* funcname = 0;
  RString utfname; 
  if (name != Nil)
  {
    utfname = name->convert(CCUtf8);
    funcname = utfname->c_str();
  }
  const ClazzInfo* ci = _clazzInfo;
  const ClazzMethodInfo* mi = StdDispatch::findMethod(ci, funcname, args, flags, false);
  if (mi == 0)
    return Nil;
  return new DClazzMethodInfo(ci, mi);
}

RDClazzInfoArray 
DClazzInfo::getSuperClasses()
{
  if (_clazzInfo == 0 || _clazzInfo->interfaces == 0)
    return new DClazzInfoArray(0);
  RDClazzInfoArray supers = new DClazzInfoArray(0);
  for (int i = 0; _clazzInfo->interfaces[i] != 0; ++i)
    supers->append(DClazzInfo::getInstance(_clazzInfo->interfaces[i]->type));
  return supers;
}

RDClazzInfo 
DClazzInfo::getSuperClass()
{
  if (_clazzInfo == 0 || _clazzInfo->interfaces == 0)
    return Nil;
  if (_clazzInfo->interfaces[0] == 0)
    return Nil;
  return DClazzInfo::getInstance(_clazzInfo->interfaces[0]->type);
}

acdk::lang::dmi::ScriptVar 
DClazzInfo::getInitializeValue() 
{
  return _clazzInfo->createInitializedValue();
}

RDClazzMethodInfo 
DClazzInfo::getDefunMethod(int flags)
{
  if (_clazzInfo->methods == 0)
    return Nil;
  for (int i = 0; _clazzInfo->methods[i] != 0; ++i)
  {
    if (strcmp(_clazzInfo->methods[i]->name, "operator_po_pc") == 0)
      return new DClazzMethodInfo(_clazzInfo, _clazzInfo->methods[i]);
  }
  return Nil;
}

RDClazzMethodInfo 
DClazzInfo::findCompatibleMethod(IN(RString) name, IN(RDClazzMethodInfo) other)
{
  if (_clazzInfo->methods == 0)
    return Nil;
  for (int i = 0; _clazzInfo->methods[i] != 0; ++i)
  {
    if (name->equals(_clazzInfo->methods[i]->name) == false)
      continue;
    if (other->hasCompatibleSignature(_clazzInfo->methods[i]) == true)
      return new DClazzMethodInfo(_clazzInfo, _clazzInfo->methods[i]);
  }
  return Nil;
}
/*
void 
DClazzInfo::addStaticInitCode(IN(RExecutableArray) code)
{
  MetaObjectImpl mo((MetaInfo*)_clazzInfo);
  RMetaAttribute ma = mo.getMetaAttribute("__aci_static_initializer");
  RExecutableArray initcode;
  if (ma != Nil)
    initcode = (RExecutableArray)ma->value;
  else
    initcode = new ExecutableArray(0);
  initcode->concat(code);
  if (ma == Nil)  
    mo.setMetaAttribute("__aci_static_initializer", (RObject)initcode);
}

void 
DClazzInfo::callStaticInitializer(IN(RCompiler) comp)
{
  MetaObjectImpl mo((MetaInfo*)_clazzInfo);
  RMetaAttribute ma = mo.getMetaAttribute("__aci_static_initializer");
  if (ma == Nil)
    return;
  REvalEnv env = comp->getCompilerEnv();
  RExecutableArray initcode = (RExecutableArray)ma->value;
  env->execute(initcode);
}
*/

RDClazzMethodInfo 
DClazzInfo::getClassInitializerMethod(bool create)
{
  if (_delete == false)
    THROW1(Exception, "cannot create method on non-dynamic DClazzInfo: " + getName());
  if (_methods == Nil)
    _methods = new DClazzMethodInfoArray(0);
  for (int i = 0; i < _methods->length(); ++i)
  {
    RDClazzMethodInfo mi = _methods[i];
    if (mi->getFlags() & MiStatic &&
        mi->getName()->equals(getName()) == true)
        return mi;
  }
  if (create == false)
    return Nil;
  RDClazzMethodInfo mi = createMethod(getName());
  mi->getFlags() |= MiStatic;
  mi->setReturnType(DClazzInfo::getInstance(ClazzInfo::getVoidClazz()));
  setClassInitializerMethod(mi);
  return mi;
}

void
DClazzInfo::setClassInitializerMethod(IN(RDClazzMethodInfo) mi)
{
  MetaObjectImpl mo((MetaInfo*)_clazzInfo);
  RMetaAttribute ma = mo.getMetaAttribute("__acdk_classinitfunction");
  if (ma != Nil)
    return ;
  mo.setMetaAttribute("__acdk_classinitfunction", (RObject)mi->getName());
  mi->makeCodeAttribute();
  return ;
}

void 
DClazzInfo::setMethodAltNames()
{
  if (_delete == false)
    THROW1(Exception, "cannot modify methods non-dynamic DClazzInfo: " + getName());
  if (_methods == Nil)
    return;
  for (int i = 0; i < _methods->length(); ++i)
  {
    _methods[i]->setAltName(i);
  }
}

void 
DClazzInfo::registerClazzInfo()
{
  if (_registered == false && _delete == true)
  {
    _registered = true;
    acdk::lang::dmi::ClazzInfo::registerClazzInfo(_clazzInfo);
  }
}

void 
DClazzInfo::deregisterClazzInfo()
{
  if (_registered == false)
    return;
  acdk::lang::dmi::ClazzInfo::unregisterClazzInfo(_clazzInfo);
}

//static 
RDClazzInfo 
DClazzInfo::selectResultingAlgExpression(const acdk::lang::dmi::ClazzInfo* lh, const acdk::lang::dmi::ClazzInfo* rh)
{
  if (lh->isDoubleClazz() == true && rh->isDoubleClazz() == true)
    return DClazzInfo::getInstance(ClazzInfo::getDoubleClazz());
  if (lh->isFloatClazz() == true && rh->isFloatClazz() == true)
    return DClazzInfo::getInstance(ClazzInfo::getFloatClazz());
  if (lh->isLongClazz() == true && rh->isLongClazz() == true)
    return DClazzInfo::getInstance(ClazzInfo::getLongClazz());
  if (lh->isIntClazz() == true && rh->isIntClazz() == true)
    return DClazzInfo::getInstance(ClazzInfo::getIntClazz());
  if (lh->isShortClazz() == true && rh->isShortClazz() == true)
    return DClazzInfo::getInstance(ClazzInfo::getShortClazz());
  return DClazzInfo::getInstance(ClazzInfo::getByteClazz());
}

bool 
DClazzInfo::hasAnyConstructor()
{
  if (_clazzInfo->methods == 0)
    return false;
  for (int i = 0; _clazzInfo->methods[i] != 0; ++i)
  {
    if (_clazzInfo->methods[i]->flags & MiMiConstructor)
      return true;
  }
  return false;
}

StringBuffer& 
operator<<(StringBuffer& sb, const acdk::lang::dmi::ClazzMethodArgInfo* ai)
{
  MetaInfo::flagsToTypeDecl(sb, ai->flags, MethodInfoExtFlags(0));
  sb << " ";
  ai->type->toTypeString(sb, TpFtTypeDef | TpFtFqName | TpFtJavaType);
  sb << " " << (char*)ai->name;
  return sb;
}

StringBuffer& 
operator<<(StringBuffer& sb, const acdk::lang::dmi::ClazzMethodInfo* mi)
{
  mi->toTypeString(sb, 0, TpFtTypeDef | TpFtFqName | TpFtJavaType);
  return sb;
}

StringBuffer& 
operator<<(StringBuffer& sb, const acdk::lang::dmi::ClazzFieldInfo* fi)
{
  fi->toTypeString(sb, TpFtTypeDef | TpFtFqName | TpFtJavaType);
  return sb;
}

StringBuffer& 
operator<<(StringBuffer& sb, const acdk::lang::dmi::ClazzInfo* ci)
{
  if (ci == 0)
  {
    sb << "ClazzInfo is 0";
    return sb;
  }
  ci->toTypeString(sb, TpFtTypeDef | TpFtFqName | TpFtJavaType);
  /*
  sb << Modifier::toString(ci->flags) << " ";
  sb << "class " << (char*)(ci->name != 0 ? ci->name : "<anon>") << "\n{\n";
  int i;
  for (i = 0; ci->fields && ci->fields[i] != 0; ++i)
  {
    sb << (const acdk::lang::dmi::ClazzFieldInfo* )ci->fields[i];
  }
  for (i = 0; ci->methods && ci->methods[i] != 0; ++i)
  {
    sb << (const acdk::lang::dmi::ClazzMethodInfo* )ci->methods[i];
  }
  sb << "}";*/
  return sb;
}

//static 
void 
DClazzInfo::printClazzInfo(IN(acdk::io::RPrintWriter) out, const acdk::lang::dmi::ClazzInfo* ci)
{
  StringBuffer sb;
  sb << ci;
  out->println(sb.toString());
}

} // aci
} // acdk

