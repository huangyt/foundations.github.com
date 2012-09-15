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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Class.cpp,v 1.75 2005/04/15 14:51:18 kommer Exp $


#include <acdk.h>

#include "Integer.h"
#include "Boolean.h"
#include "Character.h"
#include "Byte.h"
#include "Long.h"
#include "Short.h"
#include "Float.h"
#include "Double.h"
#include "Package.h"

#include "System.h"

#include "ObjectArrayImpl.h"
#include "ClassLoader.h"

#ifndef ACDK_NOMETAINFO
//extern dmi::ClazzInfo ObjectArray_clazzInfo;

extern dmi::ClazzInfo BasicArray_clazzInfo;
#endif// ACDK_NOMETAINFO

#include "ClassNotFoundException.h"
#include "Exception.h"
#include "IllegalArgumentException.h"
#include "InstantiationException.h"
#include "UnsupportedOperationException.h"
#include "ParamsMismatchException.h"
#include "reflect/Method.h"
#include "reflect/Unit.h"
#include <acdk/io/ObjectWriter.h>
#include <acdk/io/ObjectReader.h>
#include <acdk/lang/dmi/AcdkStdWeakTypeDmiClient.h>

namespace acdk {
namespace lang {
namespace dmi {

#ifndef ACDK_NOMETAINFO
extern ClazzInfo* _arrayClazzRoot;
#endif //ACDK_NOMETAINFO

} // dmi
}
}

namespace acdk {
namespace lang {


using dmi::ClazzInfo;
using dmi::ClazzMethodArgInfo;
using dmi::UnitInfo;



Class::Class(const ClazzInfo* clazzInfo)
: Object()
, _objectClazzInfo(clazzInfo)
{
}

#ifdef ACDK_MT
sys::static_mutex _sysSingeltonMutex;
#endif // ACDK_MT

//static 
RClass 
Class::getSingeltonClass(const ClazzInfo* clazzInfo) 
{
  if (clazzInfo == 0)
    THROW0(IllegalArgumentException);
  acdk::lang::RClass tclass;
  if (clazzInfo->thisClass == 0) { 
#ifdef ACDK_MT
    LockedProxy<sys::static_mutex> _guard(&_sysSingeltonMutex);
#endif //ACDK_MT
    if (clazzInfo->thisClass == 0) { 
      const_cast<ClazzInfo*>(clazzInfo)->thisClass = new Class(clazzInfo); 
#ifndef ACDK_USE_EXT_REFERER // ## no mechanism to hold reference needed?
      const_cast<ClazzInfo*>(clazzInfo)->thisClass->addRef();
#endif
      tclass = clazzInfo->thisClass; 
      if (clazzInfo->isArray() && clazzInfo->userInfo == 0)
        clazzInfo->userInfo = Object::clazzInfo();
      acdk::lang::System::registerStaticReference(clazzInfo->thisClass); 
    } else
      tclass = clazzInfo->thisClass; 
  } else 
    tclass = clazzInfo->thisClass; 
  return tclass;
}


//static
 ClazzInfo* 
Class::getUnInitializedSingeltonArrayClazz(const ClazzInfo* elementinfo)
{
  ClazzInfo* ci = dmi::_arrayClazzRoot;
  for (; ci != 0; ci = ci->_next) 
  {
    if (ci->userInfo == (void*)elementinfo)
      return const_cast<ClazzInfo*>(ci);
  }
  if (ci == 0) 
  {
    if (elementinfo->isBasicClazz() == true)
      ci = ::new ClazzInfo(BasicArray_clazzInfo);
    else
      ci = ::new ClazzInfo(*ObjectArrayBase::clazzInfo());
    const_cast<ClazzInfo*>(ci)->userInfo = const_cast<ClazzInfo*>(elementinfo);
    ci->thisClass = 0;
    //ci->thisClass = ::new Class(ci); 
    //ci->thisClass->addRef();
    ClazzInfo::registerArrayClazzInfo(ci);
  }
  return ci;
}

//static
 ClazzInfo* 
Class::getSingeltonArrayClazz(const ClazzInfo* elementinfo)
{
#ifndef ACDK_NOMETAINFO
#ifdef ACDK_MT
  LockedProxy<sys::static_mutex> _guard(&_sysSingeltonMutex);
#endif
  if (elementinfo == 0)
    elementinfo = Object::clazzInfo();
  ClazzInfo* ci = const_cast<ClazzInfo*>(ClazzInfo::findArrayClazzInfo(elementinfo));

  if (ci != 0) 
    return ci;

  if (elementinfo->isBasicClazz() == true)
    ci = ::new ClazzInfo(BasicArray_clazzInfo);
  else
    ci = ::new ClazzInfo(*ObjectArrayBase::clazzInfo());
  const_cast<ClazzInfo*>(ci)->userInfo = const_cast<ClazzInfo*>(elementinfo);
  const_cast<ClazzInfo*>(ci)->thisClass = 0;
  ClazzInfo::registerArrayClazzInfo(ci);
  return ci;
#else 
  return 0;
#endif //ACDK_NOMETAINFO
}

//static 
RClass
Class::getSingeltonArrayClass(const ClazzInfo* elementinfo)
{
  const ClazzInfo* ci = getSingeltonArrayClazz(elementinfo);
  if (ci->thisClass == 0) 
  {
    const_cast<ClazzInfo*>(ci)->thisClass = new Class(ci); 
    ci->thisClass->addRef();
    //ClazzInfo::registerArrayClazzInfo(ci);
  }
  RClass tclass(ci->thisClass);
  return tclass;
}


RClass
Class::getClassByJType(IN(RString) sign) 
{
  switch(sign->charAt(0)) {
  case 'B':
    return Byte::getTYPE();
  case 'C':
    return Character::getTYPE();
  case 'Z':
    return Boolean::getTYPE();
  case 'S':
    return Short::getTYPE();
  case 'I':
    return Integer::getTYPE();
  case 'F':
    return Float::getTYPE();
  case 'D':
    return Double::getTYPE();
  case 'J':
    return Long::getTYPE();
  case '[' : {
    RClass cl = getClassByJType(sign->substr(1));
    return getSingeltonArrayClass(cl->objectClazzInfo());
  } 
  case 'L' : {
    if (sign->charAt(sign->length() - 1) != ';')
      THROW1(Exception, "Malformed JType: " + sign);
    RString cn = sign->substr(1, sign->length() - 1);
    int idx = cn->lastIndexOf('/');
    RString ns;
    if (idx != -1) {
      ns = cn->substr(0, idx);
      cn = cn->substr(idx + 1);
    }
    const ClazzInfo* ci = ClazzInfo::findClazzInfo(cn, ns == Nil ? String::emptyString() : ns);
    if (ci == 0)
      THROW1(Exception, "Type not found in ClazzInfo-DB: " + sign);
    return Class::getSingeltonClass(ci);
  }
  default:
    THROW1(Exception, "Malformed JType: " + sign);
    return Nil;
  }
}




Class::~Class()
{
  const_cast<ClazzInfo*>(_objectClazzInfo)->thisClass = 0;
}


void 
Class::finalize()
{
  if (_objectClazzInfo->thisClass) 
  {
    releaseRef();
    const_cast<ClazzInfo*>(_objectClazzInfo)->thisClass = 0;
  }
}

//virtual 
RString 
Class::getName()
{
  if (_objectClazzInfo->name[0] == '[') {
    return _objectClazzInfo->name + getSingeltonClass((const ClazzInfo*)_objectClazzInfo->userInfo)->getName();
  }
  if (_objectClazzInfo->ns != 0 && _objectClazzInfo->ns[0] != 0)
    return (RString) _objectClazzInfo->ns  + "/" + _objectClazzInfo->name;
  else
    return _objectClazzInfo->name;
}

RString 
Class::getClassName()
{
  return _objectClazzInfo->name;
}


RClassArray
Class::getClasses()
{
  return new ClassArray(0);
}

::acdk::lang::reflect::RUnit 
Class::getUnit()
{
  _objectClazzInfo->loadFullClazzInfo(false, false);
  if (_objectClazzInfo->_scopeParent == 0)
    return Nil;
  if (_objectClazzInfo->_scopeParent->isUnitInfo() == true)
    return new ::acdk::lang::reflect::Unit(reinterpret_cast<const UnitInfo*>(_objectClazzInfo->_scopeParent));
  return Nil;
}


RClass 
Class::getComponentType()
{
  if (isArray() == false)
    return Nil;
  return Class::getSingeltonClass((const ClazzInfo*)_objectClazzInfo->userInfo);
}


RClassArray
Class::getDeclaredClasses()
{
  return new ClassArray(0); 
}


RClass 
Class::getDeclaringClass()
{
  return Nil;
}


//virtual 
RClassArray
Class::getInterfaces()
{
  if (_objectClazzInfo->interfaces == 0)
    return new ClassArray(0);
  
  int count = _objectClazzInfo->getInterfacesCount();
  RClassArray ifaces = new ClassArray(count);

  for (int i = 0; i < count; i++) 
  {
    ifaces[i] = getSingeltonClass(_objectClazzInfo->interfaces[i]->type/*, _object*/);
  }
  return ifaces;
}


//virtual 
int 
Class::getModifiers()
{
  return _objectClazzInfo->flags;
}




RClass 
Class::getSuperclass()
{
  if (_objectClazzInfo->interfaces == 0 || _objectClazzInfo->interfaces[0] == 0 || _objectClazzInfo->interfaces[0]->type == 0)
    return Nil;
  return getSingeltonClass(_objectClazzInfo->interfaces[0]->type);
}

void
Class_getSuperClasses(IN(RClass) cclass, IN(RClassArray) supers)
{
  RClass superCls = cclass->getSuperclass();
  if (superCls != Nil)
    Class_getSuperClasses(superCls, supers);
  supers->append(cclass);
}

RClassArray 
Class::getSuperClasses()
{
  RClassArray classes = new ClassArray(0);
  Class_getSuperClasses(this, classes);
  return classes;
}

jlong 
Class::getSerialVersionUID()
{
  return _objectClazzInfo->getSerialVersionUID();
}


bool 
Class::isArray()
{
  return (_objectClazzInfo->flags & dmi::MiCiArray) == dmi::MiCiArray;
}

RClass 
Class::getArrayElementClass()
{
  if (isArray() == false)
    return Nil;
  if (_objectClazzInfo->userInfo == 0)
    return Nil;
  return getSingeltonClass((ClazzInfo*)_objectClazzInfo->userInfo);
}

//virtual 
bool 
Class::isAssignableFrom(IN(RClass) cls)
{
  return _objectClazzInfo->assignableFrom(cls->_objectClazzInfo);
}

//virtual 
bool 
Class::isInstance(IN(RObject) obj)
{
  return obj->getClazzInfo() == _objectClazzInfo;
}

//virtual 
bool 
Class::isInterface()
{
  return (_objectClazzInfo->flags & dmi::MiCiInterface) == dmi::MiCiInterface;
}

//virtual 
bool 
Class::isPrimitive()
{
  return (_objectClazzInfo->flags & dmi::MiCiBasicType) == dmi::MiCiBasicType;
}


//virtual 
RObject 
Class::newInstance() THROWS1(RInstantiationException)
{
  if (isArray() == false && _objectClazzInfo->creator != 0) 
    return _objectClazzInfo->creator();

  _objectClazzInfo->loadFullClazzInfo(false);

  if (isArray() == true) 
  {
    const ClazzInfo* memberclazz = (const ClazzInfo*)_objectClazzInfo->userInfo;
    if (memberclazz == Boolean::getTYPE()->objectClazzInfo()) 
      return new boolArray(0);
    if (memberclazz == Byte::getTYPE()->objectClazzInfo()) 
      return new byteArray(0);
    if (memberclazz == Character::getTYPE()->objectClazzInfo()) 
      return new charArray(0);
    if (memberclazz == Short::getTYPE()->objectClazzInfo()) 
      return new shortArray(0);
    if (memberclazz == Integer::getTYPE()->objectClazzInfo()) 
      return new intArray(0);
    if (memberclazz == Long::getTYPE()->objectClazzInfo()) 
      return new longArray(0);
    if (memberclazz == Float::getTYPE()->objectClazzInfo()) 
      return new floatArray(0);
    if (memberclazz == Double::getTYPE()->objectClazzInfo()) 
      return new doubleArray(0);
    
    if (((const ClazzInfo*)_objectClazzInfo->userInfo)->array_creator)
      return ((const ClazzInfo*)_objectClazzInfo->userInfo)->array_creator(0);
    return new ObjectArray((const ClazzInfo*)_objectClazzInfo->userInfo, 0);

  }
  
  // we try to use a standard constructor
  
  RClassArray ca = new ClassArray(0);
  reflect::RConstructor c = getDeclaredConstructor(ca);
  if (c != Nil) {
    RObjectArray args = new ObjectArray(0);
    RObject erg = c->newInstance(args);
    if (erg != Nil)
      return erg;
  }
  THROW1(InstantiationException, getName());
  return Nil;
}


static
RString 
getNameOfSig(IN(RString) str, bool& isBasic)
{
  isBasic = true;
  return str;
  /*
  if (str->length() > 1) {
    isBasic = false;
    return str;
  }
  isBasic = true;
  if (str->compareTo((RString)"Z") == 0)
    return "bool";
  if (str->compareTo((RString)"C") == 0)
    return "char";
  if (str->compareTo((RString)"B") == 0)
    return "byte";
  if (str->compareTo((RString)"S") == 0)
    return "short";
  if (str->compareTo((RString)"I") == 0)
    return "int";
  if (str->compareTo((RString)"J") == 0)
    return "jlong";
  if (str->compareTo((RString)"F") == 0)
    return "float";
  if (str->compareTo((RString)"D") == 0)
    return "double";
  if (str->compareTo("V") == 0 || )
    return "void";
  isBasic = false;
  return str;
  */
}

//virtual 
RString 
Class::toString()
{
  return _objectClazzInfo->toTypeString(dmi::TpFtAcdkType | dmi::TpFtFqName);
}
           
//static 
RClass 
Class::forName(IN(RString) className) THROWS1(RClassNotFoundException)
{
  return forName(className, true);
}

RClassLoader 
Class::getClassLoader()
{
  return ClassLoader::getSystemClassLoader();
}

//static 
RClass 
Class::forName(IN(RString) name, bool initialize) THROWS1(RClassNotFoundException)
{
  return ClassLoader::getSystemClassLoader()->findClass(name);
}

//static 
RClass 
Class::findClass(IN(RString) name)
{
  return ClassLoader::getSystemClassLoader()->findClass(name, true);
}


//static 
RClass 
Class::forName(IN(RString) name, bool initialize, IN(RClassLoader) loader)
{
  return loader->findClass(name);
}

//static 
RClass 
Class::_forName(IN(RString) name)
{
  const ClazzInfo* clazz = ClazzInfo::findClazzInfo(name, false);
  /*
  RString namesp;
  RString pcls = name->replace(".", "/");
  pcls = pcls->replace("::", "/");
  int pos;
  if ((pos = pcls->indexOf('/')) == 0) {
    pcls = pcls->substr(1);
  }
  
  const ClazzInfo* clazz = 0;
  if (pcls->c_str()[0] == '[') {
    clazz = ClazzInfo__findArrayClazzInfo(pcls->c_str() + 1);
  } else {
    if ((pos = pcls->lastIndexOf('/')) != -1) 
    {
      namesp = pcls->substr(0, pos);
      pcls = pcls->substr(pos + 1);
    }
    clazz = ClazzInfo::findClazzInfo(pcls->c_str(), namesp == Nil ? "" : namesp->c_str());
  }*/
  if (clazz == 0)
    return Nil;
  return Class::getSingeltonClass(clazz);
}


acdk::lang::reflect::RConstructor 
Class::getDeclaredConstructor(IN(RClassArray) parameterTypes) THROWS1(RNoSuchMethodException)
{
  ::acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient;
  ::acdk::lang::sys::core_vector<dmi::ClazzMethodArgInfo> vec(parameterTypes->length(), parameterTypes->length(), dmi::ClazzMethodArgInfo());
  for (int i = 0; i < vec.size(); ++i)
  {
    vec[i].type = parameterTypes[i]->objectClazzInfo();
    vec[i].name = "";
  }
  const ::acdk::lang::dmi::ClazzMethodInfo* cmi;
  try {
    const ClazzInfo* ci = _objectClazzInfo;
    cmi = 
    _lookupMethod(ci, ACDK_STACK_STR(ci->name), vec, Nil, 
                  dmiclient, 
                  ::acdk::lang::dmi::MiPublic | 
                  ::acdk::lang::dmi::MiMiConstructor);

  } catch (RParamsMismatchException ex) {
    THROW1(NoSuchMethodException, ex->getMessage());
  }
  if (cmi == 0)
    THROW1(NoSuchMethodException, toString() + "::" + _objectClazzInfo->name + "(" + parameterTypes->toString() + ")");

  return new ::acdk::lang::reflect::Constructor(_objectClazzInfo, cmi);
}

acdk::lang::reflect::RConstructorArray
Class::getDeclaredConstructors()
{
  int count = 0;
  int i;
  for (i = 0; _objectClazzInfo->methods[i]; ++i)
  {
    if (_objectClazzInfo->methods[i]->flags & ::acdk::lang::dmi::MiMiConstructor)
      ++count;
  }
  acdk::lang::reflect::RConstructorArray constructors = new acdk::lang::reflect::ConstructorArray(count);
  int ci = 0;
  for (i = 0; _objectClazzInfo->methods[i] != 0; ++i)
  {
    if (_objectClazzInfo->methods[i]->flags & ::acdk::lang::dmi::MiMiConstructor)
      constructors[ci++] = new ::acdk::lang::reflect::Constructor(_objectClazzInfo, _objectClazzInfo->methods[i]);

  }
  return constructors;
}

acdk::lang::reflect::RField 
Class::getDeclaredField(IN(RString) name) THROWS1(RNoSuchFieldException)
{
  reflect::RFieldArray tf = getDeclaredFields();
  int length = tf->length();
  if (length == 0)
    return Nil;
  for (int i = length - 1; i >= 0 ; i--) 
  {
    if (name->compareTo(tf[i]->getName()) == 0)
      return tf[i];
  }
  THROW1(NoSuchFieldException, "Field " + getName() + "::" + name  + " cannot be found");
  return Nil;
}

acdk::lang::reflect::RFieldArray
Class::getDeclaredFields()
{
  _objectClazzInfo->loadFullClazzInfo(false, false);
  int c = _objectClazzInfo->getFieldsCount();
  if (c == 0)
    return new acdk::lang::reflect::FieldArray(0);
  reflect::RFieldArray erg = new reflect::FieldArray(c);
  for (int i = 0; _objectClazzInfo->fields[i] != 0; i++)
    erg[i] = new reflect::Field(const_cast<ClazzInfo*>(_objectClazzInfo), _objectClazzInfo->fields[i]);
  return erg;
}



acdk::lang::reflect::RField 
Class::getField(IN(RString) name) THROWS1(RNoSuchFieldException)
{
  _objectClazzInfo->loadFullClazzInfo(true, false);
  reflect::RFieldArray tf = getFields();
  for (int i = 0; i < tf->length(); i++) 
  {
    if (name->compareTo(tf[i]->getName()) == 0)
      return tf[i];
  }
  THROW1(NoSuchFieldException, "Field " + getName() + "::" + name  + " cannot be found");
  return Nil;
}


acdk::lang::reflect::RFieldArray
Class::getFields()
{
  const ClazzInfo* clzinfo = _objectClazzInfo;
  
  int fieldcount = 0;
  while (clzinfo != 0) 
  {
    clzinfo->loadFullClazzInfo(false, false);
    if (clzinfo->fields != 0) {
      for (int i = 0; clzinfo->fields[i] != 0; i++)
        fieldcount++;
    }
    if (clzinfo->interfaces[0])
      clzinfo = clzinfo->interfaces[0]->type;
    else
      break;
  }
  if (fieldcount == 0)
    return new reflect::FieldArray(0);
  reflect::RFieldArray erg = new reflect::FieldArray(fieldcount);
  int currentfield = 0;
  clzinfo = _objectClazzInfo;
  while (clzinfo != 0) 
  {
    clzinfo->loadFullClazzInfo(false, false);
    if (clzinfo->fields != 0) {
      for (int i = 0; clzinfo->fields[i] != 0; i++)
        erg[currentfield++] = new reflect::Field(clzinfo, clzinfo->fields[i]);
    }
    if (clzinfo->interfaces[0])
      clzinfo = clzinfo->interfaces[0]->type;
    else
      break;
  }

  return erg;
}



acdk::lang::reflect::RMethod 
Class::getDeclaredMethod(IN(RString) name, IN(RClassArray) parameterTypes) THROWS1(RNoSuchMethodException)
{
  _objectClazzInfo->loadFullClazzInfo(false, false);

  ::acdk::lang::sys::core_vector<dmi::ClazzMethodArgInfo> vec(parameterTypes->length(), parameterTypes->length(), dmi::ClazzMethodArgInfo());
  for (int i = 0; i < vec.size(); ++i)
  {
    vec[i].type = parameterTypes[i]->objectClazzInfo();
    vec[i].name = "";
  }
  acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient;
  const dmi::ClazzMethodInfo* cmi = 0;
  try {
    const ClazzInfo* ci = _objectClazzInfo;
    cmi = acdk::lang::dmi::StdDispatch::_lookupMethod(ci, 
                                                          name,
                                                          vec,
                                                          Nil,
                                                          dmiclient,
                                                          acdk::lang::dmi::MiPublic | acdk::lang::dmi::MiIvDeclared);
  } catch (RParamsMismatchException ex) {
    THROW1(NoSuchMethodException, ex->getMessage());
  }
  if (cmi == 0)
    THROW1(NoSuchMethodException, toString() + "::" + name + "(" + parameterTypes->toString() + ")");
  return new acdk::lang::reflect::Method(_objectClazzInfo, cmi);
}


acdk::lang::reflect::RMethodArray
Class::getDeclaredMethods()
{
  if (_objectClazzInfo == 0)
    return Nil;

  _objectClazzInfo->loadFullClazzInfo(false, false);
  int count = _objectClazzInfo->getMethodsCount();
   reflect::RMethodArray ma = new reflect::MethodArray(count);
  
  for (int i = 0; i < count; i++) 
  {
    ma[i] = new reflect::Method(_objectClazzInfo, _objectClazzInfo->methods[i]);
  }
  return ma;
}

acdk::lang::reflect::RMethod 
Class::getMethod(IN(RString) name, IN(RClassArray) parameterTypes, IN(RStringArray) namedargs) THROWS1(RNoSuchMethodException)
{
  _objectClazzInfo->loadFullClazzInfo(true, false);
  ::acdk::lang::sys::core_vector<dmi::ClazzMethodArgInfo> vec(parameterTypes->length(), parameterTypes->length(), dmi::ClazzMethodArgInfo());
  for (int i = 0; i < vec.size(); ++i)
  {
    vec[i].type = parameterTypes[i]->objectClazzInfo();
    vec[i].name = "";
  }
  acdk::lang::dmi::AcdkStdWeakTypeDmiClient dmiclient;
  const dmi::ClazzMethodInfo* cmi = 0;
  try {
    const ClazzInfo* ci = _objectClazzInfo;
    cmi = acdk::lang::dmi::StdDispatch::_lookupMethod(ci, 
                                                          name,
                                                          vec,
                                                          namedargs,
                                                          dmiclient,
                                                          acdk::lang::dmi::MiPublic);
  } catch (RParamsMismatchException ex) {
    THROW1(NoSuchMethodException, ex->getMessage());
  }
  if (cmi == 0)
    THROW1(NoSuchMethodException, toString() + "::" + name + "(" + parameterTypes->toString() + ")");
  
  return new acdk::lang::reflect::Method(_objectClazzInfo, cmi);
}


reflect::RMethodArray
Class_getMethods(const ClazzInfo* clazz) 
{
  if (clazz == 0 || clazz->methods == 0)
    return Nil;
  clazz->loadFullClazzInfo(false, false);
  reflect::RMethodArray ma = new reflect::MethodArray(0);
  int i = 0;
  for (; clazz->methods[i] != 0; ++i) 
  {
    if (clazz->methods[i]->flags & dmi::MiPublic)
      ma->append(new reflect::Method(clazz, clazz->methods[i]));
  }
  for (i = 0; clazz->interfaces[i] != 0; ++i) 
  {
    reflect::RMethodArray sar = Class_getMethods(clazz->interfaces[i]->type);
    ma->concat(sar);
  }
  return ma;
}

acdk::lang::reflect::RMethodArray
Class::getMethods()
{
  return Class_getMethods(_objectClazzInfo);
}

RPackage 
Class::getPackage()
{
  _objectClazzInfo->loadFullClazzInfo(false, false);
  if (_objectClazzInfo->ns == 0)
    return Nil;
  return Package::getPackage(_objectClazzInfo->ns);
}



RObject 
Class::_getInstance()
{
  _objectClazzInfo->loadFullClazzInfo(false, false);
  if (_objectClazzInfo->creator == 0)
    return Nil;
  return _objectClazzInfo->creator();
}


//static
RObject
Class::create_arrayInstance(IN(RClass) component, int length) 
{
  if (component->isPrimitive() == true) {
    if (component->objectClazzInfo() == Boolean::getTYPE()->objectClazzInfo())
      return new ::BasicArray<bool>(length);
    if (component->objectClazzInfo() == Byte::getTYPE()->objectClazzInfo())
      return new BasicArray<byte>(length);
    if (component->objectClazzInfo() == Character::getTYPE()->objectClazzInfo())
      return new BasicArray<char>(length);
    if (component->objectClazzInfo() == Short::getTYPE()->objectClazzInfo())
      return new BasicArray<short>(length);
    if (component->objectClazzInfo() == Integer::getTYPE()->objectClazzInfo())
      return new BasicArray<int>(length);
    if (component->objectClazzInfo() == Long::getTYPE()->objectClazzInfo())
      return new BasicArray<jlong>(length);
    if (component->objectClazzInfo() == Float::getTYPE()->objectClazzInfo())
      return new BasicArray<float>(length);
    if (component->objectClazzInfo() == Double::getTYPE()->objectClazzInfo())
      return new BasicArray<double>(length);
    THROW1(ClassNotFoundException, "Basic Type not found: " + component->toString());
  } 
  if (component->objectClazzInfo()->array_creator != 0) // ### TODO array_creator is always 0
    return component->objectClazzInfo()->array_creator(length);
  return new ObjectArrayBase(component->objectClazzInfo(), length);
}

bool 
Class::isSerializable()
{
#if defined(ACDK_NOMETAINFO)
  return false;
#else
  if (_objectClazzInfo->flags & ::acdk::lang::dmi::MiCiSerializable)
    return true;
  return acdk::io::Serializable::clazzInfo()->assignableFrom(_objectClazzInfo);
#endif
}

bool 
Class::hasWriteObject()
{
#if defined(ACDK_NOMETAINFO)
  return false;
#else
  dmi::FunctionSignature signature;
  ClazzInfo* args[2];
  signature.size = 2;
  args[0] = ::acdk::io::ObjectWriter::clazzInfo();
  args[1] = Class::clazzInfo();
  signature.functionname = "writeObject";
  signature.args = (const ClazzInfo**)args;
  const ClazzInfo* ci = _objectClazzInfo;
  return findMethod(ci, signature) != 0;
#endif
}
  
bool 
Class::hasReadObject()
{
#if defined(ACDK_NOMETAINFO)
  return false;
#else
  dmi::FunctionSignature signature;
  ClazzInfo* args[2];
  signature.size = 2;
  args[0] = ::acdk::io::ObjectReader::clazzInfo();
  args[1] = Class::clazzInfo();
  signature.functionname = "readObject"; // ### TODO use hash-value of method inf for performance
  signature.args = (const ClazzInfo**)args;
  const ClazzInfo* ci = _objectClazzInfo;
  return findMethod(ci, signature) != 0;
#endif
}

bool 
Class::hasWriteReplace()
{
  dmi::FunctionSignature signature;
  signature.size = 0;
  signature.functionname = "writeReplace";
  const ClazzInfo* ci = _objectClazzInfo;
  return findMethod(ci, signature) != 0;
}

bool 
Class::hasReadResolve()
{
  dmi::FunctionSignature signature;
  signature.size = 0;
  signature.functionname = "readResolve";
  const ClazzInfo* ci = _objectClazzInfo;
  return findMethod(ci, signature) != 0;
}

RObject 
Class::createDmiProxy(IN(RObject) dmiTarget)
{
   if (ClassLoader::getSystemClassLoader()->loadDmiProxyLibrary(getName()) == false)
    return Nil;
  RString name = getName();
  if (name->endsWith("_DmiProxy") == false)
    name = name + "_DmiProxy";
  RClass dmiProxyClass = Class::forName(name);
  RObject obj = dmiProxyClass->newInstance();
  dmi::DmiProxyBase* dbase = dynamic_cast<dmi::DmiProxyBase*>(obj.impl());
  if (dbase != 0)
    dbase->setDmiTarget(dmiTarget);
  return obj;
}

bool 
Class::getDmiProxies(IN(RObjectArray) proxies, IN(RObject) dmiTarget, int flags)
{
  if (ClassLoader::getSystemClassLoader()->loadDmiProxyLibrary(getName()) == false)
    return false;
  
  RObject obj = newInstance(); // #### todo code really works??
  dmi::DmiProxyBase* dbase = dynamic_cast<dmi::DmiProxyBase*>(obj.impl());
  if (dbase != 0)
    dbase->setDmiTarget(dmiTarget);
  proxies->append(obj);
  return true;
}


} // lang
} //acdk


