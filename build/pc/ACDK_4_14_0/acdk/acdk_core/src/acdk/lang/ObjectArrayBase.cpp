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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ObjectArrayBase.cpp,v 1.22 2005/04/16 10:46:45 kommer Exp $

#include <acdk.h>
#include "ObjectArrayBase.h"

#include "IllegalArgumentException.h"
#include "ClassCastException.h"
#include <acdk/util/ArrayIterator.h>
#include <acdk/util/ArrayList.h>

namespace acdk {
namespace lang {

//virtual 
Object* 
ObjectArrayBaseImpl::_cast(const acdk::lang::dmi::ClazzInfo* ci)
{
  return Object::_cast(ci);
}

//static 
RObject 
ObjectArrayBaseImpl::create_instance()
{
  return new ObjectArrayBase(0);
}

int 
ObjectArrayBaseImpl::hashCode()
{
  int erg = 0;
  for (array_iterator it = _begin; it < end(); ++it)
    erg = 31 * erg + (*it == Nil ? 0 : (*it)->hashCode());
  return erg;
}


bool 
ObjectArrayBaseImpl::equals(IN(RObject) o)
{
  if (instanceof(o, ObjectArrayBaseImpl) == false)
    return false;
  if (o.impl() == this)
    return true;
  RObjectArrayBaseImpl other = (RObjectArrayBaseImpl)o;
  if (length() != other->length())
    return false;
  array_iterator it = _begin;
  array_iterator oit = other->_begin;
  for (; it < end(); ++it, ++oit)
  {
    if (*it == *oit)
      continue;
    if (*it == Nil || *oit == Nil)
      return false;
    if ((*it)->equals(*oit) == false)
      return false;
  }
  return true;
}

RString 
ObjectArrayBaseImpl::toString()
{
  StringBuffer sb(1024);
  sb.append("[");
  for (array_iterator it = _begin; it < end(); ++it)
  {
    if (it > _begin)
      sb.append(", ");
    if (*it == Nil)
      sb.append("Nil");
    else
    {
      RObject o = (*it).impl();
      if (o.impl() != this)
        sb.append(o->toString());
    }
  }
  sb.append("]");
  return sb.toString();
}

RObject 
ObjectArrayBaseImpl::clone(::acdk::lang::sys::Allocator* alc) 
{
  RObjectArrayBaseImpl newa = new ObjectArrayBaseImpl(_elementClazzInfo, length());
  array_iterator it = _begin;
  array_iterator oit = newa->begin();
  for (; it < end(); ++it, ++oit)
  {
    if (*it != Nil)
      *oit = (*it).impl()->clone(alc);
  }
  return &newa;
}



void 
ObjectArrayBaseImpl::remove(int idx)
{
  _makeSetSlot(idx);
  array_iterator it = _begin + idx;
  for (; it < end() - 1; ++it)
    *it = *(it + 1);
  --_length;
  *end() = Nil;
}

void 
ObjectArrayBaseImpl::resize(int newSize)
{
  if (newSize == length())
    return;
  if (newSize > capacity())
  {
    ensureCapacity(newSize);
    _length = newSize;
    return;
  }
  for (array_iterator it = _begin + newSize; it < end(); ++it)
    *it = Nil;
  _length = newSize;
}

//foreign 
RObject
ObjectArrayBaseImpl::_castAssignmentElement(IN(RObject) value) const
{
  if (_elementClazzInfo == 0 || Nil == value)
    return value;
  /*if (_elementClazzInfo->assignableFrom(value->getClazzInfo()) == true)
    return value;
    */
  Object* o =  value->_cast(_elementClazzInfo);
  if (o == 0)
    badCast(_elementClazzInfo, value);
  RObject ret(o);
  if (_elementClazzInfo->_castToInterfacePtr != 0)
    ret._setInterfacePtr(_elementClazzInfo->_castToInterfacePtr(o));
  return ret;
  //return Nil;
}

void 
ObjectArrayBaseImpl::concat(IN(RObjectArrayBaseImpl) other)
{
  ensureCapacity(length() + other->length());
  array_iterator oit = other->begin();
  array_iterator oend = other->end();
  for (; oit < oend; ++oit)
    append(*oit);
}

acdk::util::RIterator 
ObjectArrayBaseImpl::iterator()
{
  return new acdk::util::ArrayIterator((RObjectArray)(ObjectArray*)this);
}
acdk::util::RList 
ObjectArrayBaseImpl::asContainer(bool copy)
{
  return new acdk::util::ArrayList(RObjectArray((ObjectArray*)this), copy);
}

void 
ObjectArrayBaseImpl::_dispose()
{
  if (_begin == 0)
    return;
  for (array_iterator it = _begin; it < end(); ++it)
    *it = Nil;
  _destroyData();
}

void 
ObjectArrayBaseImpl::ensureCapacity(int newsize)
{
  if (newsize < 0)
    THROW1(IllegalArgumentException, RString("Cannot create ObjectArray with negative capacity: ") + newsize);
  int oldcap = capacity();
  if (newsize <= oldcap)
    return;
  if (oldcap == 0 && newsize < 10)
    newsize = 10;
  else if (newsize < oldcap * 2)
    newsize = oldcap * 2;
  _makeCapacity(newsize); 
}

void 
ObjectArrayBaseImpl::_makeCapacity(int newCap)
{
  RObject* data = _createData(newCap);
  array_iterator it = begin();
  array_iterator eit = end();
  if (it != 0)
    for (int i = 0; it < eit; ++it, ++i)
      data[i] = *it;
  _dispose();
  _begin = data;
  _endCap = data + newCap;
}

void 
ObjectArrayBaseImpl::_makeInsertSlot(int idx)
{
  if (idx != 0)
    _checkIndexAccess(idx - 1);
  ensureCapacity(length() + 1);
  
  array_iterator insit = _begin + idx;
  array_iterator it = end();
  for (; it > insit; --it)
    *it = *(it - 1);
  ++_length;
}

//static 
const ObjectArrayBaseImpl* 
ObjectArrayBaseImpl::_tryCast(const acdk::lang::dmi::ClazzInfo* toClazz, const ObjectArrayBaseImpl* other)
{
  const acdk::lang::dmi::ClazzInfo* fromClazz = other->_elementClazzInfo;
  
  if (toClazz == fromClazz)
    return other;
  while (fromClazz != 0 && fromClazz->interfaces != 0 && fromClazz->interfaces[0] != 0)
  {
    if (fromClazz->interfaces[0]->type == toClazz)
      return other;
    fromClazz = fromClazz->interfaces[0]->type;
  }
  if (other->length() > 0 && const_cast<ObjectArrayBaseImpl*>(other)->getElement(0) != Nil)
  {
    RObject o =  const_cast<ObjectArrayBaseImpl*>(other)->getElement(0)->_cast(other->_elementClazzInfo);
    if (o != Nil)
      return 0;
    //if (try_dmi_cast<other->get(0)
  }
  if (toClazz->assignableFrom(other->_elementClazzInfo) == false)
    THROW2(ClassCastException, other->_elementClazzInfo, toClazz);
  return 0;
}

//static 
const ::acdk::lang::dmi::ClazzMethodInfo* 
ObjectArrayBaseImpl::dmiConstructor(::acdk::lang::Object* This, 
                                    IN(acdk::lang::RString) fname, 
                                    ::acdk::lang::dmi::ScriptVar& ret, 
                                    ::acdk::lang::dmi::ScriptVarArray& args, 
                                    ::acdk::lang::dmi::DmiClient& dc,
                                    IN(::acdk::lang::RStringArray) namedArgs,
                                    int flags,
                                    const ::acdk::lang::dmi::ClazzInfo* clazzinfo,
                                    const ::acdk::lang::dmi::ClazzMethodInfo* methinf)
{
  int elcount = args[0].getIntVar();
  const ::acdk::lang::dmi::ClazzInfo* elclazz = (const ::acdk::lang::dmi::ClazzInfo*)clazzinfo->userInfo;
  ret = inOf(RObject(new ObjectArrayBase(elclazz, elcount)));
  return (const ::acdk::lang::dmi::ClazzMethodInfo*)1;
}

/*
void 
ObjectArrayBaseImpl::readObject(IN(::acdk::io::RObjectReader) in)
{
  ::acdk::io::readObjectArray((ObjectArray*)this, in);
}
*/

// ****************************************** ObjectArrayBase  **************************

acdk::lang::dmi::ClazzInfo* 
ObjectArrayBase::getClazzInfo()  
{ 
  return ::acdk::lang::Class::getSingeltonArrayClazz(_elementClazzInfo); 
}

::acdk::lang::RClass 
ObjectArrayBase::getClass() 
{
  return ::acdk::lang::Class::getSingeltonArrayClass(_elementClazzInfo);
}

void 
ObjectArrayBase::getCollectableFields(FieldReferences& fields) 
{ 
  for (array_iterator it = _begin; it < end(); ++it)
    fields.push_back(it);
}


ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo ObjectArray_fields__length =
{
  2,
  0, // attributeRes
  "_length", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::dmi::ClazzInfo::getIntClazz(),
  0, 
  (void*)0 // address of field
};

ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo ObjectArray_fields__data =
{
  2,
  0, // attributeRes
  "_data", // label
  -1, // nameHashCode
  "", // ns
  0, // _scopeParent
  0, // _nextScopeSibling
  ::acdk::lang::Object::clazzInfo(),
  0, 
  (void*)0 // address of field
};

::acdk::lang::dmi::SysFields 
ObjectArrayBase::getInternalFields(int flags, const ::acdk::lang::dmi::ClazzInfo* clazz) 
{
  ::acdk::lang::dmi::SysFields __fields;// =  Object::getInternalFields(::acdk::lang::dmi::MiNonStatic);
  __fields.reserve(__fields.size() + length());
  __fields.push_back(::acdk::lang::dmi::SysField(&ObjectArray_fields__length, &_length)); // already addeed by base implementation
  array_iterator eit = end();
  for (array_iterator it = _begin; it < eit; it++) 
  {
    __fields.push_back(::acdk::lang::dmi::SysField(&ObjectArray_fields__data, (*it)._ref_this()));
  }
  return getImplFields(__fields);
}

} // namespace lang
} // namespace acdk
