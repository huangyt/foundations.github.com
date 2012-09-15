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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Object.cpp,v 1.70 2005/04/18 14:28:07 kommer Exp $


#include <acdk.h>

///#include <acdk/lang/sys/sys.h>


#include "ClassCastException.h"
#include "CloneNotSupportedException.h"
#include "Exception.h"
#include "IndexOutOfBoundsException.h"
#include "NullPointerException.h"
#include "UnsupportedOperationException.h"
#include "Cloneable.h"
#include "Integer.h"
#include "NoSuchDmiElementException.h"
#include "ParamsMismatchException.h"
#include "Double.h"
#include "Long.h"
#include "System.h"
#include <acdk/lang/dmi/ClazzAttributesRes.h>

#include "ObjectArrayImpl.h"
#include "sys/core_atomicop.h"
#include "sys/core_system.h"
#include <acdk/io/ObjectWriter.h>
#include <acdk/io/ObjectReader.h>

void 
badCast(const ::acdk::lang::dmi::ClazzInfo* clazz, ::acdk::lang::ObjectBase* iface)
{
  THROW2(ClassCastException, iface->getClazzInfo(), clazz);  
}

void 
badBasicArrayCast(const ::acdk::lang::dmi::ClazzInfo* clazz, ::acdk::lang::ObjectBase* iface)
{
  badCast(clazz, iface);
}

void 
badObjectArrayCast(const ::acdk::lang::dmi::ClazzInfo* clazz, ::acdk::lang::ObjectBase* iface)
{
  badCast(clazz, iface);
}


namespace acdk {
namespace lang {

//using namespace ::acdk::lang::sys;
using ::acdk::lang::sys::ObjectHeap;
using ::acdk::lang::dmi::ScriptVar;

void* 
ObjectBase::operator new[](size_t size) 
{ 
  acdk::lang::sys::Allocator* alloc = ObjectHeap::allocator();
  void* ptr = alloc->allocate(size, acdk::lang::sys::ObjectMem);
  return ptr;
}

void 
ObjectBase::operator delete[](void* mem) 
{ 
  ObjectBase* o = (ObjectBase*)mem;
  
  if (o->_objectRefFlags & Object::IsUserHeapRef) 
  {
    acdk::lang::sys::Allocator::getHeaderFromObject(mem)->allocator->deallocate(mem, acdk::lang::sys::ObjectMem);
  } 
  else 
  {
    ObjectHeap::allocator()->deallocate(mem, acdk::lang::sys::ObjectMem);
  }
}

void* 
ObjectBase::operator new(size_t size)
{
  acdk::lang::sys::Allocator* alloc = ObjectHeap::allocator();
  void* ptr = alloc->allocate(size, acdk::lang::sys::ObjectMem);
  if ((alloc->flags & acdk::lang::sys::StackObjectAllocatorType) == false)
    ObjectHeap::newHeapObject((Object*)ptr);
  return ptr;
}



//static 
void 
ObjectBase::operator delete(void* mem)
{
  ObjectBase* o = (ObjectBase*)mem;
  if (o->_objectRefFlags & IsStackRef) {
    sys::coreout << " *** Object [" << (void*)o << "] is Not in Heap" << sys::eofl;
    return;
  }
  if (o->_objectRefFlags & Object::IsUserHeapRef) 
  {
    //acdk::lang::sys::Allocator::getHeaderFromObject(mem)
    acdk::lang::sys::Allocator::getHeaderFromObject(mem)->allocator->deallocate(mem, acdk::lang::sys::ObjectMem);
  }  
  else 
  {
    ObjectHeap::allocator()->deallocate(mem, acdk::lang::sys::ObjectMem);
    //delete mem;
  }
}


void* 
ObjectBase::operator new(size_t size, ::acdk::lang::sys::Allocator* allocator)
{
  void* ptr = 0;
  if (allocator == 0)
    allocator = stdalloc();
  ptr = allocator->allocate(size, acdk::lang::sys::ObjectMem);
  //if ((allocator->flags & acdk::lang::sys::StackObjectAllocatorType) == false)
  ObjectHeap::newHeapObject((Object*)ptr);
  return ptr;
}

#ifdef ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
void 
ObjectBase::operator delete(void* mem, ::acdk::lang::sys::Allocator* allocator)
{
  if (allocator == 0)
    operator delete(mem);
  else
    allocator->deallocate(mem, acdk::lang::sys::ObjectMem);
}
#endif //ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE


#if !defined(DOXYGENONLY)
class LockObjectBase 
{
protected:
  mutable Object* _lock;
public:
  LockObjectBase() : _lock(0) { }
  LockObjectBase(const ObjectBase& lock) : _lock((Object*)(&lock)) { _lock->Object::lock(); }
  ~LockObjectBase() { _lock->Object::unlock(); }
};
#endif //!defined(DOXYGENONLY)

Object* 
ObjectBase::_cast(const acdk::lang::dmi::ClazzInfo* ci) 
{ 
  if (ci->assignableFrom(getClazzInfo()) == true)
    return static_cast<Object*>(this);
  return 0; 
}


Object* 
InterfaceBase::_cast(const acdk::lang::dmi::ClazzInfo* ci) 
{ 
  if (ci->assignableFrom(_getObjectPtr()->getClazzInfo()) == true)
    return dynamic_cast<Object*>(this);
  return 0; 
}

ObjectBase::ObjectBase() 
  : 
#if defined(ACDK_USE_EXT_REFERER)
  _referer(new ::acdk::lang::sys::Referer(0)), 
#else
    _refCount(0),
#endif
    _objectRefFlags(_objectRefFlags & IsUserHeapRef),
    _magic(_MagicObjectSig)
#ifdef ACDK_MT
    , _lock(0)
#endif
  {
    
  }


void 
ObjectBase::disposeRef() const
{
  if (   isWeakRef() == false 
      && isMemLocked() == false 
      && isStackRef() == false 
      && (const_cast<ObjectBase*>(this)->allocator()->flags & acdk::lang::sys::NoRefCountAllocatorType) == 0) 
  {
    bool externalRecorded = ObjectBase::isExternalRecorded();
    if (externalRecorded == true) 
    {
      if (sys::ObjectHeap::notifyBeforeObjectDestruction(reinterpret_cast<Object*>(const_cast<ObjectBase*>(this))) == false)
          return;
      lockMem(true);
      const_cast<ObjectBase*>(this)->_call_finalize();
      sys::ObjectHeap::notifyWhileObjectDestruction(reinterpret_cast<Object*>(const_cast<ObjectBase*>(this)));
      
      acdk::lang::dmi::ClazzAttributesRes::releaseInstanceData((acdk::lang::dmi::MetaInfo*)const_cast<ObjectBase*>(this)->getClazzInfo(), 
                                                                reinterpret_cast<Object*>(const_cast<ObjectBase*>(this)));

      delete const_cast<ObjectBase*>(this);

    } 
    else 
    {
      lockMem(true);
      const_cast<ObjectBase*>(this)->_call_finalize();
      if (System::isInMain() == true)
        acdk::lang::dmi::ClazzAttributesRes::releaseInstanceData((acdk::lang::dmi::MetaInfo*)const_cast<ObjectBase*>(this)->getClazzInfo(), 
                                                                reinterpret_cast<Object*>(const_cast<ObjectBase*>(this)));
      delete const_cast<ObjectBase*>(this);
    }
  }
}




void 
ObjectBase::aquireLock() 
{
#ifdef ACDK_MT
  static sys::static_mutex _aquire_lock_lock;
  if (_lock == 0) { // double barriere for performance
    TLockGuard<sys::static_mutex> __lockthis(_aquire_lock_lock);
    if (_lock == 0)
      _lock = ObjectLock::aquireLock((const Object*)this);
  }
#endif
}

//static
bool ObjectBase::singleThreaded = false; 

//virtual 
void 
ObjectBase::lock() 
{
#ifdef ACDK_MT
  if (singleThreaded == true)
    return;
  if (isUnsynchronized() == true)
    return;
  aquireLock();
  _lock->lock();
#endif //ACDK_MT
}

//virtual 
void 
ObjectBase::unlock()
{
#ifdef ACDK_MT
  if (singleThreaded == true)
    return;
  if (isUnsynchronized() == true)
    return;
  aquireLock();
  _lock->unlock();
#endif
}

RClass 
Object::getClass()
{
  return Class::getSingeltonClass(getClazzInfo());
}

Object::Object() 
: ObjectBase()
{ 
  if (sys::ObjectHeap::onHeapAllocated(this) == false) 
  {
    _setObjectRefFlag(false, IsUserHeapRef);
    ObjectBase::setStackRef(true);
  }
  if (allocator()->flags & acdk::lang::sys::NoRefCountAllocatorType)
    _setObjectRefFlag(true, NoRefCounting);
  if (allocator()->flags & acdk::lang::sys::StackObjectAllocatorType)
    _setObjectRefFlag(true, IsUnsynchronized);
}

Object::Object(const Object&)
{
  if (sys::ObjectHeap::onHeapAllocated(this) == false) 
  {
    _setObjectRefFlag(false, IsUserHeapRef);
    ObjectBase::setStackRef(true);
  }
  if (allocator()->flags & acdk::lang::sys::NoRefCountAllocatorType)
    _setObjectRefFlag(true, NoRefCounting);
  if (allocator()->flags & acdk::lang::sys::StackObjectAllocatorType)
    _setObjectRefFlag(true, IsUnsynchronized);
}

//virtual 
Object::~Object() 
{ 
    
/*
  if (ObjectBase::isExternalRecorded() == true)
    sys::ObjectHeap::notifyWhileObjectDestruction(this);
  */
  /*
  if (ObjectBase::isStackRef() == false) {
    sys::ObjectHeap::removeObject(this); 
  } */  
}


//virtual 
dmi::SysFields& 
Object::getImplFields(dmi::SysFields& fields)  
{ 
  return fields; 
}

//virtual 
RString 
Object::toString()
{
  return getName();
}

//virtual 
RString 
Object::getName()
{
  RClass cls = getClass();
  if (cls == Nil)
    return RString(new String("Nil"));
  return cls->getName();
}


//virtual 
bool 
Object::equals(IN(RObject) o)
{
  if (instanceof(o, Object) == false)
    return false;
  if (&o == this)
    return true;
  return false;
}

//virtual 
int 
Object::compareTo(IN(RObject) o)
{
  THROW1(UnsupportedOperationException, "Object::compareTo()");
  return -1;
}



//static 
void 
ObjectBase::_throwException(const char* msg/* = ""*/)
{
  THROW1(Exception, new String(msg));
}

//static
void 
ObjectBase::_throwBadCastException(const char* msg)
{
  THROW1(ClassCastException, new String(msg));
}

//static 
void 
ObjectBase::_throwNullPointerException(const char* msg)
{
  if (msg == 0 || *msg == 0)
    msg = "NullPointerException";
  THROW1(NullPointerException, msg);
}

//static 
void 
ObjectBase::_throwNotImplementedYet(const char* msg)
{
  RString smsg(RString("Not Implemented yet: ") + msg);
  THROW1(UnsupportedOperationException, smsg);
}


void
ObjectBase::_throwObjectInsane(const InterfaceBase* obj, int magicsic)
{

  if (magicsic == _MagicDeletedSig)
    sys::coreout << "Object is deleted: " << (void*)obj << sys::eofl;
  else
    sys::coreout << "Object is insane: " << (void*)obj << sys::eofl;
  StringBuffer sb("Object is Insane: ");
  sb << (int)(void*)obj << "; sig: " << magicsic;
  THROW1(Exception, sb.toString());
}

//static
void 
ObjectBase::_throwArrayIndexOutOfBoundsException(size_t idx, size_t max, const char* msg)
{
  RString smsg = RString("ArrayIndexOutOfBoundsException at: ") + String::valueOf((int)idx)  +
                RString(" of maximum: ")  + String::valueOf((int)max) + RString(": ") + msg;
  THROW1(Exception, smsg);
}

//static
void 
ObjectBase::_throwIndexOutOfBoundsException(size_t idx, size_t max, const char* msg)
{
  RString smsg = RString("IndexOutOfBoundsException at: ") + String::valueOf((int)idx)  +
                " of maximum: "  + String::valueOf((int)max) + RString(": ") + msg;
  THROW1(IndexOutOfBoundsException, smsg);
}

//static 
RObject 
Object::create_instance() 
{ 
  return new Object(); 
}

//virtual
void
Object::finalize()
{
  // nothing
}



/*
void
Object::aquireCondition()
{
  aquireLock();
  aquire();
}*/

void 
Object::notify()
{
#ifdef ACDK_MT
  aquireLock();
  _lock->notify();
#endif //ACDK_MT
}
  
void 
Object::notifyAll()
{
#ifdef ACDK_MT
  aquireLock();
  _lock->notifyAll();
#endif //ACDK_MT
}

void 
Object::wait(int timeoutms, int timeoutus)
{
#ifdef ACDK_MT
  aquireLock();
  _lock->wait(timeoutms, timeoutus);
#endif // ACDK_MT
}


// virtual
RObject
Object::clone()
{
  return clone(allocator()); 
}

//virtual 
RObject 
Object::clone(sys::Allocator* alloc)
{
  if (dynamic_cast<const Cloneable*>(this) == 0)
    THROW1(CloneNotSupportedException, "Clone is not supported");
  return new (alloc) Object();
}

//virtual 
void 
Object::writeObject(IN(acdk::io::RObjectWriter) out, IN(RClass) cls)
{
  THROW0(UnsupportedOperationException);
}

//virtual 
void 
Object::readObject(IN(acdk::io::RObjectReader) in, IN(RClass) cls)
{
  THROW0(UnsupportedOperationException);
}



ACDK_CORE_PUBLIC char* TestAllocation()
{
  return new char[10];
}

ACDK_CORE_PUBLIC void TestDeAllocation(char *ptr)
{
  delete[] ptr;
}

/* this just to test if shared library will initialize static classes
class StaticInitClass
{
public:
  StaticInitClass()
  {
    sys::coreout << "acdk_core_dll initialized" << sys::eofl;
  }
  ~StaticInitClass()
  {
    sys::coreout << "acdk_core_dll deinitialized" << sys::eofl;
  }
};
StaticInitClass __staticInitClass;
*/


RObject 
Object::serialized_clone(bool deep, bool deepserialized)
{
  RClass cls = getClass();
  
  RObject target = cls->newInstance();
  if (cls->isArray() == true)
  {
    int l = invoke("length");
    target->invoke("resize", l);
    for (int i = 0; i < l; ++i)
    {
      dmi::ScriptVar svv = invoke("get", inOf(i));
      if (deep == true && svv.isObjectType() == true && svv.getObjectVar() != Nil)
      {
        RObject co;
        if (deepserialized == true)
          co = svv.getObjectVar().impl()->serialized_clone(deep, deepserialized);
        else
          co = svv.getObjectVar().impl()->clone();
        target->invoke("set", inOf(i), inOf(co));
      }
      else
        target->invoke("set", inOf(i), svv);
    }
    return target;
  }

  // in most cases this code should not be invoked
  ::acdk::lang::dmi::SysFields sourcem = getInternalFields(::acdk::lang::dmi::MiNonStatic);
  ::acdk::lang::dmi::SysFields targetm = target->getInternalFields(::acdk::lang::dmi::MiNonStatic);
  RObject source = this;
  for (int i = 0; i < sourcem.size(); i++) 
  {
    ::acdk::lang::dmi::SysField& s = sourcem[i];
    ::acdk::lang::dmi::SysField& t = targetm[i];
    if (::acdk::lang::dmi::MetaInfo::isStatic(s.fieldInfo->flags) == true ||
        s.fieldInfo->isTransient() == true )
      continue;
    if (t.fieldInfo->accessor != 0 )
    {
      dmi::ScriptVar sv;
      dmi::AcdkDmiClient dc;
      s.fieldInfo->accessor(source, Nil, sv, dc, ::acdk::lang::dmi::MiReadOnly, 0, 0);
      if (deep == true && sv.isObjectType() == true && sv.getObjectVar() != Nil)
      {
        if (deepserialized == true)
          sv = inOf(sv.getObjectVar()->serialized_clone(deep, deepserialized));
        else
          sv = inOf(sv.getObjectVar()->clone());
      }
      t.fieldInfo->accessor(target, Nil, sv, dc, 0, 0, 0);
      continue;
    }
    switch (s.type) {
    case ::acdk::lang::dmi::SysField::FT_Void: break; // nothing
    case ::acdk::lang::dmi::SysField::FT_Bool: 
      *t.cont.bval = *s.cont.bval; 
      break;
    case ::acdk::lang::dmi::SysField::FT_Byte :
    case ::acdk::lang::dmi::SysField::FT_Char : 
      *t.cont.cval = *s.cont.cval; 
      break;
    case ::acdk::lang::dmi::SysField::FT_UcChar : 
      *t.cont.ucval = *s.cont.ucval; 
      break;
    case ::acdk::lang::dmi::SysField::FT_Short : 
      *t.cont.sval = *s.cont.sval;  
      break;
    case ::acdk::lang::dmi::SysField::FT_Int : 
      *t.cont.ival = *s.cont.ival;  
      break;
    case ::acdk::lang::dmi::SysField::FT_JLong : 
      *t.cont.jlval = *s.cont.jlval;  
      break;
    case ::acdk::lang::dmi::SysField::FT_Float : 
      *t.cont.fval = *s.cont.fval;  
      break;
    case ::acdk::lang::dmi::SysField::FT_Double : 
      *t.cont.dval = *s.cont.dval;
      break;
    case ::acdk::lang::dmi::SysField::FT_Object : 
    {
      if (deep == true) {
        if ((*s.cont.oval) != Nil)
        {
          // ### todo if (serialized_deep)
          //t.cont.oval = (*s.cont.oval).impl()->serialized_clone(true); 
          if (deepserialized == true)
            t.set((*s.cont.oval).impl()->serialized_clone(true, true));
          else
            t.set((*s.cont.oval).impl()->clone());
        }
        else
          *t.cont.oval = *s.cont.oval;
      } else
        *t.cont.oval = *s.cont.oval;
      break;
    }
    }
  }
  return target;
}
  
bool 
Object::serialized_equals(IN(RObject) o, bool recursive_serialized)
{
  RClass cls = getClass();
  if (o == Nil)
    return false;
  RClass oclass = o->getClass();
  if (oclass != cls)
    return false;
  ::acdk::lang::dmi::SysFields sourcem = getInternalFields(::acdk::lang::dmi::MiNonStatic);
  ::acdk::lang::dmi::SysFields targetm = o->getInternalFields(::acdk::lang::dmi::MiNonStatic);
  for (int i = 0; i < sourcem.size(); i++) 
  {
    ::acdk::lang::dmi::SysField& s = sourcem[i];
    ::acdk::lang::dmi::SysField& t = targetm[i];
    if (::acdk::lang::dmi::MetaInfo::isStatic(s.fieldInfo->flags) == true ||
      s.fieldInfo->isTransient() == true )
      continue;
    switch (s.type) {
    case ::acdk::lang::dmi::SysField::FT_Void: break; // nothing
    case ::acdk::lang::dmi::SysField::FT_Bool: 
      if (*t.cont.bval != *s.cont.bval)
        return false;
      break;
    case ::acdk::lang::dmi::SysField::FT_Byte :
    case ::acdk::lang::dmi::SysField::FT_Char : 
      if (*t.cont.cval != *s.cont.cval)
        return false;
      break;
   case ::acdk::lang::dmi::SysField::FT_UcChar :
      if (*t.cont.ucval != *s.cont.ucval)
        return false;
      break;
    case ::acdk::lang::dmi::SysField::FT_Short : 
      if (*t.cont.sval != *s.cont.sval)
        return false;
      break;
    case ::acdk::lang::dmi::SysField::FT_Int : 
      if (*t.cont.ival != *s.cont.ival)
        return false;
      break;
    case ::acdk::lang::dmi::SysField::FT_JLong : 
      if (*t.cont.jlval != *s.cont.jlval)
        return false;
      break;
    case ::acdk::lang::dmi::SysField::FT_Float : 
      if (*t.cont.fval != *s.cont.fval)
        return false;
      break;
    case ::acdk::lang::dmi::SysField::FT_Double : 
      if (*t.cont.dval != *s.cont.dval)
        return false;
      break;
    case ::acdk::lang::dmi::SysField::FT_Object : 
    {
      
      if ((*s.cont.oval) != Nil && (*t.cont.oval) != Nil) {
        if (recursive_serialized == true) {
          if ((*s.cont.oval).impl()->serialized_equals((*t.cont.oval), true) == false)
            return false;
          else
            break;
        } else {
          if ((*s.cont.oval).impl()->equals(*t.cont.oval) == false)
            return false;
          else
            break;
        }
      } else if ((*s.cont.oval) == Nil && (*t.cont.oval) == Nil) {
        break;
      } else {
        return false;
      }
      break;
    }
    }
  }
  return true;
}


int 
Object::serialized_hashCode(bool recursive_serialized)
{
  RClass cls = getClass();
  int result = 0;
  ::acdk::lang::dmi::SysFields sourcem = getInternalFields(::acdk::lang::dmi::MiNonStatic);
  
  for (int i = 0; i < sourcem.size(); i++) {
    ::acdk::lang::dmi::SysField& s = sourcem[i];
    if (::acdk::lang::dmi::MetaInfo::isStatic(s.fieldInfo->flags) == true ||
        s.fieldInfo->isTransient() == true )
      continue;
    switch (s.type) {
    case ::acdk::lang::dmi::SysField::FT_Void: break; // nothing
    case ::acdk::lang::dmi::SysField::FT_Bool: 
      result = result * 31 + *s.cont.bval ? 1 : 0; 
      break;
    case ::acdk::lang::dmi::SysField::FT_Byte :
    case ::acdk::lang::dmi::SysField::FT_Char : 
      result = result * 31 + *s.cont.cval; 
      break;
    case ::acdk::lang::dmi::SysField::FT_UcChar : 
      result = result * 31 + *s.cont.ucval; 
      break;
    case ::acdk::lang::dmi::SysField::FT_Short : 
      result = result * 31 + *s.cont.sval;  
      break;
    case ::acdk::lang::dmi::SysField::FT_Int : 
      result = result * 31 + *s.cont.ival;  
      break;
    case ::acdk::lang::dmi::SysField::FT_JLong : 
      result = result * 31 + *s.cont.jlval;  
      break;
    case ::acdk::lang::dmi::SysField::FT_Float : 
      result = (result * 31) + (int)*s.cont.fval;  
      break;
    case ::acdk::lang::dmi::SysField::FT_Double : 
      result = result * 31 + (int)*s.cont.dval;
      break;
    case ::acdk::lang::dmi::SysField::FT_Object : 
    {
      if ((*s.cont.oval) != Nil) {
        if (recursive_serialized == true)
          result = result * 31 + (*s.cont.oval).impl()->serialized_hashCode(recursive_serialized);
        else
          result = result * 31 + (*s.cont.oval).impl()->hashCode();
      } else {
        ;
      }
      
      break;
    }
    }
  }
  return result;
}




} // lang
} // acdk

