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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ObjectBase.h,v 1.37 2005/04/28 14:58:40 kommer Exp $

#ifndef acdk_lang_ObjectBase_h
#define acdk_lang_ObjectBase_h

#include "sys/ControlPushPopOnStack.h"
#include "sys/Allocator.h"

#include "sys/ObjectHeap.h"
#include "dmi/SysFields.h"

#ifdef ACDK_MT
#  include "sys/ObjectLockPool.h"
#endif
#include "dmi/ClazzInfo.h"
#include "sys/core_vector.h"
#include "sys/core_atomicop.h"

#include "InterfaceBase.h"

namespace acdk {
namespace lang {


namespace dmi {

  class SysField; 
  typedef ::acdk::lang::sys::core_vector<SysField> SysFields;

} // namespace dmi

/// see ObjectBase::_magic, is valid object
const unsigned short _MagicObjectSig =  0xACDF;
/// see ObjectBase::_magic, is delete object
const unsigned short _MagicDeletedSig = 0xDEAD;

/// @internal used only internally
#define OBJECTBASE_INTERNAL_ISOBJECTREFLAG(flag) ((_objectRefFlags & flag) == flag)

/** 
  ACDK specific implementation of Objects.
  API: ACDK<br>
  @author Roger Rene Kommer
  @version $Revision: 1.37 $
  @date $Date: 2005/04/28 14:58:40 $
*/  
foreign 
class ACDK_CORE_PUBLIC ObjectBase
      ACDK_INTERFACEBASE
{
public:
  typedef unsigned short RefFlagField;
public:
  /** global variable which is true if program is running in single thread mode */
  static bool singleThreaded; 
  /** Some information about memory status. */
  enum RefFlags 
  {
    /** the object is allocated on the stack */
    IsStackRef = 0x0001,
    /** Reference is allocated as static variable */
    IsStaticRef = 0x0002,
    /** Object is allocated on user Heap */
    IsUserHeapRef = 0x0004,
    /** Similar to IsWeakRef, but can also be unlocked */
    IsMemLocked = 0x0008,
    /** Object will not be deleted if _refCount == 0 */
    IsWeakRef = 0x0010,
    /** Object will not be synchronized. This means lock() and unlock will have no effect */
    IsUnsynchronized = 0x0020,
    /** Object has external records */
    IsExternalRecorded = 0x0040,
    /** 
      in case of releaseRef call objects internal _gc_releaseRef()
      In case of running System::gc GC doesn't use metainfo to retrieve
      number of references, but call getCollectableFields()
    */
    ObjectHasLocalGc = 0x0080,

    /** Object is Soft-Reference */
    IsSoftReference = 0x0100,
    /** Internal, used in GC */
    Visited = 0x1000,
    /** Internal, used in GC */
    Marked = 0x2000,
    /** True if finalize is already called */
    FinalizeCalled = 0x4000,
    /** Allocator doesn't rely on RefCounting */
    NoRefCounting = 0x8000
  };
private:
#if defined(ACDK_USE_EXT_REFERER)
public:
  ::acdk::lang::sys::Referer* _referer;
private:
#else
  /** The Refernce Conter */
  mutable sys::core_atomicop _refCount;
#endif
  /** a set of internal Flags.
    @see RefFlags
  */
  mutable RefFlagField _objectRefFlags;

  /** magic Number of Object, for sanity check */
  unsigned short _magic;
protected:
#ifdef ACDK_MT
  mutable acdk::lang::ObjectLock* _lock;
#endif // ACDK_MT

protected:
  ObjectBase();
public:  
  virtual ~ObjectBase() 
  { 
#ifdef ACDK_MT
    if (_lock != 0)
      ObjectLock::releaseLock(_lock);
#endif
    _magic = _MagicDeletedSig;
#if defined(ACDK_USE_EXT_REFERER)
    delete _referer;
#endif
  }
  /** the standard operator new  */
  void* operator new(size_t size);
  /** standard operator delete */
  void operator delete(void* ptr);
  /** 
    array allocator
    forwards to ACDK memory manager
  */
  void* operator new[](size_t size);
  /** 
    array deallocator
    forwards to ACDK memory manager
  */
  void operator delete[](void* ptr);
  
  /**
    used for other allocators, will handled as stack var!
  */
  void* operator new(size_t size, void* mem) { return mem; }
  /**
    used for other allocators, will handled as stack var!
  */
  void* operator new[](size_t size, void* mem) { return mem; }
#ifdef ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  /**
    used for other allocators, will handled as stack var!
  */
  void operator delete(void* ptr, void* mem) {}
  /**
    used for other allocators, will handled as stack var!
  */
  void operator delete[](void* ptr, void* mem) {  }
#endif
  /**
    operator new, which uses the placement operator.
    SharedMemAllocator* myObjectAllocator = new SharedMemAllocator();
    RString o = new (myObjectAllocator) Object();

  */
  void* operator new(size_t size, ::acdk::lang::sys::Allocator* allocator); 
  /**
    this operator delete will only be called, in the case of an Exception in 
    the Constructor of Object or derived
    */
#ifdef ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  void operator delete(void* mem, ::acdk::lang::sys::Allocator* allocator);
#endif //#ifdef ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  
  /** 
    returns the Allocator of this Object. 
    By default it returns the StandardHeapAllocator 
  */
  inline ::acdk::lang::sys::Allocator* allocator()
  {
    if (OBJECTBASE_INTERNAL_ISOBJECTREFLAG(ObjectBase::IsUserHeapRef))
    {
      ::acdk::lang::sys::Allocator* alc = ::acdk::lang::sys::Allocator::getHeaderFromObject(this)->allocator;
      if (alc != 0)
        return alc;
    }
    return ::acdk::lang::sys::ObjectHeap::allocator();
  } 

  /** 
    If the object was allocated with an Allocator,
    this allocator can be used to allocate internal memory
  */
  inline void* allocate(size_t size, acdk::lang::sys::AllocatedType at = acdk::lang::sys::RawMem) 
  {
    return allocator()->allocate(size, at);
  }
  
  /** 
    If the object was allocated with an Allocator,
    this allocator can be used to deallocate internal memory
  */
  inline void deallocate(void* ptr, acdk::lang::sys::AllocatedType at = acdk::lang::sys::RawMem)
  {
    allocator()->deallocate(ptr, at);
  }

  /** release Reference. 
    If not locked or WeakReference, Object will be destroyed 
  */
  inline void releaseRef(bool force = false) const
  {
    if (OBJECTBASE_INTERNAL_ISOBJECTREFLAG(IsStackRef) == false && (force == true || OBJECTBASE_INTERNAL_ISOBJECTREFLAG(NoRefCounting) == false))
    {
      testIntegrity();
      if (OBJECTBASE_INTERNAL_ISOBJECTREFLAG(ObjectHasLocalGc) == true)
        _gc_releaseRef(force);
      else
      {
        _releaseRefCount();
      }
    }
  }
  /**
    decrement the Object refcounter and dispose this object
  */
  inline bool _releaseRefCount() const
  {
    if (_refCount.decr_test_zero() == true) 
    {
      disposeRef();
      return true;
    }
    return false;
  }
  /** 
    may be called by releaseRef() if ObjectHasLocalGc is set 
    @return true if call the object destroyed
  */
  virtual bool _gc_releaseRef(bool force = false) const 
  { 
    return _releaseRefCount(); 
  }

  /** increment reference counter */
  inline void addRef(bool force = false) const
  {
    if (OBJECTBASE_INTERNAL_ISOBJECTREFLAG(IsStackRef) == false && 
        (force == true || OBJECTBASE_INTERNAL_ISOBJECTREFLAG(NoRefCounting) == false))
    {
      testIntegrity();
      _refCount.increment();
    }
  }
  /**
    releases this object.
    calls finalize and delete this
  */
  void disposeRef() const;

  /** test if object is still sane */
  inline void testIntegrity() const 
  {
    if (_magic != _MagicObjectSig) 
      _throwObjectInsane(this, _magic);
  }
  /** 
    return the magic (_MagicObjectSig  or _MagicDeletedSig) 
    from this object instance
  */
  foreign inline unsigned short magic() const { return _magic; }
  
  /** internal function to set flags before constructor */
  inline void _inititializeObjectAttrFlags(int flags) { _objectRefFlags = flags; }
   /** set or release internal flag */
  inline void _setObjectRefFlag(bool onoff, RefFlagField flag) const
  {
    if (onoff == true) 
      _objectRefFlags |= flag;
    else
      _objectRefFlags &= ~flag;
  }  
  inline bool _isObjectRefFlag(RefFlagField theFlag) const { return (_objectRefFlags & theFlag) == theFlag; }
#if !defined(ACDK_USE_EXT_REFERER)
  /** returns the current reference counter */
  inline int refCount() const { return _refCount.read();  }
#endif
  /** set Object as Stack Object.
      Object will normaly not destroyed.
      <code>
      String str("hallo"); // this is a stack object
  */
  inline void setStackRef() const
  {
    if (isStackRef())
      return;
    _objectRefFlags |= IsStackRef;
#if !defined(ACDK_USE_EXT_REFERER)
    _refCount.increment();
#endif
  }

  /** returns true if Object is allocated on Stack */
  inline bool isStackRef() const { return (_objectRefFlags & IsStackRef) == IsStackRef; } 
  /** 
      declares this object as stack object.
      This will also declare the object as synchronizable or not!
  */
  inline void setStackRef(bool asstackref) const
  {  
    if (asstackref == true) {
      setUnsynchronized(true);
      _objectRefFlags |= IsStackRef;  
    } else {
      setUnsynchronized(false);
      _objectRefFlags &= ~IsStackRef;
    }
  }
  
  /** returns true if object is declared as static referenced */
  inline bool isStaticRef() const { return OBJECTBASE_INTERNAL_ISOBJECTREFLAG(IsStaticRef); } 
  /** returns set or remove reference as static */
  inline void setStaticRef(bool asstaticref) const {  _setObjectRefFlag(asstaticref, IsStaticRef); }
  /** returns true if object is declared as weak referenced */
  inline bool isWeakRef() const { return OBJECTBASE_INTERNAL_ISOBJECTREFLAG(IsWeakRef); }
  /** returns true if object reference is locked */
  inline bool isMemLocked() const { return OBJECTBASE_INTERNAL_ISOBJECTREFLAG(IsMemLocked); }
  /** lock or unlock the object reference.
      if object is locked, object will not be destroyed
  */
  inline void lockMem(bool lockit) const {   _setObjectRefFlag(lockit, IsMemLocked); }

  /** used internally for garbage collecting */
  inline bool isMarked() const { return  OBJECTBASE_INTERNAL_ISOBJECTREFLAG(Marked); }
  /** used internally for garbage collecting */
  inline void setMarked() const { _objectRefFlags |= Marked; }
  /** used internally for garbage collecting */
  inline void unsetMarked() const { _objectRefFlags &= ~Marked; }
  
  /** returns true if object is declared as unsychronizable, which means lock/unlock will have no effect */
  inline bool isUnsynchronized() const { return OBJECTBASE_INTERNAL_ISOBJECTREFLAG(IsUnsynchronized); } 

  /** returns set or unset unsychronizable, which means lock/unlock will have no effect */
  inline void setUnsynchronized(bool unsynced) const { _setObjectRefFlag(unsynced, IsUnsynchronized); }
  
  /** @see ObjectHasLocalGc */
  inline bool hasLocalGc() const { return OBJECTBASE_INTERNAL_ISOBJECTREFLAG(ObjectHasLocalGc); }
  /** @see ObjectHasLocalGc */
  void setLocalGc(bool flag)  const { _setObjectRefFlag(flag, ObjectHasLocalGc); }

  /** returns true if object external listener, which should be notified at deletion */
  inline bool isExternalRecorded() const { return OBJECTBASE_INTERNAL_ISOBJECTREFLAG(IsExternalRecorded); } 
  /** see isExternalRecorded() */
  inline void setExternalRecorded(bool flag) const { _setObjectRefFlag(flag, IsExternalRecorded); }
  /** return true if this object is held by a soft reference */
  inline bool isSoftReference() const { return OBJECTBASE_INTERNAL_ISOBJECTREFLAG(IsSoftReference); } 
  /** see isSoftReference() */
  inline void setSoftReference(bool flag) const { _setObjectRefFlag(flag, IsSoftReference); }
  

  /** aquire MT-Lock from lock cache */
  void aquireLock();
  /**
    lock the object instance.
    Note: Will be overwritten by acdk::io::Reader and acdk::io::Writer
    Note: if Unsynchronized or isStack is set, this call will have no effect
  */
  virtual void lock();
  
  /**
    unlock the object instance
    Note: Will be overwritten by acdk::io::Reader and acdk::io::Writer
    Note: if Unsynchronized or isStack is set, this call will have no effect
  */
  virtual void unlock();
  /**
    Only for Java compatibility
  */
  virtual void finalize() 
  {
  }
  /**
    call finalize only if FinalizeCalled not set
  */
  inline void _call_finalize()
  {
    if (OBJECTBASE_INTERNAL_ISOBJECTREFLAG(FinalizeCalled) == true)
      return;
    _setObjectRefFlag(true, FinalizeCalled);
    this->finalize();
  }
  /**
    return the ClazzInfo for this class
    will be re-implemented for each class 
    by methods generated by acdkmc
  */
  static dmi::ClazzInfo* clazzInfo() { return 0; }
  /**
    return the ClazzInfo for this class
    will be re-implemented for each class 
    by methods generated by acdkmc
  */
  virtual dmi::ClazzInfo* getClazzInfo()  { return 0; }
  /**
    for DMI implemented classes with multiple interface
    allow casts. The returned Object will be casted
    to the correct type using dynamic_cast
    @param ci target type
    @return 0 if cannot be casted
  */
  virtual Object* _cast(const acdk::lang::dmi::ClazzInfo* ci);
  /**
    retrun the collectable fields of this 
    object (all Objects fields)
    will be re-implemented for each class 
    by methods generated by acdkmc
    If this is manually implemented, the flag ObjectHasLocalGc
    should be set.
  */
  virtual void getCollectableFields(FieldReferences& fields) { }
  
  /**
    @internal
  */
  static void _throwException(const char* msg = "");
  /**
    @internal
  */
  static void _throwBadCastException(const char* msg = "");
  /**
    @internal
  */
  static void _throwNullPointerException(const char* msg = "");
  /**
    @internal
  */
  static void _throwNotImplementedYet(const char* msg = "");
  /**
    @internal
  */
  static void _throwArrayIndexOutOfBoundsException(size_t idx = 0, size_t max = size_t(-1), const char* msg = "");
  /**
    @internal
  */
  static void _throwIndexOutOfBoundsException(size_t idx = 0, size_t max = size_t(-1), const char* msg = "");
  /**
    @internal
  */
  static void _throwObjectInsane(const InterfaceBase* obj, int magicsic);
 
};

/** 
  @internal 
  helper class for Heap management 
*/
foreign 
class ControlObjectSetMarket
{
  ObjectBase* _obj;
public:
  ControlObjectSetMarket(const ObjectBase* obj)  : _obj(const_cast<ObjectBase*>(obj)) { }
  void push() { if (_obj) _obj->setMarked(); }
  void pop() { if (_obj) _obj->unsetMarked(); }
};


#if !defined(ACDK_USE_EXT_REFERER)

/** 
  helper class to protect constructors for freeing this by accident 
  @internal 
  @see ACDK_SAFE_CONSTRUCTOR
*/
foreign 
class SaveConstructor 
{
  ObjectBase* _obj;
public:
  SaveConstructor(ObjectBase* obj) 
    : _obj(obj)
  {
    _obj->lockMem(true);
    _obj->addRef();
  }
  ~SaveConstructor()
  {
    _obj->releaseRef();
    _obj->lockMem(false);
  }
};
#endif //#if !defined(ACDK_USE_EXT_REFERER)



#ifndef ACDK_USE_EXT_REFERER 
/** 
  Use ACDK_SAFE_CONSTRUCTOR(), if you cast 'this' to a reference (like RObject) in a constructor:
  @code
  AClass::AClass()
  {
    ACDK_SAFE_CONSTRUCTOR();
    init(this); 
  }
  @endcode
  @ingroup acdkmacros
*/
#define ACDK_SAFE_CONSTRUCTOR() ::acdk::lang::SaveConstructor __safe_constructor(this)
#else
#define ACDK_SAFE_CONSTRUCTOR() 
#endif

/** 
  Use ACDK_SAFE_DESTRUCTOR, if you use 'this' if you cast 'this' to a reference (like RObject) in a destructor
  @ingroup acdkmacros
  @code
  AClass::~AClass()
  {
    ACDK_SAFE_DESTRUCTOR();
    deinit(this); 
  }
  @endcode
*/
#define ACDK_SAFE_DESTRUCTOR() lockMem(true) 

} // lang
} // acdk

#endif //acdk_lang_ObjectBase_h

