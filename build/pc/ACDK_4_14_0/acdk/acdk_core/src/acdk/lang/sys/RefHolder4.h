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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/RefHolder4.h,v 1.9 2005/04/09 19:26:56 kommer Exp $


/*

  RefHolder4
  Based on RefHolder3
  use bool variable for caching if Object is on Stack
*/

/* 
    RefHolder is the base template class to hold references.
    @author Roger Rene Kommer
    @version $Revision: 1.9 $
    @date $Date: 2005/04/09 19:26:56 $
    Normally the user will not handle this directly, but
    will will be declared with:<br>
    ACDK_DECL_CLASS(ClassName)<br>
    This will define:<br>
    typedef RefHolder<ClassName> RClassName;
*/


/*
   No implicit convertion from RAClass to RObject 
   possible. For convenience introduce a 
   operator RObject();
*/
//#define ACDK_CORE_NEED_ROBJECT_CONVERTER
#define upcast_explicit explicit


template<class T>
class RefHolder 
{
protected:
  /* _impl is a pointer to Object.*/
  ::acdk::lang::Object* _impl;
  /* _iptr is a rightcasted pointer to the class */
  T* _iptr;
  bool _isStackRef;
public:
  void* operator new(size_t size) { return acdk_allocate(size); }
  void operator delete(void* ptr) { acdk_deallocate(ptr); }
  void* operator new[](size_t size){ return acdk_allocate(size); }
  void operator delete[](void* ptr){ acdk_deallocate(ptr); }
  void* operator new(size_t size, ::acdk::lang::sys::Allocator* allocator) { return acdk_allocate(size, allocator); }
  void* operator new(size_t size, void* memptr) { return memptr; }
#ifdef ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  void operator delete(void* mem, ::acdk::lang::sys::Allocator* allocator) { acdk_deallocate(mem, allocator); }
  void operator delete(void* mem, void* memptr) {  }
#endif //ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  
  typedef T Type;
protected:
  void _initObject(::acdk::lang::Object* obj)
  {
    _impl = obj;
    _isStackRef = _impl->isStackRef();
    if (_isStackRef == false) {
      ACDK_REFCOUNTING_CODE( _impl->addRef(); )
    }
  }
  void _initObject(::acdk::lang::Object* obj, bool isstackref)
  {
    _impl = obj;
    _isStackRef = isstackref;
    if (_isStackRef == false) {
      ACDK_REFCOUNTING_CODE( _impl->addRef(); )
    }
  }
  void _initObjectBase(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (obj == 0)
      return;
    _iptr = dynamic_cast<T*>(iface);
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _initObject(obj);  
  }
  void _initObjectBase(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    if (obj == 0)
      return;
    _iptr = dynamic_cast<T*>(iface);
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _initObject(obj, isstackref);  
  }
  void _initInterfaceBase(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
  {
    if (obj == 0)
      return;
    _iptr = dynamic_cast<T*>(iface->_getObjectPtr());
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _initObject(obj);
  }
  void _initInterfaceBase(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    if (obj == 0)
      return;
    _iptr = dynamic_cast<T*>(iface->_getObjectPtr());
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _initObject(obj, isstackref);
  }
  void _initT(T* iface, ::acdk::lang::Object* obj)
  {
    if (iface == 0)
      return;
    _iptr = iface;
    _initObject(obj);
  }
  void _initT(T* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    if (iface == 0)
      return;
    _iptr = iface;
    _initObject(obj, isstackref);
  }
  void _init(T* iface, ::acdk::lang::Object* obj)
  {
    _initT(iface, obj);  
  }
  void _init(T* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    _initT(iface, obj, isstackref);  
  }
  void _init(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    _initObjectBase(iface, obj);
  }
  void _init(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    _initObjectBase(iface, obj, isstackref);
  }

  void _init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
  {
    _initInterfaceBase(iface, obj);
  }
  void _init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    _initInterfaceBase(iface, obj, isstackref);
  }

  void _release()
  {
    if (_impl == 0 || _isStackRef == true)
      return;
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
    _impl = 0;
    _iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
  }

  void _assignT(T* iface, ::acdk::lang::Object* obj)
  {
   if (_impl == obj)
      return;
    bool sr = false;
    if (obj != 0) {
      sr = obj->isStackRef();
      if (sr == false) {
         ACDK_REFCOUNTING_CODE( obj->addRef(); )
      }
    }
    if (_impl != 0 && _isStackRef == false) {
       ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    }
    _impl = obj;
    _iptr = iface;
    _isStackRef = sr;
  }
  void _assignT(T* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    if (_impl == obj)
      return;
    if (obj != 0 && isstackref == false) {
      ACDK_REFCOUNTING_CODE( obj->addRef(); )
    }
    if (_impl != 0 && _isStackRef == false) {
       ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    }
    _impl = obj;
    _iptr = iface;
    _isStackRef = isstackref;
  }
  void _assignT2(T* iface, ::acdk::lang::Object* obj)
  {
    bool sr = obj->isStackRef();
    if (sr == false) {
      ACDK_REFCOUNTING_CODE( obj->addRef(); )
    }
    if (_impl != 0 && _isStackRef == false) {
      ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    } 
    _impl = obj;
    _iptr = iface;
    _isStackRef = sr;
  }
  void _assign2(T* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    if (isstackref == false) {
      ACDK_REFCOUNTING_CODE( obj->addRef(); )
    }
    if (_impl != 0 && _isStackRef == false) {
      ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    } 
    _impl = obj;
    _iptr = iface;
    _isStackRef = isstackref;
  }
  void _assign(T* iface, ::acdk::lang::Object* obj) { _assignT(iface, obj); }
  void _assign(T* iface, ::acdk::lang::Object* obj, bool isstackref) { _assignT(iface, obj, isstackref); }
  /*
   precond: iface != 0 &&  obj != impl
  */
  void _assignObjectBase(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (obj == _impl)
       return;
    if (obj != 0) {
      T* tptr = dynamic_cast<T*>(iface);
      if (tptr == 0)
         ::acdk::lang::ObjectBase::_throwBadCastException();
      _assign2(tptr, obj);
    } else
      _release();
  }
  void _assignObjectBase(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    if (obj == _impl)
       return;
    if (obj != 0) {
      T* tptr = dynamic_cast<T*>(iface);
      if (tptr == 0)
         ::acdk::lang::ObjectBase::_throwBadCastException();
      _assign2(tptr, obj, isstackref);
    } else
      _release();
  }
  void _assignInterfaceBase(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
  {
    if (obj == _impl)
       return;
    if (iface != 0) {
      T* tptr = dynamic_cast<T*>(iface->_getObjectPtr());
      if (tptr == 0)
         ::acdk::lang::ObjectBase::_throwBadCastException();
      _assign2(tptr, obj);
    } else
      _release();
  }
  void _assignInterfaceBase(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    if (obj == _impl)
       return;
    if (iface != 0) {
      T* tptr = dynamic_cast<T*>(iface->_getObjectPtr());
      if (tptr == 0)
         ::acdk::lang::ObjectBase::_throwBadCastException();
      _assign2(tptr, obj, isstackref);
    } else
      _release();
  }
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    _assignObjectBase(iface, obj);
  }
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    _assignObjectBase(iface, obj, isstackref);
  }
  void _assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
  {
     _assignInterfaceBase(iface, obj);
  }
  void _assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    _assignInterfaceBase(iface, obj, isstackref);
  }
public:
  /* default constructors */
  RefHolder(NilRef n = Nil) 
  : _impl(0)
  , _iptr(0)
  , _isStackRef(false)
  { 
  }
  

  /* 
    enables:<br>
    RStringBuffer str = (RStringBuffer)new SomeSing();
  explicit
  RefHolder(::acdk::lang::ObjectBase* o)
  : _impl(0)
  , _iptr(0)
  {
    _init(o, static_cast<Object*>(o));
  }
  */
  
  explicit
  RefHolder(::acdk::lang::InterfaceBase* ibase)
    : _iptr(0)
    , _impl(0)
    , _isStackRef(false)
  {
    if (ibase == 0)
      return;
    ::acdk::lang::Object* optr = ibase->_getObjectPtr();
    _init(optr, optr);
  }

  /* 
    enables:<br>
    RStringBuffer str = new StringBuffer();
  */
  RefHolder(T* o)
  : _impl(0)
  , _iptr(0)
  , _isStackRef(false)
  {
    _init(o, o);
  }

  template <class OT>
  explicit
  RefHolder(const RefHolder<OT>& o)
  : _impl(0)
  , _iptr(0)
  , _isStackRef(false)
  {
    _init(o.iptr(), o.impl(), o.isStackRef());
  }
  
  RefHolder(const RefHolder<T>& o)
  : _impl(o.impl())
  , _iptr(o.iptr())
  , _isStackRef(o.isStackRef())
  {
    if (_isStackRef == false) {
      ACDK_REFCOUNTING_CODE(
      if (_impl != 0) {
         _impl->addRef();
         _isStackRef = o.isStackRef();
      }
      )
    }
  }


  template <class OT>
  explicit
  RefHolder(const InterfaceHolder<OT>& o)
  : _impl(0)
  , _iptr(0)
  , _isStackRef(false)
  {
    _init(o.iptr(), o.impl(), o.isStackRef());
  }
  /*
     enables 
     Integer int;
     RInteger rint = int;
  */
  RefHolder(T& t)
  : _impl(0)
  , _iptr(0)
  , _isStackRef(false)
  {
    _init(&t, &t);
  }
  ~RefHolder() 
  {
    _release();
  }
  
  RefHolder<T>& operator=(T* o)
  {
    _assign(o, o);
    return *this;
  }
  RefHolder<T>& operator=(T& o)
  {
    _assign(&o, &o);
    return *this;
  }

  RefHolder<T>& operator=(const RefHolder<T>& o)
  {
    _assign(o.iptr(), o.impl(), o.isStackRef());
    return *this;
  }
  RefHolder<T>& operator=(NilRef nil)
  {
    _release();
    return *this;
  }
  bool operator==(NilRef nil) { return _impl == 0; }
  bool operator!=(NilRef nil) { return _impl != 0; }
  
  template <class OT> bool operator==(const RefHolder<OT>& other) { return _impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) { return _impl != other.impl(); }
  
  inline T* getIPtr() const 
  {
    ACDK_ASSERT_NULL_PTR(_iptr)
    return _iptr;
  }
  inline T* iptr() const { return _iptr; }

  inline ::acdk::lang::Object* impl() const { return _impl; }
  inline ::acdk::lang::Object* getImpl() 
  {
    ACDK_ASSERT_NULL_PTR(_impl);
    return _impl;
  }
  
  inline T* operator->() 
  { 
    ACDK_ASSERT_NULL_PTR(_iptr);
    return _iptr;
  }
  
  operator T* () const { return _iptr; }
  
#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /* 
   because there is no implizite upcast functionality
   */
  operator ::acdk::lang::RObject () { return RObject(*this); }
#endif
  T& operator *() { return *operator->(); }
  T* operator &() const { return _iptr; }
  
  RefHolder<T>* _ref_this() { return this; }
  
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return T::clazzInfo(); }
  //friend  class InterfaceHolder<T>;
  bool isStackRef() const { return _isStackRef; }
};



/* 
    InterfaceHolder is the base template class to hold references to Interfaces.
    @author Roger Rene Kommer
    @version $Revision: 1.9 $
    @date $Date: 2005/04/09 19:26:56 $
    Normally the user will not handle this directly, but
    will will be declared with:<br>
    ACDK_DECL_INTERFACE(InterfaceName)<br>
    This will define:<br>
    typedef InterfaceHolder<InterfaceName> RClassName;
*/

template <class I>
class InterfaceHolder 
: public RefHolder<I>
{
public:
   typedef I Type;
protected:
  void _init(I* iface, ::acdk::lang::Object* obj)
  {
    _initT(iface, obj);
  }
  void _init(I* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
     _initT(iface, obj, isstackref);
  }
  void _init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj) 
  {
    _initInterfaceBase(iface, obj);
  }
  void _init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj, bool isstackref) 
  {
    _initInterfaceBase(iface, obj, isstackref);
  }
  void _assign(I* iface, ::acdk::lang::Object* obj)
  {
    _assignT(iface, obj);   
  }
  void _assign(I* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    _assignT(iface, obj, isstackref);
  }
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    _assignObjectBase(iface, obj);
  }
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    _assignObjectBase(iface, obj, isstackref);
  }
  void _assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
  {
    _assignInterfaceBase(iface, obj);
  }
  void _assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj, bool isstackref)
  {
    _assignInterfaceBase(iface, obj, isstackref);
  }
  
public:
  InterfaceHolder(NilRef n = Nil)
  : RefHolder<I>(n)
  {
  }
  InterfaceHolder(const InterfaceHolder<I>& iface)
  : RefHolder<I>(Nil)
  {
     _init(iface.iptr(), iface.impl(), iface.isStackRef());
  }
  InterfaceHolder(I* iface)
  : RefHolder<I>(Nil)
  {
     _init(iface, iface == 0 ? 0 : iface->_getObjectPtr());
  }
  InterfaceHolder(I& iface)
  : RefHolder<I>(Nil)
  {
     _init(&iface, iface._getObjectPtr());
  }
  /*
    Dont work, because ambiguous call in case of:
    RIterator iterator()
    {
      return new HashMapIterator(...);
    }
    
  explicit  
  InterfaceHolder(::acdk::lang::ObjectBase* impl)
  : _impl(0)
  , _iptr(0)
  {
    _init(impl, impl);
  }
  */

  /*
   Usage sample:
   RIterator iterator()
    {
      return new HashMapIterator(...);
    }
*/
  explicit
  InterfaceHolder(::acdk::lang::InterfaceBase* ibase)
  : RefHolder<I>(Nil)
  {
    if (ibase == 0)
      return;
    ::acdk::lang::Object* optr = ibase->_getObjectPtr();
    _init(optr, optr);
  }
  template <class OT>
  explicit
  InterfaceHolder(const RefHolder<OT>& other)
  : RefHolder<I>(Nil)
  {
    if (other.iptr() == 0)
       return;
    _init(other.impl(), other.impl(), other.isStackRef());
  }

  InterfaceHolder(I* iface, ::acdk::lang::Object* optr)
  : RefHolder<I>(Nil)
  {
    _init(iface, optr);
  }
 
  ~InterfaceHolder()
  {
  }
  
  InterfaceHolder<I>& operator=(NilRef nil)
  {
    _release();
    return *this;
  }
  InterfaceHolder<I>& operator=(I* iptr)
  {
    if (iptr == 0)
      _release();
    else
      _assign(iptr, iptr->_getObjectPtr());
    return *this;
  }
  InterfaceHolder<I>& operator=(I& iptr)
  {
    _assign(&iptr, iptr._getObjectPtr());
    return *this;
  }
  InterfaceHolder<I>& operator=(const InterfaceHolder<I>& other)
  {
    _assign(other.iptr(), other.impl(), other.isStackRef());
    return *this;
  }

  InterfaceHolder<I>& assign(I* iface, ::acdk::lang::Object* obj)
  {
    _assign(iface, obj);
    return *this;
 
  }

  I* operator->() 
  { 
    ACDK_ASSERT_NULL_PTR(_iptr)
    return _iptr; 
  }

  bool operator==(NilRef nil) { return _impl == 0; }
  bool operator!=(NilRef nil) { return _impl != 0; }
  
  template <class OT> bool operator==(const RefHolder<OT>& other) { return _impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) { return _impl != other.impl(); }

  inline ::acdk::lang::Object* getImpl() const 
  {
    ACDK_ASSERT_NULL_PTR(_iptr)
    return const_cast< ::acdk::lang::Object*>(_impl);
  }
  inline ::acdk::lang::Object* impl() const { return const_cast< ::acdk::lang::Object*>(_impl); }
  I* iptr() const { return _iptr; }
  
  
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return I::clazzInfo(); } 
  
  
  
#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /* 
      because there is no implizite upcast functionality
  */
  operator ::acdk::lang::RObject () { return ::acdk::lang::RObject(*this); }
#endif //ACDK_CORE_NEED_ROBJECT_CONVERTER  

  
  operator I*() { return _iptr; }

  I& operator *() { return *operator->(); }
  
  I* operator &() const { return _iptr; }

  InterfaceHolder<I>* _ref_this() { return this; }
};


/*
  The base of interfaces
*/
#define ACDK_INTERFACEBASE : virtual public ::acdk::lang::InterfaceBase 
   
/* 
    ThrowableHolder is the base template class to hold references to Throwables.
    @author Roger Rene Kommer
    @version $Revision: 1.9 $
    @date $Date: 2005/04/09 19:26:56 $
    Normally the user will not handle this directly, but
    will will be declared with:<br>
    ACDK_DECL_THROWABLE(exception)<br>
    This will define:<br>
    typedef ThrowableHolder<Exception> RException;
*/

template<class T, class S>
class ThrowableHolder
: public S 
{
protected:
public:
  ThrowableHolder(NilRef n = Nil) 
  : S(n)
  { 
  }
  ThrowableHolder(T* o)
  : S(o)
  {
  }

  explicit 
  ThrowableHolder(::acdk::lang::Object* o)
  : S(o)
  {
  }
  ~ThrowableHolder() 
  {
  }
  ThrowableHolder<T, S>& operator=(T* o)
  {
    S::operator=(o);
    return *this;
  }
  ThrowableHolder<T, S>& operator=(const ThrowableHolder<T, S>& o)
  {
    S::operator=(o);
    return *this;
  }
  ThrowableHolder<T, S>& operator=(NilRef nil)
  {
    if (_impl == 0)
      return *this;
    S::operator=(nil);
    return *this;
  }
  bool operator==(NilRef nil) { return _impl == 0; }
  bool operator!=(NilRef nil) { return _impl != 0; }
  
  template <class OT> bool operator==(const RefHolder<OT>& other) { return _impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) { return _impl != other.impl(); }
  

  inline T* getIPtr() const 
  {
    ACDK_ASSERT_NULL_PTR(_iptr)
    T* ret;
    REFH_TRYDOWNCAST_OBJ(ret, T, _iptr);    
    return ret;
  }
  inline T* iptr() const 
  { 
    T* ret;
    REFH_TRYDOWNCAST_OBJ(ret, T, _iptr);    
    return ret;
  }

  inline ::acdk::lang::Object* impl() const { return _impl; }
  inline ::acdk::lang::Object* getImpl() 
  {
    ACDK_ASSERT_NULL_PTR(_iptr)
    return _impl;
  }
  
  inline T* operator->() 
  { 
    ACDK_ASSERT_NULL_PTR(_iptr)
    return iptr();
  }

#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /* 
      because there is no implizite upcast functionality
  */
  operator ::acdk::lang::RObject () { return ::acdk::lang::RObject(*this); }
#endif //ACDK_CORE_NEED_ROBJECT_CONVERTER  

  operator T* () const { return iptr(); }
  T& operator *() { return *operator->(); }
};
