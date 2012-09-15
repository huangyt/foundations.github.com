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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/RefHolder2.h,v 1.12 2005/04/09 19:26:56 kommer Exp $


// RefHolder1.

/* 
    RefHolder is the base template class to hold references.
    @author Roger Rene Kommer
    @version $Revision: 1.12 $
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
public:
  void* operator new(size_t size) { return acdk_allocate(size); }
  void operator delete(void* ptr) { acdk_deallocate(ptr); }
  void* operator new[](size_t size){ return acdk_allocate(size); }
  void operator delete[](void* ptr){ acdk_deallocate(ptr); }
  void* operator new(size_t size, ::acdk::lang::sys::Allocator* allocator) { return acdk_allocate(size, allocator); }
#ifdef ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  void operator delete(void* mem, ::acdk::lang::sys::Allocator* allocator) { acdk_deallocate(mem, allocator); }
#endif //ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  
  typedef T Type;

  void _init(::acdk::lang::Object* obj)
  {
    _impl = obj;
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
  }
  void _init(T* iface, ::acdk::lang::Object* obj)
  {
    if (iface == 0)
      return;
    _iptr = iface;
    _init(obj);
    
  }
  void _init(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (obj == 0)
      return;
    _iptr = dynamic_cast<T*>(iface);
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _init(obj);
  }
  void _init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
  {
    if (obj == 0)
      return;
    _iptr = dynamic_cast<T*>(iface->_getObjectPtr());
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _init(obj);
  }
  void _release()
  {
    if (_impl == 0)
      return;
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
    _impl = 0;
    _iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
  }
  void _assign(T* iface, ::acdk::lang::Object* obj)
  {
    if (_impl == obj)
      return;
    if (obj != 0) {
      ACDK_REFCOUNTING_CODE( obj->addRef(); )
    }
    if (_impl != 0) {
      ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    }
    _impl = obj;
    _iptr = iface;

  }
  /*
   precond: iface != 0 &&  obj != impl
  */
  void _assing2(T* iface, ::acdk::lang::Object* obj)
  {
    ACDK_REFCOUNTING_CODE( obj->addRef(); )
    if (_impl != 0) {
      ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    } 
    _impl = obj;
    _iptr = iface;
  }
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (obj == _impl)
       return;
    if (obj != 0) {
      T* tptr = dynamic_cast<T*>(iface);
      if (tptr == 0)
         ::acdk::lang::ObjectBase::_throwBadCastException();
      _assing2(tptr, obj);
    } else
      _release();
  }
  void _assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
  {
    
    if (obj == _impl)
       return;
    if (iface != 0) {
      T* tptr = dynamic_cast<T*>(iface->_getObjectPtr());
      if (tptr == 0)
         ::acdk::lang::ObjectBase::_throwBadCastException();
      _assing2(tptr, obj);
    } else
      _release();

  }


  /* default constructors */
  RefHolder(NilRef n = Nil) 
  : _impl(0)
  , _iptr(0)
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
  {
    _init(o, o);
  }

  template <class OT>
  explicit
  RefHolder(const RefHolder<OT>& o)
  : _impl(0)
  , _iptr(0)
  {
    _init(o.iptr(), o.impl());
  }
  
  RefHolder(const RefHolder<T>& o)
  : _impl(o.impl())
  , _iptr(o.iptr())
  {
    ACDK_REFCOUNTING_CODE(
      if (_impl != 0)
      _impl->addRef();
    )
  }


  template <class OT>
  explicit
  RefHolder(const InterfaceHolder<OT>& o)
  : _impl(0)
  , _iptr(0)
  {
    _init(o.iptr(), o.impl());
  }
  /*
     enables 
     Integer int;
     RInteger rint = int;
  */
  RefHolder(T& t)
  : _impl(0)
  , _iptr(0)
  {
    _init(&t, &t);
  }
  ~RefHolder() 
  {
    if(_impl != 0) {
      _impl->releaseRef();
      _impl = 0;
      _iptr = 0;
    }
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
    _assign(o.iptr(), o.impl());
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
  
  inline const T* operator->() 
  { 
    ACDK_ASSERT_NULL_PTR(_iptr);
    return const_cast<T*>(_iptr);
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
};



/* 
    InterfaceHolder is the base template class to hold references to Interfaces.
    @author Roger Rene Kommer
    @version $Revision: 1.12 $
    @date $Date: 2005/04/09 19:26:56 $
    Normally the user will not handle this directly, but
    will will be declared with:<br>
    ACDK_DECL_INTERFACE(InterfaceName)<br>
    This will define:<br>
    typedef InterfaceHolder<InterfaceName> RClassName;
*/

template <class I>
class InterfaceHolder 
{
  ::acdk::lang::Object* _impl;
  I* _iptr;
#ifdef ACDK_USE_EXT_REFERER
public:
  ::acdk::lang::sys::Referer* _referer;
#endif
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
#endif // ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  typedef I Type;


  void _init(::acdk::lang::Object* obj)
  {
     _impl = obj;
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
  }
  void _init(I* iface, ::acdk::lang::Object* obj)
  {
    if (iface == 0)
      return;
    _iptr = iface;
   _init(obj);
  }
  void _init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj) 
  {
    if (iface == 0)
      return;
    _iptr = dynamic_cast<I*>(obj);
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _init(obj);
  }
  /*
  void _init(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (iface == 0)
      return;
    
    _iptr = dynamic_cast<I*>(iface);
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _init(obj);
  }*/
  void _release()
  {
    if (_impl == 0)
      return;
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
    _impl = 0;
    _iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )

  }
  void _assign(I* iface, ::acdk::lang::Object* obj)
  {
    if (_impl == obj)
      return;
    // first increment, in case of cyclic references
    if (obj != 0) { 
      ACDK_REFCOUNTING_CODE( obj->addRef(); )
    }
    if (_impl != 0) {
      ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    }
    _impl = obj;
    _iptr = iface;

  }
  /*
   precond: iface != 0 &&  obj != impl
  */
  void _assing2(I* iface, ::acdk::lang::Object* obj)
  {
    ACDK_REFCOUNTING_CODE( obj->addRef(); )
    if (_impl != 0) {
      ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
      _impl = 0;
      _iptr = 0;
      ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
    } 
    _impl = obj;
    _iptr = iface;
  }
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (obj == _impl)
       return;
    if (obj != 0) {
      I* tptr = dynamic_cast<I*>(iface);
      if (tptr == 0)
         ::acdk::lang::ObjectBase::_throwBadCastException();
      _assing2(tptr, obj);
    } else
      _release();
  }
  void _assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj)
  {
    
    if (obj == _impl)
       return;
    if (iface != 0) {
      I* tptr = dynamic_cast<I*>(iface->_getObjectPtr());
      if (tptr == 0)
         ::acdk::lang::ObjectBase::_throwBadCastException();
      _assing2(tptr, obj);
    } else
      _release();

  }

  InterfaceHolder(NilRef n = Nil)
  : _impl(0)
  , _iptr(0)
  {
  }
  InterfaceHolder(const InterfaceHolder<I>& iface)
  : _impl(0)
  , _iptr(0)
  {
     _init(iface.iptr(), iface.impl());
  }
  InterfaceHolder(I* iface)
  : _impl(0)
  , _iptr(0)
  {
     _init(iface, iface == 0 ? 0 : iface->_getObjectPtr());
  }
  InterfaceHolder(I& iface)
  : _impl(0)
  , _iptr(0)
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
  : _impl(0)
  , _iptr(0)
  {
    if (ibase == 0)
      return;
    ::acdk::lang::Object* optr = ibase->_getObjectPtr();
    _init(optr, optr);
  }
  template <class OT>
  explicit
  InterfaceHolder(const RefHolder<OT>& other)
  : _impl(0)
  , _iptr(0)
  {
    if (other.iptr() == 0)
       return;
    _init(other.impl(), other.impl());
  }

  InterfaceHolder(I* iface, ::acdk::lang::Object* optr)
  : _impl(optr)
  , _iptr(iface)
  {
    _init(iface, optr);
  }
 
  ~InterfaceHolder()
  {
    _release();
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
    _assign(other.iptr(), other.impl());
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
    @version $Revision: 1.12 $
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
