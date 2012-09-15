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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/RefHolder3.h,v 1.43 2005/04/13 12:57:09 kommer Exp $

/*

  RefHolder3
  Different to RefHolder2, the instances are derived.
  @internal
*/

#ifndef ACDK_REFCOUNTING_CODE
#define ACDK_REFCOUNTING_CODE(code) code
#endif 

template <typename T>
inline
T* dmi_cast(::acdk::lang::ObjectBase* objbase);

template <typename T>
inline
void dmi_cast2(::acdk::lang::ObjectBase* from, T*& toI, ::acdk::lang::Object*& toO);

/**
   No implicit convertion from RAClass to RObject 
   possible. For convenience introduce a 
   operator RObject();
*/
//#define ACDK_CORE_NEED_ROBJECT_CONVERTER
#ifdef ACDK_NO_EXPLICIT_OVERLOADING_BUG
#define upcast_explicit 
#else
#define upcast_explicit explicit
#endif

/* 
  Documentation in RefHolderX.h

    RefHolder is the base template class to hold references.
    @author Roger Rene Kommer
    @version $Revision: 1.43 $
    @date $Date: 2005/04/13 12:57:09 $
    Normally the user will not handle this directly, but
    will will be declared with:<br>
    ACDK_DECL_CLASS(ClassName)<br>
    This will define:<br>
    typedef RefHolder<ClassName> RClassName;
    
*/

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
  void* operator new(size_t size, void* memptr) { return memptr; }
#ifdef ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  void operator delete(void* mem, ::acdk::lang::sys::Allocator* allocator) { acdk_deallocate(mem, allocator); }
  void operator delete(void* mem, void* memptr) {  }
#endif //ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  
  typedef T Type;

  void _init(::acdk::lang::Object* obj);

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
    dmi_cast2<T>(iface, _iptr, obj);
    _init(obj);
  }
  void _init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj);
  void _release();
  void _assign(T* iface, ::acdk::lang::Object* obj);
  /*
   precond: iface != 0 &&  obj != impl
  */
  void _assing2(T* iface, ::acdk::lang::Object* obj);
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj);
  void _assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj);

  /* default constructors */
  RefHolder(NilRef n = Nil) 
  : _impl(0)
  , _iptr(0)
  { 
  }
  

  /*
    enables:<br>
    RStringBuffer str = (RStringBuffer)new SomeSing();
  upcast_explicit
  RefHolder(::acdk::lang::ObjectBase* o)
  : _impl(0)
  , _iptr(0)
  {
    _init(o, static_cast<Object*>(o));
  }
  */
  
  explicit
  RefHolder(::acdk::lang::InterfaceBase* ibase);
/*
    : _impl(0)
    , _iptr(0)
  {
    if (ibase == 0)
      return;
    ::acdk::lang::Object* optr = ibase->_getObjectPtr();
    _init(optr, optr);
  }
*/
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
  upcast_explicit
  RefHolder(const RefHolder<OT>& o)
  : _impl(0)
  , _iptr(0)
  {
    _init(o.iptr(), o.impl());
  }
  RefHolder(const RefHolder<T>& o);
/*
  : _impl(o.impl())
  , _iptr(o.iptr())
  {
    ACDK_REFCOUNTING_CODE(
      if (_impl != 0)
      _impl->addRef();
    )
  }
*/

  template <class OT>
  upcast_explicit
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
     too dangerous missing new in RInteger rint = Integer(32);
  */
  /*
  RefHolder(T& t)
  : _impl(0)
  , _iptr(0)
  {
    _init(&t, &t);
  }
  */
  ~RefHolder();
/*
  {
    if(_impl != 0) {
      _impl->releaseRef();
    }
    _impl = 0;
    _iptr = 0;
  }
*/
  /* 
    allows to cast from a scriptvar to a object type
  */
  explicit RefHolder(const ::acdk::lang::dmi::ScriptVar& sv);

  RefHolder<T>& operator=(T* o)
  {
    _assign(o, o);
    return *this;
  }
  /* dangerous:
    //Object o;
    RObject o;
    o = Object();
  */
  /*
  RefHolder<T>& operator=(T& o)
  {
    _assign(&o, &o);
    return *this;
  }
  */
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
  bool operator==(NilRef nil) const { return _impl == 0; }
  bool operator!=(NilRef nil) const { return _impl != 0; }
  
  template <class OT> bool operator==(const RefHolder<OT>& other) const { return _impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) const { return _impl != other.impl(); }
  /*
    @throw NullPointerException if Nil
  */
  inline T* getIPtr() const;
  /*
    @throw nothing
  */
  inline T* iptr() const throw() { return _iptr; }
  /*
    @throw nothing
  */
  inline ::acdk::lang::Object* impl() const throw() { return _impl; }
  /*
    @throw NullPointerException if Nil
  */
  inline ::acdk::lang::Object* getImpl();
  /*
    @throw NullPointerException if Nil
  */
  inline T* operator->() const;
  /*
    @throw nothing
  */
  operator T* () const { return _iptr; }
  
#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /* 
   because there is no implizite upcast functionality
   */
  operator ::acdk::lang::RObject () const { return RObject(*this); }
#endif
  /*
    overloading * act like a pointer
  */
  T& operator *() const { return *operator->(); }
  /*
    overloading & to receive internal pointer
    @see _ref_this
  */
  T* operator &() const { return _iptr; }
  
  /*
    return the pointer to this reference (operator& will not work, because it is overloaded
  */
  RefHolder<T>* _ref_this() { return this; }
  /*
    resets internal pointer to 0 without release them
  */
  void _reset_ptr() { _iptr = 0; _impl = 0; }

  /**
    set the interface pointer the hard way.
    This method doesn't ajust reference count
    @ptr should be the address to a right casted Interface Pointer
  */
  void _setInterfacePtr(void* ptr)
  {
    _iptr = reinterpret_cast<T*>(ptr);
  }
  /*
    return the ClazzInfo for this type
  */
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return T::clazzInfo(); }
  //friend  template <class T> class InterfaceHolder;
};



/* 
    InterfaceHolder is the base template class to hold references to Interfaces.
    @author Roger Rene Kommer
    @version $Revision: 1.43 $
    @date $Date: 2005/04/13 12:57:09 $
    Normally the user will not handle this directly, but
    will will be declared with:<br>
    ACDK_DECL_INTERFACE(InterfaceName)<br>
    This will define:<br>
    typedef InterfaceHolder<InterfaceName> RClassName;
    @ingroup acdksmartptr
*/

template <class I>
class InterfaceHolder 
: public RefHolder<I>
{
public:
   typedef I Type;
protected:

  void _init(::acdk::lang::Object* obj);
  void _init(I* iface, ::acdk::lang::Object* obj);
  void _init(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj);
  void _release();
  void _assign(I* iface, ::acdk::lang::Object* obj);
  /*
   precond: iface != 0 &&  obj != impl
  */
  void _assing2(I* iface, ::acdk::lang::Object* obj);
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj);
  void _assign(::acdk::lang::InterfaceBase* iface, ::acdk::lang::Object* obj);
public:
  InterfaceHolder(NilRef n = Nil)
  : RefHolder<I>(n)
  {
  }
  InterfaceHolder(const InterfaceHolder<I>& iface)
  : RefHolder<I>(Nil)
  {
     _init(iface.iptr(), iface.impl());
  }
  InterfaceHolder(I* iface);
  /*
    too dangerous:
  InterfaceHolder(I& iface)
  : RefHolder<I>(Nil)
  {
     _init(&iface, iface._getObjectPtr());
  }
  */
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
  InterfaceHolder(::acdk::lang::InterfaceBase* ibase);
  template <class OT>
  upcast_explicit
  InterfaceHolder(const RefHolder<OT>& other)
  : RefHolder<I>(Nil)
  {
    if (other.iptr() == 0)
       return;
    _init(other.impl(), other.impl());
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
  InterfaceHolder<I>& operator=(I* iptr);
  /*
    too dangerous
  InterfaceHolder<I>& operator=(I& iptr)
  {
    _assign(&iptr, iptr._getObjectPtr());
    return *this;
  }
  */
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
  /*
    @trow NullPointerException on Nil
  */
  I* operator->() const;
  bool operator==(NilRef nil) const throw();
  bool operator!=(NilRef nil) const throw();
#if defined(ACDK_MEMBERTEMPL_TEMPLATE_TEMPLATE)
  template <class OT> bool operator==(const RefHolder<OT>& other) const throw();
  template <class OT> bool operator!=(const RefHolder<OT>& other) const throw();
#else
  template <class OT> bool operator==(const RefHolder<OT>& other) const throw()
  { return InterfaceHolder<I>::_impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) const throw()
  { return InterfaceHolder<I>::_impl != other.impl(); }
#endif //defined(ACDK_MEMBERTEMPL_TEMPLATE_TEMPLATE)
  /*
    @trow NullPointerException on Nil
  */
  inline ::acdk::lang::Object* getImpl() const;
  /*
    @throw nothing
  */
  inline ::acdk::lang::Object* impl() const throw();
  I* iptr() const throw () { return this->_iptr; }
  
  
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return I::clazzInfo(); } 
  
  
  
#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /* 
      because there is no implizite upcast functionality
  */
  operator ::acdk::lang::RObject () const { return ::acdk::lang::RObject(*this); }
#endif //ACDK_CORE_NEED_ROBJECT_CONVERTER  

  
  operator I*() throw() { return this->_iptr; }

  I& operator *() const { return *operator->(); }
  
  I* operator &() const throw() { return this->_iptr; }

  InterfaceHolder<I>* _ref_this() { return this; }
};


/*
  The base of interfaces
  @ingroup acdkmacros
  @ingroup acdkmetainfo
*/
#define ACDK_INTERFACEBASE : virtual public ::acdk::lang::InterfaceBase 
   
/* 
    ThrowableHolder is the base template class to hold references to Throwables.
    @author Roger Rene Kommer
    @version $Revision: 1.43 $
    @date $Date: 2005/04/13 12:57:09 $
    Normally the user will not handle this directly, but
    will will be declared with:<br>
    ACDK_DECL_THROWABLE(exception)<br>
    This will define:<br>
    typedef ThrowableHolder<Exception> RException;
    @ingroup acdksmartptr
*/

template<class T, class S>
class ThrowableHolder
: public S 
{
protected:
public:
  typedef T Type;

  ThrowableHolder(NilRef n = Nil) 
  : S(n)
  { 
  }
  ThrowableHolder(T* o)
  : S(o)
  {
  }
  template <class OT>
  upcast_explicit
  ThrowableHolder(const RefHolder<OT>& o)
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
    if (this->_impl == 0)
      return *this;
    S::operator=(nil);
    return *this;
  }
  bool operator==(NilRef nil) const throw() { return this->_impl == 0; }
  bool operator!=(NilRef nil) const throw() { return this->_impl != 0; }
  
  template <class OT> bool operator==(const RefHolder<OT>& other) const throw() { return this->_impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) const throw() { return this->_impl != other.impl(); }
  

  inline T* getIPtr() const;
  inline T* iptr() const 
  { 
    T* ret;
    REFH_TRYDOWNCAST_OBJ(ret, T, this->_iptr);    
    return ret;
  }

  inline ::acdk::lang::Object* impl() const { return this->_impl; }
  inline ::acdk::lang::Object* getImpl();
  
  inline T* operator->() const;
  ThrowableHolder<T, S>* _ref_this() { return this; }
#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /* 
      because there is no implizite upcast functionality
  */
  operator ::acdk::lang::RObject () const { return ::acdk::lang::RObject(*this); }
#endif //ACDK_CORE_NEED_ROBJECT_CONVERTER  

  operator T* () const { return iptr(); }
  T& operator *() const { return *operator->(); }

  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return T::clazzInfo(); }
};
