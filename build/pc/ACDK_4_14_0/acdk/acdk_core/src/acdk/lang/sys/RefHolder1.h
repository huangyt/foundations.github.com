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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/RefHolder1.h,v 1.11 2005/04/09 19:26:56 kommer Exp $


// RefHolder1.

/*
    RefHolder is the base template class to hold references.
    @author Roger Rene Kommer
    @version $Revision: 1.11 $
    @date $Date: 2005/04/09 19:26:56 $
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
#ifdef ACDK_USE_EXT_REFERER
public:
  ::acdk::lang::sys::Referer* _referer;
#endif //ACDK_USE_EXT_REFERER

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
  /* default constructors */
  RefHolder(NilRef n = Nil) 
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif //ACDK_USE_EXT_REFERER

  { 
  }
  void _init(::acdk::lang::Object* obj)
  {
     _impl = obj;
#ifdef ACDK_USE_EXT_REFERER
    if (_impl->_referer == 0) {
      _referer = new ::acdk::lang::sys::Referer(1);
      _impl->_referer = _referer;
    } else {
      _referer = _impl->_referer;
      _referer->addRef();
    }
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
#endif
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
  void _release()
  {
    if (_impl == 0)
      return;
#ifdef ACDK_USE_EXT_REFERER
    if (_referer->releaseRef() == 0) 
    {
      ::acdk::lang::Object* relobj = _impl;
      _impl = 0;
      _iptr = 0;
      relobj->dispose();
    } else {
      _impl = 0;
      _iptr = 0;
    }
#else
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
    _impl = 0;
    _iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
#endif
  }
  void _assign(T* iface, ::acdk::lang::Object* obj)
  {
    if (_impl == obj)
      return;
    _release();
    _init(iface, obj);

  }
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (_impl == obj)
      return;
    _release();
    _init(iface, obj);
  }

/* use overloading to ObjectBase instead
  template <class OT> 
  //no explicit 
  RefHolder(OT* o)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif //ACDK_USE_EXT_REFERER

  {
    if (o == 0)
      return;
    _iptr = o;
    _impl = o;
#ifdef ACDK_USE_EXT_REFERER
    if (_impl->_referer == 0) {
      _referer = new ::acdk::lang::sys::Referer(1);
      _impl->_referer = _referer;
    } else {
      _referer = _impl->_referer;
      _referer->addRef();
    }
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
#endif
  
  }
*/

  /* 
    enables:<br>
    RStringBuffer str = (RStringBuffer)new SomeSing();
  */
  explicit
  RefHolder(::acdk::lang::ObjectBase* o)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif //ACDK_USE_EXT_REFERER
  {
    _init(o, o);
  }

  /* 
    enables:<br>
    RStringBuffer str = new StringBuffer();
  */
  RefHolder(T* o)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif //ACDK_USE_EXT_REFERER
  {
    _init(o, o);
  }


  template <class OT> 
  explicit 
  RefHolder(const InterfaceHolder<OT>& o)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif //ACDK_USE_EXT_REFERER
  
  {
    if (o.impl() == 0)
      return;
    _init(o.impl(), o.impl());
    /*
    REFH_TRYDOWNCAST_OBJ(_iptr, T, o.impl());
    _impl = _iptr;
    if (_impl == 0) 
      ::acdk::lang::ObjectBase::_throwBadCastException();
#ifdef ACDK_USE_EXT_REFERER
    _referer = o._referer;
    _referer->addRef();
#else
    ACDK_REFCOUNTING_CODE(_impl->addRef();)
#endif
    */
  }



  template <class OT> 
  // no explicit otherwise RObject o; o = str; doesn work
   //explicit 
  RefHolder(const RefHolder<OT>& o)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif
  {
    _init(o.iptr(), o.impl());
/*
    if (o.impl() == 0)
      return;

    REFH_TRYDOWNCAST_OBJ(_iptr, T, o.iptr());
    _impl = _iptr;
    if (_impl == 0) 
      ::acdk::lang::ObjectBase::_throwBadCastException();
#ifdef ACDK_USE_EXT_REFERER
    _referer = o._referer;
    _referer->addRef();
#else
    ACDK_REFCOUNTING_CODE(_impl->addRef(); )
#endif
    //_iptr = t;
    */
  }
  RefHolder(const RefHolder<T>& o)
  : _impl(o.impl())
  , _iptr(o.iptr())
#ifdef ACDK_USE_EXT_REFERER
  , _referer(o._referer)
#endif
  {
#ifdef ACDK_USE_EXT_REFERER
    if (_impl != 0)
      _referer->addRef();
#else
    ACDK_REFCOUNTING_CODE(
      if (_impl != 0)
      _impl->addRef();
    )
#endif
  }

  ~RefHolder() 
  {
#ifdef ACDK_USE_EXT_REFERER
    releaseRef();
#else 
    if(_impl != 0) {
      releaseRef();
    }
#endif
  }
  RefHolder<T>& operator=(T* o)
  {
    _assign(o, o);
    return *this;
    /*
    if (o == _iptr)
      return *this;
#ifdef ACDK_USE_EXT_REFERER
    releaseRef();
    _impl = o;
    _iptr = o;
    if (_iptr != 0) {
      if (_impl->_referer == 0) {
        _referer = new ::acdk::lang::sys::Referer(1);
        _impl->_referer = _referer;
      } else {
        _referer = _impl->_referer;
        _referer->addRef();
      }
    }
#else
    if (_impl != 0)
      releaseRef();
    _impl = o;
    _iptr = o;
 ACDK_REFCOUNTING_CODE(
    if (_impl != 0)
      _impl->addRef();
  )
#endif
    return *this;
    */
  }
  template <class OT> 
  RefHolder<T>& operator=(const RefHolder<OT>& o)
  {
    _assign(o.iptr(), o.impl());
    return *this;
  /*
    if (_impl == 0 && o.impl() == 0)
      return *this;
#ifdef ACDK_USE_EXT_REFERER
    releaseRef();
    if (o.iptr() == 0)
      return *this;
    REFH_TRYDOWNCAST_OBJ(_iptr, T, o.iptr());
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _impl = o.impl();
    _referer = o._referer;
    _referer->addRef();
    return *this;
#else
    // if compiler errorcompatible cast, use explicit cast construction 
    //    type = (RClassType)o;
    //
    return operator=((OT*)o);
#endif
    */
  }
  RefHolder<T>& operator=(const RefHolder<T>& o)
  {
    _assign(o.iptr(), o.impl());
    return *this;
    /*
    if (_impl == 0 && o.impl() == 0)
      return *this;
#ifdef ACDK_USE_EXT_REFERER
    if (o.impl() == impl())
      return *this;
    releaseRef();
    if (o.iptr() == 0)
      return *this;
    _iptr = o.iptr();
    _impl = o.impl();
    _referer = o._referer;
    _referer->addRef();
    return *this;
#else
    return operator=((T*)o);
#endif
    */
  }
  RefHolder<T>& operator=(NilRef nil)
  {
    _release();
    return *this;
    /*
#ifdef ACDK_USE_EXT_REFERER
    releaseRef();
    return *this;
#else
    if (_impl == 0)
      return *this;
    releaseRef();
    return *this;
#endif
    */
  }
  void releaseRef()
  {
#ifdef ACDK_USE_EXT_REFERER
    if (_impl == 0)
      return;
    if (_referer->releaseRef() == 0) {
      _impl->dispose();
      
    }
    _impl = 0;
    _iptr = 0;
#else
    ACDK_REFCOUNTING_CODE(::acdk::lang::Object* simpl = _impl;)
    _impl = 0;
    _iptr = 0;
    ACDK_REFCOUNTING_CODE(simpl->releaseRef();)
#endif
  }
  bool operator==(NilRef nil) { return _impl == 0; }
  bool operator!=(NilRef nil) { return _impl != 0; }
  
  template <class OT> bool operator==(const RefHolder<OT>& other) { return _impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) { return _impl != other.impl(); }
  

  inline T* getIPtr() const 
  {
#ifndef ACDK_NO_NULLPOINTER_CHECKING 
    if (_impl == 0) 
      ::acdk::lang::ObjectBase::_throwNullPointerException();
#endif
    return _iptr;
  }
  inline T* iptr() const { return _iptr; }

  inline ::acdk::lang::Object* impl() const { return _impl; }
  inline ::acdk::lang::Object* getImpl() 
  {
#ifndef ACDK_NO_NULLPOINTER_CHECKING 
    if (_impl == 0) 
      ::acdk::lang::ObjectBase::_throwNullPointerException();
#endif
    return _impl;
  }
  
  inline T* operator->() 
  { 
#ifndef ACDK_NO_NULLPOINTER_CHECKING 
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwNullPointerException();
#endif
    return _iptr;
  }
  
  operator T* () const { return _iptr; }

  T& operator *() { return *operator->(); }
  T* operator &() const { return _iptr; }
  
  RefHolder<T>* _ref_this() { return this; }
  
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return T::clazzInfo(); }

  /* 
    conceptional idea, will unfortunatelly not work with vc:
    RInteger integer = new Integer(42);
    // error C2275: "RComparable" : Ungueltige Verwendung dieses Typs als  Ausdruck
    RComparable comp = integer.castTo<RComparable>();
      
  
  template <class IH> 
  IH castTo() 
  { 
    if (_iptr == 0)
      return IH(0, 0);
    return IH(static_cast<I::Type*>(_iptr), _impl);
  }
  */
};



/* 
    InterfaceHolder is the base template class to hold references to Interfaces.
    @author Roger Rene Kommer
    @version $Revision: 1.11 $
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
#ifdef ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  void operator delete(void* mem, ::acdk::lang::sys::Allocator* allocator) { acdk_deallocate(mem, allocator); }
#endif // ACDK_HAS_USER_DEFINDED_OPERATOR_DELETE
  typedef I Type;


  void _init(::acdk::lang::Object* obj)
  {
     _impl = obj;
#ifdef ACDK_USE_EXT_REFERER
    if (_impl->_referer == 0) {
      _referer = new ::acdk::lang::sys::Referer(1);
      _impl->_referer = _referer;
    } else {
      _referer = _impl->_referer;
      _referer->addRef();
    }
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
#endif
  }
  void _init(I* iface, ::acdk::lang::Object* obj)
  {
    if (iface == 0)
      return;
    _iptr = iface;
   _init(obj);
    
  }
  void _init(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (iface == 0)
      return;
    
    _iptr = dynamic_cast<I*>(iface);
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _init(obj);
  }
  void _release()
  {
    if (_impl == 0)
      return;
#ifdef ACDK_USE_EXT_REFERER
    if (_referer->releaseRef() == 0) 
    {
      ::acdk::lang::Object* relobj = _impl;
      _impl = 0;
      _iptr = 0;
      relobj->dispose();
    } else {
      _impl = 0;
      _iptr = 0;
    }
#else
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
    _impl = 0;
    _iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
#endif
  }
  void _assign(I* iface, ::acdk::lang::Object* obj)
  {
    if (_impl == obj)
      return;
    _release();
    _init(iface, obj);

  }
  void _assign(::acdk::lang::ObjectBase* iface, ::acdk::lang::Object* obj)
  {
    if (_impl == obj)
      return;
    _release();
    _init(iface, obj);
  }


  InterfaceHolder(NilRef n = Nil)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif
  {
  }


  template <class OT>
  //explicit 
  InterfaceHolder(const InterfaceHolder<OT>& iface)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif
  {
    _init(iface.impl(), iface.impl());
    /*
    if (iface.impl() == 0)
      return;
    
    _impl = iface.impl();
    _iptr = dynamic_cast<I*>(iface.iptr());
    
    if (_impl == 0 || _iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
#ifdef ACDK_USE_EXT_REFERER
    _referer = iface._referer;
    _referer->addRef();
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef();  )
#endif
    */
  }
  
  InterfaceHolder(const InterfaceHolder<I>& iface)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(iface._referer)
#endif
  {
    _init(iface.iptr(), iface.impl());
    /*
    if (iface.impl() == 0)
      return;
    _impl = iface.impl();
    _iptr = iface.iptr();
#ifdef ACDK_USE_EXT_REFERER
    _referer->addRef();
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
#endif
    */
  }

  /// casting from ROtherClass to RInterface
  template <class OT>
  //explicit 
  InterfaceHolder(const RefHolder<OT>& iface)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif
  {
    _init(iface.impl(), iface.impl());
    /*
    if (iface.impl() == 0)
      return;
    
    // doesn't work: I* thempiface = (OT*)iface,
    //  otherwise
    //    RComparable c; RObject o = new Integer(42); 
    //    c = (RComparable)o; // here
    //  will not work
    
    
    _impl = iface.impl();
    _iptr = dynamic_cast<I*>(iface.iptr());
    

    if (_impl == 0 || _iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
#ifdef ACDK_USE_EXT_REFERER
    _referer = iface._referer;
    _referer->addRef();
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
#endif
    */
  }
  InterfaceHolder(::acdk::lang::Object* obj)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif
  {
    _init(obj, obj);
  }
/*
  template <class OT>
  //explicit 
  InterfaceHolder(OT* iface)
  : _impl(0)
  , _iptr(0)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif
  {
    if (iface == 0)
      return;
    I* tempvar = iface; // ## disable warnings here

    _impl = iface;
    _iptr = dynamic_cast<I*>(iface);

    if (_impl == 0 || _iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
#ifdef ACDK_USE_EXT_REFERER
    if (_impl->_referer == 0) {
      _referer = new ::acdk::lang::sys::Referer(1);
       _impl->_referer = _referer;
    } else {
      _referer = _impl->_referer;
      _referer->addRef();
    }
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
#endif
   
  }
  */
  InterfaceHolder(I* iface, ::acdk::lang::Object* optr)
  : _impl(optr)
  , _iptr(iface)
#ifdef ACDK_USE_EXT_REFERER
  , _referer(0)
#endif
  {
    _init(iface, optr);
    /*
#ifdef ACDK_USE_EXT_REFERER
    if (_iptr != 0) {
      if (_impl->_referer == 0) {
        _referer = new ::acdk::lang::sys::Referer(1);
        _impl->_referer = _referer;
      } else {
        _referer = _impl->_referer;
        _referer->addRef();
      }
    }
#else
  ACDK_REFCOUNTING_CODE( 
    if (_impl != 0)
      _impl->addRef();
  )
#endif
    */
  }
 
  ~InterfaceHolder()
  {
    _release();
/*
#ifdef ACDK_USE_EXT_REFERER
    releaseRef();
#else
#ifndef ACDK_NO_REFCOUNTING
    if (_impl != 0) {
      releaseRef();
    }
#endif 
#endif
    */
  }
  InterfaceHolder<I>& operator=(NilRef nil)
  {
    _release();
    return *this;
/*
#ifdef ACDK_USE_EXT_REFERER
    releaseRef();
#else
#ifndef ACDK_NO_REFCOUNTING
    if (_impl != 0)
      releaseRef();
#endif
#endif
    return *this;
*/
  }
  template <class OT>
  InterfaceHolder<I>& operator=(const RefHolder<OT>& iface)
  {
    _assign(iface.iptr(), iface.impl());
    return *this;
    /*
    if (_impl == 0 && iface.impl() == 0)
      return *this;
#ifdef ACDK_USE_EXT_REFERER
    if (iface.iptr() == 0) {
      releaseRef();
      return *this;
    }
    
    REFH_TRYDOWNCAST_OBJ(_iptr, I, iface.iptr());
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    _impl = iface.impl();
    if (_impl->_referer == 0) {
      _referer = new ::acdk::lang::sys::Referer(1);
      _impl->_referer = _referer;
    } else {
      _referer = _impl->_referer;
      _referer->addRef();
    }
    
    return *this;
#else
    // if compiler errorcompatible cast, use explicit cast construction 
    //    type = (RClassType)o;
    //
    //I* tempvar = (OT*)iface;
    return operator=((OT*)iface);
#endif
    */
  }
  /*
  InterfaceHolder<I>& operator=(I* iface, ::acdk::lang::Object* obj)
  {
    _assign(iface, obj);
    return *this;
  }
  */
  /*
    Usage:
    RWriter writer;
    writer = new BinaryWriter();

    bad: because and no typechecking
         RWriter writer;
         writer = new Integer(); // should produce a compile error
  */
  InterfaceHolder<I>& operator=(::acdk::lang::Object* iface)
  {
    _assign(iface, iface);
    return *this;
  }
  
  

  /*
    template <class OT>
  InterfaceHolder<I>& operator=(OT* iface)
  {
    _assign(iface, iface);
    return *this;
    // if compiler errorcompatible cast, use explicit cast construction 
    //    type = (RClassType)o;
    
    
    if ((::acdk::lang::Object*)iface == impl())
      return *this;
ACDK_REFCOUNTING_CODE(
    if (_impl != 0)
      releaseRef();
)
    
     REFH_TRYDOWNCAST_OBJ(_impl, ::acdk::lang::Object, iface);
    //old: _impl = dynamic_cast< ::acdk::lang::Object*>(iface);

    //old: I* tempvar = (OT*)iface;
    
    REFH_TRYDOWNCAST_OBJ(_iptr, I, iface);
    
    if (_impl == 0 || _iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
#ifdef ACDK_USE_EXT_REFERER
    if (_impl->_referer == 0) {
      _referer = new ::acdk::lang::sys::Referer(1);
      _impl->_referer = _referer;
    } else {
      _referer = _impl->_referer;
      _referer->addRef();
    }
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
#endif
    return *this;

  }
    */  
  template <class OT> 
  InterfaceHolder<I>& operator=(const InterfaceHolder<OT>& other)
  {
    _assign(other.iptr(), other.impl());
    return *this;
    /*
    if (((::acdk::lang::Object*)other.impl()) == ((::acdk::lang::Object*)impl()) || other.iptr() == iptr())
      return *this;
#ifdef ACDK_USE_EXT_REFERER
    releaseRef();
    if (o.iptr() == 0)
      return;
    REFH_TRYDOWNCAST_OBJ(_iptr, I, o.iptr());
    //_iptr = dynamic_cast<I*>(o.iptr());
    REFH_TRYDOWNCAST_OBJ(_impl, ::acdk::lang::Object, o.iptr());
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwBadCastException();
    
    _referer = other._referer;
    _referer->addRef();
    return *this;
#else
    return operator=((OT*)other);
#endif
    */
  }
  InterfaceHolder<I>& operator=(const InterfaceHolder<I>& other)
  {
    _assign(other.iptr(), other.impl());
    return *this;
 /*
    if (other.impl() == impl())
      return *this;
    if (_impl != 0)
      releaseRef();
    if (other.impl() == 0)
      return *this;
    _impl = other.impl();
    _iptr = other.iptr();
#ifdef ACDK_USE_EXT_REFERER
    _referer = other._referer;
    _referer->addRef();
#else
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
#endif
    return *this;
*/
  }

  InterfaceHolder<I>& assign(I* iface, ::acdk::lang::Object* obj)
  {
    _assign(iface, obj);
    return *this;
  
 /*
    if (obj == impl())
      return *this;
    if (_impl != 0)
      releaseRef();
    if (obj == 0)
      return *this;
    _impl = obj;
    _iptr = iface;
    ACDK_REFCOUNTING_CODE( _impl->addRef(); )
    return *this;
*/
  }

  I* operator->() 
  { 
#ifndef ACDK_NO_NULLPOINTER_CHECKING 
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwNullPointerException();
#endif //ACDK_NO_NULLPOINTER_CHECKING 
    return _iptr; 
  }

  bool operator==(NilRef nil) { return _impl == 0; }
  bool operator!=(NilRef nil) { return _impl != 0; }
  
  template <class OT> bool operator==(const RefHolder<OT>& other) { return _impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) { return _impl != other.impl(); }

  inline ::acdk::lang::Object* getImpl() const 
  {
#ifndef ACDK_NO_NULLPOINTER_CHECKING 
    if (_impl == 0) 
      ::acdk::lang::ObjectBase::_throwNullPointerException();
#endif
    return const_cast< ::acdk::lang::Object*>(_impl);
  }
  inline ::acdk::lang::Object* impl() const { return const_cast< ::acdk::lang::Object*>(_impl); }
  I* iptr() const { return _iptr; }
  operator I*() { return _iptr; }
  operator ::acdk::lang::Object*() { return _impl; }
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return I::clazzInfo(); } 
  void releaseRef()
  {
#ifdef ACDK_USE_EXT_REFERER
    if (_impl != 0) {
      //_referer->releaseRef();
      if (_referer->releaseRef() == 0) {
        _impl->dispose();
      }
      _impl = 0;
      _iptr = 0;
    }
#else
    ACDK_REFCOUNTING_CODE( ::acdk::lang::Object* simpl = _impl; )
    _impl = 0;
    _iptr = 0;
    ACDK_REFCOUNTING_CODE( simpl->releaseRef(); )
#endif
  }
  operator ::acdk::lang::RObject() { return _impl; }

  I& operator *() { return *operator->(); }
  
  I* operator &() const { return _iptr; }
  InterfaceHolder<I>* _ref_this() { return this; }
};


/*
  The base of interfaces
*/
#define ACDK_INTERFACEBASE

/* 
    ThrowableHolder is the base template class to hold references to Throwables.
    @author Roger Rene Kommer
    @version $Revision: 1.11 $
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
  template <class OT> 
  explicit 
  ThrowableHolder(const InterfaceHolder<OT>& o)
  : S(o)
  {
  }
  template <class OT> 
  explicit 
  ThrowableHolder(const RefHolder<OT>& o)
  : S(o)
  {
  }
  /*
  ThrowableHolder(const RefHolder<T>& o)
  : S(o)
  {
  }
  */
  ~ThrowableHolder() 
  {
  }
  ThrowableHolder<T, S>& operator=(T* o)
  {
    S::operator=(o);
    return *this;
  }
  template <class OT> 
  ThrowableHolder<T, S>& operator=(const RefHolder<OT>& o)
  {
#ifdef ACDK_USE_EXT_REFERER
    S::operator=(o);
    return *this;
#else
    if (_impl == 0 && o.impl() == 0)
      return *this;
    /* if compiler errorcompatible cast, use explicit cast construction 
        type = (RClassType)o;
    */
    return operator=((OT*)o);
#endif
  }
  ThrowableHolder<T, S>& operator=(const ThrowableHolder<T, S>& o)
  {
#ifdef ACDK_USE_EXT_REFERER
    S::operator=(o);
    return *this;
#else

    if (_impl == 0 && o.impl() == 0)
      return *this;
    return operator=((T*)o);
#endif
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
#ifndef ACDK_NO_NULLPOINTER_CHECKING 
    if (_impl == 0) 
      ::acdk::lang::ObjectBase::_throwNullPointerException();
#endif
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
#ifndef ACDK_NO_NULLPOINTER_CHECKING 
    if (_impl == 0) 
      ::acdk::lang::ObjectBase::_throwNullPointerException();
#endif
    return _impl;
  }
  
  inline T* operator->() 
  { 
#ifndef ACDK_NO_NULLPOINTER_CHECKING 
    if (_iptr == 0)
      ::acdk::lang::ObjectBase::_throwNullPointerException();
#endif
    return iptr();
  }
  
  operator T* () const { return iptr(); }
  T& operator *() { return *operator->(); }
};
