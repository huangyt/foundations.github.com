// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 

#ifndef ORef1_h
#define ORef1_h
#if !defined(DOXYGENONLY)

#include <iostream>

class Object;
class ObjectBase;
class InterfaceBase;

//template <class T> class RefCast;
template <class T> class ORef;


template <class I> class IRef;

template <class T>
class ORef
{
public:
  typedef T Type;
  //typedef RefCast<T> Cast;
  T* _iptr;
  Object* _impl;
public:
  ORef() 
  : _iptr(0)
  , _impl(0)
  {
  }
  void init(T* iface, Object* obj)
  {
    _iptr = iface;
    _impl = obj;
  }
  void assign(T* iface, Object* obj)
  {
  }
  // a version for downcasts
  ORef(T* iptr)
  {
    _iptr = iptr;
    _impl = iptr;
  }
  ORef(T& o)
  {
    _iptr = &o;
    _impl &o; 
  }
  explicit
  ORef(ObjectBase* iptr)
  {
    _impl = dynamic_cast<Object*>(iptr);
    _iptr = dynamic_cast<T*>(iptr);

  }
  /**
     Note on gcc 2.95.2: explicit will be ignored
   */
  template <class I>
  explicit
  ORef(const IRef<I>& iptr)
  {
    _iptr = dynamic_cast<T*>(iptr._iptr);
    _impl = iptr._impl;
  }
  ORef<T>& operator=(T* t)
  {
    _iptr = t;
    _impl = t;
    return *this;
  }
  T* operator&() { return _iptr; }
  operator T* (){ return _iptr; }
  T& operator*() { return *_iptr; }
  T* operator->() { return _iptr; }
  ORef<T>* _ref_this() { return this; }
};

template <class I>
class IRef
{
public:
  I* _iptr;
  Object* _impl;

  
  IRef(I* iref)
    : _iptr(iref)
    , _impl(0)
  {
    if (iref == 0)
      return;
    _impl = iref->getObject();
    
  }
  IRef(I& iref)
    : _iptr(&iref)
    , _impl(iref.getObject())
  {
  }
    
  explicit
  IRef(ObjectBase* obj)
    : _iptr(0)
    , _impl(0)
  {
    if (obj == 0)
      return;
    _iptr = dynamic_cast<I*>(obj);
    if (_iptr == 0)
      throw "Oops";
    _impl = (Object*)obj;
  }
  explicit
  IRef(InterfaceBase* obj)
    : _iptr(0)
    , _impl(0)
  {
    if (obj == 0)
      return;
    _iptr = dynamic_cast<I*>(obj);
    if (_iptr == 0)
      throw "Oops";
    _impl = obj->getObject();
  }

  IRef<I>& operator=(const IRef<I>& o)
  {
    _iptr = o._iptr;
    _impl = o._impl;
    return *this;
  }
  IRef<I>& operator=(I* o)
  {
    _iptr = o;
    if (o != 0)
      _impl = o->getObject();
    else
      _impl = 0;
    return *this;
  }
  
  operator I* ()  {    return _iptr;  }
  I* operator&() {  return _iptr;  }
  I& operator*()  {    return *_iptr;  }
  I* operator -> () { return _iptr; }
  I* i() { return _iptr; }
  IRef<I>* _ref_this() { return this; }
};



#endif //!defined(DOXYGENONLY)
#endif // ORef1_h
