#ifndef ACDK_GENDOC
// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/tests/acdk/lang/acdk_lang_Object_Test2.cpp,v 1.6 2005/03/07 17:52:08 kommer Exp $
//
// $Log: acdk_lang_Object_Test2.cpp,v $
// Revision 1.6  2005/03/07 17:52:08  kommer
// panta rei
//
// Revision 1.5  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.4  2002/11/26 10:59:08  kommer
// panta rei
//
// Revision 1.3  2001/12/28 21:05:52  kommer
// panta rei
//
// Revision 1.2  2001/05/18 09:01:02  kommer
// panta rei
//
// Revision 1.1  2001/05/05 18:12:07  kommer
// initial revision
//
// Revision 1.1  2001/04/30 11:43:02  kommer
// initial revision
//

#if 0 // this is only scribble tests
#include <iostream>

namespace {
#define implements virtual public
#define extends public

class Object;



class InterfaceBase
{
public:
  virtual Object* getObject() = 0;
};

class ObjectBase
: implements InterfaceBase
{
protected:
  ObjectBase() {}
public:
  virtual ~ObjectBase() { }
  
};

class Object
: extends ObjectBase
{
public:
  virtual ~Object() { }
  Object* getObject() { return this; }
};

//template <class T> class RefCast;
template <class T> class ORef;

/*
template <class T>
class RefCast
{
public:
  Object* _impl;
  T* _iptr;

  RefCast(T* objptr)
  {
    _impl = objptr;
    _iptr = objptr;
  }
  explicit
  RefCast(ObjectBase* objptr)
  {
    _impl = dynamic_cast<Object*>(objptr);
    _iptr = dynamic_cast<T*>(objptr);
  }
  RefCast(void* objptr)
  {

  }
  operator ORef<T>()
  {
    return ORef<T>(_iptr, _impl);
  }
};
*/

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
  void init(ObjectBase* iface, Object* obj)
  {
  }
  void assign(T* iface, Object* obj)
  {
  }
  void assign(ObjectBase* iface, Object* obj)
  {
  }
  // a version for downcasts
  ORef(T* iptr)
  {
    _iptr = iptr;
    _impl = iptr;
  }
  ORef(const T& o)
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
  template <class OT>
  explicit
  ORef(const ORef<OT>& iptr)
  {
    assign(iptr._iptr, iptr._impl);
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
  /*
  IRef(const IRef<I>& o)
    : _iptr(o._iptr)
    , _impl(o._impl)
  {
  }
  */
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


typedef ORef<Object> RObject;

class Comparable;
typedef IRef<Comparable> RComparable;

class Comparable
: implements InterfaceBase
{
public:
  virtual int compare(RComparable other) = 0;
};

class Writable
: implements InterfaceBase
{
public:
  virtual void write(int c) = 0;
};

typedef IRef<Writable> RWritable;


class StringWritable
: implements Writable
{
public:
  virtual void write(const char* cptr) = 0;
};

typedef IRef<StringWritable> RStringWritable;

class Number
: public Object
, implements Comparable
, implements StringWritable
{
public:
  virtual int compare(RComparable other)
  {
    return 0;
  }
  virtual void write(int c) { }
  virtual void write(const char* cptr) { }
};

typedef ORef<Number> RNumber;



void fooObject(RObject obj)
{
}

void fooNumber(RNumber obj)
{
}
void fooComparable(RComparable obj)
{
}

} // anon namespace
void ObjectTest2()
{
  RObject obj = new Object();
  RNumber num = new Number();
  RObject obj2 = obj;
  RObject obj3 = &num;
  fooObject((RObject)num);
  fooNumber(num);
  sys::coreout << "expect intit<T>: ";
  obj = num;
  
  sys::coreout << "expect intit<TO>: ";
  //num = obj;
  num = (RNumber)obj;
  
  sys::coreout << "expect ORef(OT*): ";
  obj = num;
  
  sys::coreout << "expect ORef(T*): ";
  //num = &obj;
  obj = new Object();
  obj = new Number();
  //num = new Object();
  RComparable c = num;
  c = num;
  c->compare(*num);
  RComparable c2 = (RComparable)obj;
  c->compare(c2);
  num = (RNumber)c;
  fooComparable(&num);
  //num = c;
  RStringWritable swr = &num;
  RWritable wr = &swr;
  wr = swr;
  swr = (RStringWritable)wr;
  RWritable* wrptr = wr._ref_this();
}

#endif //0
#endif //ACDK_GENDOC
