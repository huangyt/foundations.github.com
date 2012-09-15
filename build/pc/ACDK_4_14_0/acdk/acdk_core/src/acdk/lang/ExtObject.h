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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ExtObject.h,v 1.13 2005/04/10 15:02:47 kommer Exp $


#ifndef acdk_lang_ExtObject_h
#define acdk_lang_ExtObject_h

#include "Object.h"

template <class T> class ExtObjectSharedRef;

/**
  Wrapper to external class pointer.
  I do not recommend to use this wrapper, because
  it doesn't interoperate with ACDK Collections (acdk::util).
  Better is to use ExtObjectVal
  @ingroup acdksmartptr
*/
template <class T>
class ExtObjectPtr
: extends ::acdk::lang::Object
{
public:
  T* _extPtr;
  bool _owns;
  ExtObjectPtr(T* ptr, bool owns = true)
  : _extPtr(ptr)
  , _owns(owns)
  {
  }
  ~ExtObjectPtr()
  {
    if (_owns == true && _extPtr != 0)
    {
      delete _extPtr;
      _extPtr = 0;
    }
  }
  /**
    @todo nullpointerexception
  */
  T* operator->() { return _extPtr; }
};

/**
  Wrapper to external classes.
  To wrap any type or class with an ACDK object
  you have to define following:
  @code
    inline int acdk_hashCode(INP(MyClass) rt);
    inline bool acdk_equals(INP(MyClass) rt1, INP(MyClass) rt2) { return rt1 == rt2; }
    inline int acdk_compareTo(INP(MyClass) rt1, INP(MyClass) rt2) { return (rt1 < rt2) ? -1 : ((rt2 < rt1) ? 1 : 0); }
    inline acdk::lang::RString acdk_toString(INP(MyClass) rt) { return ""; }

    typedef ExtObjectVal<MyClass, MyClassAdapterObjectAdapter> MyMyClassObject;
    typedef RefHolder<MyMyClassObject> RMyMyClassObject;
  @endcode
  @ingroup acdksmartptr
*/
template <class T>
class ExtObjectVal
: extends ::acdk::lang::Object
, extends T
{
  typedef ExtObjectVal<T> ThisType;
  typedef ExtObjectSharedRef<ThisType> RThisType;
  typedef T Base;
public:
  ExtObjectVal()
  {
  }
  template <typename P1>
  ExtObjectVal(const P1& p1)
  : Base(p1)
  {
  }
  template <typename P1, typename P2>
  ExtObjectVal(const P1& p1, const P2& p2)
  : Base(p1, p2)
  {
  }
  template <typename P1, typename P2, typename P3>
  ExtObjectVal(const P1& p1, const P2& p2, const P3& p3)
  : Base(p1, p2, p3)
  {
  }
  template <typename P1, typename P2, typename P3, typename P4>
  ExtObjectVal(const P1& p1, const P2& p2, const P3& p3, const P4& p4)
  : Base(p1, p2, p3, p4)
  {
  }
  ~ExtObjectVal()
  {
  }
  int hashCode()
  {
    return acdk_hashCode(RThisType(this));
  }
  inline bool equals(INP(RThisType) rt2)
  {
    return acdk_equals(RThisType(this), rt2);
  }
  inline int compareTo(INP(RThisType) rt2)
  {
    return acdk_compareTo(RThisType(this), RThisType(rt2));
  }
  inline acdk::lang::RString toString()
  {
    return acdk_toString(RThisType(this));
  }

};


/**
  RefHolder type (R-type) for external (foreign) pointer.
  This class is used via the ACDK_DECL_EXT_CLASS_PTR(ClassName) macro
  @ingroup acdksmartptr
*/
template <class T>
class ExtObjectSharedRef
: public RefHolder<T>
{
protected:
  typedef RefHolder<T> Base;
public:

  ExtObjectSharedRef(NilRef n = Nil)
  : Base(n)
  {
  }
  ExtObjectSharedRef(T* o)
  : Base(o)
  {
  }
  template <class OT>
  upcast_explicit
  ExtObjectSharedRef(const RefHolder<OT>& o)
  : Base(o)
  {
  }
  explicit
  ExtObjectSharedRef(::acdk::lang::Object* o)
  : Base(o)
  {
  }
  ~ExtObjectSharedRef()
  {
  }
  ExtObjectSharedRef<T>& operator=(T* o)
  {
    Base::operator=(o);
    return *this;
  }
  ExtObjectSharedRef<T>& operator=(const ExtObjectSharedRef<T>& o)
  {
    Base::operator=(o);
    return *this;
  }
  ExtObjectSharedRef<T>& operator=(NilRef nil)
  {
    if (ExtObjectSharedRef<T>::_impl == 0)
      return *this;
    Base::operator=(nil);
    return *this;
  }
  bool operator==(NilRef nil) const { return ExtObjectSharedRef<T>::_impl == 0; }
  bool operator!=(NilRef nil) const { return ExtObjectSharedRef<T>::_impl != 0; }

  template <class OT> bool operator==(const RefHolder<OT>& other) const { return ExtObjectSharedRef<T>::_impl == other.impl(); }
  template <class OT> bool operator!=(const RefHolder<OT>& other) const { return ExtObjectSharedRef<T>::_impl != other.impl(); }


  inline T* getIPtr() const
  {
    ACDK_ASSERT_NULL_PTR(ExtObjectSharedRef<T>::_iptr)
    T* ret;
    ret = ExtObjectSharedRef<T>::_iptr;
    return ret;
  }
  inline T* iptr() const
  {
    return ExtObjectSharedRef<T>::_iptr;
  }

  inline ::acdk::lang::Object* impl() const { return ExtObjectSharedRef<T>::_impl; }
  inline ::acdk::lang::Object* getImpl()
  {
    ACDK_ASSERT_NULL_PTR(ExtObjectSharedRef<T>::_iptr)
    return ExtObjectSharedRef<T>::_impl;
  }

  inline T* operator->() const
  {
    ACDK_ASSERT_NULL_PTR(ExtObjectSharedRef<T>::_iptr)
    return ExtObjectSharedRef<T>::iptr();
  }
  ExtObjectSharedRef<T>* _ref_this() { return this; }

#ifdef ACDK_CORE_NEED_ROBJECT_CONVERTER
  /**
      because there is no implizite upcast functionality
  */
  operator ::acdk::lang::RObject () const { return ::acdk::lang::RObject(*this); }
#endif //ACDK_CORE_NEED_ROBJECT_CONVERTER

  operator T* () const { return ExtObjectSharedRef<T>::iptr(); }
  T& operator *() const { return *operator->(); }

};

/**
  Wrapp a non-ACDK pointer as a ACDK compible class.
  @ingroup acdksmartptr
*/
#define ACDK_DECL_EXT_CLASS_PTR(ClassName) \
class ClassName; \
typedef ::ExtObjectPtr<ClassName> ClassName##ObjectPtr; \
typedef ::ExtObjectSharedRef<ClassName##ObjectPtr> R##ClassName##ObjectPtr; \
typedef ::ObjectArrayImpl<R##ClassName##ObjectPtr> ClassName##ObjectPtr##Array; \
typedef ::RObjectArrayImpl<ClassName##ObjectPtr##Array> R##ClassName##ObjectPtr##Array

/**
  Wrapp a non-ACDK value as a ACDK compible class.
  @ingroup acdksmartptr
*/
#define ACDK_DECL_EXT_CLASS_VAL(ClassName) \
class ClassName; \
typedef ::ExtObjectVal<ClassName> ClassName##Object; \
typedef ::RefHolder<ClassName##Object> R##ClassName##Object; \
typedef ::ObjectArrayImpl<R##ClassName##Object> ClassName##Object##Array; \
typedef ::RObjectArrayImpl<R##ClassName##Object> R##ClassName##Object##Array

#if !defined(DOXYGENONLY)

template <typename T>
inline
int acdk_compareTo(INP(InterfaceHolder<T>) o1, INP(InterfaceHolder<T>) o2)
{
  if ((o1 == Nil && o2 != Nil))
    return -1;
  if (o2 == Nil && o1 != Nil)
    return 1;
  return acdk::lang::RObject(o1)->compareTo(acdk::lang::RObject(o2));
}

/// standard object interface
template <typename T>
inline
bool acdk_equals(INP(RefHolder<T>) o1, INP(RefHolder<T>) o2)
{
  if (o1 == Nil && o2 == Nil)
    return true;
  if (o1 == Nil || o2 == Nil)
    return false;
  return acdk::lang::RObject(o1)->equals(acdk::lang::RObject(o2));
}

/// standard object interface
template <typename T>
inline
bool acdk_equals(INP(InterfaceHolder<T>) o1, INP(InterfaceHolder<T>) o2)
{
  if (o1 == Nil && o2 == Nil)
    return true;
  if (o1 == Nil || o2 == Nil)
    return false;
  return RObject(o1)->equals(RObject(o2));
}

/// standard object interface
template <typename T>
inline
bool acdk_equals(INP(RBasicArray<T>) o1, INP(RBasicArray<T>) o2)
{
  if (o1 == Nil && o2 == Nil)
    return true;
  if (o1 == Nil || o2 == Nil)
    return false;
  return o1->equals(o2);
}



template <typename T>
inline
int acdk_compareTo(INP(RefHolder<T>) o1, INP(RefHolder<T>) o2)
{
  if ((o1 == Nil && o2 != Nil))
    return -1;
  if (o2 == Nil && o1 != Nil)
    return 1;
  return o1->compareTo(o2);
}

template <typename T>
inline
int acdk_hashCode(INP(RefHolder<T>) o1)
{
  if (o1 == Nil)
    return 0;
  return o1->hashCode();
}

template <typename T>
inline
::acdk::lang::RString acdk_toString(INP(RefHolder<T>) o1)
{
  if (o1 == Nil)
    return "<Nil>";
  return o1->toString();
}





template <typename T>
inline
int acdk_hashCode(INP(InterfaceHolder<T>) o1)
{
  if (o1 == Nil)
    return 0;
  return ::acdk::lang::RObject(o1)->hashCode();
}

template <typename T>
inline
::acdk::lang::RString acdk_toString(INP(InterfaceHolder<T>) o1)
{
  if (o1 == Nil)
    return "<Nil>";
  return ::acdk::lang::RObject(o1)->toString();
}

template <typename T>
inline
bool acdk_isNil(INP(RefHolder<T>) o1)
{
  return o1 == Nil;
}

template <typename T>
inline
bool acdk_isNil(INP(InterfaceHolder<T>) o1)
{
  return o1 == Nil;
}


#define STANDARD_NUMVAL_OPS(Type) \
  inline bool acdk_equals(INP(Type) o1, INP(Type) o2) { return o1 == o2; } \
  inline  int acdk_compareTo(INP(Type) o1, INP(Type) o2) { return o1 - o2 < 0 ? -1 : 1; } \
  inline  int acdk_hashCode(INP(Type) o1) { return (int)o1; } \
  inline ::acdk::lang::RString acdk_toString(INP(Type) o1) { return ::acdk::lang::String::valueOf(o1); } \
  inline bool acdk_isNil(INP(Type)) { return false; }

STANDARD_NUMVAL_OPS(bool)
STANDARD_NUMVAL_OPS(byte)
STANDARD_NUMVAL_OPS(short)
STANDARD_NUMVAL_OPS(int)
STANDARD_NUMVAL_OPS(jlong)
STANDARD_NUMVAL_OPS(float)
STANDARD_NUMVAL_OPS(double)
STANDARD_NUMVAL_OPS(char)
STANDARD_NUMVAL_OPS(ucchar)

#endif //!defined(DOXYGENONLY)


#endif //acdk_lang_ExtObject_h

