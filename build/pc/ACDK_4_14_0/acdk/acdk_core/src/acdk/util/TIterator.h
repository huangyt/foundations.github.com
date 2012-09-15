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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/TIterator.h,v 1.11 2005/02/05 10:45:06 kommer Exp $

#ifndef acdk_util_TIterator_h
#define acdk_util_TIterator_h

#ifndef acdk_h
#include <acdk.h>
#endif

namespace acdk {
namespace util {

using namespace acdk::lang;

#if defined(__gcc__)
# define ACKD_NO_TCOLLECTIONS
#endif


/**
  little helper to receive
  an Array type from the basic type
*/
template <class T>
class TArrayType
{
public:
  typedef ::ObjectArrayImpl<T> Type;
  typedef ::RObjectArrayImpl<T> RefType;
};

template <>
class TArrayType<bool>
{
public:
  typedef ::BasicArray<bool> Type;
  typedef ::RBasicArray<bool> RefType;
};

template <>
class TArrayType<char>
{
public:
  typedef ::BasicArray<char> Type;
  typedef ::RBasicArray<char> RefType;
};

template <>
class TArrayType<uc2char>
{
public:
  typedef ::BasicArray<uc2char> Type;
  typedef ::RBasicArray<uc2char> RefType;
};

template <>
class TArrayType<uc4char>
{
public:
  typedef ::BasicArray<uc4char> Type;
  typedef ::RBasicArray<uc4char> RefType;
};

template <>
class TArrayType<byte>
{
public:
  typedef ::BasicArray<byte> Type;
  typedef ::RBasicArray<byte> RefType;
};


template <>
class TArrayType<short>
{
public:
  typedef ::BasicArray<short> Type;
  typedef ::RBasicArray<short> RefType;
};

template <>
class TArrayType<int>
{
public:
  typedef ::BasicArray<int> Type;
  typedef ::RBasicArray<int> RefType;
};

template <>
class TArrayType<jlong>
{
public:
  typedef ::BasicArray<jlong> Type;
  typedef ::RBasicArray<jlong> RefType;
};

template <>
class TArrayType<float>
{
public:
  typedef ::BasicArray<float> Type;
  typedef ::RBasicArray<float> RefType;
};

template <>
class TArrayType<double>
{
public:
  typedef ::BasicArray<double> Type;
  typedef ::RBasicArray<double> RefType;
};

#if !defined(ACKD_NO_TCOLLECTIONS)

#define ACDK_DECL_TINTERFACE1(InterfaceName, T) \
template <class T> class InterfaceName; \
typedef ::InterfaceHolder<InterfaceName<T> > R##InterfaceName<T>; \
typedef ::ObjectArrayImpl<R##InterfaceName<T> > InterfaceName##Array<T>; \
typedef ::RObjectArrayImpl<R##InterfaceName<T> > R##InterfaceName##Array<T>

#define ACDK_DECL_TINTERFACE2(ClassName, T1, T2) \
typedef ClassName<R##T1, R##T2> T1##To##T2##ClassName; \
typedef ::InterfaceHolder<T1##To##T2##ClassName > R##T1##To##T2##ClassName; \
typedef ::ObjectArrayImpl<R##T1##To##T2##ClassName > T1##To##T2##ClassName##Array; \
typedef ::InterfaceHolder<R##T1##To##T2##ClassName > R##T1##To##T2##ClassName##Array



#define ACDK_DECL_TCLASS1(ClassName, T) \
typedef ClassName<T> T##ClassName; \
typedef ::RefHolder<T##ClassName > R##T##ClassName; \
typedef ::ObjectArrayImpl<R##T##ClassName > T##ClassName##Array; \
typedef ::RObjectArrayImpl<R##T##ClassName > R##T##ClassName##Array

#define ACDK_DECL_TCLASS2(ClassName, T1, T2) \
typedef ClassName<R##T1, R##T2> T1##To##T2##ClassName; \
typedef ::RefHolder<T1##To##T2##ClassName > R##T1##To##T2##ClassName; \
typedef ::ObjectArrayImpl<R##T1##To##T2##ClassName > T1##To##T2##ClassName##Array; \
typedef ::RObjectArrayImpl<R##T1##To##T2##ClassName > R##T1##To##T2##ClassName##Array

#else //!defined(ACKD_NO_TCOLLECTIONS)


#endif //!defined(ACKD_NO_TCOLLECTIONS)

// support template friends?
//#define friends_private pivate
#define friends_private public

#define ACDK_DECL_ITERATOR(Type, RType) \
typedef ::acdk::util::TIterator<RType> Type##Iterator; \
typedef Type##Iterator::RefType R##Type##Iterator

//ACDK_DECL_TINTERFACE1(TIterator, T);

/**
  To walk through all elements of a collection an Iterator
  is used. TIerator is typed version.

  @see Java: http://java.sun.com/j2se/1.3/docs/api/java/util/Iterator.html
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:45:06 $
  
*/
template <class T>
class TIterator
      ACDK_INTERFACEBASE
{
public :
  typedef T RValueType;
  typedef TIterator<RValueType> ThisType;
  typedef InterfaceHolder<ThisType> RefType;

  typedef T RElementType;
  typedef typename RElementType::Type ElementType;

  virtual bool hasNext() = 0;

  virtual RElementType next() = 0;
  //const RObject next() { return const_cast<Iterator*>(this)->next(); }

  /***
    API: Extension, may not supported by some Containers 
    returns current Element, whithout forward to next element
  */
  virtual RElementType element() = 0;

  //const RObject element() { return const_cast<Iterator*>(this)->element(); }
  /** 
    remove current element from the iterator. 
    May not support by all Iterator
  */
  virtual void remove() = 0;
  /** for java.util.Enumeration - compatibility */
  bool hasMoreElements()
  {
    return hasNext();
  }
  /** for java.util.Enumeration - compatibility */
  RElementType nextElement() 
  {
    return next();
  }
};

} // util
} // acdk

#endif //acdk_util_TIterator_h

