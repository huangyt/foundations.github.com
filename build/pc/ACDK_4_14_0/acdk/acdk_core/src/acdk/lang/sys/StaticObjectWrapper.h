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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/StaticObjectWrapper.h,v 1.5 2005/04/28 15:00:05 kommer Exp $

#ifndef acdk_lang_sys_StaticObjectWrapper_h
#define acdk_lang_sys_StaticObjectWrapper_h

namespace acdk {
namespace lang {
namespace sys {

/**
  In case an Object has to be defined statically,
  this class helps to manage initialization and deinitialion order.
  @see ACDK_STATIC_INSTANCE0, ACDK_STATIC_INSTANCE1, etc.
  T has to be a type of RObject or RInterface
  @ingroup acdksmartptr
*/
template <class T, class RT>
class StaticObjectWrapper
{
  RT _var;
public:
  StaticObjectWrapper()
  {
  }
  bool isCreated() const { return _var != Nil; }
  operator RT& ()
  {
    if (_var == Nil)
    {
      _var = new T();
      ::acdk::lang::System::registerStaticReference(_var);
    }
    return _var;
  }
};

/**
  @see StaticObjectWrapper
  @ingroup acdksmartptr
*/
template <class T, class RT, class P1>
class StaticObjectWrapper1
{
  RT _var;
  P1 _arg1;
public:
  StaticObjectWrapper1(INP(P1) arg1)
    : _arg1(arg1)
  {
  }
  bool isCreated() const { return _var != Nil; }
  operator RT& ()
  {
    if (_var == Nil)
    {
      _var = new T(_arg1);
      ::acdk::lang::System::registerStaticReference(_var);
    }
    return _var;
  }
};


/**
  @see StaticObjectWrapper
  @ingroup acdksmartptr
*/

template <class T, class RT, class P1, class P2>
class StaticObjectWrapper2
{
  RT _var;
  P1 _arg1;
  P2 _arg2;
public:
  StaticObjectWrapper2(INP(P1) arg1, INP(P2) arg2)
  : _arg1(arg1)
  , _arg2(arg2)
  {
  }
  bool isCreated() const { return _var != Nil; }
  operator RT& ()
  {
    if (_var == Nil)
    {
      _var = new T(_arg1, _arg2);
      ::acdk::lang::System::registerStaticReference(_var);
    }
    return _var;
  }
};


}
}
}

/**
  use this macro in method to declare a static method.
  See usage in acdk/util/logging/LogManager.cpp
  @param Type type of the hold static instance
  @param name of the static variable
  @ingroup acdksmartptr
*/
#define ACDK_STATIC_INSTANCE0(Type, name) \
  static ::acdk::lang::sys::StaticObjectWrapper<Type, R##Type> name
/**
  use this macro in method to declare a static method.
  See usage in acdk/util/logging/LogManager.cpp
  @param Type type of the hold static instance
  @param name of the static variable
  @param ArgTyp1 type of the first constructor argument to create Type
  @param arg1 variable delivered to constructor of Type
  @ingroup acdksmartptr
*/
#define ACDK_STATIC_INSTANCE1(Type, ArgType1, arg1, name) \
  static ::acdk::lang::sys::StaticObjectWrapper1<Type, R##Type, ArgType1> name(arg1)
/**
  use this macro in method to declare a static method.
  See usage in acdk/util/logging/LogManager.cpp
   @param Type type of the hold static instance
  @param name of the static variable
  @param ArgTyp1 type of the first constructor argument to create Type
  @param arg1 variable delivered to constructor of Type
  @ingroup acdksmartptr
*/
#define ACDK_STATIC_INSTANCE2(Type, ArgType1, arg1, ArgType2, arg2, name) \
  static ::acdk::lang::sys::StaticObjectWrapper2<Type, R##Type, ArgType1, ArgType2> name(arg1, arg2)


#endif //acdk_lang_sys_StaticObjectWrapper_h

