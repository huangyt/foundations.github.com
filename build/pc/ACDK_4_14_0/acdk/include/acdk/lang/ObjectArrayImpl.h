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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ObjectArrayImpl.h,v 1.46 2005/03/14 12:22:40 kommer Exp $

#ifndef acdk_lang_sys_ObjectArrayImpl_h
#define acdk_lang_sys_ObjectArrayImpl_h

#include <acdk.h>

#include "Class.h"

#include "StringBuffer.h"


  
extern ACDK_CORE_PUBLIC ::acdk::lang::dmi::ClazzFieldInfo ObjectArray_fields__data;


#include "ObjectArrayBase.h"


/**
  Helper function to create and initialize an ObjectArray with a single expression
*/
template <class TO>
inline 
RObjectArrayImpl<TO> makeObjectArray(IN(TO) f)
{
  RObjectArrayImpl<TO> t = new ObjectArrayImpl<TO>(1);
  t[0] = f;
  return t;
}

/**
  Helper function to create and initialize an ObjectArray with a single expression
*/
template <class TO>
inline 
RObjectArrayImpl<TO> makeObjectArray(IN(TO) a0, IN(TO) a1)
{
  RObjectArrayImpl<TO> t = new ObjectArrayImpl<TO>(2);
  t[0] = a0;
  t[1] = a1;
  return t;
}

/**
  Helper function to create and initialize an ObjectArray with a single expression
*/
template <class TO>
inline 
RObjectArrayImpl<TO> makeObjectArray(IN(TO) a0, IN(TO) a1, IN(TO) a2)
{
  RObjectArrayImpl<TO> t = new ObjectArrayImpl<TO>(3);
  t[0] = a0;
  t[1] = a1;
  t[2] = a2;
  return t;
}

/**
  Helper function to create and initialize an ObjectArray with a single expression
*/
template <class TO>
inline 
RObjectArrayImpl<TO> makeObjectArray(IN(TO) a0, IN(TO) a1, IN(TO) a2, IN(TO) a3)
{
  RObjectArrayImpl<TO> t = new ObjectArrayImpl<TO>(4);
  t[0] = a0;
  t[1] = a1;
  t[2] = a2;
  t[3] = a3;
  return t;
}

/**
  Helper function to create and initialize an ObjectArray with a single expression
*/
template <class TO>
inline 
RObjectArrayImpl<TO> makeObjectArray(IN(TO) a0, IN(TO) a1, IN(TO) a2, IN(TO) a3, IN(TO) a4)
{
  RObjectArrayImpl<TO> t = new ObjectArrayImpl<TO>(5);
  t[0] = a0;
  t[1] = a1;
  t[2] = a2;
  t[3] = a3;
  t[4] = a4;
  return t;
}



namespace acdk {
namespace lang {
  typedef ::ObjectArrayImpl<RObject> ObjectArray;
  typedef ::RObjectArrayImpl<RObject> RObjectArray;
  typedef ::ObjectArrayImpl<RObjectArray> ObjectArrayArray;
  typedef ::RObjectArrayImpl<RObjectArray> RObjectArrayArray;
} 
}

#endif //acdk_lang_sys_ObjectArrayImpl_h


