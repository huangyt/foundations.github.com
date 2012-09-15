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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ObjectInline.h,v 1.14 2005/02/05 10:44:56 kommer Exp $

#ifndef acdk_lang_ObjectInline_h
#define acdk_lang_ObjectInline_h

//#include "NullPointerException.h"

#include "Object.h"
#include "sys/RefHolderExt.h"

namespace acdk {
namespace lang {

#ifdef ACDK_MT

/** 
  API: ACDK internal<br>
  @author Roger Rene Kommer
  @version $Revision: 1.14 $
  @date $Date: 2005/02/05 10:44:56 $
*/  


class ObjectLockOnHeap
{
  RObject _lock;
public:
  ObjectLockOnHeap(Object* lock) : _lock(lock) { _lock->lock(); }
  //ObjectLockOnHeap(const RefHolder<T>& lock) : _lock(&lock) { _lock->lock(); }
  ~ObjectLockOnHeap() { _lock->unlock(); }
};


#  define LOCKTHIS() ::acdk::lang::TLockGuard< ::acdk::lang::Object> ___lock_this(*this)
#  define LOCKOBJECT(o) ::acdk::lang::ObjectLockOnHeap ___lock_##o(o.impl())

#else // ACDK_MT

#  define LOCKTHIS() 
#  define LOCKOBJECT(o) 

#endif // ACDK_MT


#define synchronized LOCKTHIS()
#define SYNCHRONIZETHIS() LOCKTHIS()
#define SYNCTHIS() LOCKTHIS()
#define SYNCHRONIZEOBJECT(o) LOCKOBJECT(o) 
#define SYNCOBJECT(o) LOCKOBJECT(o) 

/** Synchronize all instances of Class.
    This can be used to synchronize access to static members of
    a class. */

#ifdef ACDK_MT
# define SYNCCLASS() ::acdk::lang::ObjectLockOnHeap ___lock_class(&GetClass())
#else
# define SYNCCLASS() 
#endif

inline 
Object* 
InterfaceBase::getDmiTarget(const ::acdk::lang::dmi::ClazzInfo*& ci) 
{ 
  bool bret = false;
  Object* o = _getObjectPtr();
  while ((o = o->getDmiTarget(bret, ci)) != 0 && bret == true)
    ;
  return o;
}



} // namespace lang
} // namespace acdk


#endif //acdk_lang_ObjectInline_h

