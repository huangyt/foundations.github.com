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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ThreadLocal.h,v 1.19 2005/05/02 23:08:35 kommer Exp $

#ifndef acdk_lang_ThreadLocal_h
#define acdk_lang_ThreadLocal_h

#include "ThreadLocalImpl.h"
#include "sys/core_specific.h"

namespace acdk {
namespace lang {


ACDK_DECL_CLASS(ThreadLocal);

/** 
  Implements a dynamic allocatable ThreadLocal variable.
  @see acdk::lang::Thread
  @author Roger Rene Kommer
  @version $Revision: 1.19 $
  @date $Date: 2005/05/02 23:08:35 $
*/  
class ACDK_CORE_PUBLIC ThreadLocal 
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(ThreadLocal)  
private:
  foreign ::acdk::lang::sys::specific<RObject> _tls;
public:
  ThreadLocal()
  : Object(),
    _tls()
  {
  }
  ThreadLocal(IN(RObject) obj)
  : Object(),
    _tls()
  {
    _checkValidObject(obj);
    set(obj);
  }
  foreign virtual ~ThreadLocal()
  {
  }
  RObject get() 
  { 
    return _tls.get();
  }
  void set(IN(RObject) obj)
  {
    _checkValidObject(obj);
    RObject old = _tls.get();
    if (old != Nil)
      old->setStaticRef(false);
    _tls.get() = obj;
    if (obj != Nil)
      obj->setStaticRef(true);
  }

public:
  foreign void _checkValidObject(IN(RObject) obj)
  {
    if (obj != Nil && obj->isStackRef() == true)
      THROW1(RuntimeException, "ThreadLocal: stack variable must not register as ThreadLocal: " + obj->getClass()->getName() + "; " + obj->toString());
  }

   /** will be called at the end of a Thread to cleanup */
  static void threadEnd();
};


} // lang
} // acdk

#endif //acdk_lang_ThreadLocal_h

