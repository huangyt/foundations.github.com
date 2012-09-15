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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/ReferenceQueue.h,v 1.11 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_ref_ReferenceQueue_h
#define acdk_lang_ref_ReferenceQueue_h

#include <acdk.h>
#include <acdk/lang/InterruptedException.h>

namespace acdk {
namespace lang {
namespace ref {

using namespace acdk::lang;

ACDK_DECL_CLASS(Reference);
ACDK_DECL_CLASS(ReferenceQueue);

/** 
  References, which not longer held by 
  a strong references but managed through a Reference
  are collected in a ReferenceQueue. The poll() and or remove()
  call can be used to clear these References. 

  @author Roger Rene Kommer
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:44:59 $
  @see Reference
  @see WeakReference
  @see SoftReference
*/

class ACDK_CORE_PUBLIC ReferenceQueue
:  public ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(ReferenceQueue)
protected:
  RReference _first;
public:
  ReferenceQueue();
  RReference poll();
  RReference remove(jlong timeout = 0);// throw(RInterruptedException, RThrowable);
  void enqueue(IN(RReference) ref);
protected:
  RReference _dequeue();
  friend struct ReferenceQueue_MetaInfoWrapper;
  
};

} // ref
} // lang
} // acdk

#endif //acdk_lang_ref_ReferenceQueue_h


