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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/WeakReference.h,v 1.11 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_ref_WeakReference_h
#define acdk_lang_ref_WeakReference_h

#include <acdk.h>
#include "ref.h"

namespace acdk {
namespace lang {
namespace ref {

using namespace acdk::lang;

ACDK_DECL_CLASS(WeakReference);


/** 
  Similar or equal to the Java API

  if no other Reference hold this object :
  <li> The referenced Object will be destroyed or a ReferenceQueue is given. 
  <li> If a ReferenceQueue is given, the the queue owns the reference. The reference
       can be cleared by calling poll() or remove(). If the ReferenceQueue itself
       will be destroyed all References held by this ReferenceQueue will also be destroyed.
       WeakReferences, which are hold by the ReferenceQueue still are reachable through 
       the WeakReferences instance.
  
  @author Roger Rene Kommer
  @version $Revision: 1.11 $
  @date $Date: 2005/02/05 10:44:59 $
  @see NotifyRepository
*/

class ACDK_CORE_PUBLIC WeakReference
: extends Reference
{
  ACDK_WITH_METAINFO(WeakReference)
public:
  WeakReference(IN(RObject) ref, IN(RReferenceQueue) queue = Nil);
  ~WeakReference();
  virtual RObject get();
  foreign virtual bool notifyBeforeDestruction(Object* obj);
};

} // ref
} // lang
} // acdk

#endif //acdk_lang_ref_WeakReference_h

