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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/SoftReference.h,v 1.12 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_ref_SoftReference_h
#define acdk_lang_ref_SoftReference_h

#include <acdk.h>
#include "ref.h"

namespace acdk {
namespace lang {
namespace ref {

using namespace acdk::lang;

ACDK_DECL_CLASS(SoftReference);


/** 
  API: Java
  @author Roger Rene Kommer
  @version $Revision: 1.12 $
  @date $Date: 2005/02/05 10:44:59 $
  @see NotifyRepository
  @see ReferenceQueue
*/

class ACDK_CORE_PUBLIC SoftReference
: extends Reference
{
  ACDK_WITH_METAINFO(SoftReference)
public:
  SoftReference(IN(RObject) ref, IN(RReferenceQueue) queue = Nil);
  ~SoftReference();
  foreign virtual bool notifyBeforeDestruction(Object* obj);
  
};

} // ref
} // lang
} // acdk

#endif //acdk_lang_ref_SoftReference_h

