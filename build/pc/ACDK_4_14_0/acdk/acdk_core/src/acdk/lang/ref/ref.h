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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/ref/ref.h,v 1.6 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_ref_ref_h
#define acdk_lang_ref_ref_h

ACDK_DECL_UNIT(acdk_lang_ref)

namespace acdk {
namespace lang {
/**
  Equally to the Java package java.lang.ref
*/
namespace ref {

} // namespace ref
} // namespace lang
} // namespace acdk

#include "Reference.h"
#include "ReferenceQueue.h"
#include "SoftReference.h"
#include "WeakReference.h"
#include "PhantomReference.h"

#endif //acdk_lang_ref_ref_h

