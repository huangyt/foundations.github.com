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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/Cloneable.h,v 1.13 2005/02/05 10:44:55 kommer Exp $

#ifndef acdk_lang_Cloneable_h
#define acdk_lang_Cloneable_h

#ifndef acdk_h
#include <acdk.h>
#endif

namespace acdk {
namespace lang {

ACDK_DECL_INTERFACE(Cloneable);

/**
  If an Object is clonable (implements Clonable) 
  it must either
  overwrite virtual RObject clone();
  to simplify the implementation also Object::serialized_clone()
  can be used.

  API: Java<br>
  @author Roger Rene Kommer
  @version $Revision: 1.13 $
  @date $Date: 2005/02/05 10:44:55 $
*/

class ACDK_CORE_PUBLIC Cloneable
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Cloneable)
public:
  virtual RObject clone() = 0;
  foreign virtual RObject clone(sys::Allocator* alloc) 
  { 
    ObjectBase::_throwNotImplementedYet("Cloneable::clone(sys::Allocator* alloc)");
    return Nil; 
  }
};




} // lang
} // acdk

#endif //acdk_lang_Cloneable_h

