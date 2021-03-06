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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/reflect/AccessibleObject.h,v 1.7 2005/02/05 10:44:59 kommer Exp $

#ifndef acdk_lang_reflect_AccessibleObject_h
#define acdk_lang_reflect_AccessibleObject_h

namespace acdk {
namespace lang {
namespace reflect {

ACDK_DECL_CLASS(AccessibleObject);

class ACDK_CORE_PUBLIC AccessibleObject
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(AccessibleObject)
private:
  bool _accessible;
public:
  AccessibleObject(bool accessable = false) 
  : Object(), 
    _accessible(accessable) 
  { 
  }
  virtual bool isAccessible() { return _accessible; }
  virtual void setAccessible(bool flag) { _accessible = flag; }
  // static void setAccessible(AccessibleObject[] array, boolean flag) 
};

} // reflect
} // lang
} // acdk

#endif //acdk_lang_reflect_AccessibleObject_h

