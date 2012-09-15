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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/InterfaceBase.h,v 1.16 2005/04/14 10:41:54 kommer Exp $

#ifndef acdk_lang_InterfaceBase_h
#define acdk_lang_InterfaceBase_h


namespace acdk {
namespace lang {

/**
  Classes serves as base for all Interfaces.
  Please refer to ACDK_INTERFACEBASE
*/
foreign 
class ACDK_CORE_PUBLIC InterfaceBase
{
public:
  virtual ~InterfaceBase() {}
  /**
     return the underlying implementation pointer
     @see acdk::lang:Object
  */
  virtual Object* _getObjectPtr() = 0;
   /**
    for DMI implemented classes with multiple interface
    allow casts. The returned Object will be casted
    to the correct type using dynamic_cast
  */

  virtual Object* _cast(const acdk::lang::dmi::ClazzInfo* ci);
  /**
     return the class meta information of this object
     will be implemented by each Object
   */
  static ::acdk::lang::dmi::ClazzInfo* clazzInfo() { return 0; }
  /**
    return the current dmi target.
    implementation in ObjectInline.h
  */
  inline Object* getDmiTarget(const ::acdk::lang::dmi::ClazzInfo*& ci);

};



} // lang
} // acdk

#endif //acdk_lang_InterfaceBase_h

