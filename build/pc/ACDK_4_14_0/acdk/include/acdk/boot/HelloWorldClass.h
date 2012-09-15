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
// $Id: HelloWorldClass.h,v 1.20 2005/04/08 10:53:16 kommer Exp $

#ifndef acdk_boot_HelloWorldClass_h
#define acdk_boot_HelloWorldClass_h

#include "boot.h"
#include "HelloWorldInterface.h"

namespace acdk {
namespace boot {

using namespace acdk::lang;

ACDK_DECL_CLASS(HelloWorldClass);

/** 
  This is the standard implementation, how to say Hello to an user
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/08 10:53:16 $
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("Key1", "Val1"))
ACDK_CLASSATTRIBUTE(acdk.tools.mc.StringTagAttribute("Key2", "Val2"))
class ACDK_ACDK_BOOT_PUBLIC HelloWorldClass
: extends acdk::lang::Object,
  implements HelloWorldInterface
{
  ACDK_WITH_METAINFO(HelloWorldClass)
public:
  RString _greetings;
  int _ival;
public:
  /**
     Standard constructor 
     @param msg a Hello Message
  */
  HelloWorldClass(IN(RString) msg);
  /** 
    implemnted from HelloWorldInterface
    @see HelloWorldInterface
  */
  virtual void sayHello(IN(RString) username);
  /**
     return the Hello message
  */
  RString getGreetings() { return _greetings; }
};


} // namespace acdk 
} // namespace boot 


#endif //acdk_boot_HelloWorldClass_h

