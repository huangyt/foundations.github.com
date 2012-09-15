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
// $Id: HelloWorldInterface.h,v 1.15 2005/04/08 10:53:16 kommer Exp $

#ifndef acdk_boot_HelloWorldInterface_h
#define acdk_boot_HelloWorldInterface_h

#include "boot.h"

namespace acdk {
namespace boot {

using namespace acdk::lang;


ACDK_DECL_INTERFACE(HelloWorldInterface);

/** 
  A General Interface to say an user "Hello"
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.15 $
  @date $Date: 2005/04/08 10:53:16 $
  @see gw_ref[acdk_hb_lang_arrays]
  <pre>
  
  \code
    RHelloWorldInterface hw = new HelloWord();
     hw->toString();
  \endcode
  
  \htmlonly
  <source>
    RHelloWorldInterface hw = new HelloWord();
     hw->toString();
  </source>
  <htmlauto/>
  This is a paragraph with a %ref[Reference to Arrays, arrays].

  This is only a line.
  The next line resumes.
  <fl>
  <f> Bla
  <f> Blubb
  </fl>
  \endhtmlonly
*/
class ACDK_ACDK_BOOT_PUBLIC HelloWorldInterface
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(HelloWorldInterface)
public:
  /**
    Say given user "hello"
    @param username the user's name
    @return nothing
  */
  virtual void sayHello(IN(RString) username) = 0;
  virtual RString getGreetings() = 0;
};

} // namespace acdk 
} // namespace boot 

#endif //acdk_boot_HelloWorldInterface_h
