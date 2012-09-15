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
// $Id: HelloWorldClass.cpp,v 1.8 2005/03/31 17:20:49 kommer Exp $



#include "HelloWorldClass.h"

#include <acdk/lang/System.h>


namespace acdk {
namespace boot {

using namespace acdk::lang;

HelloWorldClass::HelloWorldClass(IN(RString) msg)
: _greetings(msg)
, _ival(0)
{
}

//virtual 
void 
HelloWorldClass::sayHello(IN(RString) username)
{
  System::out->println(_greetings + " to " + username);
}


} // namespace acdk 
} // namespace boot 




