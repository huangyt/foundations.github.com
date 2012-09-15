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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/rmi/Naming.cpp,v 1.6 2005/02/05 10:45:11 kommer Exp $

#include "Naming.h"

namespace acdk {
namespace java {
namespace rmi {


//static 
void 
Naming::bind(IN(RString) name, IN(RRemote) obj)
{
}

//static 
RStringArray 
Naming::list(IN(RString) name)
{
  return Nil;
}

//static 
RRemote 
Naming::lookup(IN(RString) name)
{
  return Nil;
}

//static 
void 
Naming::rebind(IN(RString) name, IN(RRemote) obj)
{
}
//static 
void 
Naming::unbind(IN(RString) name)
{
}


} // namespace acdk
} // namespace java
} // namespace rmi

