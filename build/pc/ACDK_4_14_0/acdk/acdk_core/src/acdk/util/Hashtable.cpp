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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/Hashtable.cpp,v 1.11 2005/03/08 12:45:45 kommer Exp $



#include "Hashtable.h"
#include "Enumeration.h"

#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

Hashtable::Hashtable() 
: _table(new (allocator()) HashMap())
{
}

Hashtable::Hashtable(int initialCapacity, float loadFactor) 
: _table(new (allocator()) HashMap(initialCapacity, loadFactor))
{
}

Hashtable::Hashtable(int initialCapacity) 
: _table(new (allocator()) HashMap(initialCapacity))
{
}

Hashtable::Hashtable(IN(RMap) other) 
: _table(new (allocator()) HashMap(other))
{
}


Hashtable::~Hashtable() 
{
}

bool 
Hashtable::equals(IN(RObject) obj)
{
  THROW0(UnsupportedOperationException);
  return false;
}

int 
Hashtable::hashCode()
{
  THROW0(UnsupportedOperationException);
  return 0;
} 

REnumeration 
Hashtable::keys()
{
  THROW0(UnsupportedOperationException);
  return Nil;
}

REnumeration 
Hashtable::elements()
{
  THROW0(UnsupportedOperationException);
  return Nil;  
}


} // Util
} // acdk

