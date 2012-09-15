// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Parts of this class are ported from the of GNU Classpath project 
//  (http://www.gnu.org/software/classpath/classpath.html)
//   with following copyright statement:

// Copyright (c) 1998 by Stuart Ballard (stuart.ballard@mcmail.com)
// Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Library General Public License as published
// by the Free Software Foundation, version 2. (see COPYING.LIB)
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library General Public License for more details.
//
// You should have received a copy of the GNU Library General Public License
// along with this program; if not, write to the Free Software Foundation
// Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
// END

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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractSet.cpp,v 1.11 2005/03/08 12:45:45 kommer Exp $


#include <acdk.h>
#include "AbstractSet.h"

#include <acdk/lang/NullPointerException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;

//virtual 
bool 
AbstractSet::equals(IN(RObject) o)
{
  if (o.impl() == (const Object*)this) {
    return true;
  } else if (instanceof(o, Set) && RSet(o)->size() == size()) {
    return containsAll(RCollection(o));
  } else {
    return false;
  }
}

//virtual 
int 
AbstractSet::hashCode()
{
  int hash = 0;
  RIterator i = iterator();
  while (i->hasNext()) {
    try {
      hash += i->next()->hashCode();
    } catch (RNullPointerException ) {
    }
  }
  return hash;
}
//foreign virtual 
RObjectArray 
AbstractSet::toArray() 
{ 
  return AbstractCollection::toArray(); 
}

//foreign virtual 
RObjectArray 
AbstractSet::toArray(IN(RObjectArray) array) 
{ 
  return AbstractCollection::toArray(array); 
}

} // Util
} // acdk
