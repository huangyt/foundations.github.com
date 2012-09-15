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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractSequentialList.cpp,v 1.8 2005/03/08 12:45:45 kommer Exp $



#include <acdk.h>
#include "AbstractSequentialList.h"

#include <acdk/lang/IndexOutOfBoundsException.h>

namespace acdk {
namespace util {

//virtual 
bool 
AbstractSequentialList::addAll(int index, IN(RCollection) c) 
{
  bool changed = false;
  RIterator ci = c->iterator();
  RListIterator lit = listIterator(index);
  while (ci->hasNext() == true) {
    lit->add(ci->next());
    changed = true;
  }
  return changed;
}

//virtual 
RObject 
AbstractSequentialList::get(int index) 
{
  RListIterator it = listIterator(index);
  if (it->hasNext() == false) 
    THROW0(IndexOutOfBoundsException);
  return it->next();
}

//virtual 
RObject 
AbstractSequentialList::remove(int index) 
{
  RListIterator it = listIterator(index);
  if (it->hasNext() == false) 
    THROW0(IndexOutOfBoundsException);
  RObject removed = it->next();
  it->remove();
  return removed;
}

//virtual 
RObject 
AbstractSequentialList::set(int index, IN(RObject) o) 
{
  RListIterator it = listIterator(index);
  if (it->hasNext() == false)
    THROW0(IndexOutOfBoundsException);
  RObject old = it->next();
  it->set(o);
  return old;
}

} // Util
} // acdk
