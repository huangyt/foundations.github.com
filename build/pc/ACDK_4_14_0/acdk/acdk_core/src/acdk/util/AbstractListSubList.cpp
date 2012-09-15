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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/AbstractListSubList.cpp,v 1.8 2005/03/08 12:45:45 kommer Exp $


#include <acdk.h>
#include "AbstractListSubList.h"

namespace acdk {
namespace util {

using namespace acdk::lang;

//virtual 
RObject 
AbstractListSubList::set(int index, IN(RObject) o) 
{
  _checkMod();
  _checkBoundsExclusive(index);
  RObject reto = _list->set(index + _offset, o);
  _modCount = _list->_modCount;
  return reto;
}


//virtual 
RObject 
AbstractListSubList::get(int index) 
{
  _checkMod();
  _checkBoundsExclusive(index);
  return _list->get(index + _offset);
}

//virtual 
void 
AbstractListSubList::add(int index, IN(RObject) o) 
{
  _checkMod();
  _checkBoundsInclusive(index);
  _list->add(index + _offset, o);
  _modCount = _list->_modCount;
  _size++;
}

//virtual 
RObject 
AbstractListSubList::remove(int index) 
{
  _checkMod();
  _checkBoundsExclusive(index);
  RObject o = _list->remove(index + _offset);
  _modCount = _list->_modCount;
  _size--;
  return o;
}


//virtual 
void 
AbstractListSubList::removeRange(int fromIndex2, int toIndex2) 
{
  _checkMod();
  _checkBoundsExclusive(fromIndex2);
  _checkBoundsInclusive(toIndex2);
  
  // this call will catch the toIndex2 < fromIndex2 condition
  _list->removeRange(_offset + fromIndex2, _offset + toIndex2);
  _modCount = _list->_modCount;
  _size -= toIndex2 - fromIndex2;
}

//virtual 
bool 
AbstractListSubList::addAll(int index, IN(RCollection) c) 
{
  _checkMod();
  _checkBoundsInclusive(index);
  int s = _list->size();
  bool result = _list->addAll(_offset + index, c);
  _modCount = _list->_modCount;
  _size += _list->size() - s;
  return result;
}

RListIterator 
AbstractListSubList::listIterator(int index) 
{
  _checkMod();
  _checkBoundsInclusive(index);
  return new AbstractListSubListListIterator(this, index, _offset);
}


} // Util
} // acdk
