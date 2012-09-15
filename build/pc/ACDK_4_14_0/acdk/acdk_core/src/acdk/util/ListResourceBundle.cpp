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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/ListResourceBundle.cpp,v 1.15 2005/03/15 14:51:42 kommer Exp $


#include <acdk.h>

#include "ListResourceBundle.h"

#include "NoSuchElementException.h"
#include <acdk/lang/UnsupportedOperationException.h>

#include "Iterator.h"
#include "DoubleIterator.h"

#include <acdk/lang/ObjectArrayImpl.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;


class ListResourceBundleIterator
: extends acdk::lang::Object,
  implements Iterator
{
private:
  RObjectArrayArray _contents;
  int _idx;
public:
  ListResourceBundleIterator(IN(RObjectArrayArray) contents)
  : Object(),
    _contents(contents),
    _idx(0)
  {
  }
  virtual bool hasNext()
  {
    if (_contents->length() > _idx)
      return true;
    return false;
  }
  virtual RObject next()
  {
    if (_contents->length() > _idx)
      THROW1(NoSuchElementException, RString("Index out of bound: ") + _idx);

    RObject erg = _contents[_idx][0];
    _idx++;
    return erg;
  }
  virtual RObject element()
  {
    if (_contents->length() > _idx)
      THROW1(NoSuchElementException, RString("Index out of bound: ") + _idx);
    RObject erg = _contents[_idx][0];
    return erg;
  }
  virtual void remove()
  {
    THROW1(UnsupportedOperationException, "ListResourceBundleIterator::remove() is not supported");
  }
};

//virtual 
RIterator 
ListResourceBundle::getKeys() 
{
  if (_parent != Nil) 
    return new DoubleIterator(new ListResourceBundleIterator(getContents()), _parent->getKeys());
  return new ListResourceBundleIterator(getContents());
}  

//virtual 
RObject 
ListResourceBundle::handleGetObject(IN(RString) key)
{
  RObjectArrayArray cont = getContents();
  if (cont == Nil)
    return Nil;
  for (int i = 0; i < cont->length(); i++) {
    if (cont[i] == Nil)
      break;
    if (key->equals(cont[i][0]) == true)
      return cont[i][1];
  }
  return Nil;
}
  
  

} // namespace util 
} //namespace acdk 


