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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/SimpleListResourceBundle.cpp,v 1.11 2005/03/14 15:03:40 kommer Exp $


#include <acdk.h>
#include "SimpleListResourceBundle.h"
#include "Iterator.h"

#include "NoSuchElementException.h"
#include <acdk/lang/UnsupportedOperationException.h>

namespace acdk {
namespace util {

using namespace acdk::lang;
using namespace acdk::io;


class SimpleListResourceBundleIterator
: extends acdk::lang::Object,
  implements Iterator
{
private:
  SimpleListResourceBundle::KeyValues* _keyValues;
  int _idx;
public:
  SimpleListResourceBundleIterator(SimpleListResourceBundle::KeyValues* keyValues)
  : Object(),
    _keyValues(keyValues),
    _idx(0)
  {
  }
  virtual bool hasNext()
  {
    if (_keyValues[_idx].key == 0)
      return false;
    else
      return true;
  }
  virtual RObject next()
  {
    if (_keyValues[_idx].key == 0)
      THROW1(NoSuchElementException, RString("Index out of bound: ") + _idx);
    RString erg = new String(_keyValues[_idx].key);
    _idx++;
    return (RObject)erg;
  }
  virtual RObject element()
  {
    if (_keyValues[_idx].key == 0)
      THROW1(NoSuchElementException, RString("Index out of bound: ") + _idx);
    RString erg = new String(_keyValues[_idx].key);
    return (RObject)erg;
  }
  virtual void remove()
  {
    THROW1(UnsupportedOperationException, "SimpleListResourceBundleIterator::remove() is not supported");
  }
};

//virtual 
RIterator 
SimpleListResourceBundle::getKeys() 
{
  if (_parent != Nil) 
    return new DoubleIterator(new SimpleListResourceBundleIterator(getContents()), _parent->getKeys());
  return new SimpleListResourceBundleIterator(getContents());
}  

//virtual 
RObject 
SimpleListResourceBundle::handleGetObject(IN(RString) key)
{
  KeyValues* keyValues = getContents();
  for (int i = 0; keyValues[i].key; i++) {
    if (strcmp(key->c_str(), keyValues[i].value) == 0)
      return new String(keyValues[i].value);
  }
  return Nil;
}
  
  

} // namespace util 
} //namespace acdk 


