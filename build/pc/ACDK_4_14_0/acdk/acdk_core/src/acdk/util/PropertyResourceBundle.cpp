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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/PropertyResourceBundle.cpp,v 1.13 2005/03/18 20:28:29 kommer Exp $


#include <acdk.h>
#include "MissingResourceException.h"

#include "PropertyResourceBundle.h"
#include "DoubleIterator.h"

namespace acdk {
namespace util {

using namespace acdk::io;

// #### FEATURE implement PropertyResourceBundle which also enables to read (but not write) StringArrays

PropertyResourceBundle::PropertyResourceBundle(IN(RReader) in) THROWS1(RIOException)
 : ResourceBundle(),
  _properties(new (allocator()) Properties())
{
  _properties->load(in);
}

RStringArray 
PropertyResourceBundle::getStringArray(IN(RString) key) THROWS1(RMissingResourceException)
{
  return _properties->getArrayProperty(key, 1);
}
RMap 
PropertyResourceBundle::getMap(IN(RString) key) THROWS1(RMissingResourceException)
{
  return _properties->getMapProperty(key);
}

//virtual 
RIterator 
PropertyResourceBundle::getKeys() 
{
  if (_parent != Nil) 
    return new DoubleIterator(_properties->propertyNames(), _parent->getKeys());
  return _properties->propertyNames();
}  

bool 
PropertyResourceBundle::hasValue(IN(RString) key) 
{ 
  return _properties->getProperty(key, Nil) != Nil; 
}

} // namespace util 
} //namespace acdk 


