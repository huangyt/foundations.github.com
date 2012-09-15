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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/xml/sax/helpers/AttributesImpl.cpp,v 1.4 2005/02/05 10:45:38 kommer Exp $


#include "AttributesImpl.h"
#include <acdk/lang/ArrayIndexOutOfBoundsException.h>

namespace org {
namespace xml {
namespace sax {
namespace helpers {
  
  
int 
AttributesImpl::getIndex(IN(RString) uri, IN(RString) localName)
{
  int max = _length * 5;
  for (int i = 0; i < max; i += 5) 
    if (_data[i]->equals(uri) && _data[i + 1]->equals(localName)) 
      return i / 5;
    return -1;
}

int 
AttributesImpl::getIndex(IN(RString) qName)
{
  int max = _length * 5;
  for (int i = 0; i < max; i += 5) 
    if (_data[i + 2]->equals(qName) == true) 
      return i / 5;
    return -1;
}

RString 
AttributesImpl::getType(IN(RString) uri, IN(RString) localName)
{
  int max = _length * 5;
  for (int i = 0; i < max; i += 5) 
    if (_data[i]->equals(uri) == true && _data[i + 1]->equals(localName) == true) 
      return _data[i + 3];
    
    return Nil;
}

RString 
AttributesImpl::getType(IN(RString) qName)
{
  int max = _length * 5;
  for (int i = 0; i < max; i += 5) 
    if (_data[i + 2]->equals(qName)) 
      return _data[i + 3];
    return Nil;
}

RString 
AttributesImpl::getValue(IN(RString) uri, IN(RString) localName)
{
  int max = _length * 5;
  for (int i = 0; i < max; i += 5) 
    if (_data[i]->equals(uri) == true && _data[i + 1]->equals(localName) == true) 
      return _data[i + 4];
    
    return Nil;
}

RString 
AttributesImpl::getValue(IN(RString) qName)
{
  int max = _length * 5;
  for (int i = 0; i < max; i += 5) 
    if (_data[i + 2]->equals(qName) == true)
      return _data[i + 4];
    return Nil;
}

void 
AttributesImpl::setAttributes(IN(RAttributes) atts)
{
  clear();
  _length = atts->getLength();
  if (_length == 0) 
    return;
  _data = new StringArray(_length * 5);
  for (int i = 0; i < _length; i++) 
  {
    _data[i * 5] = atts->getURI(i);
    _data[i * 5 + 1] = atts->getLocalName(i);
    _data[i * 5 + 2] = atts->getQName(i);
    _data[i * 5 + 3] = atts->getType(i);
    _data[i * 5 + 4] = atts->getValue(i);
  }
}


void 
AttributesImpl::addAttribute(IN(RString) uri, IN(RString) localName, IN(RString) qName, IN(RString) type, IN(RString) value)
{
  _ensureCapacity(_length + 1);
  _data[_length * 5] = uri;
  _data[_length * 5 + 1] = localName;
  _data[_length * 5 + 2] = qName;
  _data[_length * 5 + 3] = type;
  _data[_length * 5 + 4] = value;
  _length++;
}

void 
AttributesImpl::setAttribute(int index, IN(RString) uri, IN(RString) localName, IN(RString) qName, IN(RString) type, IN(RString) value)
{
  if (index < 0 || index >= _length) 
    badIndex(index);
  _data[index * 5] = uri;
  _data[index * 5 + 1] = localName;
  _data[index * 5 + 2] = qName;
  _data[index * 5 + 3] = type;
  _data[index * 5 + 4] = value;
}
void 
AttributesImpl::removeAttribute(int index)
{
  if (index < 0 || index >= _length)
    badIndex(index);
  
  if (index < _length - 1) 
    _data->resize(index * 5);
    //System::arraycopy(_data, (index + 1) * 5, _data, index * 5, (_length - index) * 5);
  _length--;
}

void 
AttributesImpl::_ensureCapacity(int n)
{
  if (n <= 0)
    return;
  int maxn = 0;
  if (_data == Nil || _data->length() == 0)
    maxn = 25;
  else if (_data->length() >= n * 5) 
      return;
  else 
    maxn = _data->length();
  
  while (maxn < n * 5) 
    maxn *= 2;
  if (_data == Nil)
    _data = new StringArray(maxn);
  else
    _data->resize(maxn);
  
}
void 
AttributesImpl::badIndex(int index)
{
  THROW1(ArrayIndexOutOfBoundsException, "Attempt to modify attribute at illegal index: " + index);
}

} // namespace org
} // namespace xml
} // namespace sax
} // namespace helpers
