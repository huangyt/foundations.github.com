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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/StringTokenizer.cpp,v 1.16 2005/02/05 10:45:06 kommer Exp $


#include <acdk.h>

#include "StringTokenizer.h"

#include <acdk/lang/Exception.h>
#include "NoSuchElementException.h"

namespace acdk {
namespace util {

using namespace acdk::lang;


int findFirstOfChars(IN(RString) text, IN(RString) del)
{
  String::iterator it = del->begin();
  String::iterator end = del->end();
  int idx = -1;
  for (; it < end; ++it)
  {
    int sidx = text->indexOf(*it);
    if (sidx != -1 && (idx == -1 || idx > sidx))
      idx = sidx;
  }
  return idx;
}

int _indexOf(IN(RString) text, IN(RString) del, bool useDelimiterAsChars)
{
  if (useDelimiterAsChars == true)
    return findFirstOfChars(text, del);
  return text->indexOf(del);
}

int _delimiterLength(IN(RString) del, bool useDelimiterAsChars)
{
  if (useDelimiterAsChars == true)
    return 1;
  return del->length();
}

bool 
StringTokenizer::_findNextToken(int offset)
{
 
  if (offset >= _buffer->length())
    return false;
  RString text = _buffer->substr(offset);
  
  int idx = _indexOf(text, _delimiter, _useDelimerAsChars);
  
  if (idx == 0)
  {
    if (_delimiterAreToken == true)
    {
      _nextBegin = offset + idx;
      _nextEnd = offset + idx + _delimiterLength(_delimiter, _useDelimerAsChars);
      return true;
    }
    // skip delimiters
    while (idx != -1)
    {
      offset += idx + _delimiterLength(_delimiter, _useDelimerAsChars);
      text = _buffer->substr(offset);
      idx = _indexOf(text, _delimiter, _useDelimerAsChars);
      if (idx == 0)
        continue;
      break;
    }
  }
  if (idx == -1)
  {
    if (text->length() > 0)
    {
      _nextBegin = offset;
      _nextEnd = offset + text->length();
      return true;
    }
    return false;
  }
  if (idx != 0)
  {
    _nextBegin = offset;
    _nextEnd = offset + idx;
    return true;
  }
  // should never reach here
  return false;
  
}

bool 
StringTokenizer::hasMoreTokens()
{
  if (_nextBegin != -1)
    return true;
  return _findNextToken(_pos);
}

RString 
StringTokenizer::nextToken() THROWS1(RNoSuchElementException)
{
  if (_nextBegin == -1)
  {
    if (_findNextToken(_pos) == false)
      THROW0(NoSuchElementException);
  }
  RString erg = _buffer->substr(_nextBegin, _nextEnd);
  _pos = _nextEnd;
  _nextBegin = _nextEnd = -1;
  return _lastElement = erg;
}

RString 
StringTokenizer::nextToken(IN(RString) delim, bool useDelimerAsChars) THROWS1(RNoSuchElementException)
{
  if (delim->equals(_delimiter) == false || _useDelimerAsChars != useDelimerAsChars)
  {
    _delimiter = delim;
    _useDelimerAsChars = useDelimerAsChars;
    _nextBegin = _nextEnd = -1;
  }
  return nextToken();
}

int 
StringTokenizer::countTokens()
{
  
  int cpos = _pos;
  int sicStart = _nextBegin;
  int sicEnd = _nextEnd;
  _nextBegin = _nextEnd = -1;
  int num = 0;
  while (_findNextToken(cpos) == true)
  {
    ++num;
    cpos = _nextEnd;
  }
  _nextBegin = sicStart;
  _nextEnd = sicEnd;
  return num;
}

//virtual 
void 
StringTokenizer::remove()
{
  THROW1(Exception, "not supported: StringTokenizer::remove()");
}  

//virtual 
RObject 
StringTokenizer::element()
{
  return (RObject)_lastElement;
}

RStringArray 
StringTokenizer::allToken()
{
  RStringArray sa = new StringArray(0);
  while (hasMoreTokens() == true)
  {
    sa->append(nextToken());
  }
  return sa;
}

} // util
} // acdk

