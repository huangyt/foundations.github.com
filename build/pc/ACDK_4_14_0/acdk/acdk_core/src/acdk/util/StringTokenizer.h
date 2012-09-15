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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/util/StringTokenizer.h,v 1.21 2005/04/09 19:26:57 kommer Exp $

#ifndef acdk_util_StringTokenizer_h
#define acdk_util_StringTokenizer_h

#include "Iterator.h"
#include "NoSuchElementException.h"
namespace acdk {
namespace util {

using namespace acdk::lang;


ACDK_DECL_CLASS(StringTokenizer);

/**
  API: Java extended<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:57 $
  
*/
class ACDK_CORE_PUBLIC StringTokenizer
: extends acdk::lang::Object,
  implements acdk::util::Iterator
{
  ACDK_WITH_METAINFO(StringTokenizer)
private:
  mutable int _pos;
  RString _buffer;
  RString _delimiter;
  RString _lastElement;
  bool _delimiterAreToken;
  bool _useDelimerAsChars;
  /** next token start */
  mutable int _nextBegin;
  /** next token end */
  mutable int _nextEnd;
public:
  /**
    @param text to parse
    @param delimiter used as delimiter
    @param delimiteraretoken return found delimiter as token
    @param useDelimerAsChars will not search for delimiteraretoken as string, but each character in delimiteraretoken is a delimiter
  */
  StringTokenizer(IN(RString) text, IN(RString) delimiter, bool delimiteraretoken = false, bool useDelimerAsChars = false)
  : Object()
  , _pos(0)
  , _buffer(text)
  , _delimiter(delimiter)
  , _lastElement("")
  , _delimiterAreToken(delimiteraretoken)
  , _useDelimerAsChars(useDelimerAsChars)
  , _nextBegin(-1)
  , _nextEnd(-1)
  {
  }
  /**
    The character ' \t\n\r\f' are token bounds
    is equal to call StringTokenizer(text, " \t\n\r\f", false, true)
  */
  StringTokenizer(IN(RString) text)
  : _pos(0)
  , _buffer(text)
  , _delimiter(" \t\n\r\f")
  , _lastElement("")
  , _delimiterAreToken(false)
  , _useDelimerAsChars(true)
  , _nextBegin(-1)
  , _nextEnd(-1)
  {
  }
  bool hasMoreTokens();
  int countTokens();
  RString nextToken() THROWS1(RNoSuchElementException);
  RString nextToken(IN(RString) delim, bool useDelimerAsChars = false) THROWS1(RNoSuchElementException);
  /**
    returns all parsed token
  */
  RStringArray allToken();
  // Iterator
  foreign virtual bool hasNext() { return hasMoreTokens(); }
  foreign virtual RObject next() { return (RObject)nextToken(); }
  foreign virtual RObject element();
  foreign virtual void remove();
protected:
  foreign bool _findNextToken(int offset);

};


} // util
} // acdk

#endif //acdk_util_StringTokenizer_h

