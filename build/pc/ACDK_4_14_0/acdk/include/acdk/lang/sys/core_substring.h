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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_substring.h,v 1.2 2005/02/05 10:45:01 kommer Exp $

#ifndef acdk_lang_sys_core_substring_h
#define acdk_lang_sys_core_substring_h

#include <string.h>

namespace acdk {
namespace lang {
namespace sys {

/** 
  a 'very core' sub string
*/

/** 
  mainly used to operate on literal strings
*/
class core_substring
{
  const char* _begin;
  const char* _end;
public:

  core_substring()
  : _begin(0)
  , _end(0)
  {
  }
  core_substring(const char* text)
    : _begin(text)
    , _end(text + strlen(text))
  {
  }
  core_substring(const char* b, const char* e)
  : _begin(b)
  , _end(e)
  {
  }
  core_substring(const core_substring& other)
  : _begin(other._begin)
  , _end(other._end)
  {
  }
  int length() const { return _end - _begin; }
  core_substring substr(int startidx, int endidx = -1) const
  {
    if (endidx == -1)
      endidx = length();
    return core_substring(_begin + startidx, _begin + endidx);
  }
  bool equals(const char* ptr) const
  {
    return strncmp(_begin, ptr, length()) == 0;
  }
  bool equals(const core_substring& other) const
  {
    if (length() != other.length())
      return false;
    return strncmp(_begin, other._begin, length()) == 0;
  }
  int compareTo(const char* ptr) const
  {
    int plen = strlen(ptr);
    int minLen = length() < plen ? length() : plen;
    return strncmp(_begin, ptr, plen);
  }
  int indexOf(char tk) const
  {
    for (const char* it = _begin; it < _end; ++it)
    {
      if (tk == *it)
        return it - _begin;
    }
    return -1;
  }
};


} // sys
} // lang
} // acdk


#endif //acdk_lang_sys_core_substring_h

