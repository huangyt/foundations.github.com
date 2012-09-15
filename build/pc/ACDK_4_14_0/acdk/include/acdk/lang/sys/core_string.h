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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_string.h,v 1.9 2005/02/05 10:45:01 kommer Exp $

#ifndef acdk_lang_sys_core_string_h
#define acdk_lang_sys_core_string_h

#include <string.h>

namespace acdk {
namespace lang {
namespace sys {

/** 
  a 'very core' string
*/
class core_string
{
  char* _ptr;
  bool _deleteBuffer;
public:
  core_string(const char* ptr, bool deleteBuffer = true)
    : _ptr(0),
      _deleteBuffer(deleteBuffer)
  {
    if (deleteBuffer == false) {
      _ptr = const_cast<char*>(ptr);
      return;
    }
    if (ptr == 0)
      return;
    int len = strlen(ptr);
    _ptr = new char[len + 1];
    memcpy(_ptr, ptr, len + 1);
  }
  core_string(const core_string& other)
    : _ptr(0),
      _deleteBuffer(other._deleteBuffer)
  {
    if (other._ptr == 0)
      return;
    if (_deleteBuffer == false) {
      _ptr = other._ptr;
      return;
    }
    int len = strlen(other._ptr);
    _ptr = new char[len + 1];
    memcpy(_ptr, other._ptr, len + 1);
  }
  ~core_string()
  {
    if (_deleteBuffer == true && _ptr != 0)
      delete[] _ptr;
  }
  char* c_str() { return (_ptr == 0 ? (char*)"" : _ptr); }
  const char* c_str() const { return (_ptr == 0 ? "" : _ptr); }
};

} // sys
} // lang
} // acdk


#endif //acdk_lang_sys_core_string_h

