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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_static_vector.h,v 1.7 2005/02/05 10:45:01 kommer Exp $

#ifndef acdk_lang_sys_core_static_vector_h
#define acdk_lang_sys_core_static_vector_h


namespace acdk {
namespace lang {
namespace sys {


/** 
  Static versoin (fixed number of elements)
  of a vector<T>
*/
template <class T, int Size>
class core_static_vector
{
public:
  T _cont[Size];
  int _curpos;
  core_static_vector()
  : _curpos(0)
  {
  }
  core_static_vector(const core_static_vector<T, Size>& other)
  : _cont(other._cont)
  , _curpos(other._curpos)
  {
  }
  T& push_back()
  {
    return _cont[_curpos++];
  }
  int size() const { return _curpos; }
  typedef const T* const_iterator;
  typedef T* iterator;
  const_iterator begin() const { return &_cont[0]; }
  const_iterator end() const { return &_cont[_curpos]; }
  iterator begin() { return &_cont[0]; }
  iterator end() { return &_cont[_curpos]; }
};

} // sys
} // lang
} // acdk


#endif //acdk_lang_sys_core_static_vector_h

