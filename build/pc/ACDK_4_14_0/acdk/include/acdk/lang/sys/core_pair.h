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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_pair.h,v 1.4 2005/02/05 10:45:00 kommer Exp $

#ifndef acdk_lang_sys_core_pair_h
#define acdk_lang_sys_core_pair_h

#include "../../Config.h"

namespace acdk {
namespace lang {
namespace sys {

/**
  Duplicate of std::pair struct
*/
template <class First, class Second>
struct core_pair 
{
  typedef First first_type;
  typedef Second second_type;
  First first;
  Second second;
  core_pair() { }
  core_pair(const First& f, const Second& s)
  : first(f)
  , second(s)
  {
  }
  core_pair(const core_pair<first_type, second_type>& other)
  : first(other.first)
  , second(other.second)
  {
  }
};

template <class First, class Second>
inline
core_pair<First, Second>
core_make_pair(const First& f, const Second& s)
{
  return core_pair<First, Second>(f, s);
}

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_pair_h

