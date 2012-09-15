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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_value_scope.h,v 1.6 2005/03/17 11:32:08 kommer Exp $

#ifndef acdk_lang_sys_core_value_scope_h
#define acdk_lang_sys_core_value_scope_h

#include "../../Config.h"
#include "core_system.h"

namespace acdk {
namespace lang {
namespace sys {

/**
   helper to wrap a basic type value into a scope
*/
template <class T>
class core_value_scope
{
   T& _valRef;
   T _oldVal;
public:
   core_value_scope(T& val, T newVal)
   : _valRef(val)
   , _oldVal(val)
   {
     _valRef = newVal;
   }
   core_value_scope(T& val)
   : _valRef(val)
   , _oldVal(val)
   {
   }
   ~core_value_scope()
   {
      _valRef = _oldVal;
   }
};

template <class T>
class core_setbit_scope 
{
  T& _orgVal;
  T _sicVal;
  T _setBits;
public:
  core_setbit_scope(T& b, T setbits)
  : _orgVal(b)
  , _sicVal(b)
  , _setBits(setbits)
  {
    _orgVal |= setbits;
  }
  ~core_setbit_scope()
  {
    _orgVal &= ~_setBits;
    _orgVal |= (_sicVal & _setBits);
  }
};

/**
  set the bit when leaving scope
*/
template <class T>
class core_setbit_leave_scope 
{
  T& _orgVal;
  T _setBits;
public:
  core_setbit_leave_scope(T& b, T setbits)
  : _orgVal(b)
  , _setBits(setbits)
  {
  }
  ~core_setbit_leave_scope()
  {
    _orgVal |= _setBits;
  }
};

} // namespace sys 
} //namespace lang 
} // namespace acdk 

#endif //acdk_lang_sys_core_value_scope_h

