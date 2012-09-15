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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/core_smartptr.h,v 1.7 2005/04/25 13:19:34 kommer Exp $

#ifndef acdk_lang_sys_core_smartptr_h
#define acdk_lang_sys_core_smartptr_h

#if !defined(DOXYGENONLY)

namespace acdk {
namespace lang {
namespace sys {
/**
  a very simple (not so) smart pointer
  to delete a pointer after leaving scope
*/
template <class T> 
class core_scoped_ptr
{
   T* _ptr;
public:
   core_scoped_ptr(T* ptr = 0)
   : _ptr(0)
   {
   }
   ~core_scoped_ptr()
   {
      if (_ptr)
         delete _ptr;
   }
   core_scoped_ptr& operator=(T* ptr)
   {
      if (_ptr)
         delete _ptr;
      _ptr = ptr;
      return *this;
   }
   T* operator->() { return _ptr; }
   operator T*() { return _ptr; }
   T* operator&() { return _ptr; }
   bool operator==(T* p) const { return p == _ptr; }
};

}
}
}

#endif //!defined(DOXYGENONLY)
#endif //acdk_lang_sys_core_smartptr_h

