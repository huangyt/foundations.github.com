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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/sys/SysRefHolder.h,v 1.8 2005/04/08 10:53:20 kommer Exp $

#ifndef acdk_lang_sys_SysRefHolder_h
#define acdk_lang_sys_SysRefHolder_h

#include "sys.h"
#include "RefHolder.h"
#include "core_atomicop.h"

namespace acdk {
namespace lang {

  
namespace sys {

/** 
    SysObject is the base of reference counted Sysobjects
    @author Roger Rene Kommer
    @version $Revision: 1.8 $
    @date $Date: 2005/04/08 10:53:20 $
*/
class ACDK_CORE_PUBLIC SysObject 
{
protected:
   /** The Refernce Conter */
  mutable sys::core_atomicop _refCount;
public:
  SysObject();
  virtual ~SysObject(); 
  void releaseRef() const;
  /** increment reference counter */
  void addRef() const;
  static void _throwNullPointerException();
};


} // namespace acdk 
} // namespace lang 
} // namespace sys 



/** 
    SysRefHolder is quite the same to RefHolder
    @author Roger Rene Kommer
    @version $Revision: 1.8 $
    @date $Date: 2005/04/08 10:53:20 $
*/
template <class T>
class SysRefHolder 
{
protected:
  T* _impl;
public:
  /** default constructors */
  SysRefHolder(NilRef n = Nil) 
  : _impl(0)
  { 
  }

  /** 
  */
  SysRefHolder(T* o)
    : _impl(o)
  {
    if (_impl != 0)
      _impl->addRef();
  }
  SysRefHolder(const SysRefHolder<T>& o)
  : _impl(o.impl())
  {
    if (_impl != 0)
      _impl->addRef();
  }
  ~SysRefHolder() 
  {
    if(_impl != 0) {
      releaseRef();
    }
  }
  SysRefHolder<T>& operator=(T* o)
  {
    if (o == _impl)
      return *this;
    if (_impl != 0)
      releaseRef();
    _impl = o;
    if (_impl != 0)
      _impl->addRef();
    return *this;
  }
  SysRefHolder<T>& operator=(const SysRefHolder<T>& o)
  {
    if (_impl == 0 && o.impl() == 0)
      return *this;
    return operator=((T*)o);
  }
  SysRefHolder<T>& operator=(NilRef nil)
  {
    if (_impl == 0)
      return *this;
    releaseRef();
    return *this;
  }
  void releaseRef()
  {
    ::acdk::lang::sys::SysObject* simpl = _impl;
    _impl = 0;
    simpl->releaseRef();
  }
  bool operator==(NilRef nil) const { return _impl == 0; }
  bool operator!=(NilRef nil) const { return _impl != 0; }
  
  template <class OT> bool operator==(const SysRefHolder<OT>& other) const { return _impl == other.impl(); }
  template <class OT> bool operator!=(const SysRefHolder<OT>& other) const { return _impl != other.impl(); }

  
  inline T* impl() const { return _impl; }
  inline T* getImpl() const 
  {
    if (_impl == 0) 
      ::acdk::lang::sys::SysObject::_throwNullPointerException();
    return _impl;
  }
  
  inline T* operator->() const 
  { 
    if (_impl == 0)
      ::acdk::lang::sys::SysObject::_throwNullPointerException();
    return _impl;
  }
  operator T* () const { return _impl; }
 
};

#define ACDK_DECL_SYS_CLASS(ClassName) \
  class ClassName; \
  typedef ::SysRefHolder<ClassName> R##ClassName


#endif // acdk_lang_sys_SysRefHolder_h

