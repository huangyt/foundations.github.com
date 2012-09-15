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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/lang/dmi/DmiProxy.h,v 1.22 2005/04/09 19:26:52 kommer Exp $

#ifndef acdk_lang_dmi_DmiProxy_h
#define acdk_lang_dmi_DmiProxy_h

#include <acdk/lang/ref/WeakReference.h>

namespace acdk {
namespace lang {
namespace dmi {



ACDK_DECL_CLASS(DmiProxy);

/** 
  @deprecated DmiProxy
  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:52 $
*/

foreign
class ACDK_CORE_PUBLIC DmiProxy
: extends ::acdk::lang::Object
{
protected:
  RObject _dmiserver;
  DmiClient& _dmiClient;
public:
  DmiProxy()
  : _dmiserver(Nil)
  , _dmiClient(AcdkDmiClient::getDmiClient())
  {
  }
  DmiProxy(IN(::acdk::lang::RObject) dmiserver)
  : _dmiserver(dmiserver)
  , _dmiClient(AcdkDmiClient::getDmiClient())
  {
  }
  // implemented from StdDispatch
  foreign virtual Object* getDmiTarget(OUT(bool) forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci)
  {
    forwarded = true;
    return _dmiserver.operator->();
  }
  
  DmiClient& getDmiClient() { return _dmiClient; }
  void setDmiClient(const DmiClient& dc) { _dmiClient = dc; } // will not work, because const reference
};


/** 
  DmiProxy is the base class for DMI proxys for interfaces,
  which enables to implement interfaces with DMI server implemenation.
  If an interface not tagged with the final attribute 
  the acdkmc generated the proxy.
  The proxy delegates all interface calls via invoke to the
  given DMI Server delegate object.

  @author Roger Rene Kommer (kommer@artefaktur.com)
  @version $Revision: 1.22 $
  @date $Date: 2005/04/09 19:26:52 $
*/

foreign
class ACDK_CORE_PUBLIC DmiProxyBase
{
public:
#if defined(ACDK_USE_WEAK_PROXY_BASE)
  acdk::lang::ref::RWeakReference _dmiTarget;
#else
  acdk::lang::RObject _dmiTarget;
#endif
  DmiClient& _dmiClient;
  
public:
  DmiProxyBase()
  : _dmiTarget(Nil)
  , _dmiClient(AcdkDmiClient::getDmiClient())
  {
  }
  virtual ~DmiProxyBase() {}
  void _initThis(Object* This);
  DmiProxyBase(IN(RObjectArray) proxies, IN(RObject) dmiTarget, int flags, const ::acdk::lang::dmi::ClazzInfo* ci);

  inline bool _dmiProxyIsOverloaded(const ClazzInfo* ci, const ClazzMethodInfo* mi)
  {
#if defined(ACDK_USE_WEAK_PROXY_BASE)
    return _dmiTarget != Nil && _dmiTarget->get() != Nil && _dmiTarget->get()->isDmiOverLoaded(ci, mi);
#else
    return _dmiTarget != Nil && _dmiTarget->isDmiOverLoaded(ci, mi);
#endif
  }
  bool isDmiOverLoaded(const ClazzInfo* ci, const ClazzMethodInfo* mi)
  {
    return _dmiProxyIsOverloaded(ci, mi);
  }
  /**
    Cast underlying dmi proxy
  */
  inline Object* _dmiProxyCast(const ClazzInfo* ci)
  {
#if defined(ACDK_USE_WEAK_PROXY_BASE)
    return (_dmiTarget == Nil || _dmiTarget->get() == Nil) ? 0 :  _dmiTarget->get()->_cast(ci); 
#else
    return (_dmiTarget == Nil) ? 0 :  _dmiTarget->_cast(ci); 
#endif
  }
  /** 
    return valid object or throw NullPointerException
  */
  RObject _dmiProxyGetTarget() 
  {
#if defined(ACDK_USE_WEAK_PROXY_BASE)
    return _dmiTarget->get().operator->();
#else
    return _dmiTarget.operator->();
#endif
  }
  virtual Object* getDmiTarget(OUT(bool) forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci)
  {
#if defined(ACDK_USE_WEAK_PROXY_BASE)
    if (_dmiTarget == Nil || _dmiTarget->get() == Nil)
      return 0;
    forwarded = true;
    return _dmiTarget->get().operator->();
#else
    if (_dmiTarget == Nil)
      return 0;
    forwarded = true;
    return _dmiTarget.operator->();
#endif
  }
  Object* _dmiProxygetDmiTarget(OUT(bool) forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci)
  {
#if defined(ACDK_USE_WEAK_PROXY_BASE)
    if (_dmiTarget == Nil || _dmiTarget->get() == Nil)
      return 0;
    forwarded = true;
    return _dmiTarget->get().operator->();
#else
    if (_dmiTarget == Nil)
      return 0;
    forwarded = true;
    return _dmiTarget.operator->();
#endif
  }
  void setDmiTarget(INP(RObject) target) 
  { 
#if defined(ACDK_USE_WEAK_PROXY_BASE)
    if (target == Nil)
      _dmiTarget = Nil;
    else
      _dmiTarget = new acdk::lang::ref::WeakReference(target); 
#else
    _dmiTarget = target;
#endif
  }
  Object* _cast( const ::acdk::lang::dmi::ClazzInfo* ci);
  
  DmiClient& getDmiClient() { return _dmiClient; }
  void setDmiClient(const DmiClient& dc) { _dmiClient = dc; } // will not work, because const reference
  bool _gc_releaseRef(const Object* This) const;
};

} // dmi
} // lang
} // acdk

using ::acdk::lang::dmi::DmiProxy;

#endif //acdk_lang_dmi_DmiProxy_h

