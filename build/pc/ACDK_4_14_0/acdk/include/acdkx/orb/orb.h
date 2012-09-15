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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/orb.h,v 1.22 2005/02/05 10:45:39 kommer Exp $

#ifndef acdkx_orb_orb_h
#define acdkx_orb_orb_h

#include <acdk.h>

#include "Config.h"

#include <acdk/net/ServerSocket.h>
#include <acdk/net/Socket.h>

ACDK_DECL_UNIT(acdkx_orb)

namespace acdkx {
/** 
  An implementation of an ORB for ACDK
  */
namespace orb {

/*
template <class T>
class Holder
: extends ::acdk::lang::Object
{
public:
  T& _value;
  Holder(const T& val)
  : Object(),
    _value(const_cast<T&>(val))
  {
  }
  T& value() { return _value; }
  operator T& () { return _value; }
  operator const T& () const { return _value; }

  Holder<T>& operator=(const T& v) { _value = v; return *this; }
  
};

template <class T> 
class RHolder
: public RefHolder<Holder<T> >
{
public:
  RHolder(const T& val)
  : RefHolder<Holder<T> >(new Holder<T>(val))
  {
  }
  RHolder<T>& operator=(const T& t) { iptr()->value() = t;  return *this; }
  operator T& () { return iptr()->value(); }
  T& value() { return iptr()->value(); }
};
*/


#define readonly // ## global
#define oneway // ## global

/* doesn't work, because meta info is needed
#define ACDK_DECL_DEFINE_STDEXCEPTION(ClassName, SuperName, Export) \
class ClassName; \
typedef ::ThrowableHolder<ClassName, R##SuperName> R##ClassName; \
typedef ::ObjectArrayImpl<R##ClassName> ClassName##Array; \
typedef ::RObjectArrayImpl<R##ClassName> R##ClassName##Array; \
class Export ClassName \
: implements SuperName \
  { \
public: \
  ClassName() : SuperName() {} \
  ClassName(RString msg) : SuperName(msg) {} \
}
*/
/** used for tagging class as CORBA-interface */

#define ACDK_ARB_INTERFACE(InterfaceName) \
public: \
  static R##InterfaceName GetProxy(IN(::acdkx::arb::RObjectID) objid); \
  virtual ::acdk::lang::RObject getProxy(IN(::acdkx::arb::RObjectID) objid) { return (::acdk::lang::RObject)GetProxy(objid); } \
  static R##InterfaceName GetProxy(IN(::acdk::lang::RObject) localobject); \
  virtual ::acdk::lang::RObject getProxy(IN(::acdk::lang::RObject) localobject) { return (::acdk::lang::RObject)GetProxy(localobject); } \
  virtual ::acdk::lang::dmi::ClazzMethodInfo* orbDispatch(const char* fname, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::ScriptVarArray& ergs, ::acdk::lang::dmi::ScriptVar& ex, ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0);


#define ACDK_CORBA_INTERFACE(InterfaceName) \
public: \
  RString get_typeid() { return GetClass()->getName(); } \
  static R##InterfaceName GetSkel(IN(::acdk::lang::RString) objKey); \
  virtual ::acdk::lang::RObject getSkel(IN(::acdk::lang::RString) objKey) { return (::acdk::lang::RObject)GetSkel(objKey); } \
  static R##InterfaceName GetSkel(IN(::acdk::lang::RObject) localobject); \
  virtual ::org::omg::CORBA::portable::ROutputStream _invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& input, ::org::omg::CORBA::portable::ResponseHandler& handler) THROWS1(::org::omg::CORBA::RSystemException);  

#define SIC_ACDK_CORBA_INTERFACE(InterfaceName) \
public: \
  static R##InterfaceName GetSkel(IN(::acdk::lang::RString) objKey); \
  virtual ::acdk::lang::RObject getSkel(IN(::acdk::lang::RString) objKey) { return (::acdk::lang::RObject)GetSkel(objKey); } \
  static R##InterfaceName GetSkel(IN(::acdk::lang::RObject) localobject); \
  virtual ::acdk::lang::RObject getProxy(IN(::acdk::lang::RObject) localobject) { return (::acdk::lang::RObject)GetSkel(localobject); } \
  virtual ::org::omg::CORBA::portable::ROutputStream _invoke(IN(RString) method, ::org::omg::CORBA::portable::InputStream& input, ::org::omg::CORBA::portable::ResponseHandler& handler) THROWS1(::org::omg::CORBA::RSystemException);  

/**
  used in clases which implements an CORBA interface.

*/
#define ACDK_ORB_IMPL_INTERFACE(interfacename) \
    RString get_typeid() { return interfacename::get_typeid(); }

/**
Usage:
<c>
class MyClass
: extends ::acdkx::orb::ServerDelegate
, implements ::acdkx::orb::selftests::TestInterface 
{
  ACDK_ORB_IMPL_INTERFACE_FQ(::acdk::my::ns::, TestInterface)
};
</c>
*/
#define ACDK_ORB_IMPL_INTERFACE_FQ(ns, interfacename) \
    RString get_typeid() { return ACDK_FQ_SUPER_QUALIFIER(ns, interfacename)::get_typeid(); }

ACDK_DECL_THROWABLE(OrbException, Exception);

class ACDKX_ORB_PUBLIC OrbException
: extends ::acdk::lang::Exception
{ 
  ACDK_WITH_METAINFO(OrbException)  
public: 
  OrbException() : Exception() {} 
  OrbException(IN(RString) msg) : Exception(msg) {} 
};


/** 
  Information about a CORBA interface Skeleton / Proxy.
  This will be write by the acdkx meta compiler, if 
  ACDK_CORBA_INTERFACE is set.
*/
class ACDKX_ORB_PUBLIC SkelInfoClassesStruct
{
public:
  ::acdk::lang::dmi::ClazzInfo* clazz;
  ObjectCreator creator;
  SkelInfoClassesStruct* next;
};


class ACDKX_ORB_PUBLIC RegisterSkelInfoClass
{
  SkelInfoClassesStruct* _proxy;
public:
  RegisterSkelInfoClass(SkelInfoClassesStruct* proxy);
  ~RegisterSkelInfoClass();
};


/**
  Used for DMI over GIOP
  Format: flags [clazzname SerializedObject | Remote Reference | String]
  int flags
*/
enum DmiGiopArgFlags
{
  /**
    No more data is following
  */
  DmiGiopIsNil         = 0x0001, 
  /**
    A CORBA reference is following
  */
  DmiGiopIsReference   = 0x0002,
  /**
    String with the classname following
    after this the serialized class
  */
  DmiGiopIsSerialized  = 0x0004,
  /**
    a corba string is following
  */
  DmiGiopIsString      = 0x0010
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, DmiGiopArgFlags);



} // namespace orb 
} // namespace acdkx 




#endif //acdkx_orb_orb_h

