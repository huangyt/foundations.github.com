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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/OrbExceptions.h,v 1.12 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_CORBA_OrbExceptions_h
#define org_omg_CORBA_OrbExceptions_h

#include <acdk.h>
#include <acdk/lang/RuntimeException.h>
#include <acdkx/orb/orb.h>

#ifdef minor
# undef minor
#endif

namespace org {
namespace omg {
namespace CORBA {

USING_CLASS(::acdk::lang::, RuntimeException);

/*
ACDK_DECL_THROWABLE(AdapterInactive, SystemException);

class ACDKX_ORB_PUBLIC AdapterInactive 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(AdapterInactive)  
public: 
  AdapterInactive() : SystemException() {} 
  AdapterInactive(RString msg) : SystemException(msg) {} 
  AdapterInactive(RString reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};
  */

enum CompletionStatus
{
  COMPLETED_YES, 
  COMPLETED_NO, 
  COMPLETED_MAYBE 
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, CompletionStatus);

ACDK_DECL_THROWABLE(SystemException, RuntimeException);

class ACDKX_ORB_PUBLIC SystemException 
: extends ::acdk::lang::RuntimeException
{ 
  ACDK_WITH_METAINFO(SystemException)  
protected:
  int _minor;
  CompletionStatus _completed;
  
  
public: 
  SystemException()
  : RuntimeException(),
    _minor(0),
    _completed(COMPLETED_MAYBE)
  {
  } 
  SystemException(IN(RString) msg) 
  : RuntimeException(msg),
    _minor(0),
    _completed(COMPLETED_MAYBE)
  {
  } 
  SystemException(IN(RString) reason, int minor, CompletionStatus completed)
  : RuntimeException(reason),
    _minor(minor),
    _completed(completed)
  {
  }
  virtual RString toString();
  virtual RString getMessage() { return toString(); }
  int minor() { return _minor; }
  int completed() { return (int)_completed; }
  foreign void minor(int min) { _minor = min; }
  foreign void completed(int complet) { _completed = (CompletionStatus)complet; }
};

ACDK_DECL_THROWABLE(COMM_FAILURE, SystemException);

class ACDKX_ORB_PUBLIC COMM_FAILURE 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(COMM_FAILURE)  
public: 
  COMM_FAILURE() : SystemException() {} 
  COMM_FAILURE(int minor, CompletionStatus completed) : SystemException("COMM_FAILURE", minor, completed) { }
  COMM_FAILURE(IN(RString) msg) : SystemException(msg) {} 
  COMM_FAILURE(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};




USING_CLASS(::acdkx::orb::, OrbException);

ACDK_DECL_THROWABLE(InvalidName, OrbException);

class ACDKX_ORB_PUBLIC InvalidName 
: extends ::acdkx::orb::OrbException
{ 
  ACDK_WITH_METAINFO(InvalidName)  
public: 
  InvalidName() : OrbException() {} 
  InvalidName(RString msg) : OrbException(msg) {} 
};



ACDK_DECL_THROWABLE(NO_IMPLEMENT, SystemException);

class ACDKX_ORB_PUBLIC NO_IMPLEMENT 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(NO_IMPLEMENT)  
public: 
  NO_IMPLEMENT() : SystemException() {} 
  NO_IMPLEMENT(int minor, CompletionStatus completed) : SystemException("NO_IMPLEMENT", minor, completed) { }
  NO_IMPLEMENT(IN(RString) msg) : SystemException(msg) {} 
  NO_IMPLEMENT(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(BAD_OPERATION, SystemException);

class ACDKX_ORB_PUBLIC BAD_OPERATION 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(BAD_OPERATION)  
public: 
  BAD_OPERATION() : SystemException() {} 
  BAD_OPERATION(int minor, CompletionStatus completed) : SystemException("BAD_OPERATION", minor, completed) { }
  BAD_OPERATION(IN(RString) msg) : SystemException(msg) {} 
  BAD_OPERATION(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(UNKNOWN, SystemException);

class ACDKX_ORB_PUBLIC UNKNOWN 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(UNKNOWN)  
public: 
  UNKNOWN() : SystemException() {} 
  UNKNOWN(int minor, CompletionStatus completed) : SystemException("UNKNOWN", minor, completed) { }
  UNKNOWN(IN(RString) msg) : SystemException(msg) {} 
  UNKNOWN(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(MARSHAL, SystemException);

class ACDKX_ORB_PUBLIC MARSHAL 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(MARSHAL)  
public: 
  MARSHAL() : SystemException() {} 
  MARSHAL(int minor, CompletionStatus completed) : SystemException("MARSHAL", minor, completed) { }
  MARSHAL(IN(RString) msg) : SystemException(msg) {} 
  MARSHAL(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};



ACDK_DECL_THROWABLE(BAD_CONTEXT, SystemException);

class ACDKX_ORB_PUBLIC BAD_CONTEXT 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(BAD_CONTEXT)  
public: 
  BAD_CONTEXT() : SystemException() {} 
  BAD_CONTEXT(int minor, CompletionStatus completed) : SystemException("BAD_CONTEXT", minor, completed) { }
  BAD_CONTEXT(IN(RString) msg) : SystemException(msg) {} 
  BAD_CONTEXT(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(NO_MEMORY, SystemException);

class ACDKX_ORB_PUBLIC NO_MEMORY 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(NO_MEMORY)  
public: 
  NO_MEMORY() : SystemException() {} 
  NO_MEMORY(int minor, CompletionStatus completed) : SystemException("NO_MEMORY", minor, completed) { }
  NO_MEMORY(IN(RString) msg) : SystemException(msg) {} 
  NO_MEMORY(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};



ACDK_DECL_THROWABLE(NO_PERMISSION, SystemException);

class ACDKX_ORB_PUBLIC NO_PERMISSION 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(NO_PERMISSION)  
public: 
  NO_PERMISSION() : SystemException() {} 
  NO_PERMISSION(int minor, CompletionStatus completed) : SystemException("NO_PERMISSION", minor, completed) { }
  NO_PERMISSION(IN(RString) msg) : SystemException(msg) {} 
  NO_PERMISSION(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};
ACDK_DECL_THROWABLE(NO_RESOURCES, SystemException);

class ACDKX_ORB_PUBLIC NO_RESOURCES 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(NO_RESOURCES)  
public: 
  NO_RESOURCES() : SystemException() {} 
  NO_RESOURCES(int minor, CompletionStatus completed) : SystemException("NO_RESOURCES", minor, completed) { }
  NO_RESOURCES(IN(RString) msg) : SystemException(msg) {} 
  NO_RESOURCES(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};


ACDK_DECL_THROWABLE(NO_RESPONSE, SystemException);

class ACDKX_ORB_PUBLIC NO_RESPONSE 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(NO_RESPONSE)  
public: 
  NO_RESPONSE() : SystemException() {} 
  NO_RESPONSE(int minor, CompletionStatus completed) : SystemException("NO_RESPONSE", minor, completed) { }
  NO_RESPONSE(IN(RString) msg) : SystemException(msg) {} 
  NO_RESPONSE(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(OBJ_ADAPTER, SystemException);

class ACDKX_ORB_PUBLIC OBJ_ADAPTER 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(OBJ_ADAPTER)  
public: 
  OBJ_ADAPTER() : SystemException() {} 
  OBJ_ADAPTER(int minor, CompletionStatus completed) : SystemException("OBJ_ADAPTER", minor, completed) { }
  OBJ_ADAPTER(IN(RString) msg) : SystemException(msg) {} 
  OBJ_ADAPTER(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};


ACDK_DECL_THROWABLE(OBJECT_NOT_EXIST, SystemException);

class ACDKX_ORB_PUBLIC OBJECT_NOT_EXIST 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(OBJECT_NOT_EXIST)  
public: 
  OBJECT_NOT_EXIST() : SystemException() {} 
  OBJECT_NOT_EXIST(int minor, CompletionStatus completed) : SystemException("OBJECT_NOT_EXIST", minor, completed) { }
  OBJECT_NOT_EXIST(IN(RString) msg) : SystemException(msg) {} 
  OBJECT_NOT_EXIST(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(PERSIST_STORE, SystemException);

class ACDKX_ORB_PUBLIC PERSIST_STORE 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(PERSIST_STORE)  
public: 
  PERSIST_STORE() : SystemException() {} 
  PERSIST_STORE(int minor, CompletionStatus completed) : SystemException("PERSIST_STORE", minor, completed) { }
  PERSIST_STORE(IN(RString) msg) : SystemException(msg) {} 
  PERSIST_STORE(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(TRANSACTION_REQUIRED, SystemException);

class ACDKX_ORB_PUBLIC TRANSACTION_REQUIRED 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(TRANSACTION_REQUIRED)  
public: 
  TRANSACTION_REQUIRED() : SystemException() {} 
  TRANSACTION_REQUIRED(int minor, CompletionStatus completed) : SystemException("TRANSACTION_REQUIRED", minor, completed) { }
  TRANSACTION_REQUIRED(IN(RString) msg) : SystemException(msg) {} 
  TRANSACTION_REQUIRED(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(TRANSACTION_ROLLEDBACK, SystemException);

class ACDKX_ORB_PUBLIC TRANSACTION_ROLLEDBACK 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(TRANSACTION_ROLLEDBACK)  
public: 
  TRANSACTION_ROLLEDBACK() : SystemException() {} 
  TRANSACTION_ROLLEDBACK(int minor, CompletionStatus completed) : SystemException("TRANSACTION_ROLLEDBACK", minor, completed) { }
  TRANSACTION_ROLLEDBACK(IN(RString) msg) : SystemException(msg) {} 
  TRANSACTION_ROLLEDBACK(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(TRANSIENT, SystemException);

class ACDKX_ORB_PUBLIC TRANSIENT 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(TRANSIENT)  
public: 
  TRANSIENT() : SystemException() {} 
  TRANSIENT(int minor, CompletionStatus completed) : SystemException("TRANSIENT", minor, completed) { }
  TRANSIENT(IN(RString) msg) : SystemException(msg) {} 
  TRANSIENT(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};


ACDK_DECL_THROWABLE(INV_OBJREF, SystemException);

class ACDKX_ORB_PUBLIC INV_OBJREF 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(INV_OBJREF)  
public: 
  INV_OBJREF() : SystemException() {} 
  INV_OBJREF(int minor, CompletionStatus completed) : SystemException("INV_OBJREF", minor, completed) { }
  INV_OBJREF(IN(RString) msg) : SystemException(msg) {} 
  INV_OBJREF(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(BAD_PARAM, SystemException);

class ACDKX_ORB_PUBLIC BAD_PARAM 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(BAD_PARAM)  
public: 
  BAD_PARAM() : SystemException() {} 
  BAD_PARAM(int minor, CompletionStatus completed) : SystemException("BAD_PARAM", minor, completed) { }
  BAD_PARAM(IN(RString) msg) : SystemException(msg) {} 
  BAD_PARAM(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};

ACDK_DECL_THROWABLE(BAD_INV_ORDER, SystemException);

class ACDKX_ORB_PUBLIC BAD_INV_ORDER 
: extends ::org::omg::CORBA::SystemException
{ 
  ACDK_WITH_METAINFO(BAD_INV_ORDER)  
public: 
  BAD_INV_ORDER() : SystemException() {} 
  BAD_INV_ORDER(int minor, CompletionStatus completed) : SystemException("BAD_INV_ORDER", minor, completed) { }
  BAD_INV_ORDER(IN(RString) msg) : SystemException(msg) {} 
  BAD_INV_ORDER(IN(RString) reason, int minor, CompletionStatus completed) : SystemException(reason, minor, completed) { }
};





} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_OrbExceptions_h
