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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/arb/AObjectImpl.h,v 1.11 2005/02/05 10:45:39 kommer Exp $

#ifndef acdkx_arb_AObjectImpl_h
#define acdkx_arb_AObjectImpl_h

#include <acdk.h>
#include <acdk/lang/Process.h>

#include "arb.h"
#include "ADelegate.h"

namespace acdkx {
namespace arb {

ACDK_DECL_INTERFACE(ADelegate);
ACDK_DECL_CLASS(ObjectID);

/**
  The format of of the id is:
  protokoll:network:port:pid:class:object
*/

class ACDKX_ORB_PUBLIC ObjectID
: extends ::acdk::lang::Object
{

public:
  RString protokoll;
  RString network;
  int port;
  int pid;
  RString classname;
  /** */
  RString object;

  ProxyClassesStruct* _proxy;
  ObjectID(IN(RString) prot, IN(RString) net, int theport, IN(RObject) obj)
  : Object(),
    protokoll(prot),
    network(net),
    port(theport),
    pid(-1),
    classname(),
    object(Integer::toString((int)obj.impl())),
    _proxy(0)
  {
    classname = obj->getClass()->getName();
    pid = Process::getProcessId();
  }
  ObjectID(IN(RString) str);
  
  RString toString()
  {
    RString erg = protokoll + ":" + network + ":" + Integer::toString(port) + ":" + Integer::toString(pid) + ":" + classname + ":" + object;
    
    return erg;
  }
  RObject getLocalObject()
  {
    return (Object*)Integer::parseInt(object);
  }
};

ACDK_DECL_INTERFACE(ArbInterface);

class ACDKX_ARB_PUBLIC ArbInterface
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ArbInterface)
public:
  virtual const ::acdk::lang::dmi::ClazzMethodInfo* orbDispatch(const char* fname, ::acdk::lang::dmi::ScriptVarArray& args, 
                                                    ::acdk::lang::dmi::ScriptVarArray& ergs, 
                                                    ::acdk::lang::dmi::ScriptVar& ex,
                                                    const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0)
  {
    return 0;
  }
  
}; 

ACDK_DECL_CLASS(AObjectImpl);

class ACDKX_ARB_PUBLIC AObjectImpl
: extends ::acdk::lang::Object
{
protected:
  RObject _localImpl;
  RObjectID _objID;
  RADelegate _delegate;
  AObjectImpl();
public:
  
  AObjectImpl(IN(RObject) limpl);
  AObjectImpl(IN(RObjectID) objid);
  bool isLocal()
  {
    if (_localImpl != Nil)
      return true;
    if (_objID->pid == Process::getProcessId()) {
      _localImpl = _objID->getLocalObject();
      return true;
    }
    return false;
  }
  Object* localImpl() 
  {
    return _localImpl.impl();
  }
  RObjectID objID() { return _objID; }
  void setObjID(IN(RObjectID) objid) { _objID = objid; }
  
  RADelegate delegater();
  void setDelegater(IN(RADelegate) del);

  static const ::acdk::lang::dmi::ClazzMethodInfo* lookupMethod(const char* fname, ::acdk::lang::dmi::ScriptVarArray& args, const ::acdk::lang::dmi::ClazzInfo* clazz);
  
  virtual const ::acdk::lang::dmi::ClazzMethodInfo* orbDispatch(const char* fname, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::ScriptVarArray& ergs, const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0)
  {
    return 0;
  }
};

} // namespace arb 
} // namespace acdkx 

#endif //acdkx_arb_AObjectImpl_h

