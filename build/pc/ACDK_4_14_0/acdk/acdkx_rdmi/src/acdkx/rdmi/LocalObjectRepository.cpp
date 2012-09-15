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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/LocalObjectRepository.cpp,v 1.1 2005/02/28 08:28:58 kommer Exp $

#include "LocalObjectRepository.h"

#include <acdk/util/logging/Log.h>

namespace acdkx {
namespace rdmi {

//static 
RLocalObjectRepository 
LocalObjectRepository::get()
{
  static RLocalObjectRepository rep = new LocalObjectRepository();
  return rep;
}

//static 
RRemoteObjectId 
RemoteObjectId::createId(IN(RObject) o)
{
  int rid = (int)o.impl();
  return new RemoteObjectId(rid);
}

//static 
RRemoteObjectId 
RemoteObjectId::getId(IN(acdk::lang::dmi::ScriptVar) sv)
{
  return new RemoteObjectId(sv.getIntVar());
}

LocalObjectRepository::LocalObjectRepository() 
{
}

bool 
LocalObjectRepository::hasLocalRef(IN(RRemoteObjectId) id)
{
  RRemoteObjectReferences ror = _map.get(id);
  if (ror == Nil)
    return false;
  return true;
}

RObject 
LocalObjectRepository::getLocalRef(IN(RRemoteObjectId) id)
{
  RRemoteObjectReferences ror = _map.get(id);
  if (ror == Nil)
    return Nil;
  return ror->_object;
  
}

RRemoteObjectReferences 
LocalObjectRepository::_getExistant(IN(RRemoteObjectId) roi)
{
  RRemoteObjectReferences ror = _map.get(roi);
  if (ror == Nil)
    THROW1(RuntimeException, "ObjectId not found: " + roi->toString());
  return ror;
}


RRemoteObjectId 
LocalObjectRepository::addCreateConnectionRef(IN(RObject) o, IN(RConnection) conn)
{
  RRemoteObjectId roi = RemoteObjectId::createId(o);
  RRemoteObjectReferences ror = _map.get(roi);
  ACDK_NLOG("acdkx.rdmi.LocalObjectRepository", Debug, "Create new ROI [" << roi->toString() 
            << "] on [" << conn->getLocalServerId() << "] for [" << conn->getRemoteServerId() << "]");
  if (ror == Nil)
  {
    ror = new RemoteObjectReferences(o);
    _map.put(roi, ror);
    
  }
  RConnectionRefsArray cons = ror->_connections;
  for (int i = 0; i < cons->length(); ++i)
  {
    RConnectionRefs conrefs = cons[i];
    if (conrefs->_connection == conn)
    {
      conrefs->_count += 1;
      return roi;
    }
  }
  RConnectionRefs conrefs = new ConnectionRefs(conn, 1);
  cons->append(conrefs);
  return roi;
}

void 
LocalObjectRepository::addConnectionRef(IN(RRemoteObjectId) oid, IN(RConnection) conn)
{
  RRemoteObjectReferences ror = _getExistant(oid);
  RConnectionRefsArray cons = ror->_connections;
  for (int i = 0; i < cons->length(); ++i)
  {
    RConnectionRefs conrefs = cons[i];
    if (conrefs->_connection == conn)
    {
      conrefs->_count += 1;
      return;
    }
  }
  RConnectionRefs conrefs = new ConnectionRefs(conn, 1);
  cons->append(conrefs);
}

void 
LocalObjectRepository::releaseConnectionRef(IN(RObject) o, IN(RConnection) conn)
{
  releaseConnectionRef(RemoteObjectId::createId(o), conn);
}

void 
LocalObjectRepository::releaseConnectionRef(IN(RRemoteObjectId) oid, IN(RConnection) conn)
{
  ACDK_NLOG("acdkx.rdmi.LocalObjectRepository", Debug, "Release ROI [" << oid->toString() 
            << "] on [" << conn->getLocalServerId() << "] for [" << conn->getRemoteServerId() << "]");
  RRemoteObjectReferences ror = _getExistant(oid);
  RConnectionRefsArray cons = ror->_connections;
  bool foundConnection = false;
  for (int i = 0; i < cons->length(); ++i)
  {
    RConnectionRefs conrefs = cons[i];
    if (conrefs->_connection == conn)
    {
      conrefs->_count -= 1;
      if (conrefs->_count == 0)
        cons->remove(i);
      foundConnection = true;
      break;
    }
  }
  if (foundConnection == false)
  {
    // ### throw here?
  }
  if (cons->length() == 0)
  {
    _map.remove(oid);
  }
  
}

void 
LocalObjectRepository::addPersistentRef(IN(RRemoteObjectId) o)
{
}

void 
LocalObjectRepository::releasePersistentRef(IN(RRemoteObjectId) o)
{
}

void 
LocalObjectRepository::disconnetConnectionObjects(IN(RConnection) conn)
{
  // release all objects
}

} // namespace rdmi 
} // namespace acdkx


