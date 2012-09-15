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
// $Header: /cvsroot/acdk/acdk/acdkx_rdmi/src/acdkx/rdmi/LocalObjectRepository.h,v 1.1 2005/02/28 08:28:58 kommer Exp $

#ifndef acdkx_rdmi_LocalObjectRepository_h
#define acdkx_rdmi_LocalObjectRepository_h

#include "Connection.h"

#include <acdk/util/THashMap.h>
#include <acdk/lang/Integer.h>

namespace acdkx {
namespace rdmi {

ACDK_DECL_CLASS(RemoteObjectId);

class ACDKX_RDMI_LIB_PUBLIC RemoteObjectId
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(RemoteObjectId)
public:
  int _id;
  RemoteObjectId(int id = 0)
    : _id(id) 
  {
  }
  virtual int hashCode() { return _id; }
  virtual bool equals(IN(RRemoteObjectId) o)
  {
    if (o == Nil)
      return false;
    return _id == o->_id;
  }
  virtual bool equals(IN(RObject) o)
  {
    if (instanceof(o, RemoteObjectId) == false)
      return false;
    return equals(RRemoteObjectId(o));
  }
  RString toString() { return Integer(_id).toString(); }
  static RRemoteObjectId createId(IN(RObject) o);
  foreign static RRemoteObjectId getId(IN(acdk::lang::dmi::ScriptVar) sv);
};

ACDK_DECL_CLASS(ConnectionRefs);

class ACDKX_RDMI_LIB_PUBLIC ConnectionRefs
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(ConnectionRefs)
public:
  int _count;
  RConnection _connection;
  
  ConnectionRefs(IN(RConnection) con, int count = 0)
  : _count(count)
  , _connection(con)
  {
  }
};

ACDK_DECL_CLASS(RemoteObjectReferences);

class ACDKX_RDMI_LIB_PUBLIC RemoteObjectReferences
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(RemoteObjectReferences)
public:
  RObject _object;
  /** how many persitent reference are hold */
  int _persitentCount;
  /** 
    The connections, which referes
    to this object
  */
  RConnectionRefsArray _connections;

  RemoteObjectReferences(IN(RObject) o)
    : _object(o)
    , _persitentCount(0)
    , _connections(new ConnectionRefsArray(0))
  {
  }
};


ACDK_DECL_CLASS(LocalObjectRepository);
/**
  The LocalObjectRepository tracks all local objects, which
  are referenced by remote clients
*/
class ACDKX_RDMI_LIB_PUBLIC LocalObjectRepository
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(LocalObjectRepository)
public:

  typedef acdk::util::THashMap<RRemoteObjectId, RRemoteObjectReferences> IdentityHashMap;
  /**
    RemoteObject -> RemoteObjectReferences
  */
  IdentityHashMap _map;
  LocalObjectRepository();
  /// get single instance object
  static RLocalObjectRepository get();
  bool hasLocalRef(IN(RRemoteObjectId) id);
  RObject getLocalRef(IN(RRemoteObjectId) id);
  RRemoteObjectId addCreateConnectionRef(IN(RObject) o, IN(RConnection) conn);

  void addConnectionRef(IN(RRemoteObjectId) oid, IN(RConnection) conn);
  void releaseConnectionRef(IN(RObject) o, IN(RConnection) conn);
  void releaseConnectionRef(IN(RRemoteObjectId) oid, IN(RConnection) conn);
  void addPersistentRef(IN(RRemoteObjectId) o);
  void releasePersistentRef(IN(RRemoteObjectId) o);

  void disconnetConnectionObjects(IN(RConnection) conn);

  RRemoteObjectReferences _getExistant(IN(RRemoteObjectId) roi);
};

} // namespace rdmi 
} // namespace acdkx


#endif // acdkx_rdmi_LocalObjectRepository_h
