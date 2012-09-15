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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/ObjectKey.h,v 1.16 2005/02/05 10:45:39 kommer Exp $

#ifndef acdkx_orb_ObjectKey_h
#define acdkx_orb_ObjectKey_h
#include "orb.h"
#include <org/omg/CORBA/portable/ObjectImpl.h>
#include <org/omg/CORBA/IOP/IOP.h>
#include <org/omg/CORBA/GIOP/GIOP.h>

namespace acdkx {
namespace orb {

class ServerDelegate;

ACDK_DECL_CLASS(ObjectKey);

/**
  ObjectKey wrappes the CORBA Object Indentity
  */
class ACDKX_ORB_PUBLIC ObjectKey
: extends ::acdk::lang::Object
{

public:
  /**
    Holds GIOP / IIOP Version 
    */
  org::omg::CORBA::GIOP::Version version;
  org::omg::CORBA::IOP::IOR ior;
  
  bool ior_inited;
  /** the CORBA type id. Nil if not set */
  RString type_id;
  /** the CORBA stringified object reference. Nil if not set */
  RbyteArray object_key;
  
  
  RString protokoll;
  RString network;

  int port;
  /** proces id, which serves this object. -1 if not set */
  int pid;
  
  
  /** in case is local object.
    The instance will be hold by outer ServerDelegate 
  */
  ServerDelegate* localObject;
  
  /**
    This instance is a local object
    localObject can be addressed
  */
  bool _isLocal;
  
  /**
    Object is a ACDK object and 
    understand DMI DII
  */
  bool _isAcdkObject;
  /** in case it has a ProxhyClass */
  SkelInfoClassesStruct* _skel;
  ObjectKey()
  : Object(),
    ior_inited(false),
    port(0),
    pid(-1),
    localObject(0),
    _isLocal(false),
    _isAcdkObject(false),
    _skel(0)
  {
    version.major = 1;
    version.minor =2;
  }
  
  /*
  ObjectKey(RString prot, RString net, int theport, RObject obj)
  : Object(),
    ior(),
    ior_inited(false),
    protokoll(prot),
    network(net),
    port(theport),
    pid(-1),
    localObject(obj),
    _isLocal(true),
    _skel(0)
  {
    //classname = obj->getClass()->getName();
    pid = Process::getProcessId();
  }
  */
  /** server side implementation */
  ObjectKey(ServerDelegate* impl); 

  /** client side */
  ObjectKey(IN(RString) str);
  ObjectKey(::org::omg::CORBA::GIOP::MessageHeader& messageHeader, ::org::omg::CORBA::GIOP::RequestHeader& requestHeader);
  RString toString()
  {
    return object_to_string();
  }
  RObject getLocalObject();
  bool isLocal()
  {
    return _isLocal;
  }
  /// returns true if the underlying reference is Nil
  bool isNil();
  static RObjectKey string_to_object(IN(RString) str);
  RString object_to_string();
  
  /** set information from allready set object_key */
  void fromObjectKey();
  /** write information from information into object_key */
  void toObjectKey();
  void fromInterIOPData(sequence<octet>& profile_data);

  /** set information from already readed IOR */
  void fromIOR();
  /** write information from information into IOR */
  void  toIOR();
  org::omg::CORBA::IOP::IOR& getIOR()
  {
    toIOR();
    return ior;
  }
  RbyteArray objectId()
  {
    return object_key;
  }
  static RString classNameFromRepId(IN(RString) str);
  static void registerRepId(const acdk::lang::dmi::ClazzInfo* ci, const char* name, short major, short minor);
  
};


inline
RbyteArray
core_octet_array_to_byteArray(sequence<octet>& seq)
{
  int length = seq.size();
  RbyteArray ch = new byteArray(length);
  for (int i = 0; i < length; i++)
    ch[i] = seq[i];
  return ch;
}

inline
void
byteArray_to_core_octet_array(IN(RbyteArray) ch, sequence<octet>& seq)
{
  int length = ch->length();
  seq.ensureSize(length);
  for (int i = 0; i < length; i++)
    seq[i] = ch[i];
}

struct RegisterRepId
{
  RegisterRepId(const acdk::lang::dmi::ClazzInfo* ci, const char* name, short major, short minor)
  {
    ObjectKey::registerRepId(ci, name, major, minor);
  }
};




} // namespace orb
} // namespace acdkx

#endif //acdkx_orb_ObjectKey_h
