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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/acdkx/orb/AORB.h,v 1.21 2005/02/05 10:45:39 kommer Exp $

#ifndef acdkx_orb_AORB_h
#define acdkx_orb_AORB_h

#include <acdk.h>

#include <acdk/lang/ThreadGroup.h>
#include <acdk/io/Reader.h>
#include <acdk/io/Writer.h>
#include <acdk/util/HashMap.h>
#include <acdk/net/Socket.h>
#include <acdk/net/InetAddress.h>
#include <acdk/net/ServerSocket.h>
#include "orb.h"
#include <org/omg/CORBA/ORB.h>
#include "ObjectKey.h"


namespace acdkx {
namespace orb {

USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, Writer);
USING_CLASS(::acdk::util::, HashMap);
USING_CLASS(::acdk::net::, Socket);
USING_CLASS(::acdk::net::, InetAddress);
USING_CLASS(::org::omg::CORBA::, ORB);
USING_CLASS(::org::omg::CORBA::, InvalidName);

ACDK_DECL_CLASS(AServerRequestImpl);
ACDK_DECL_CLASS(ServerDelegate);

ACDK_DECL_CLASS(AORB);

const int StdOrbPort = 2765;

class ACDKX_ORB_PUBLIC AORB 
: extends ::acdk::lang::Thread,
  implements ::org::omg::CORBA::ORB
{
  ACDK_WITH_METAINFO(AORB)
private:
  RString _serverHost;
  int _port;
  /**
    key: ::acdk::lang::RObject
    value: RObjectKey
  */
  RHashMap _objects;
  /**
    key: RcharArray object_id;
    value: RObjectKey
  */
  RHashMap _objectsIds;
  bool _shutdown;
  RThreadGroup _treadGroup;
  static RAORB _theORB;
  
  bool _isServer;
  /** ### test only */
  RObject _theObject;
  ::org::omg::CORBA::GIOP::Version _giopVersion;

  ::acdk::net::RServerSocket _serverSocket;
  ::acdk::util::RProperties _properties;
public:
  /** always use network */
  bool ignoreLocal;
  /** don't use DMI over GIOP */
  bool ignoreDmiOverGIOP;

  AORB();
  static AORB& getAORB();
  static ::org::omg::CORBA::RORB getORB()
  {
    return &getAORB();
  }
  /**
    initialize this orb.
    This method also reads the file $ACDKHOME/cfgs/acdkx_orb.cfg
    @param args commandline arguments
    @param props properties which used as basic initialization
                 and also will return new set values
  */
  void init(IN(RStringArray) args, IN(acdk::util::RProperties) props);

  /** reset ORB deletes ORB and set it up */
  static void reset();
  /**
    return the port number of the local server 
    */
  int port();
  RString serverHost() { return _serverHost; }
  void setGIOPVersion(int major, int minor)
  {
    _giopVersion.major = major;
    _giopVersion.minor = minor;
  }
  virtual RStringArray list_initial_services();
      // Initial reference operation
  virtual ::org::omg::CORBA::RObject resolve_initial_references(IN(RString) identifier) THROWS1(RInvalidName);
  
  virtual void connect(IN(::org::omg::CORBA::RObject) obj);
  
  /// single threaded not supported
  virtual bool work_pending();
  /// single threaded not supported
  virtual void perform_work();
  /**
    run the ORB-Server. Will not return.
    use ORB::start() to start server in background
  */
  virtual void run();
  /**
    run the ORB server in background
  */
  virtual void start()
  {
    Thread::start();
  }
  /**
    stop reading Messages
  */
  virtual void shutdown(bool wait_for_completion);
  /**
    If the ORB was shutdown, it restarts here
  */
  virtual void destroy();
  
  /** nonstandard register object for testing */
  virtual RString impl_is_ready(IN(::acdk::lang::RObject) obj);

  bool _doShutdown() { return _shutdown; }

  RObject resolve_object(AServerRequestImpl& req);
  RObject object_key_to_object(sequence<octet>& object_key);

   
  virtual RString object_to_string(IN(::org::omg::CORBA::RObject) obj);
  virtual ::org::omg::CORBA::RObject string_to_object(IN(RString) str);
  ::org::omg::CORBA::RObject corbaloc_to_object(IN(RString) str);

  static const acdk::lang::dmi::ClazzMethodInfo* lookupMethod(IN(RString) method, const dmi::ClazzInfo* clazz);

  /** returns the local host */
  static RString getLocalHost(); 
  /** returns the local Server Port */
  static int getLocalPort();
  static int _lookupFreePort();
  static RServerDelegate createProxy(IN(RObjectKey) key);
  bool isOwnObjectId(IN(RObjectKey) key);
private:
  /** initialize the ORB Server */
  void _initServer();
  /*
  RTypeCode create_abstract_interface_tc(String id, String name);

  
  RTypeCode create_alias_tc(RString id, RString name, RTypeCode original_type);

  Any create_any();

  RTypeCode create_array_tc(int length, RTypeCode element_type);

  RDynAny create_basic_dyn_any(RTypeCode type);
                          
  ContextList create_context_list();

  RDynAny create_dyn_any(RAny value);
  RDynArray create_dyn_array(RTypeCode type);
  RDynEnum create_dyn_enum(RTypeCode type);
  RDynSequence create_dyn_sequence(RTypeCode type);
  RDynStruct create_dyn_struct(RTypeCode type);
  RDynUnion create_dyn_union(TypeCode type);
  RTypeCode create_enum_tc(RString id, RString name, RStringArray members);
  REnvironment create_environment();
  RExceptionList create_exception_list();
  RTypeCode create_exception_tc(RString id, RString name, RStructMemberArray members);
  TypeCode create_fixed_tc(short digits, short scale) 
  RTypeCode create_interface_tc(RString id, RString name);
  RNVList create_list(int count);
  RNamedValue create_named_value(RString s, RAny any, int flags);
  TypeCode create_native_tc(String id, String name);
  RNVList create_operation_list(org::omg::corba::RObject oper);
  
  OutputStream create_output_stream();

  RPolicy create_policy(int type, RAny val);
  RTypeCode create_recursive_tc(RString id);
  RTypeCode create_sequence_tc(int bound, RTypeCode element_type);
  RTypeCode create_string_tc(int bound);
  RTypeCode create_struct_tc(RString id, RString name, RStructMemberArray members);
  RTypeCode create_union_tc(RString id, RString name,
                             RTypeCode discriminator_type,
                             UnionMemberArray members);
  
  TypeCode create_value_box_tc(String id, String name, TypeCode boxed_type);
                          
  TypeCode create_value_tc(String id, String name, short type_modifier,
                            TypeCode concrete_base, RValueMemberArray members) 
                          
  abstract TypeCode create_wstring_tc(int bound);
  
  void disconnect(org::omg::corba::RObject obj);

  RContext get_default_context();

  // Java: abstract
  // Not yet implemented.
  RRequest get_next_response();

  // Java only:
  abstract TypeCode get_primitive_tc(TCKind tcKind);

  // Java only:
  //boolean get_service_information(short service_type, ServiceInformationHolder service_info) 
  static RORB init();

  
  // Java: abstract
  RObjectArrayImpl<acdk::lang::RString> list_initial_services();
  
  // Java: abstract
  RString object_to_string(org::omg::corba::RObject obj);

  // Java only:
  void perform_work();

  // Java: abstract
  bool poll_next_response();

  // Java: abstract
  org::omg::corba::RObject resolve_initial_references(RString object_name);

  void run();

  // Java: abstract
  // Not yet implemented.
  // void send_multiple_requests_deferred(RObjectArrayImpl<RRequest> req);

  // Java: abstract
  // Not yet implemented.
  // void send_multiple_requests_oneway(RObjectArrayImpl<RRequest> req); 

  // Java only:
  void shutdown(bool wait_for_completion);

  // Java: abstract
  org::omg::corba::RObject string_to_object(RString str);

  // Java only:
  bool work_pending();
  */
           
};


} // namespace orb
} // namespace acdkx

#endif //acdkx_orb_AORB_h
