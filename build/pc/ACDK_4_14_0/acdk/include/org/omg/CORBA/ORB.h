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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/CORBA/ORB.h,v 1.12 2005/02/05 10:45:40 kommer Exp $

#ifndef org_omg_CORBA_ORB_h
#define org_omg_CORBA_ORB_h

#include <acdk.h>
#include <acdk/util/Properties.h>


#include "OrbExceptions.h"
#include "ORB.h"
#include <acdkx/orb/orb.h>
#include "Object.h"

namespace org {
namespace omg {
namespace CORBA {



ACDK_DECL_INTERFACE(ORB);

class ACDKX_ORB_PUBLIC ORB 
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(ORB)
public:
  
  virtual RStringArray list_initial_services() = 0;
  virtual ::org::omg::CORBA::RObject resolve_initial_references(IN(RString) identifier) THROWS1(RInvalidName) = 0;
  virtual void connect(IN(::org::omg::CORBA::RObject) obj) = 0;
  virtual bool work_pending() = 0;
  virtual void perform_work() = 0;
  /**
    run the ORB-Server. Will not return.
    use ORB::start() to start server in background
  */
  virtual void run() = 0;
   /**
    run the ORB server in background
  */
  virtual void start() = 0;
  virtual void shutdown(bool wait_for_completion) = 0;
  virtual void destroy() = 0;
  static RORB init();
  static RORB init(IN(RStringArray) args, IN(acdk::util::RProperties) props = Nil);

  /** 
    nonstandard register object for testing 
    returns the stringified object id
    */
  virtual RString impl_is_ready(IN(::acdk::lang::RObject) obj) = 0;
  
  /** java standard */
  virtual RString object_to_string(IN(::org::omg::CORBA::RObject) obj) = 0;
  
  /** java standard */
  virtual ::org::omg::CORBA::RObject string_to_object(IN(RString) str) = 0;
  
  
  
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

  static RORB init(RObjectArrayImpl<acdk::lang::RString> args,
                   acdk::util::RProperties props);

  // Java: abstract
  RObjectArrayImpl<acdk::lang::RString> list_initial_services();
  
  
  // Java only:
  void perform_work();

  // Java: abstract
  bool poll_next_response();

  

  void run();

  // Java: abstract
  // Not yet implemented.
  // void send_multiple_requests_deferred(RObjectArrayImpl<RRequest> req);

  // Java: abstract
  // Not yet implemented.
  // void send_multiple_requests_oneway(RObjectArrayImpl<RRequest> req); 

  // Java only:
  void shutdown(bool wait_for_completion);

  

  // Java only:
  bool work_pending();
  */
           
};


} // namespace CORBA
} // namespace omg
} // namespace org 

#endif //org_omg_CORBA_ORB_h
