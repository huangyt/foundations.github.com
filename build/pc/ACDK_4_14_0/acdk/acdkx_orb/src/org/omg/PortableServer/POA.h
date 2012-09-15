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
// $Header: /cvsroot/acdk/acdk/acdkx_orb/src/org/omg/PortableServer/POA.h,v 1.7 2005/02/05 10:45:41 kommer Exp $

#ifndef org_omg_PortableServer_POA_h
#define org_omg_PortableServer_POA_h


#include "../../../acdkx/orb/orb.h"


namespace org {
namespace omg {
namespace PortableServer {

ACDK_DECL_CLASS(POA);
ACDK_DECL_CLASS(Servant);

enum PolicyType
{
  THREAD_POLICY_ID = 16,
  LIFESPAN_POLICY_ID = 17,
  ID_UNIQUENESS_POLICY_ID = 18,
  ID_ASSIGNMENT_POLICY_ID = 19,
  IMPLICIT_ACTIVATION_POLICY_ID = 20,
  SERVANT_RETENTION_POLICY_ID = 21,
  REQUEST_PROCESSING_POLICY_ID = 22
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, PolicyType);

enum ThreadPolicyValue 
{
  ORB_CTRL_MODEL,
  SINGLE_THREAD_MODEL
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, ThreadPolicyValue);


class ACDKX_ORB_PUBLIC ThreadPolicy 
: extends ::acdk::lang::Object
{
//: CORBA::Policy {
public:
  readonly ThreadPolicyValue value;
};
//typedef ThreadPolicyValue ThreadPolicy; 


enum LifespanPolicyValue
{
  TRANSIENT,
  PERSISTENT
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, LifespanPolicyValue);

typedef LifespanPolicyValue  LifespanPolicy;
/*
interface LifespanPolicy : CORBA::Policy {
      readonly attribute LifespanPolicyValue value;
   };*/


enum IdUniquenessPolicyValue
{
  UNIQUE_ID,
  MULTIPLE_ID
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, IdUniquenessPolicyValue);

typedef IdUniquenessPolicyValue IdUniquenessPolicy;
/*
interface IdUniquenessPolicy : CORBA::Policy {
      readonly attribute IdUniquenessPolicyValue value;
   };
*/


enum IdAssignmentPolicyValue
{
  USER_ID,
  SYSTEM_ID
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, IdAssignmentPolicyValue);

/*
   interface IdAssignmentPolicy : CORBA::Policy {
      readonly attribute IdAssignmentPolicyValue value;
   };
*/
enum ImplicitActivationPolicyValue
{
  IMPLICIT_ACTIVATION,
  NO_IMPLICIT_ACTIVATION
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, ImplicitActivationPolicyValue);
/*  
 interface ImplicitActivationPolicy : CORBA::Policy {
      readonly attribute ImplicitActivationPolicyValue value;
   };
*/
enum ServantRetentionPolicyValue 
{
  RETAIN,
  NON_RETAIN
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, ServantRetentionPolicyValue);

/*
interface ServantRetentionPolicy : CORBA::Policy {
      readonly attribute ServantRetentionPolicyValue value;
   };
*/

enum RequestProcessingPolicyValue 
{
  USE_ACTIVE_OBJECT_MAP_ONLY,
  USE_DEFAULT_SERVANT,
  USE_SERVANT_MANAGER
};
ACDK_DEF_LIB_ENUM(ACDKX_ORB_PUBLIC, RequestProcessingPolicyValue);

/*
   interface RequestProcessingPolicy : CORBA::Policy {
      readonly attribute RequestProcessingPolicyValue value;
   };
*/

#if 0

class ACDKX_ORB_PUBLIC AdapterActivator 
: extends ::acdk::lang::Object
{
public:
  virtual bool unknown_adapter(IN(RPOA) parent, IN(RString) name);
};

class ACDKX_ORB_PUBLIC ServantManager
{ 

};

class ACDKX_ORB_PUBLIC ServantActivator 
: extends ServantManager 
{
public:
  virtual RServant incarnate(IN(RObjectId) oid, IN(RPOA) adapter) THROWS1(RForwardRequest);
  virtual void etherealize(IN(RObjectId) oid, IN(RPOA) adapter, IN(RServant) serv, IN(bool) cleanup_in_progress, IN(bool) remaining_activations);
};

class ACDKX_ORB_PUBLIC ServantLocator 
: extends ServantManager 
{
//# pragma version ServantLocator 2.3
  /*native */RString Cookie;
  virtual RServant preinvoke(IN(RObjectId) oid, IN(RPOA) adapter, IN(CORBA::RIdentifier) operation, OUT(RCookie) the_cookie) throw (RForwardRequest);
  virtual void postinvoke(IN(RObjectId) oid, IN(RPOA) adapter, IN(CORBA::RIdentifier) operation, IN(RCookie) the_cookie, IN(RServant) the_servant);
};

/*
ACDK_DECL_DEFINE_STDEXCEPTION(AdapterAlreadyExists, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
ACDK_DECL_DEFINE_STDEXCEPTION(AdapterNonExistent, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
//exception InvalidPolicy {unsigned short index;};
ACDK_DECL_DEFINE_STDEXCEPTION(InvalidPolicy, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
ACDK_DECL_DEFINE_STDEXCEPTION(NoServant, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
ACDK_DECL_DEFINE_STDEXCEPTION(ObjectAlreadyActive, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
ACDK_DECL_DEFINE_STDEXCEPTION(ObjectNotActive, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
ACDK_DECL_DEFINE_STDEXCEPTION(ServantAlreadyActive, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
ACDK_DECL_DEFINE_STDEXCEPTION(ServantNotActive, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
ACDK_DECL_DEFINE_STDEXCEPTION(WrongAdapter, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
ACDK_DECL_DEFINE_STDEXCEPTION(WrongPolicy, ::acdkx::lang::Throwable, ACDKX_ORB_PUBLIC);
*/


class ACDKX_ORB_PUBLIC POA 
{
  //# pragma version POA 2.3
  // POA creation and destruction
  static RPOA create_POA(IN(RString adapter_name), IN(POAManager) a_POAManager, IN(CORBA::RPolicyList) policies) 
    THROWS2(RAdapterAlreadyExists, RInvalidPolicy);
  static RPOA find_POA(IN(RString adapter_name), IN(bool) activate_it) THROWS1(RAdapterNonExistent);
  virtual void destroy(IN(bool) etherealize_objects, IN(bool( wait_for_completion);
  // Factories for Policy objects
  ThreadPolicy create_thread_policy(IN(ThreadPolicyValue) value);
  LifespanPolicy create_lifespan_policy(IN(LifespanPolicyValue) value);
  IdUniquenessPolicy create_id_uniqueness_policy(IN(IdUniquenessPolicyValue) value);
  IdAssignmentPolicy create_id_assignment_policy(IN(IdAssignmentPolicyValue) value);
  ImplicitActivationPolicy create_implicit_activation_policy(IN(ImplicitActivationPolicyValue) value);
  ServantRetentionPolicy create_servant_retention_policy(IN(ServantRetentionPolicyValue) value);
  RequestProcessingPolicy create_request_processing_policy(IN(RequestProcessingPolicyValue) value);
  // POA attributes
  readonly RString the_name;
  readonly RPOA the_parent;
  readonly RPOAList the_children;
  readonly RPOAManager the_POAManager;
  RAdapterActivator the_activator;
  // Servant Manager registration:
  RServantManager get_servant_manager() THROWS1(RWrongPolicy);
  void set_servant_manager( IN(ServantManager) imgr) THROWS1(RWrongPolicy);
  // operations for the USE_DEFAULT_SERVANT policy
  RServant get_servant() THROWS2(RNoServant, RWrongPolicy);
  void set_servant(IN(RServant) p_servant) THROWS1(RWrongPolicy);
  // object activation and deactivation
  ObjectId activate_object(IN(RServant) p_servant) THROWS2(RServantAlreadyActive, RWrongPolicy);
  void activate_object_with_id(IN(RObjectId) id, IN(RServant) p_servant)
        THROWS3(RServantAlreadyActive, RObjectAlreadyActive, RWrongPolicy);
  void deactivate_object(IN(ObjectId) oid) THROWS2(RObjectNotActive, RWrongPolicy);
  // reference creation operations
  RObject create_reference(IN(CORBA::RepositoryId) intf) THROWS1(RWrongPolicy);
  RObject create_reference_with_id(IN(ObjectId oid, IN(CORBA::RepositoryId intf) THROWS1(WrongPolicy);
  // Identity mapping operations:
  ObjectId servant_to_id(IN(Servant p_servant) THROWS2(ServantNotActive, WrongPolicy);
  Object servant_to_reference(IN(Servant p_servant) THROWS2(ServantNotActive, WrongPolicy);
  // Modif: Wrongpolicy -> WrongPolicy
  RServant reference_to_servant(IN(RObject reference) THROWS1(ObjectNotActive, WrongPolicy, RThrowable);
  ObjectId reference_to_id(
    IN(Object reference)
    throw (WrongAdapter, WrongPolicy, RThrowable);
  Servant id_to_servant(IN(ObjectId) oid)
    throw (ObjectNotActive, WrongPolicy, RThrowable);
  Object id_to_reference(IN(ObjectId) oid)
    throw (ObjectNotActive, WrongPolicy, RThrowable);
};

exception NoContext { };

class Current 
: CORBA::Current 
{
  //# pragma version Current 2.3
  
  RPOA get_POA()
    throw (RNoContext, RThrowable);
  ObjectId get_object_id()
    throw (RNoContext, RThrowable);
};

#endif //0

} // namespace PortableServer
} // namespace omg
} // namespace org

#endif //org_omg_PortableServer_POA_h
