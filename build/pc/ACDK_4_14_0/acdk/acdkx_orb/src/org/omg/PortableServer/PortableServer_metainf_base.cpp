// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>


#include "POA.h"
#include "POAManager.h"
#include "PortableServer.h"

namespace org { 
namespace omg { 
namespace PortableServer { 


::acdk::lang::dmi::ClazzEnumValueInfo PolicyType_THREAD_POLICY_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "THREAD_POLICY_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::THREAD_POLICY_ID, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo PolicyType_LIFESPAN_POLICY_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "LIFESPAN_POLICY_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::LIFESPAN_POLICY_ID, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo PolicyType_ID_UNIQUENESS_POLICY_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ID_UNIQUENESS_POLICY_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::ID_UNIQUENESS_POLICY_ID, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo PolicyType_ID_ASSIGNMENT_POLICY_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ID_ASSIGNMENT_POLICY_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::ID_ASSIGNMENT_POLICY_ID, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo PolicyType_IMPLICIT_ACTIVATION_POLICY_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "IMPLICIT_ACTIVATION_POLICY_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::IMPLICIT_ACTIVATION_POLICY_ID, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo PolicyType_SERVANT_RETENTION_POLICY_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "SERVANT_RETENTION_POLICY_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::SERVANT_RETENTION_POLICY_ID, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo PolicyType_REQUEST_PROCESSING_POLICY_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "REQUEST_PROCESSING_POLICY_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::REQUEST_PROCESSING_POLICY_ID, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* PolicyType_enumValues[] = {
  &PolicyType_THREAD_POLICY_ID,
  &PolicyType_LIFESPAN_POLICY_ID,
  &PolicyType_ID_UNIQUENESS_POLICY_ID,
  &PolicyType_ID_ASSIGNMENT_POLICY_ID,
  &PolicyType_IMPLICIT_ACTIVATION_POLICY_ID,
  &PolicyType_SERVANT_RETENTION_POLICY_ID,
  &PolicyType_REQUEST_PROCESSING_POLICY_ID,
0
};

::acdk::lang::dmi::ClazzEnumInfo* PolicyTypeMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo PolicyType_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "PolicyType", // name
  -1, // hashCode
  "org/omg/PortableServer", // ns
  0, // _scopeParent
  0, // _nextSibling
  PolicyType_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_PolicyType(&PolicyType_enumInfo);

  return &PolicyType_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_PolicyTypeEnumInfo(PolicyTypeMetaInf::GetEnumInfo());


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 


::acdk::lang::dmi::ClazzEnumValueInfo ThreadPolicyValue_ORB_CTRL_MODEL = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ORB_CTRL_MODEL", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::ORB_CTRL_MODEL, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo ThreadPolicyValue_SINGLE_THREAD_MODEL = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "SINGLE_THREAD_MODEL", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::SINGLE_THREAD_MODEL, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* ThreadPolicyValue_enumValues[] = {
  &ThreadPolicyValue_ORB_CTRL_MODEL,
  &ThreadPolicyValue_SINGLE_THREAD_MODEL,
0
};

::acdk::lang::dmi::ClazzEnumInfo* ThreadPolicyValueMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo ThreadPolicyValue_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "ThreadPolicyValue", // name
  -1, // hashCode
  "org/omg/PortableServer", // ns
  0, // _scopeParent
  0, // _nextSibling
  ThreadPolicyValue_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_ThreadPolicyValue(&ThreadPolicyValue_enumInfo);

  return &ThreadPolicyValue_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_ThreadPolicyValueEnumInfo(ThreadPolicyValueMetaInf::GetEnumInfo());


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 


::acdk::lang::dmi::ClazzEnumValueInfo LifespanPolicyValue_TRANSIENT = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "TRANSIENT", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::TRANSIENT, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo LifespanPolicyValue_PERSISTENT = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "PERSISTENT", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::PERSISTENT, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* LifespanPolicyValue_enumValues[] = {
  &LifespanPolicyValue_TRANSIENT,
  &LifespanPolicyValue_PERSISTENT,
0
};

::acdk::lang::dmi::ClazzEnumInfo* LifespanPolicyValueMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo LifespanPolicyValue_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "LifespanPolicyValue", // name
  -1, // hashCode
  "org/omg/PortableServer", // ns
  0, // _scopeParent
  0, // _nextSibling
  LifespanPolicyValue_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_LifespanPolicyValue(&LifespanPolicyValue_enumInfo);

  return &LifespanPolicyValue_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_LifespanPolicyValueEnumInfo(LifespanPolicyValueMetaInf::GetEnumInfo());


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 


::acdk::lang::dmi::ClazzEnumValueInfo IdUniquenessPolicyValue_UNIQUE_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "UNIQUE_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::UNIQUE_ID, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo IdUniquenessPolicyValue_MULTIPLE_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "MULTIPLE_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::MULTIPLE_ID, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* IdUniquenessPolicyValue_enumValues[] = {
  &IdUniquenessPolicyValue_UNIQUE_ID,
  &IdUniquenessPolicyValue_MULTIPLE_ID,
0
};

::acdk::lang::dmi::ClazzEnumInfo* IdUniquenessPolicyValueMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo IdUniquenessPolicyValue_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "IdUniquenessPolicyValue", // name
  -1, // hashCode
  "org/omg/PortableServer", // ns
  0, // _scopeParent
  0, // _nextSibling
  IdUniquenessPolicyValue_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_IdUniquenessPolicyValue(&IdUniquenessPolicyValue_enumInfo);

  return &IdUniquenessPolicyValue_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_IdUniquenessPolicyValueEnumInfo(IdUniquenessPolicyValueMetaInf::GetEnumInfo());


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 


::acdk::lang::dmi::ClazzEnumValueInfo IdAssignmentPolicyValue_USER_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "USER_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::USER_ID, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo IdAssignmentPolicyValue_SYSTEM_ID = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "SYSTEM_ID", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::SYSTEM_ID, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* IdAssignmentPolicyValue_enumValues[] = {
  &IdAssignmentPolicyValue_USER_ID,
  &IdAssignmentPolicyValue_SYSTEM_ID,
0
};

::acdk::lang::dmi::ClazzEnumInfo* IdAssignmentPolicyValueMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo IdAssignmentPolicyValue_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "IdAssignmentPolicyValue", // name
  -1, // hashCode
  "org/omg/PortableServer", // ns
  0, // _scopeParent
  0, // _nextSibling
  IdAssignmentPolicyValue_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_IdAssignmentPolicyValue(&IdAssignmentPolicyValue_enumInfo);

  return &IdAssignmentPolicyValue_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_IdAssignmentPolicyValueEnumInfo(IdAssignmentPolicyValueMetaInf::GetEnumInfo());


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 


::acdk::lang::dmi::ClazzEnumValueInfo ImplicitActivationPolicyValue_IMPLICIT_ACTIVATION = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "IMPLICIT_ACTIVATION", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::IMPLICIT_ACTIVATION, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo ImplicitActivationPolicyValue_NO_IMPLICIT_ACTIVATION = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "NO_IMPLICIT_ACTIVATION", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::NO_IMPLICIT_ACTIVATION, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* ImplicitActivationPolicyValue_enumValues[] = {
  &ImplicitActivationPolicyValue_IMPLICIT_ACTIVATION,
  &ImplicitActivationPolicyValue_NO_IMPLICIT_ACTIVATION,
0
};

::acdk::lang::dmi::ClazzEnumInfo* ImplicitActivationPolicyValueMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo ImplicitActivationPolicyValue_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "ImplicitActivationPolicyValue", // name
  -1, // hashCode
  "org/omg/PortableServer", // ns
  0, // _scopeParent
  0, // _nextSibling
  ImplicitActivationPolicyValue_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_ImplicitActivationPolicyValue(&ImplicitActivationPolicyValue_enumInfo);

  return &ImplicitActivationPolicyValue_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_ImplicitActivationPolicyValueEnumInfo(ImplicitActivationPolicyValueMetaInf::GetEnumInfo());


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 


::acdk::lang::dmi::ClazzEnumValueInfo ServantRetentionPolicyValue_RETAIN = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "RETAIN", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::RETAIN, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo ServantRetentionPolicyValue_NON_RETAIN = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "NON_RETAIN", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::NON_RETAIN, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* ServantRetentionPolicyValue_enumValues[] = {
  &ServantRetentionPolicyValue_RETAIN,
  &ServantRetentionPolicyValue_NON_RETAIN,
0
};

::acdk::lang::dmi::ClazzEnumInfo* ServantRetentionPolicyValueMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo ServantRetentionPolicyValue_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "ServantRetentionPolicyValue", // name
  -1, // hashCode
  "org/omg/PortableServer", // ns
  0, // _scopeParent
  0, // _nextSibling
  ServantRetentionPolicyValue_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_ServantRetentionPolicyValue(&ServantRetentionPolicyValue_enumInfo);

  return &ServantRetentionPolicyValue_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_ServantRetentionPolicyValueEnumInfo(ServantRetentionPolicyValueMetaInf::GetEnumInfo());


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 


::acdk::lang::dmi::ClazzEnumValueInfo RequestProcessingPolicyValue_USE_ACTIVE_OBJECT_MAP_ONLY = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "USE_ACTIVE_OBJECT_MAP_ONLY", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::USE_ACTIVE_OBJECT_MAP_ONLY, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo RequestProcessingPolicyValue_USE_DEFAULT_SERVANT = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "USE_DEFAULT_SERVANT", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::USE_DEFAULT_SERVANT, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo RequestProcessingPolicyValue_USE_SERVANT_MANAGER = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "USE_SERVANT_MANAGER", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  org::omg::PortableServer::USE_SERVANT_MANAGER, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* RequestProcessingPolicyValue_enumValues[] = {
  &RequestProcessingPolicyValue_USE_ACTIVE_OBJECT_MAP_ONLY,
  &RequestProcessingPolicyValue_USE_DEFAULT_SERVANT,
  &RequestProcessingPolicyValue_USE_SERVANT_MANAGER,
0
};

::acdk::lang::dmi::ClazzEnumInfo* RequestProcessingPolicyValueMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo RequestProcessingPolicyValue_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "RequestProcessingPolicyValue", // name
  -1, // hashCode
  "org/omg/PortableServer", // ns
  0, // _scopeParent
  0, // _nextSibling
  RequestProcessingPolicyValue_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_RequestProcessingPolicyValue(&RequestProcessingPolicyValue_enumInfo);

  return &RequestProcessingPolicyValue_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_RequestProcessingPolicyValueEnumInfo(RequestProcessingPolicyValueMetaInf::GetEnumInfo());


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 

::acdk::lang::dmi::ClazzSuperInfo _AdapterInactive_super___acdk__lang__Throwable =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  ::acdk::lang::Throwable::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _AdapterInactive_interfaces[] =
{
  &_AdapterInactive_super___acdk__lang__Throwable,
  0
};

::acdk::lang::dmi::ClazzInfo* AdapterInactive::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "AdapterInactive", // name of class
  -1, // hashCode
  "org/omg/PortableServer", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _AdapterInactive_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 0, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    AdapterInactive::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_AdapterInactive(AdapterInactive::clazzInfo());

::acdk::lang::RClass
AdapterInactive::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
AdapterInactive::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Throwable)::getCollectableFields(fields);
}


} // namespace org
} // namespace omg
} // namespace PortableServer


namespace org { 
namespace omg { 
namespace PortableServer { 

::acdk::lang::dmi::ClazzSuperInfo _POAManager_super___org__omg__CORBA__portable__InvokeHandler =
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiCiThrowable,
  0, //AttributesRes
  ::org::omg::CORBA::portable::InvokeHandler::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _POAManager_interfaces[] =
{
  &_POAManager_super___org__omg__CORBA__portable__InvokeHandler,
  0
};

::acdk::lang::dmi::ClazzInfo* POAManager::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiCiInterface | ::acdk::lang::dmi::MiCiAbstract, // clazz-flags
  0, //AttributesRes
  "POAManager", // name of class
  -1, // hashCode
  "org/omg/PortableServer", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _POAManager_interfaces, // pointer to Array of ClazzInfo references
  0, // count of Super / Interfaces
  0, // pointer to Array of fields
  0, // count of Fields
  0, // pointer to Array of Methods
  0, // count of Methods
  0, // create-function for cloning/serializing
  0, // create-function for cloning/serializing arrays
  0, // create-function for cloning/serializing arrays
  0, // Class* thisClass; chaching instance
  0, // jlong serialVersionUID; for serialization
  ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 0, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    POAManager::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_POAManager(POAManager::clazzInfo());

::acdk::lang::RClass
POAManager::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
POAManager::getCollectableFields(FieldReferences& fields)
{
}


} // namespace org
} // namespace omg
} // namespace PortableServer
