// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>


#include "Config.h"
#include "Naming.h"
#include "Remote.h"
#include "rmi.h"

namespace acdk { 
namespace java { 
namespace rmi { 

::acdk::lang::dmi::ClazzSuperInfo _Naming_super___acdk__lang__Object =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  ::acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _Naming_interfaces[] =
{
  &_Naming_super___acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo* Naming::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "Naming", // name of class
  -1, // hashCode
  "acdk/java/rmi", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Naming_interfaces, // pointer to Array of ClazzInfo references
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
    Naming::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Naming(Naming::clazzInfo());

::acdk::lang::RClass
Naming::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Naming::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::getCollectableFields(fields);
}


} // namespace acdk
} // namespace java
} // namespace rmi


namespace acdk { 
namespace java { 
namespace rmi { 

::acdk::lang::dmi::ClazzSuperInfo* _Remote_interfaces[] =
{
  0
};

::acdk::lang::dmi::ClazzInfo* Remote::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiCiInterface, // clazz-flags
  0, //AttributesRes
  "Remote", // name of class
  -1, // hashCode
  "acdk/java/rmi", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Remote_interfaces, // pointer to Array of ClazzInfo references
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
    Remote::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Remote(Remote::clazzInfo());

::acdk::lang::RClass
Remote::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Remote::getCollectableFields(FieldReferences& fields)
{
}


} // namespace acdk
} // namespace java
} // namespace rmi

