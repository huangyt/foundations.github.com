// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>


#include "boot.h"
#include "boot_all.h"
#include "Config.h"
#include "HelloWorldClass.h"
#include "HelloWorldInterface.h"

namespace acdk { 
namespace boot { 

using namespace acdk::lang;
::acdk::lang::dmi::ClazzSuperInfo _HelloWorldClass_super_acdk__lang__Object =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo _HelloWorldClass_super_HelloWorldInterface =
{
  ::acdk::lang::dmi::MiPublic | ::acdk::lang::dmi::MiCiInterface | ::acdk::lang::dmi::MiCiThrowable,
  0, //AttributesRes
  HelloWorldInterface::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _HelloWorldClass_interfaces[] =
{
  &_HelloWorldClass_super_acdk__lang__Object,
  &_HelloWorldClass_super_HelloWorldInterface,
  0
};

::acdk::lang::dmi::ClazzInfo* HelloWorldClass::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "HelloWorldClass", // name of class
  -1, // hashCode
  "acdk/boot", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _HelloWorldClass_interfaces, // pointer to Array of ClazzInfo references
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
 1, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    HelloWorldClass::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_HelloWorldClass(HelloWorldClass::clazzInfo());

::acdk::lang::RClass
HelloWorldClass::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
HelloWorldClass::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(acdk::lang::, Object)::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_greetings._ref_this()); // RString _greetings 
}


} // namespace acdk
} // namespace boot


namespace acdk { 
namespace boot { 

using namespace acdk::lang;
::acdk::lang::dmi::ClazzSuperInfo* _HelloWorldInterface_interfaces[] =
{
  0
};

::acdk::lang::dmi::ClazzInfo* HelloWorldInterface::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiCiInterface | ::acdk::lang::dmi::MiCiAbstract, // clazz-flags
  0, //AttributesRes
  "HelloWorldInterface", // name of class
  -1, // hashCode
  "acdk/boot", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _HelloWorldInterface_interfaces, // pointer to Array of ClazzInfo references
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
    HelloWorldInterface::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_HelloWorldInterface(HelloWorldInterface::clazzInfo());

::acdk::lang::RClass
HelloWorldInterface::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
HelloWorldInterface::getCollectableFields(FieldReferences& fields)
{
}


} // namespace acdk
} // namespace boot

