// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>


#include "ArgumentInfo.h"
#include "ClassInfo.h"
#include "ClassInitAttribute.h"
#include "ClazzFlagAttribute.h"
#include "ClazzNameAttribute.h"
#include "CMCException.h"
#include "CodeAttribute.h"
#include "CodeInfo.h"
#include "Config.h"
#include "DispatchForwardAttributeTest.h"
#include "DmiProxyAttribute.h"
#include "DmiProxyGenerator.h"
#include "DmiProxyGeneratorExt.h"
#include "EnumArgAttribute.h"
#include "EnumInfo.h"
#include "FieldInfo.h"
#include "InvokeForwardAttribute.h"
#include "mc.h"
#include "McConfigAttribute.h"
#include "MetaCompiler.h"
#include "MethodAltNameAttribute.h"
#include "MethodInfo.h"
#include "ModuleInfo.h"
#include "SetDispatchAttribute.h"
#include "StringTagAttribute.h"
#include "SuperInfo.h"
#include "ThrowableAttribute.h"
#include "TokenStack.h"
#include "TypeScope.h"
#include "UnitInfo.h"

namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _ClassInitAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _ClassInitAttribute_interfaces[] =
{
  &_ClassInitAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* ClassInitAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "ClassInitAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _ClassInitAttribute_interfaces, // pointer to Array of ClazzInfo references
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
 2, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    ClassInitAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_ClassInitAttribute(ClassInitAttribute::clazzInfo());

::acdk::lang::RClass
ClassInitAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
ClassInitAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_deinitFunction._ref_this()); // RString _deinitFunction 
  fields.push_back((::acdk::lang::RObject*)this->_initFunction._ref_this()); // RString _initFunction 
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _ClazzFlagAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _ClazzFlagAttribute_interfaces[] =
{
  &_ClazzFlagAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* ClazzFlagAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "ClazzFlagAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _ClazzFlagAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    ClazzFlagAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_ClazzFlagAttribute(ClazzFlagAttribute::clazzInfo());

::acdk::lang::RClass
ClazzFlagAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
ClazzFlagAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _ClazzNameAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _ClazzNameAttribute_interfaces[] =
{
  &_ClazzNameAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* ClazzNameAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "ClazzNameAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _ClazzNameAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    ClazzNameAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_ClazzNameAttribute(ClazzNameAttribute::clazzInfo());

::acdk::lang::RClass
ClazzNameAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
ClazzNameAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_name._ref_this()); // RString _name 
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _CodeAttribute_super___acdk__lang__Object =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  ::acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _CodeAttribute_interfaces[] =
{
  &_CodeAttribute_super___acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo* CodeAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "CodeAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _CodeAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    CodeAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_CodeAttribute(CodeAttribute::clazzInfo());

::acdk::lang::RClass
CodeAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
CodeAttribute::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::getCollectableFields(fields);
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 


::acdk::lang::dmi::ClazzEnumValueInfo CodeWhere_ModuleInclude = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ModuleInclude", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::ModuleInclude, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo CodeWhere_ModuleBeforeDispatch = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ModuleBeforeDispatch", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::ModuleBeforeDispatch, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo CodeWhere_ModuleAfterDispatch = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ModuleAfterDispatch", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::ModuleAfterDispatch, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo CodeWhere_ModuleBeforeInit = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ModuleBeforeInit", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::ModuleBeforeInit, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo CodeWhere_ModuleInit = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ModuleInit", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::ModuleInit, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo CodeWhere_ClassInit = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ClassInit", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::ClassInit, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* CodeWhere_enumValues[] = {
  &CodeWhere_ModuleInclude,
  &CodeWhere_ModuleBeforeDispatch,
  &CodeWhere_ModuleAfterDispatch,
  &CodeWhere_ModuleBeforeInit,
  &CodeWhere_ModuleInit,
  &CodeWhere_ClassInit,
0
};

::acdk::lang::dmi::ClazzEnumInfo* CodeWhereMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo CodeWhere_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "CodeWhere", // name
  -1, // hashCode
  "acdk/tools/mc", // ns
  0, // _scopeParent
  0, // _nextSibling
  CodeWhere_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_CodeWhere(&CodeWhere_enumInfo);

  return &CodeWhere_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_CodeWhereEnumInfo(CodeWhereMetaInf::GetEnumInfo());


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _DispatchForwardAttributeTest_super_acdk__lang__Object =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _DispatchForwardAttributeTest_interfaces[] =
{
  &_DispatchForwardAttributeTest_super_acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo* DispatchForwardAttributeTest::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "DispatchForwardAttributeTest", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _DispatchForwardAttributeTest_interfaces, // pointer to Array of ClazzInfo references
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
  DispatchForwardAttributeTest::stddispatch, // dynamic_dispatch
  ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch
 1, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    DispatchForwardAttributeTest::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_DispatchForwardAttributeTest(DispatchForwardAttributeTest::clazzInfo());

::acdk::lang::RClass
DispatchForwardAttributeTest::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
DispatchForwardAttributeTest::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(acdk::lang::, Object)::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->dummyvar._ref_this()); // RObject dummyvar 
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _DmiProxyAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _DmiProxyAttribute_interfaces[] =
{
  &_DmiProxyAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* DmiProxyAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "DmiProxyAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _DmiProxyAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    DmiProxyAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_DmiProxyAttribute(DmiProxyAttribute::clazzInfo());

::acdk::lang::RClass
DmiProxyAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
DmiProxyAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _InvokeForwardAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _InvokeForwardAttribute_interfaces[] =
{
  &_InvokeForwardAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* InvokeForwardAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "InvokeForwardAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _InvokeForwardAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    InvokeForwardAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_InvokeForwardAttribute(InvokeForwardAttribute::clazzInfo());

::acdk::lang::RClass
InvokeForwardAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
InvokeForwardAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 


::acdk::lang::dmi::ClazzEnumValueInfo McConfigAttributes_McConfNoMetaInfo = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "McConfNoMetaInfo", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::McConfNoMetaInfo, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo McConfigAttributes_McConfNoFields = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "McConfNoFields", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::McConfNoFields, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo McConfigAttributes_McConfWithFields = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "McConfWithFields", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::McConfWithFields, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo McConfigAttributes_McConfNoMethods = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "McConfNoMethods", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::McConfNoMethods, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo McConfigAttributes_McConfWithMethods = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "McConfWithMethods", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::McConfWithMethods, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo McConfigAttributes_McConfOwnCollectableFields = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "McConfOwnCollectableFields", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::McConfOwnCollectableFields, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo McConfigAttributes_McConfNoDmiProxy = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "McConfNoDmiProxy", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::McConfNoDmiProxy, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* McConfigAttributes_enumValues[] = {
  &McConfigAttributes_McConfNoMetaInfo,
  &McConfigAttributes_McConfNoFields,
  &McConfigAttributes_McConfWithFields,
  &McConfigAttributes_McConfNoMethods,
  &McConfigAttributes_McConfWithMethods,
  &McConfigAttributes_McConfOwnCollectableFields,
  &McConfigAttributes_McConfNoDmiProxy,
0
};

::acdk::lang::dmi::ClazzEnumInfo* McConfigAttributesMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo McConfigAttributes_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "McConfigAttributes", // name
  -1, // hashCode
  "acdk/tools/mc", // ns
  0, // _scopeParent
  0, // _nextSibling
  McConfigAttributes_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_McConfigAttributes(&McConfigAttributes_enumInfo);

  return &McConfigAttributes_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_McConfigAttributesEnumInfo(McConfigAttributesMetaInf::GetEnumInfo());


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _McConfigAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _McConfigAttribute_interfaces[] =
{
  &_McConfigAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* McConfigAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "McConfigAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _McConfigAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    McConfigAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_McConfigAttribute(McConfigAttribute::clazzInfo());

::acdk::lang::RClass
McConfigAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
McConfigAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _MethodAltNameAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _MethodAltNameAttribute_interfaces[] =
{
  &_MethodAltNameAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* MethodAltNameAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "MethodAltNameAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _MethodAltNameAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    MethodAltNameAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_MethodAltNameAttribute(MethodAltNameAttribute::clazzInfo());

::acdk::lang::RClass
MethodAltNameAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
MethodAltNameAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_altName._ref_this()); // RString _altName 
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _SetDispatchAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _SetDispatchAttribute_interfaces[] =
{
  &_SetDispatchAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* SetDispatchAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "SetDispatchAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _SetDispatchAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    SetDispatchAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_SetDispatchAttribute(SetDispatchAttribute::clazzInfo());

::acdk::lang::RClass
SetDispatchAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
SetDispatchAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_functionSignature._ref_this()); // RString _functionSignature 
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _StringTagAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _StringTagAttribute_interfaces[] =
{
  &_StringTagAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* StringTagAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "StringTagAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _StringTagAttribute_interfaces, // pointer to Array of ClazzInfo references
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
 2, // count off all collectable members in this class
  0, // member type info for arrays or typed container
    StringTagAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_StringTagAttribute(StringTagAttribute::clazzInfo());

::acdk::lang::RClass
StringTagAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
StringTagAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->key._ref_this()); // RString key 
  fields.push_back((::acdk::lang::RObject*)this->value._ref_this()); // RString value 
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 

::acdk::lang::dmi::ClazzSuperInfo _ThrowableAttribute_super_CodeAttribute =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CodeAttribute::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _ThrowableAttribute_interfaces[] =
{
  &_ThrowableAttribute_super_CodeAttribute,
  0
};

::acdk::lang::dmi::ClazzInfo* ThrowableAttribute::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "ThrowableAttribute", // name of class
  -1, // hashCode
  "acdk/tools/mc", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _ThrowableAttribute_interfaces, // pointer to Array of ClazzInfo references
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
    ThrowableAttribute::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_ThrowableAttribute(ThrowableAttribute::clazzInfo());

::acdk::lang::RClass
ThrowableAttribute::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
ThrowableAttribute::getCollectableFields(FieldReferences& fields)
{
  CodeAttribute::getCollectableFields(fields);
}


} // namespace acdk
} // namespace tools
} // namespace mc


namespace acdk { 
namespace tools { 
namespace mc { 


::acdk::lang::dmi::ClazzEnumValueInfo TypeScopeType_TsUnknown = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "TsUnknown", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::TsUnknown, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo TypeScopeType_TsEnum = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "TsEnum", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::tools::mc::TsEnum, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* TypeScopeType_enumValues[] = {
  &TypeScopeType_TsUnknown,
  &TypeScopeType_TsEnum,
0
};

::acdk::lang::dmi::ClazzEnumInfo* TypeScopeTypeMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo TypeScopeType_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "TypeScopeType", // name
  -1, // hashCode
  "acdk/tools/mc", // ns
  0, // _scopeParent
  0, // _nextSibling
  TypeScopeType_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_TypeScopeType(&TypeScopeType_enumInfo);

  return &TypeScopeType_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_TypeScopeTypeEnumInfo(TypeScopeTypeMetaInf::GetEnumInfo());


} // namespace acdk
} // namespace tools
} // namespace mc

