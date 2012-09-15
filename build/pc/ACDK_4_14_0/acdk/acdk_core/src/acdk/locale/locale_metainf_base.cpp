// Generated by ACDK Metacompiler, 
// Copyrighted by Roger Rene Kommer, artefaktur
// 
// Dont edit this file manually
// 
#include <acdk.h>


#include "AsciiEncoding.h"
#include "AsciiUtfEncoding.h"
#include "ByteAsciiEncoding.h"
#include "CEscapeEncoding.h"
#include "CharacterCodingException.h"
#include "CodingErrorAction.h"
#include "Decoder.h"
#include "Encoder.h"
#include "Encoding.h"
#include "IllegalCharsetNameException.h"
#include "IsoEncoding.h"
#include "locale.h"
#include "UCS2Encoding.h"
#include "UnicodeTable.h"
#include "UnmappableCharacterException.h"
#include "UTF8Encoding.h"

namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _AsciiEncoding_super_Encoding =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Encoding::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _AsciiEncoding_interfaces[] =
{
  &_AsciiEncoding_super_Encoding,
  0
};

::acdk::lang::dmi::ClazzInfo* AsciiEncoding::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "AsciiEncoding", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _AsciiEncoding_interfaces, // pointer to Array of ClazzInfo references
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
    AsciiEncoding::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_AsciiEncoding(AsciiEncoding::clazzInfo());

::acdk::lang::RClass
AsciiEncoding::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
AsciiEncoding::getCollectableFields(FieldReferences& fields)
{
  Encoding::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _AsciiUtfEncoding_super_Encoding =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Encoding::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _AsciiUtfEncoding_interfaces[] =
{
  &_AsciiUtfEncoding_super_Encoding,
  0
};

::acdk::lang::dmi::ClazzInfo* AsciiUtfEncoding::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "AsciiUtfEncoding", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _AsciiUtfEncoding_interfaces, // pointer to Array of ClazzInfo references
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
    AsciiUtfEncoding::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_AsciiUtfEncoding(AsciiUtfEncoding::clazzInfo());

::acdk::lang::RClass
AsciiUtfEncoding::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
AsciiUtfEncoding::getCollectableFields(FieldReferences& fields)
{
  Encoding::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _ByteAsciiEncoding_super_Encoding =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Encoding::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _ByteAsciiEncoding_interfaces[] =
{
  &_ByteAsciiEncoding_super_Encoding,
  0
};

::acdk::lang::dmi::ClazzInfo* ByteAsciiEncoding::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "ByteAsciiEncoding", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _ByteAsciiEncoding_interfaces, // pointer to Array of ClazzInfo references
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
    ByteAsciiEncoding::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_ByteAsciiEncoding(ByteAsciiEncoding::clazzInfo());

::acdk::lang::RClass
ByteAsciiEncoding::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
ByteAsciiEncoding::getCollectableFields(FieldReferences& fields)
{
  Encoding::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _CharacterCodingException_super_acdk__io__IOException =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  acdk::io::IOException::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _CharacterCodingException_interfaces[] =
{
  &_CharacterCodingException_super_acdk__io__IOException,
  0
};

::acdk::lang::dmi::ClazzInfo* CharacterCodingException::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "CharacterCodingException", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _CharacterCodingException_interfaces, // pointer to Array of ClazzInfo references
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
    CharacterCodingException::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_CharacterCodingException(CharacterCodingException::clazzInfo());

::acdk::lang::RClass
CharacterCodingException::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
CharacterCodingException::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(acdk::io::, IOException)::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 


::acdk::lang::dmi::ClazzEnumValueInfo CodingErrorAction_IgnoreCodingError = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "IgnoreCodingError", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::locale::IgnoreCodingError, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo CodingErrorAction_ReplaceCodingError = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ReplaceCodingError", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::locale::ReplaceCodingError, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo CodingErrorAction_ReportCodingError = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "ReportCodingError", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::locale::ReportCodingError, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* CodingErrorAction_enumValues[] = {
  &CodingErrorAction_IgnoreCodingError,
  &CodingErrorAction_ReplaceCodingError,
  &CodingErrorAction_ReportCodingError,
0
};

::acdk::lang::dmi::ClazzEnumInfo* CodingErrorActionMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo CodingErrorAction_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "CodingErrorAction", // name
  -1, // hashCode
  "acdk/locale", // ns
  0, // _scopeParent
  0, // _nextSibling
  CodingErrorAction_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_CodingErrorAction(&CodingErrorAction_enumInfo);

  return &CodingErrorAction_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_CodingErrorActionEnumInfo(CodingErrorActionMetaInf::GetEnumInfo());


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _Decoder_super_acdk__lang__Object =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _Decoder_interfaces[] =
{
  &_Decoder_super_acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo* Decoder::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiCiAbstract, // clazz-flags
  0, //AttributesRes
  "Decoder", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Decoder_interfaces, // pointer to Array of ClazzInfo references
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
    Decoder::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Decoder(Decoder::clazzInfo());

::acdk::lang::RClass
Decoder::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Decoder::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(acdk::lang::, Object)::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_decodingReplacement._ref_this()); // RString _decodingReplacement 
  fields.push_back((::acdk::lang::RObject*)this->_encoding._ref_this()); // REncoding _encoding 
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _Encoder_super_acdk__lang__Object =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _Encoder_interfaces[] =
{
  &_Encoder_super_acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo* Encoder::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiCiAbstract, // clazz-flags
  0, //AttributesRes
  "Encoder", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Encoder_interfaces, // pointer to Array of ClazzInfo references
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
    Encoder::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Encoder(Encoder::clazzInfo());

::acdk::lang::RClass
Encoder::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Encoder::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(acdk::lang::, Object)::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_encoding._ref_this()); // REncoding _encoding 
  fields.push_back((::acdk::lang::RObject*)this->_encodingReplacement._ref_this()); // RbyteArray _encodingReplacement 
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _Encoding_super___acdk__lang__Object =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  ::acdk::lang::Object::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _Encoding_interfaces[] =
{
  &_Encoding_super___acdk__lang__Object,
  0
};

::acdk::lang::dmi::ClazzInfo* Encoding::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiCiAbstract, // clazz-flags
  0, //AttributesRes
  "Encoding", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _Encoding_interfaces, // pointer to Array of ClazzInfo references
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
    Encoding::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_Encoding(Encoding::clazzInfo());

::acdk::lang::RClass
Encoding::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
Encoding::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::, Object)::getCollectableFields(fields);
  fields.push_back((::acdk::lang::RObject*)this->_name._ref_this()); // RString _name 
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _IllegalCharsetNameException_super_acdk__lang__IllegalArgumentException =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  acdk::lang::IllegalArgumentException::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _IllegalCharsetNameException_interfaces[] =
{
  &_IllegalCharsetNameException_super_acdk__lang__IllegalArgumentException,
  0
};

::acdk::lang::dmi::ClazzInfo* IllegalCharsetNameException::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "IllegalCharsetNameException", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _IllegalCharsetNameException_interfaces, // pointer to Array of ClazzInfo references
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
    IllegalCharsetNameException::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_IllegalCharsetNameException(IllegalCharsetNameException::clazzInfo());

::acdk::lang::RClass
IllegalCharsetNameException::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
IllegalCharsetNameException::getCollectableFields(FieldReferences& fields)
{
  ACDK_FQ_SUPER_QUALIFIER(acdk::lang::, IllegalArgumentException)::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _IsoEncoding_super_Encoding =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Encoding::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _IsoEncoding_interfaces[] =
{
  &_IsoEncoding_super_Encoding,
  0
};

::acdk::lang::dmi::ClazzInfo* IsoEncoding::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "IsoEncoding", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _IsoEncoding_interfaces, // pointer to Array of ClazzInfo references
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
    IsoEncoding::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_IsoEncoding(IsoEncoding::clazzInfo());

::acdk::lang::RClass
IsoEncoding::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
IsoEncoding::getCollectableFields(FieldReferences& fields)
{
  Encoding::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 


::acdk::lang::dmi::ClazzEnumValueInfo UCSEndianess_NativeEndian = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "NativeEndian", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::locale::NativeEndian, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo UCSEndianess_BigEndian = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "BigEndian", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::locale::BigEndian, // value

};


::acdk::lang::dmi::ClazzEnumValueInfo UCSEndianess_LittleEndian = 
{
  ::acdk::lang::dmi::MiEnumValInfo, // flags
  0, // attributeRes
  "LittleEndian", // name
  -1, // hashCode
  "", // ns
  0, // _scopeParent
  0, // _nextSibling
  0,  // ClazzEnum definition
  acdk::locale::LittleEndian, // value

};

::acdk::lang::dmi::ClazzEnumValueInfo* UCSEndianess_enumValues[] = {
  &UCSEndianess_NativeEndian,
  &UCSEndianess_BigEndian,
  &UCSEndianess_LittleEndian,
0
};

::acdk::lang::dmi::ClazzEnumInfo* UCSEndianessMetaInf::GetEnumInfo()
{
static ::acdk::lang::dmi::ClazzEnumInfo UCSEndianess_enumInfo = {
  ::acdk::lang::dmi::MiEnumInfo, // flags
  0, // attribute rest
  "UCSEndianess", // name
  -1, // hashCode
  "acdk/locale", // ns
  0, // _scopeParent
  0, // _nextSibling
  UCSEndianess_enumValues, // values
  0, // internal next link
};

  static ::acdk::lang::dmi::RegisterEnumInfo _register_UCSEndianess(&UCSEndianess_enumInfo);

  return &UCSEndianess_enumInfo;
}
static ::acdk::lang::dmi::RegisterEnumInfo _register_UCSEndianessEnumInfo(UCSEndianessMetaInf::GetEnumInfo());


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _UCS2Encoding_super_Encoding =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Encoding::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _UCS2Encoding_interfaces[] =
{
  &_UCS2Encoding_super_Encoding,
  0
};

::acdk::lang::dmi::ClazzInfo* UCS2Encoding::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "UCS2Encoding", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _UCS2Encoding_interfaces, // pointer to Array of ClazzInfo references
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
    UCS2Encoding::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_UCS2Encoding(UCS2Encoding::clazzInfo());

::acdk::lang::RClass
UCS2Encoding::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
UCS2Encoding::getCollectableFields(FieldReferences& fields)
{
  Encoding::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _UnmappableCharacterException_super_CharacterCodingException =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  CharacterCodingException::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _UnmappableCharacterException_interfaces[] =
{
  &_UnmappableCharacterException_super_CharacterCodingException,
  0
};

::acdk::lang::dmi::ClazzInfo* UnmappableCharacterException::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "UnmappableCharacterException", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _UnmappableCharacterException_interfaces, // pointer to Array of ClazzInfo references
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
    UnmappableCharacterException::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_UnmappableCharacterException(UnmappableCharacterException::clazzInfo());

::acdk::lang::RClass
UnmappableCharacterException::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
UnmappableCharacterException::getCollectableFields(FieldReferences& fields)
{
  CharacterCodingException::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale


namespace acdk { 
namespace locale { 

::acdk::lang::dmi::ClazzSuperInfo _UTF8Encoding_super_Encoding =
{
  ::acdk::lang::dmi::MiPublic,
  0, //AttributesRes
  Encoding::clazzInfo()
};

::acdk::lang::dmi::ClazzSuperInfo* _UTF8Encoding_interfaces[] =
{
  &_UTF8Encoding_super_Encoding,
  0
};

::acdk::lang::dmi::ClazzInfo* UTF8Encoding::clazzInfo()
{
  static ::acdk::lang::dmi::ClazzInfo _clazzInfo =
{
  ::acdk::lang::dmi::MiClazzInfo, // clazz-flags
  0, //AttributesRes
  "UTF8Encoding", // name of class
  -1, // hashCode
  "acdk/locale", // the namespace
  0, // _scopeParent
  0, // _nextSibling
  0, // type
  0, // _firstChild
  _UTF8Encoding_interfaces, // pointer to Array of ClazzInfo references
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
    UTF8Encoding::_castToInterfacePtr, // cast object to interface pointer
0 // next ClazzInfo in chain
};
  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);
  return &_clazzInfo;
};

static ::acdk::lang::dmi::RegisterClazzInfo _register_UTF8Encoding(UTF8Encoding::clazzInfo());

::acdk::lang::RClass
UTF8Encoding::GetClass()
{
  return ::acdk::lang::Class::getSingeltonClass(clazzInfo());
}

//virtual
void
UTF8Encoding::getCollectableFields(FieldReferences& fields)
{
  Encoding::getCollectableFields(fields);
}


} // namespace acdk
} // namespace locale
