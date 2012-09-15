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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/a2jser_java_lang.cpp,v 1.9 2005/02/05 10:45:12 kommer Exp $


#include <acdk/java/serialization/ClassDescription.h>
#include <acdk/java/serialization/JavaObjectReader.h>
#include <acdk/java/serialization/JavaObjectWriter.h>

::acdk::java::serialization::MemberTypeMapping* a2jser_ZArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_ZArray = 
{
  "[bool", // acdk_name
  "[Z", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x578f203914b85de2), // UID
  JLONG_CONSTANT(0x631f1721cb9a1c15), //[] UID
  a2jser_ZArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_ZArray(&a2jser_ZArray);




::acdk::java::serialization::MemberTypeMapping* a2jser_CArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_CArray = 
{
  "[char", // acdk_name
  "[C", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0xb02666b0e25d84ac), // UID
  JLONG_CONSTANT(0x98327eb72369a9ca), //[] UID
  a2jser_CArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_CArray(&a2jser_CArray);

::acdk::java::serialization::MemberTypeMapping* a2jser_UCArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_UCArray = 
{
  "[ucchar", // acdk_name
  "[C", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0xb02666b0e25d84ac), // UID
  JLONG_CONSTANT(0x98327eb72369a9ca), //[] UID
  a2jser_UCArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_UCArray(&a2jser_UCArray);



::acdk::java::serialization::MemberTypeMapping* a2jser_BArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_BArray = 
{
  "[byte", // acdk_name
  "[B", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0xacf317f8060854e0), // UID
  JLONG_CONSTANT(0x4bfd19156767db37), //[] UID
  a2jser_BArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_BArray(&a2jser_BArray);




::acdk::java::serialization::MemberTypeMapping* a2jser_SArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_SArray = 
{
  "[short", // acdk_name
  "[S", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0xef832e06e55db0fa), // UID
  JLONG_CONSTANT(0x7cae5dae123b4435), //[] UID
  a2jser_SArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_SArray(&a2jser_SArray);




::acdk::java::serialization::MemberTypeMapping* a2jser_IArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_IArray = 
{
  "[int", // acdk_name
  "[I", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x4dba602676eab2a5), // UID
  JLONG_CONSTANT(0x17f7e44f198f893c), //[] UID
  a2jser_IArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_IArray(&a2jser_IArray);




::acdk::java::serialization::MemberTypeMapping* a2jser_JArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_JArray = 
{
  "[long", // acdk_name
  "[J", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x782004b512b17593), // UID
  JLONG_CONSTANT(0xfe76f8764a55dfbd), //[] UID
  a2jser_JArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_JArray(&a2jser_JArray);




::acdk::java::serialization::MemberTypeMapping* a2jser_FArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_FArray = 
{
  "[float", // acdk_name
  "[F", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0xb9c818922e00c42), // UID
  JLONG_CONSTANT(0x77aa8d1669fe1976), //[] UID
  a2jser_FArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_FArray(&a2jser_FArray);




::acdk::java::serialization::MemberTypeMapping* a2jser_DArray_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_DArray = 
{
  "[double", // acdk_name
  "[D", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x3ea68c14ab635a1e), // UID
  JLONG_CONSTANT(0xc7ad0bff6467ff45), //[] UID
  a2jser_DArray_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_DArray(&a2jser_DArray);




::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_Number_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_Number = 
{
  "acdk/lang/Number", // acdk_name
  "java/lang/Number", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x86ac951d0b94e08b), // UID
  JLONG_CONSTANT(0x8d4686cc5ce1152c), //[] UID
  a2jser_java_lang_Number_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_Number(&a2jser_java_lang_Number);




::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_String_fields[] = 
{
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_String = 
{
  "acdk/lang/String", // acdk_name
  "java/lang/String", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0xa0f0a4387a3bb342), // UID
  JLONG_CONSTANT(0xadd256e7e91d7b47), //[] UID
  a2jser_java_lang_String_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_String(&a2jser_java_lang_String);




::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_Integer_field_value = 
{
  "value", // acdk_name
  "int", // acdk_type
  "value", // java_name
  "I" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_Integer_fields[] = 
{
  &a2jser_java_lang_Integer_field_value,
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_Integer = 
{
  "acdk/lang/Integer", // acdk_name
  "java/lang/Integer", // java_name
  "acdk/lang/Number", // acdk_super
  "java/lang/Number", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x12e2a0a4f7818738), // UID
  JLONG_CONSTANT(0xfe97ada00183e21b), //[] UID
  a2jser_java_lang_Integer_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_Integer(&a2jser_java_lang_Integer);




::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_Double_field_value = 
{
  "value", // acdk_name
  "double", // acdk_type
  "value", // java_name
  "D" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_Double_fields[] = 
{
  &a2jser_java_lang_Double_field_value,
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_Double = 
{
  "acdk/lang/Double", // acdk_name
  "java/lang/Double", // java_name
  "acdk/lang/Number", // acdk_super
  "java/lang/Number", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x80b3c24a296bfb04), // UID
  JLONG_CONSTANT(0xe112ad8900a656a6), //[] UID
  a2jser_java_lang_Double_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_Double(&a2jser_java_lang_Double);







::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_Character_field_value = 
{
  "value", // acdk_name
  "char", // acdk_type
  "value", // java_name
  "C" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_Character_fields[] = 
{
  &a2jser_java_lang_Character_field_value,
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_Character = 
{
  "acdk/lang/Character", // acdk_name
  "java/lang/Character", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x348b47d96b1a2678), // UID
  JLONG_CONSTANT(0x30556b9d7f899e70), //[] UID
  a2jser_java_lang_Character_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_Character(&a2jser_java_lang_Character);




::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_Byte_field_value = 
{
  "value", // acdk_name
  "byte", // acdk_type
  "value", // java_name
  "B" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_Byte_fields[] = 
{
  &a2jser_java_lang_Byte_field_value,
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_Byte = 
{
  "acdk/lang/Byte", // acdk_name
  "java/lang/Byte", // java_name
  "acdk/lang/Number", // acdk_super
  "java/lang/Number", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x9c4e6084ee50f51c), // UID
  JLONG_CONSTANT(0x94106c2f84688b6e), //[] UID
  a2jser_java_lang_Byte_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_Byte(&a2jser_java_lang_Byte);




::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_Short_field_value = 
{
  "value", // acdk_name
  "short", // acdk_type
  "value", // java_name
  "S" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_Short_fields[] = 
{
  &a2jser_java_lang_Short_field_value,
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_Short = 
{
  "acdk/lang/Short", // acdk_name
  "java/lang/Short", // java_name
  "acdk/lang/Number", // acdk_super
  "java/lang/Number", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x684d37133460da52), // UID
  JLONG_CONSTANT(0xa6bcea37405cc749), //[] UID
  a2jser_java_lang_Short_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_Short(&a2jser_java_lang_Short);




::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_Long_field_value = 
{
  "value", // acdk_name
  "long", // acdk_type
  "value", // java_name
  "J" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_Long_fields[] = 
{
  &a2jser_java_lang_Long_field_value,
  0
};

::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_Long = 
{
  "acdk/lang/Long", // acdk_name
  "java/lang/Long", // java_name
  "acdk/lang/Number", // acdk_super
  "java/lang/Number", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x3b8be490cc8f23df), // UID
  JLONG_CONSTANT(0x7de10ab2bbbc632b), //[] UID
  a2jser_java_lang_Long_fields, // Fields
  0, // read_func
  0, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_Long(&a2jser_java_lang_Long);




::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_StringBuffer_field_count = 
{
  "count", // acdk_name
  "int", // acdk_type
  "count", // java_name
  "I" // java_type
};

::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_StringBuffer_field_shared = 
{
  "shared", // acdk_name
  "bool", // acdk_type
  "shared", // java_name
  "Z" // java_type
};

::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_StringBuffer_field_value = 
{
  "value", // acdk_name
  "[U", // acdk_type
  "value", // java_name
  "[C" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_StringBuffer_fields[] = 
{
  &a2jser_java_lang_StringBuffer_field_count,
  &a2jser_java_lang_StringBuffer_field_shared,
  &a2jser_java_lang_StringBuffer_field_value,
  0
};


RObject 
a2jser_read_acdk_lang_StringBuffer(IN(::acdk::java::serialization::RJavaObjectReader) in, 
                                   const ::acdk::java::serialization::ClassTypeMapping* ctm)
{
  int count = in->readInt();
  bool shared = in->readBoolean();
  RuccharArray ba = (RuccharArray)in->readObject();
  RString str = new String(ba, 0, count);
  RObject sb = new StringBuffer(str);
  in->registerNewObject(sb);
  return sb;
}

void 
a2jser_write_acdk_lang_StringBuffer(IN(::acdk::java::serialization::RJavaObjectWriter) out, 
                                    const ::acdk::java::serialization::ClassTypeMapping* ctm, 
                                    IN(::acdk::lang::RObject) obj)
{
  RStringBuffer sb = (RStringBuffer)obj;
  RString str = sb->toString();
  out->writeInt(str->length());
  out->writeBoolean(false);
  out->writeObject(&str->getUcChars());
}


::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_StringBuffer = 
{
  "acdk/lang/StringBuffer", // acdk_name
  "java/lang/StringBuffer", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0x2f0707d9eac8ead3), // UID
  JLONG_CONSTANT(0xa2e0af375551ace2), //[] UID
  a2jser_java_lang_StringBuffer_fields, // Fields
  a2jser_read_acdk_lang_StringBuffer, // read_func
  a2jser_write_acdk_lang_StringBuffer, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_StringBuffer(&a2jser_java_lang_StringBuffer);




::acdk::java::serialization::MemberTypeMapping a2jser_java_lang_Throwable_field_detailMessage = 
{
  "_what", // acdk_name
  "acdk/lang/String", // acdk_type
  "detailMessage", // java_name
  "Ljava/lang/String;" // java_type
};

::acdk::java::serialization::MemberTypeMapping* a2jser_java_lang_Throwable_fields[] = 
{
  &a2jser_java_lang_Throwable_field_detailMessage,
  0
};

// not used
RObject 
a2jser_read_acdk_lang_Throwable(IN(::acdk::java::serialization::RJavaObjectReader) in, 
                                   const ::acdk::java::serialization::ClassTypeMapping* ctm)
{
  RString msg = in->readString();
  RThrowable ex = new Throwable(msg);
  in->registerNewObject(ex);
  return ex;
}

// not used
void 
a2jser_write_acdk_lang_Throwable(IN(::acdk::java::serialization::RJavaObjectWriter) out, 
                                    const ::acdk::java::serialization::ClassTypeMapping* ctm, 
                                    IN(::acdk::lang::RObject) obj)
{
  RThrowable ex = (RThrowable)obj;
  RString str = ex->getMessage();
  out->writeString(str);
}


::acdk::java::serialization::ClassTypeMapping a2jser_java_lang_Throwable = 
{
  "acdk/lang/Throwable", // acdk_name
  "java/lang/Throwable", // java_name
  "", // acdk_super
  "", // java_super
  ::acdk::java::serialization::SC_SERIALIZABLE, // flags
  JLONG_CONSTANT(0xd5c635273977b8cb), // UID
  JLONG_CONSTANT(0x54109a381ae98e56), //[] UID
  a2jser_java_lang_Throwable_fields, // Fields
  0, //a2jser_read_acdk_lang_Throwable, // read_func
  0, //a2jser_write_acdk_lang_Throwable, // write_func
  0 // used for internal linked list
};

::acdk::java::serialization::RegisterTypeMapping register_a2jser_java_lang_Throwable(&a2jser_java_lang_Throwable);




