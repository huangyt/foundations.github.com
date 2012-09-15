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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/ClassTypeMapping.h,v 1.9 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_serialization_ClassTypeMapping_h
#define acdk_java_serialization_ClassTypeMapping_h

#include <acdk.h>
#include "Config.h"

namespace acdk {
namespace java {
namespace serialization {

/**
  These Conctants are used in the Java serialized stream
*/
enum ClassConstants
{
  TC_NULL = 0x70,
  TC_REFERENCE = 0x71,
  TC_CLASSDESC = 0x72,
  TC_OBJECT = 0x73,
  TC_STRING = 0x74,
  TC_ARRAY = 0x75,
  TC_CLASS = 0x76,
  TC_BLOCKDATA = 0x77,
  TC_ENDBLOCKDATA = 0x78,
  TC_RESET = 0x79,
  TC_BLOCKDATALONG = 0x7A,
  TC_EXCEPTION = 0x7B,
  TC_LONGSTRING = 0x7C,
  TC_PROXYCLASSDESC = 0x7D
};


enum ClassDescFlags
{ 
  SC_WRITE_METHOD = 0x01, //if SC_SERIALIZABLE
  SC_SERIALIZABLE = 0x02,
  SC_BLOCK_DATA = 0x08,    //if SC_EXTERNALIZABLE
  SC_EXTERNALIZABLE = 0x04
};

const short STREAM_MAGIC = (short)0xaced;
const short STREAM_VERSION = 5;
const int baseWireHandle = 0x7E0000;

/**
  The MemberTypeMapping is to used to map a Java member
  to a ACDK member.
  You can use the gw_ref[JavaClassDecl, acdk_java_serialization_man] to generate
  the mapping for a given Java class
*/
struct ACDK_JAVA_SERIALIZATION_PUBLIC MemberTypeMapping
{
  /**
    Name of the ACDK field
  */
  const char* acdk_field;
  /** type of the acdk field */
  const char* acdk_type;
  /** 
    name of the java field.
  */
  const char* java_field;
  /** 
    name of the java type in JVM notation
    for example: "Ljava/lang/String;"
  */
  const char* java_type;
  char getJavaTypeCode() const;
};

ACDK_DECL_CLASS(JavaObjectWriter);
ACDK_DECL_CLASS(JavaObjectReader);

struct ACDK_JAVA_SERIALIZATION_PUBLIC ClassTypeMapping;

typedef RObject (*ReadJavaObjectFunc)(IN(RJavaObjectReader) in, const ClassTypeMapping* ctm);
typedef void (*WriteJavaObjectFunc)(IN(RJavaObjectWriter) out, const ClassTypeMapping* ctm, IN(::acdk::lang::RObject) obj);

struct ACDK_JAVA_SERIALIZATION_PUBLIC ClassTypeMapping
{
  /** 
    Name of the ACDK class.
    For example: "acdk/lang/StringBuffer"
  */
  const char* acdk_name;
  /**
    Name of the java name.
    For example: "java/lang/StringBuffer"
  */
  const char* java_name;
  /**
    Name of the acdk super class.
    "" if no superclass (or acdk::lang::Object)
  */
  const char* acdk_super;
  /**
      Name of the java super class.
    "" if no superclass (or java.lang.Object)
  */
  const char* java_super;

  /**
    Serialization flags as a combination of ClassDescFlags.
  */
  byte flags;
  /**
    classSerialVersionUID used by java
  */
  jlong classSerialVersionUID;
  /**
    classSerialVersionUID of the array version of the class
  */
  jlong arraySerialVersionUID;
  // Fields 
  MemberTypeMapping** fields;
  /**
    If read_func is no 0 this function will be called
    to read an ACDK object from a java serialized object stream.
    After creating the result object before reading any member objects
    call in->registerNewObject(newObject) with the new created object.
  */
  ReadJavaObjectFunc read_func;
  /**
    If write_func is no 0 this function will be called
    to write an ACDK object to a java serialized object stream.
  */
  WriteJavaObjectFunc write_func;
  /**
    used internally
  */
  const ClassTypeMapping* next;

  static const ClassTypeMapping* root;
  
  static void registerMapping(const ClassTypeMapping* ctm);
  static void unregisterMapping(const ClassTypeMapping* ctm);
  const MemberTypeMapping* findAcdkMember(const char* name) const;
  const MemberTypeMapping* findJavaMember(const char* name) const;
  static const ClassTypeMapping* findJavaClass(RString in);
  
  static const ClassTypeMapping* findAcdkClass(const char* name);
  static const ClassTypeMapping* findAcdkClass(IN(RString) name)
  {
    return findAcdkClass(name->c_str());
  }

  int fieldSize() const
  {
    if (fields == 0)
      return 0;
    int size = 0;
    for (; fields[size] != 0; ++size)
      ;
    return size;
  }

};

class ACDK_JAVA_SERIALIZATION_PUBLIC RegisterTypeMapping
{
  const ClassTypeMapping* _typeMapping;
public: 
  RegisterTypeMapping(const ClassTypeMapping* tpm)
  : _typeMapping(tpm)
  {
    ClassTypeMapping::registerMapping(tpm);
  }
  ~RegisterTypeMapping()
  {
    ClassTypeMapping::unregisterMapping(_typeMapping);
  }
};

} // namespace serialization
} // namespace java 
} // namespace acdk 

#endif //acdk_java_serialization_ClassTypeMapping_h
