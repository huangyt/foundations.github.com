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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/ClassDescription.h,v 1.7 2005/02/05 10:45:11 kommer Exp $

#ifndef acdk_java_serialization_ClassDescription_h
#define acdk_java_serialization_ClassDescription_h

#include <acdk.h>
#include "Config.h"
#include "ClassTypeMapping.h"

namespace acdk {
namespace java {
namespace serialization {


ACDK_DECL_CLASS(ClassDescription);
ACDK_DECL_CLASS(FieldDescription);
ACDK_DECL_CLASS(JavaObjectReader);
ACDK_DECL_CLASS(JavaObjectWriter);

class ACDK_JAVA_SERIALIZATION_PUBLIC FieldDescription
: extends ::acdk::lang::Object
{
public:
  char _typeCode;
  RString _fieldName;
  RString _className;
  const MemberTypeMapping* _mtm;
  
  FieldDescription() 
  : Object()
  , _typeCode(0)
  , _mtm(0)
  { 
  }
  FieldDescription(const MemberTypeMapping* mtm) 
  : Object()
  , _typeCode(0)
  , _fieldName(mtm->java_field)
  , _className(mtm->java_type)
  , _mtm(mtm)
  {
  }
  
  RString fieldName() { return _fieldName; }
  RString className() { return _className; }
  
  char typeCode() { return _typeCode; }
  void write(IN(RJavaObjectWriter) out);

  static RFieldDescription read(IN(RJavaObjectReader) in);
  

};

class ACDK_JAVA_SERIALIZATION_PUBLIC ClassDescription
: extends ::acdk::lang::Object
{
protected:
  RString _className;
  jlong _classSerialVersionUID;
  byte _classDescFlags;
  RFieldDescriptionArray _fields;
  RbyteArray _block;
  RClassDescription _super;
  const ClassTypeMapping* _ctm;
  RClass _class;
public:
  ClassDescription() 
  : Object()
  , _classSerialVersionUID(0)
  , _classDescFlags(0)
  , _ctm(0)
  {
  }
  ClassDescription(const ClassTypeMapping* ctm);

  static RClassDescription read(IN(RJavaObjectReader) in);
  void write(IN(RJavaObjectWriter) out);
  RString className() { return _className; }
  byte flags() { return _classDescFlags; }
  RFieldDescriptionArray fields() { return _fields; }
  const ClassTypeMapping* classTypeMapping() { return _ctm; }
  void setClass(IN(RClass) cls) { _class = cls; }
  RClass getObjectClass() { return _class; }
};

} // namespace serialization
} // namespace java 
} // namespace acdk 

#endif //acdk_java_serialization_ClassDescription_h
