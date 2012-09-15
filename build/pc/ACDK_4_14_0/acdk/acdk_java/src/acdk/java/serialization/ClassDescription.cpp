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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/ClassDescription.cpp,v 1.7 2005/02/05 10:45:11 kommer Exp $


#include "ClassDescription.h"
#include "JavaObjectReader.h"
#include "JavaObjectWriter.h"

namespace acdk {
namespace java {
namespace serialization {




//static 
RFieldDescription 
FieldDescription::read(IN(RJavaObjectReader) in)
{
  RFieldDescription fd = new FieldDescription();
  fd->_typeCode = (byte)in->read();
  fd->_fieldName = in->readUtf();
  if (fd->_typeCode == '[' || fd->_typeCode == 'L') {
    fd->_className = in->readString();
  } 
  return fd;
}

void 
FieldDescription::write(IN(RJavaObjectWriter) out)
{
  char c = _mtm->getJavaTypeCode();
  out->writeChar(c);
  out->writeShortUtf(_fieldName);
  if (c == '[' || c == 'L') 
  {
    out->writeString(_className);
  }
}

ClassDescription::ClassDescription(const ClassTypeMapping* ctm) 
: Object()
, _classSerialVersionUID(0)
, _classDescFlags(0)
, _ctm(ctm)
{
  _className = ctm->java_name;
  _className = _className->replace('/', '.');
  _classSerialVersionUID = ctm->classSerialVersionUID;
  _classDescFlags = ctm->flags;
  int fieldsize = ctm->fieldSize();
  _fields = new FieldDescriptionArray(fieldsize);
  for (int i = 0; i < fieldsize; ++i)
  {
    _fields[i] = new FieldDescription(ctm->fields[i]);
  }
  //RbyteArray _block;
  RClass supercls = Class::forName(ctm->acdk_name)->getSuperclass();
  if (supercls == Nil)
    return;
  const ClassTypeMapping* superctm = ClassTypeMapping::findAcdkClass(supercls->getName()->c_str());
  if (superctm == 0)
    return;
  _super = new ClassDescription(superctm);
}

RClassDescription 
ClassDescription::read(IN(RJavaObjectReader) in)
{
  // classDescFlags fields classAnnotation superClassDesc 
  // 
  RClassDescription cd = new ClassDescription();
  cd->_className = in->readUtf();
  cd->_classSerialVersionUID = in->readLong();\
  in->registerNewObject(&cd);
  //cd->_handle = in->readInt(); // not in stream!
  cd->_classDescFlags = (byte)in->read();
  int fieldcount = in->readShort();
  cd->_fields = new FieldDescriptionArray(fieldcount);
  for (int i = 0; i < fieldcount; ++i)
  {
    cd->_fields[i] = FieldDescription::read(in);
  }
  cd->_block = in->readBlock();
  cd->_super = in->readClassDesc(); // super
  return cd;
}

void 
ClassDescription::write(IN(RJavaObjectWriter) out)
{
  if (_class != Nil && _class->isArray() && _class->getArrayElementClass()->isPrimitive() == false)
  {
    out->writeShortUtf("[L" + _className + ";");
    out->writeLong(_ctm->arraySerialVersionUID);
    out->writeChar(_classDescFlags);
    out->writeShort(0); // fields count
  } else {
    out->writeShortUtf(_className);
    out->writeLong(_classSerialVersionUID);
    out->writeChar(_classDescFlags);
    out->writeShort(_fields->length());
    for (int i = 0; i < _fields->length(); ++i)
    {
      _fields[i]->write(out);
    }
  }
  out->registerNewObject(this);
  out->writeBlock(_block);
  if (_class != Nil && _class->isArray())
  {
    out->writeClassDesc(Nil);
  } else {
    out->writeClassDesc(_super);
  }

}

} // namespace serialization
} // namespace java 
} // namespace acdk 
