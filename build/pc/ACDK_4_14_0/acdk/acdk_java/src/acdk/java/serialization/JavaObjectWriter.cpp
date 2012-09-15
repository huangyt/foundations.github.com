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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/JavaObjectWriter.cpp,v 1.12 2005/02/05 10:45:11 kommer Exp $

#include "JavaObjectWriter.h"
#include <acdk/locale/UTF8Encoding.h>
#include <acdk/io/MemWriter.h>

namespace acdk {
namespace java {
namespace serialization {

using namespace acdk::lang::dmi;
using namespace acdk::lang::reflect;

JavaObjectWriter::JavaObjectWriter(IN(::acdk::io::RWriter) out)
: BinaryObjectWriter(out)
, _prevObjects(new JavaObjectReadWriteCache())
{
  _writeStreamHeader();
}

void 
JavaObjectWriter::_writeStreamHeader()
{
  writeShort(STREAM_MAGIC);
  writeShort(5); // version
}

#define WRITE_PRIM_ARRAY(class, obj, type, Type, WriteType) \
  if (class->objectClazzInfo() == ::acdk::lang::dmi::ClazzInfo::get##Type##Clazz()) \
{ \
  R##type##Array ta = (R##type##Array)obj; \
  int arlen = ta->length(); \
  writeInt(arlen); \
  for (int i = 0; i < arlen; ++i) \
  { \
    write##WriteType(ta[i]); \
  } \
}

void 
JavaObjectWriter::writeString(IN(RString) str)
{
  writeUtf(str);
  registerNewObject(&str);
}



void 
JavaObjectWriter::writeObject(IN(RObject) obj)
{
  if (obj == Nil)
  {
    writeChar(TC_NULL);
    return;
  }
  int prevo = findPrevObject(obj);
  if (prevo != -1) 
  {
    writeChar(TC_REFERENCE);
    writeInt(prevo + baseWireHandle);
    return;
  }

  if (instanceof(obj, String) == true)
  {
    writeString(RString(obj));
    return;
  }
  RClass cls = obj->getClass();
  RString clsname = cls->getName();
  
  if (cls->isArray() == true)
  {
    RString clsname = cls->getName();
    RClass arrayelement = cls->getArrayElementClass();
    if (arrayelement->isPrimitive() == false)
      clsname = clsname->substr(1);
    const ClassTypeMapping* ctm = ClassTypeMapping::findAcdkClass(clsname);
    if (ctm == 0)
      THROW1_FQ(::acdk::io::, ObjectStreamException, 
        "No type mapping found for: " + cls->getName());

    writeChar(TC_ARRAY);
    RClassDescription clsdesc = getClassDescr(ctm, cls);
    clsdesc->setClass(cls);
    writeClassDesc(clsdesc);
    registerNewObject(obj);
    if (arrayelement->isPrimitive() == true)
    {
      WRITE_PRIM_ARRAY(arrayelement, obj, bool, Bool, Boolean)
      //WRITE_PRIM_ARRAY(arrayelement, obj, char, Char, Char)
      WRITE_PRIM_ARRAY(arrayelement, obj, byte, Byte, Char)
      WRITE_PRIM_ARRAY(arrayelement, obj, ucchar, UcChar, Short)
      WRITE_PRIM_ARRAY(arrayelement, obj, short, Short, Short)
      WRITE_PRIM_ARRAY(arrayelement, obj, int, Int, Int)
      WRITE_PRIM_ARRAY(arrayelement, obj, long, Long, Long)
      WRITE_PRIM_ARRAY(arrayelement, obj, float, Float, Float)
      WRITE_PRIM_ARRAY(arrayelement, obj, double, Double, Double)
      if (arrayelement->objectClazzInfo() == ::acdk::lang::dmi::ClazzInfo::getCharClazz()) 
      { 
        RcharArray ta = (RcharArray)obj; 
        int arlen = ta->length();
        writeInt(arlen); 
        for (int i = 0; i < arlen; ++i) 
        { 
          writeShort(ta[i]); 
        } 
      }
      return;
    }
    acdk::lang::dmi::SysFields fields = obj->getInternalFields(MiNonStatic);
    writeInt(*fields[0].cont.ival);
    for (int i = 1; i < fields.size(); ++i)
    {
      RObject el = *fields[i].cont.oval;
      writeObject(el);
    }

    return;
  }
  const ClassTypeMapping* ctm = ClassTypeMapping::findAcdkClass(cls->getName());
  if (ctm == 0)
    THROW1_FQ(::acdk::io::, ObjectStreamException, 
      "No type mapping found for: " + cls->getName());

  
  writeChar(TC_OBJECT);
  
  RClassDescription clsdesc = getClassDescr(ctm, cls);
  clsdesc->setClass(cls);
  writeClassDesc(clsdesc);
  registerNewObject(obj);
  if (ctm->write_func != 0)
  {
    ctm->write_func(this, ctm, obj);
    return;
  }
  for (int i = 0; i < clsdesc->fields()->length(); ++i)
  {
    RFieldDescription fd = clsdesc->fields()[i];
    RString fieldname = fd->fieldName();
    const MemberTypeMapping* mtm = ctm->findJavaMember(fd->_fieldName->c_str());
    if (mtm == 0)
      continue;
    SysField field = obj->getInternalField(mtm->acdk_field, MiNonStatic);
    //ScriptVar sv = obj->getMember(*fieldname);

    //fieldname = mtm->java_field;
    //const ClazzFieldInfo* fi = ci->findField(ci, *fieldname, 0, false);
    //SysField  field = SysField::getField(fi, ScriptVar());
    //acdk::lang::dmi::SysField field = obj->lookupMember(mtm->acdk_field, 0);
    switch (field.type)
    {
    case acdk::lang::dmi::SysField::FT_Bool: writeBoolean(*field.cont.zval); break;
    case acdk::lang::dmi::SysField::FT_Char: writeChar(*field.cont.cval); break;
    case acdk::lang::dmi::SysField::FT_UcChar: writeUcChar(*field.cont.cval); break;
    case acdk::lang::dmi::SysField::FT_Byte: writeChar(*field.cont.bval); break;
    case acdk::lang::dmi::SysField::FT_Short: writeShort(*field.cont.sval); break;
    case acdk::lang::dmi::SysField::FT_Int: writeInt(*field.cont.ival); break;
    case acdk::lang::dmi::SysField::FT_JLong: writeLong(*field.cont.jlval); break;
    case acdk::lang::dmi::SysField::FT_Float: writeFloat(*field.cont.fval); break;
    case acdk::lang::dmi::SysField::FT_Double: writeDouble(*field.cont.dval); break;
    case acdk::lang::dmi::SysField::FT_Object:
      writeObject(*field.cont.oval);
      break;
    default:
      THROW1_FQ(::acdk::io::, ObjectStreamException, 
             "Invalide type field in " + cls->getName());
    }

  }
}

namespace {

void writeJavaString(JavaObjectWriter& out, IN(RString) str, bool longstr)
{
  acdk::io::MemWriter mout;
  acdk::locale::UTF8Encoder dec;
  dec.encode(&mout, str);
  RbyteArray buf = mout.getBuffer();
  if (longstr == true)
  {
    out.writeInt(buf->length());
  } 
  else
  {
    out.writeShort(short(buf->length()));
  }
  out.write(buf);
}

} // anon namespace

void 
JavaObjectWriter::writeShortUtf(IN(RString) str)
{
  writeJavaString(*this, str, false);
}

void 
JavaObjectWriter::writeUtf(IN(RString) str)
{
  int len = str->length();
  if (len >= Short::MAX_VALUE)
    writeChar(TC_LONGSTRING);
  else
    writeChar(TC_STRING);
  writeJavaString(*this, str, len >= Short::MAX_VALUE);
}

void 
JavaObjectWriter::writeBlock(IN(RbyteArray) block)
{
  if (block == Nil)
  {
    writeChar(TC_ENDBLOCKDATA);
    return;
  }
  int size = block->length();
  if (size < Byte::MAX_VALUE - 1)
  {
    writeChar(TC_BLOCKDATA);
    writeChar(byte(size));
  } else {
    writeChar(TC_BLOCKDATALONG);
    writeInt(size);
  }
  write(block);
}

void 
JavaObjectWriter::writeClassDesc(IN(RClassDescription) cdesc)
{
  if (cdesc == Nil)
  {
    writeChar(TC_NULL);
    return;
  }
  int prevo = findPrevObject(&cdesc);
  if (prevo != -1) 
  {
    writeChar(TC_REFERENCE);
    writeInt(prevo + baseWireHandle);
    return;
  }
  writeChar(TC_CLASSDESC);
  cdesc->write(this);
  
}

RClassDescription 
JavaObjectWriter::getClassDescr(const ClassTypeMapping* ctm, IN(RClass) cls)
{
  RObjectArray objects = _prevObjects->objects();
  for (int i = 0; i < objects->length(); ++i)
  {
    if (instanceof(objects[i], ClassDescription) == true)
    {
      const ClassTypeMapping* octm = RClassDescription(objects[i])->classTypeMapping();
      RClass ocls = RClassDescription(objects[i])->getObjectClass() ;
      if (octm == ctm && ocls == cls)
      {
        return RClassDescription(objects[i]);
      }
    }
  }
  return new ClassDescription(ctm);
}

} // namespace serialization
} // namespace java 
} // namespace acdk 


