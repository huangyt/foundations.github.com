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
// $Header: /cvsroot/acdk/acdk/acdk_java/src/acdk/java/serialization/JavaObjectReader.cpp,v 1.12 2005/02/05 10:45:11 kommer Exp $


#include "JavaObjectReader.h"
#include "ClassDescription.h"
#include <acdk/locale/UTF8Encoding.h>

/*
stream:
  magic version contents

contents:
  content
  contents content

content:
  object
  blockdata

object:
  newObject
  newClass
  newArray
  newString
  newClassDesc
  prevObject
  nullReference
  exception
  TC_RESET

newClass:
  TC_CLASS classDesc newHandle

classDesc:
  newClassDesc
  nullReference
  (ClassDesc)prevObject      // an object required to be of type
                             // ClassDesc

superClassDesc:
  classDesc

newClassDesc:
  TC_CLASSDESC className serialVersionUID newHandle classDescInfo
  TC_PROXYCLASSDESC newHandle proxyClassDescInfo

classDescInfo:
  classDescFlags fields classAnnotation superClassDesc 

className:
  (utf)

serialVersionUID:
  (long)

classDescFlags:
  (byte)                  // Defined in Terminal Symbols and
                            // Constants

proxyClassDescInfo:
  (int)<count> proxyInterfaceName[count] classAnnotation
      superClassDesc

proxyInterfaceName:
  (utf)

fields:
  (short)<count>  fieldDesc[count]

fieldDesc:
  primitiveDesc
  objectDesc

primitiveDesc:
  prim_typecode fieldName

objectDesc:
  obj_typecode fieldName className1

fieldName:
  (utf)

className1:
  (String)object             // String containing the field's type,
                             // in field descriptor format

classAnnotation:
  endBlockData
  contents endBlockData      // contents written by annotateClass

prim_typecode:
  `B'	// byte
  `C'	// char
  `D'	// double
  `F'	// float
  `I'	// integer
  `J'	// long
  `S'	// short
  `Z'	// boolean

obj_typecode:
  `[`	// array
  `L'	// object

newArray:
  TC_ARRAY classDesc newHandle (int)<size> values[size]

newObject:
  TC_OBJECT classDesc newHandle classdata[]  // data for each class

classdata:
  nowrclass                 // SC_SERIALIZABLE & classDescFlag &&
                            // !(SC_WRITE_METHOD & classDescFlags)
  wrclass objectAnnotation  // SC_SERIALIZABLE & classDescFlag &&
                            // SC_WRITE_METHOD & classDescFlags
  externalContents          // SC_EXTERNALIZABLE & classDescFlag &&
                            // !(SC_BLOCKDATA  & classDescFlags
  objectAnnotation          // SC_EXTERNALIZABLE & classDescFlag&& 
                            // SC_BLOCKDATA & classDescFlags

nowrclass:
  values                    // fields in order of class descriptor

wrclass:
  nowrclass

objectAnnotation:
  endBlockData
  contents endBlockData     // contents written by writeObject
                            // or writeExternal PROTOCOL_VERSION_2.

blockdata:
  blockdatashort
  blockdatalong

blockdatashort:
  TC_BLOCKDATA (unsigned byte)<size> (byte)[size]

blockdatalong:
  TC_BLOCKDATALONG (int)<size> (byte)[size]

endBlockData	:
  TC_ENDBLOCKDATA

externalContent:          // Only parseable by readExternal
  ( bytes)                // primitive data
    object

externalContents:         // externalContent written by 
  externalContent         // writeExternal in PROTOCOL_VERSION_1.
  externalContents externalContent

newString:
  TC_STRING newHandle (utf)
  TC_LONGSTRING newHandle (long-utf)

prevObject
  TC_REFERENCE (int)handle

nullReference
  TC_NULL

exception:
  TC_EXCEPTION reset (Throwable)object	 reset 

magic:
  STREAM_MAGIC

version
  STREAM_VERSION

values:          // The size and types are described by the
                 // classDesc for the current object

newHandle:       // The next number in sequence is assigned
                 // to the object being serialized or deserialized

reset:           // The set of known objects is discarded
                 // so the objects of the exception do not
                 // overlap with the previously sent objects 
                 // or with objects that may be sent after 
                 // the exception


  */
namespace acdk {
namespace java {
namespace serialization {

using namespace acdk::lang::dmi;

JavaObjectReader::JavaObjectReader(IN(::acdk::io::RReader) in)
: BinaryObjectReader(in)
, _prevObjects(new JavaObjectReadWriteCache())
{
  _readStreamHeader();
}




void 
JavaObjectReader::_readStreamHeader()
{
  short magic = readShort();
  if (magic != STREAM_MAGIC)
    return;
  short version = readShort();
  if (version != 5)
    return;

}

RString 
JavaObjectReader::readUtf()
{
  int len = readShort(); // is byte len
  acdk::locale::UTF8Decoder dec;
  StringBuffer sb;
  while (dec.bytesReaded() < len)
    sb.append((ucchar)dec.decodeToChar(this));
  return sb.toString();
  /*bytesReaded()
  return dec.decodeToString(this, len);
  */
  /*
  
  RbyteArray ba = new byteArray(len);
  BinaryObjectReader::read(ba);
  return 
  return new String(ba);
  */
}

RString 
JavaObjectReader::readLongUtf()
{
  int len = readInt();
  acdk::locale::UTF8Decoder dec;
   StringBuffer sb;
  while (dec.bytesReaded() < len)
    sb.append((ucchar)dec.decodeToChar(this));
  return sb.toString();

  //return dec.decodeToString(this, len);
  /*
  RbyteArray ba = new byteArray(len);
  BinaryObjectReader::read(ba);
  return new String(ba);
  */
}

RString 
JavaObjectReader::readString()
{
  int tp = read();
  if (tp == -1)
    return Nil;
  if (tp != TC_STRING && tp != TC_LONGSTRING)
    THROW1_FQ(::acdk::io::, ObjectStreamException, "Expecting a String in stream");
  RString str;
  if (tp == TC_STRING)
  {
    str = readUtf();
  } else {
    str = readLongUtf();
  }
  registerNewObject(&str);
  return str;
}

RbyteArray 
JavaObjectReader::readBlock()
{
  int tc = read();
  if (tc == -1)
    return Nil;
  if (tc == TC_ENDBLOCKDATA)
    return Nil;

  RbyteArray ret;
  int size = 0;
  if (tc == TC_BLOCKDATA)
    size = readChar();
  else if (tc == TC_ENDBLOCKDATA)
    size = readInt();
  ret = new byteArray(size);
  read(ret);
  return ret;
}

RClassDescription 
JavaObjectReader::readClassDesc()
{
  int tc = read();
  if (tc == -1)
    return Nil;
  if (tc == TC_NULL)
    return Nil;
  if (tc == TC_REFERENCE)
    return (RClassDescription)_prevObjects->get(readInt());

  RClassDescription cd = ClassDescription::read(this);
  return cd;
}


RObject 
JavaObjectReader::createObject(IN(RString) javaclassname, const ClassTypeMapping* ctm)
{
  RString cname = javaclassname;
  if (ctm != 0)
  {
    cname = ctm->acdk_name;
  } else {
    if (cname->startsWith("java/") == true)
      cname = "acdk/" + cname->substr(strlen("java/"));
  }
  RClass cls = Class::forName(cname);
  if (cls == Nil)
    return Nil;
  return cls->newInstance();
}



#define READ_ARRAY(size, arraytype, readType) \
do { \
  R##arraytype##Array array = new arraytype##Array(size); \
  registerNewObject(&array); \
  for (int i = 0; i < size; ++i) \
    array[i] = read##readType(); \
  return (RObject)array; \
} while (false) 

//foreign virtual 
RObject 
JavaObjectReader::readObject()
{
  int tp = read();
  if (tp == -1)
    return Nil;
  if (tp == TC_NULL)
    return Nil;
  if (tp == TC_REFERENCE)
  {
    return _prevObjects->get(readInt());
  }
  RObject obj = readObject2(tp);
  return obj;
}

//foreign virtual 
RObject 
JavaObjectReader::readObject2(int tp)
{
  if (tp == TC_STRING || tp == TC_LONGSTRING)
  {
    RString str;
    if (tp == TC_STRING)
    {
      str = readUtf();
    } else {
      str = readLongUtf();
    }
    registerNewObject(&str);
    return &str;
  } 

  if (tp == TC_OBJECT)
  {
    RClassDescription clsdesc = readClassDesc();
    
    const ClassTypeMapping* ctm = ClassTypeMapping::findJavaClass(clsdesc->className());
    if (clsdesc->flags() & SC_SERIALIZABLE)
    {
      if (ctm->read_func != 0)
      {
        RObject obj = ctm->read_func(this, ctm);
        return obj;
      }
      RObject obj = createObject(clsdesc->className(), ctm);
      registerNewObject(obj); 
      for (int i = 0; i < clsdesc->fields()->length(); ++i)
      {
        RFieldDescription fd = clsdesc->fields()[i];
        RString fieldname = fd->fieldName();
        if (ctm != 0)
        {
          const MemberTypeMapping* mtm = ctm->findJavaMember(fd->_fieldName->c_str());
          if (mtm != 0)
          {
            fieldname = mtm->acdk_field;
          }
        }
        SysField  field = obj->getInternalField(fieldname, MiNonStatic); //SysField::getField(fi, ScriptVar());
        switch (fd->typeCode())
        {
        case 'B': 
          field.set(read());
          break;
        case 'C':
          field.set(read());
          break;
        case 'U':
          field.set(readShort());
          break;
        case 'D': 
          field.set(readDouble());
          break;
        case 'F': 
          field.set(readFloat());
          break;
        case 'I':
          field.set(readInt());
          break;
        case 'J':
          field.set(readLong());
          break;
        case 'S':
          field.set(readShort());
          break;
        case 'Z':
          field.set(readBoolean());
          break;
        case '[':
          field.set(readObject());
          break;
        case 'L':
          field.set(readObject());
          break;
        }
      }
      return obj;
    }
 
  } else if (tp == TC_ARRAY) {
    RClassDescription clsdesc = readClassDesc();
    int arraySize = readInt();
    switch (clsdesc->className()->charAt(1))
    {
    case 'B': 
      READ_ARRAY(arraySize, byte, Char);
    case 'C':
    case 'U':
    {
      RuccharArray array = new uccharArray(arraySize);
      registerNewObject(&array); 
      for (int i = 0; i < arraySize; ++i) 
      {
        short s = readShort();
        array[i] = (ucchar)s;
      }
      return &array;
    }/*
    case 'C':
    {
      RcharArray array = new charArray(arraySize);
      registerNewObject(&array); 
      for (int i = 0; i < arraySize; ++i) 
      {
        short s = readShort();
        if (s > 255)
        {
          //####
        }
        array[i] = (char)s;
      }
      return &array;
    }*/
    case 'D': 
      READ_ARRAY(arraySize, double, Double);
    case 'F': 
      READ_ARRAY(arraySize, float, Float);
    case 'I':
      READ_ARRAY(arraySize, int, Int);
    case 'J':
      READ_ARRAY(arraySize, long, Long);
    case 'S':
      READ_ARRAY(arraySize, short, Short);
    case 'Z':
      READ_ARRAY(arraySize, bool, Boolean);
    case '[':
      THROW1_FQ(::acdk::io::, ObjectStreamException, "Arrays from Arrays not supported yet: " + clsdesc->className());
      // ### not supported field.set(readObject());
      break;
    case 'L':
    {
      RString cname = clsdesc->className();
      cname = cname->substr(2, cname->length() - 1);
      
      const ClassTypeMapping* ctm = ClassTypeMapping::findJavaClass(cname);
      if (ctm == 0)
        THROW1_FQ(::acdk::io::, ObjectStreamException, "No Typemapping found for java: " + cname);
      RObject oa = Class::create_arrayInstance(Class::forName(ctm->acdk_name), arraySize);
      registerNewObject(oa); 
      acdk::lang::dmi::SysFields fields = oa->getInternalFields(0);

      //RObjectArray oa = new ObjectArray(arraySize);
      for (int i = 0; i < arraySize; ++i)
      {
        fields[i + 1].set(readObject());
      }
      return oa;
    }
    }
    
  } else if (tp == TC_BLOCKDATA || tp == TC_BLOCKDATALONG) {
    int size = 0;
    if (tp == TC_BLOCKDATA)
      size = (byte)read();
    else
      size = readInt();
    RbyteArray ba = new byteArray(size);
    read(ba); // ignore this 
    return readObject();
    //return ba;
    //THROW1_FQ(::acdk::io::, ObjectStreamException, RString("TC_BLOCKDATA not supported"));
  }
  THROW1_FQ(::acdk::io::, ObjectStreamException, RString("Unknown Stream element type: ") + tp);
  return Nil;
}

} // namespace serialization
} // namespace java 
} // namespace acdk 


