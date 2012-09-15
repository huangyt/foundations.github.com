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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractObjectWriter.h,v 1.27 2005/03/14 12:20:44 kommer Exp $

#ifndef acdk_io_AbstractObjectWriter_h
#define acdk_io_AbstractObjectWriter_h

#include <acdk.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/util/HashMap.h>
#include <acdk/util/TreeMap.h>

#include "BinaryDataWriter.h"
#include "ObjectWriter.h"
#include "AbstractFilterWriter.h"


namespace acdk {
namespace io {

/**
  a combination of SerializeFlags controls how to 
  serialize in acdk::io::AbstractObjectWriter and acdk::io::AbstractObjectReader
*/
enum SerializeFlags 
{
/**
  Write the Class name of Object into Stream.
  This flag is needed, if read/writeObject will be used
  without given Class
  */
  SerializeNamed =    0x0001,
/**
  Stream will structured by tags.
  The Implemenation of ObjectWriter/Reader must
  implement the given writeTagStart() and writeTagEnd()
*/
  SerializeTagged =   0x0002,
/**
  The member fields of an class will be written
  with the fields names.
*/
  SerializeLabeled =  0x0004,
/**
  If cyclic object references in the Object
  member exists, this flag advise to write
  the Object reduced into the stream. dublicated
  Object instances will only written once.

*/
  SerializeReduced =   0x0008,

/**
  String with the same content will be joined
  to one instance.
  Will reduce file size, especially if also
  SerializeNamed or SerializeTagged or SerializeLabeled
  will be used.
  Does not make sense without SerializeReduced flag.
*/
  SerializeJoinedStrings = 0x0010,


/**
  Only serialize the classes, which direcly implements
  the Serializable interface.
  If this is not set, it will also serialize classes which
  indirectly implements Serializable
*/
  SerializeOnlySerializable = 0x0020,
  /**
  Serialize all members, independed this class
  have implemented directly or inderectly the Serializable interface
  */
  SerializeAll               = 0x0040,
/**
  In connection with SerializeOnlySerializeable. If
  an object not implemnting Serializable interface
  will be written, it simply writes Nil instead of
  throwing exception
*/
  SerializeIgnoreNotSerializable = 0x0080,

  /**
    check the SerialVersionUID of the class, if the classes
    are compatible or not.
    Compatible serialization will only work, if SerializeNamed
    is used.
  */
  SerializeCheckCompatVersion           = 0x0100,

  SerializeUseSerializedObjectDescriptor =  0x0200,

  SerializeDefaultFlags     = SerializeNamed
                            | SerializeReduced
                            | SerializeCheckCompatVersion | SerializeLabeled

                            
};
ACDK_DEF_LIB_ENUM(ACDK_CORE_PUBLIC, SerializeFlags);



ACDK_DECL_CLASS(AbstractObjectWriter);


/**
  Standard implementation for Writing Objects into a stream
  (Serialization/Marshaling)
  API: ACDK
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.27 $
  @date $Date: 2005/03/14 12:20:44 $

  @see AbstractObjectReader
*/
class ACDK_CORE_PUBLIC AbstractObjectWriter
: extends ::acdk::io::AbstractFilterWriter
, implements ::acdk::io::ObjectWriter
{
  ACDK_WITH_METAINFO(AbstractObjectWriter)
private:
  int _serializeFlags;
  ObjectArray _lrefs;
  /**
    RString -> objectId;
  */
  ::acdk::util::RTreeMap _stringCache;

  int _maxStringId;
public:
   /**

      @param out a storage writer, where to write the data
      @param flags please refer to the Serialize* flags

  */
  AbstractObjectWriter(IN(RWriter) out,
                      int flags = SerializeDefaultFlags )
  : AbstractFilterWriter(out)
  , _serializeFlags(flags)
  , _lrefs(0)
  , _maxStringId(0)
  {
    if (joinStrings() == true)
      _stringCache = new ::acdk::util::TreeMap();
  }
  void resetLocalRefs()
  {
    _lrefs.resize(0);
    if (_stringCache != Nil)
    {
      _stringCache = new ::acdk::util::TreeMap();
      _maxStringId = 0;
    }

  }
  // Writer
  void flush() { AbstractFilterWriter::flush(); }
  foreign void write(byte b) { AbstractFilterWriter::write(b); }
  foreign void write(const byte* cstr, int offset, int len) { Writer::write(cstr, offset, len); }
  // FilterWriter
  foreign void setOut(IN(RWriter) writer) { AbstractFilterWriter::setOut(writer); }
  foreign RStorage getStorage() { return AbstractFilterWriter::getStorage(); }
  foreign RWriter getStorageWriter() { return AbstractFilterWriter::getStorageWriter(); }

  virtual void writeObject(IN(RObject) obj);

  virtual void writeObject(IN(RClass) cls, IN(RObject) obj);


  bool isNamed() { return (_serializeFlags & SerializeNamed); }
  bool isLabeled() { return (_serializeFlags & SerializeLabeled); }
  bool isTagged() { return (_serializeFlags & SerializeTagged); }
  bool isReduced() { return _serializeFlags & SerializeReduced; }
  bool joinStrings() { return _serializeFlags & SerializeJoinedStrings; }
  bool withSerialVersionUID() { return _serializeFlags & SerializeCheckCompatVersion; }
  /**
    Writes start of Tag.
    This function will be written, toIN(
    For example '<int>' for a int type in a XML style writer.
    This function will be called by writeObject
    if the object should be written with tags
  */
  virtual void writeTagStart(IN(RString) key, IN(RString) value = Nil) = 0;

  /**
    Writes end of Tag.
    This function will be written, to
    For example '</int>' for a int type.
    This function will be called by writeObject
    if the object should be written with tags
  */
  virtual void writeTagEnd(IN(RString) key, IN(RString) value = Nil) = 0;

  /**
    Write boot-straping ID for the given, so the corresponding
    AbstractObjectReader::readClassId() can create an
    Object instance.
    if withSerialVersionUID() is true the SerialVersionUID should be written too.
    @param cls may be Nil for writing a Nil object
    
  */
  virtual void writeClassId(IN(::acdk::lang::RClass) cls) = 0;
  
  /**
    In case ObjectWriter should handle writing Objects which
    may have cyclic references, for each Object a local reference
    id. Recursive and duplicated elements will be written only
    one in the stream.
    If the implementing ObjectReader does not support
    local reference resolutions, it should throw NotSupportedException
  */
  virtual void writeObjectLocalId(int id)
  {
    writeIntElement(id);
  }

  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  foreign virtual void writeChar(char b) = 0;
  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  foreign virtual void writeUcChar(uc2char b) = 0;
  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  foreign virtual void writeShort(short b) = 0;

  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  foreign virtual void writeInt(int b) = 0;

  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  foreign virtual void writeLong(jlong b) = 0;

  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  foreign virtual void writeFloat(float b) = 0;

  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  foreign virtual void writeDouble(double b) = 0;

  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  virtual void write(IN(RbyteArray) array, int offset = 0, int len = -1) = 0;

  /**
    From ::acdk::io::DataWriter.
    In the concrete class, you have to implement these classes
  */
  foreign virtual void writeOpaque(IN(RbyteArray) array)
  {
    writeIntElement(array->length());
    write(array);
  }

  /**
    From ::acdk::io::DataWriter.
    Classes derived from AbstractObjectWriter should
    overwrite writeStringImpl.
  */
  virtual void writeString(IN(RString) str);
  virtual void writeStringImpl(IN(RString) str) = 0;

  foreign virtual void writeScriptVar(acdk::lang::dmi::ScriptVar& sv, bool withTypeInfo = true, bool withFlags = true);

  void writeStringElement(IN(RString) str)
  {
    if (isTagged() == true) {
      writeTagStart(String::GetClass()->getName());
      writeString(str);
      writeTagEnd(String::GetClass()->getName());
    } else {
      writeString(str);
    }
  }
  void writeBooleanElement(bool b)
  {
    if (isTagged() == true) {
      writeTagStart(::acdk::lang::Boolean::getTYPE()->getName());
      writeBoolean(b);
      writeTagEnd(::acdk::lang::Boolean::getTYPE()->getName());
    } else {
      writeBoolean(b);
    }
  }
  void writeCharElement(char b)
  {
    if (isTagged() == true) {
      writeTagStart(::acdk::lang::Character::getTYPE()->getName());
      writeChar(b);
      writeTagEnd(::acdk::lang::Character::getTYPE()->getName());
    } else {
      writeChar(b);
    }
  }
  void writeUcCharElement(uc2char b)
  {
    if (isTagged() == true) {
      writeTagStart(::acdk::lang::UnicodeCharacter::getTYPE()->getName());
      writeChar(b);
      writeTagEnd(::acdk::lang::UnicodeCharacter::getTYPE()->getName());
    } else {
      writeUcChar(b);
    }
  }
  void writeShortElement(short b)
  {
    if (isTagged() == true) {
      writeTagStart(::acdk::lang::Short::getTYPE()->getName());
      writeShort(b);
      writeTagEnd(::acdk::lang::Short::getTYPE()->getName());
    } else {
      writeShort(b);
    }
  }
  void writeIntElement(int b)
  {
    if (isTagged() == true) {
      writeTagStart(::acdk::lang::Integer::getTYPE()->getName());
      writeInt(b);
      writeTagEnd(::acdk::lang::Integer::getTYPE()->getName());
    } else {
      writeInt(b);
    }
  }
  void writeLongElement(jlong b)
  {
    if (isTagged() == true) {
      writeTagStart(::acdk::lang::Long::getTYPE()->getName());
      writeLong(b);
      writeTagEnd(::acdk::lang::Long::getTYPE()->getName());
    } else {
      writeLong(b);
    }
  }
  void writeFloatElement(float b)
  {
    if (isTagged() == true) {
      writeTagStart(::acdk::lang::Float::getTYPE()->getName());
      writeFloat(b);
      writeTagEnd(::acdk::lang::Float::getTYPE()->getName());
    } else {
      writeFloat(b);
    }
  }
  void writeDoubleElement(double b)
  {
    if (isTagged() == true) {
      writeTagStart(::acdk::lang::Double::getTYPE()->getName());
      writeDouble(b);
      writeTagEnd(::acdk::lang::Double::getTYPE()->getName());
    } else {
      writeDouble(b);
    }
  }
  virtual void defaultWriteObject(IN(RClass) cls, IN(RObject) obj);

  virtual void writeUnshared(IN(RClass) cls, IN(RObject) obj);
  virtual void writeClassDescriptor(IN(RClass) cls, IN(RObject) obj);
  virtual void writeObjectHierarchy(IN(RClass) cls, IN(RObject) obj);
  /** used to write recursive objects */
  void writeObject2(IN(RObject) obj);
  /** used to write recursive objects */
  void writeObject2(IN(RClass) cls, IN(RObject) obj);
  
protected:

  /**
    looks if a string with same content already written.
    If found returns ID, otherwise -1
    */
  int _lookupStringRef(IN(RString) str);
  void _writeObject(IN(RClass) cls, IN(RObject) obj);
};




} // io
} // acdk

#endif //acdk_io_AbstractObjectWriter_h

