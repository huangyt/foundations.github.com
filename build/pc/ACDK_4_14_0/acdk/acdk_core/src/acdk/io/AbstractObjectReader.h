// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" with express or implied warranty.
// Any commercial use of this software requires a license.
// 
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/AbstractObjectReader.h,v 1.21 2005/04/09 19:26:44 kommer Exp $

#ifndef acdk_io_AbstractObjectReader_h
#define acdk_io_AbstractObjectReader_h

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

#include "BinaryDataReader.h"
#include "ObjectReader.h"
#include "AbstractFilterReader.h"
#include "ObjectStreamException.h"
#include "AbstractObjectWriter.h"
#include "SerializedObjectDescriptor.h"

namespace acdk {
namespace io {


ACDK_DECL_CLASS(AbstractObjectReader);


/**
  Abstract basic implementation for a ObjectReader.
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.21 $
  @date $Date: 2005/04/09 19:26:44 $
  @see AbstractObjectWriter
*/
class ACDK_CORE_PUBLIC AbstractObjectReader 
: extends ::acdk::io::AbstractFilterReader
, implements ::acdk::io::ObjectReader
{
  ACDK_WITH_METAINFO(AbstractObjectReader)
private:
  int _serializeFlags;
  ::acdk::util::RTreeMap _lref;
  RObjectArray _lrefs;
  /**
    Integer -> RString;
  */
  RStringArray _stringCache;
  //::acdk::util::RTreeMap _stringCache;
  //int _maxStringId;
public:
  AbstractObjectReader(IN(RReader) in, int flags = SerializeDefaultFlags )
  : AbstractFilterReader(in)
  , _serializeFlags(flags)
  , _lrefs(new (allocator()) ObjectArray(0))
  , _stringCache(new (allocator()) StringArray(0))
  {
    //if (joinStrings() == true)
    //  _stringCache = new ::acdk::util::TreeMap();
  }
  
  virtual jlong seek(SeekPos seekrel, jlong seekpos) { return AbstractFilterReader::seek(seekrel, seekpos); }
  virtual jlong skip(jlong n) { return AbstractFilterReader::skip(n); }
  virtual void reset() { AbstractFilterReader::reset(); }
  virtual void setIn(IN(RReader) reader) { AbstractFilterReader::setIn(reader); }

  virtual RStorage getStorage() { return AbstractFilterReader::getStorage(); }
  virtual RReader getStorageReader() { return AbstractFilterReader::getStorageReader(); }

  virtual RObject readObject();
  RObject readObject2();

  virtual RObject readObject(IN(::acdk::lang::RClass) cls);
  RObject readObject2(IN(::acdk::lang::RClass) cls);
  
  bool isNamed() { return (_serializeFlags & SerializeNamed); }
  bool isLabeled() { return (_serializeFlags & SerializeLabeled); }
  bool isTagged() { return (_serializeFlags & SerializeTagged); }
  bool isReduced() { return _serializeFlags & SerializeReduced; }
  bool joinStrings() { return _serializeFlags & SerializeJoinedStrings; }
  bool withSerialVersionUID() { return _serializeFlags & SerializeCheckCompatVersion; }
  
  /**
    Reads start of Tag. 
    For example '<int>' for a int type in a XML style Reader.
    This function will be called by writeObject 
    if the object should be written with tags
    if the expected tag does not exists, throw 
    ObjectStreamException
  */
  virtual RString readTagStart(IN(RString) key, IN(RString) expected = Nil) = 0;
  
  /**
    Writes end of Tag. 
    For example '</int>' for a int type.
    This function will be called by writeObject 
    if the object should be written with tags
  */
  virtual RString readTagEnd(IN(RString) key, IN(RString) expected = Nil) = 0;

  /**
    reads from the stream the Object Id (for example the fully
    qualified class name) and create an new Object
    if withSerialVersionUID() is true, the should be read and checked agains local class
  */
  virtual RClass readClassId() = 0;
  /**
    To enable cyclic containment Objects.
    
  */
  virtual int readObjectLocalId() 
  {
    return readIntElement();
  }
  virtual void defaultReadObject(IN(RClass) cls, IN(RObject) obj);
  virtual RClass readClassDescriptor(IN(RClass) cls);

  void  readObjectHierarchy(IN(RClass) cls, IN(RObject) obj);
  /**
    From ::acdk::io::DataReader.
  */
  virtual bool readBoolean() = 0;
  virtual char readChar() = 0;
  virtual double readDouble() = 0;
  virtual float readFloat() = 0;
  virtual int readInt() = 0;
  virtual jlong readLong() = 0;
  virtual short readShort() = 0;
  virtual RString readString();
  

  virtual RString readStringImpl() = 0;
  virtual RbyteArray readOpaque() 
  {
    int len = readIntElement();
    RbyteArray erg = new (allocator()) byteArray(len);
    read(erg);
    return erg;
  }
  foreign virtual acdk::lang::dmi::ScriptVar readScriptVar(bool withTypeInfo = true, bool withFlags = true);

  RString readStringElement()
  {
    if (isTagged() == true) {
      readTagStart(String::GetClass()->getName());
      RString erg = readString();
      readTagEnd(String::GetClass()->getName());
      return erg;
    } else {
      return readString();
    }
  }
  bool readBooleanElement()
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::Boolean::getTYPE()->getName());
      bool erg = readBoolean();
      readTagEnd(::acdk::lang::Boolean::getTYPE()->getName());
      return erg;
    } else {
      return readBoolean();
    }
  }
  char readCharElement()
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::Character::getTYPE()->getName());
      bool erg = readChar();
      readTagEnd(::acdk::lang::Character::getTYPE()->getName());
      return erg;
    } else {
      return readChar();
    }
  }
  uc2char readUcCharElement()
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::UnicodeCharacter::getTYPE()->getName());
      bool erg = readUcChar();
      readTagEnd(::acdk::lang::UnicodeCharacter::getTYPE()->getName());
      return erg;
    } else {
      return readUcChar();
    }
  }
  byte readByteElement()
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::Byte::getTYPE()->getName());
      byte erg = (byte)readChar();
      readTagEnd(::acdk::lang::Byte::getTYPE()->getName());
      return erg;
    } else {
      return (byte)readChar();
    }
  }
  short readShortElement()
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::Short::getTYPE()->getName());
      short erg = readShort();
      readTagEnd(::acdk::lang::Short::getTYPE()->getName());
      return erg;
    } else {
      return readShort();
    }
  }
  int readIntElement() 
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::Integer::getTYPE()->getName());
      int erg = readInt();
      readTagEnd(::acdk::lang::Integer::getTYPE()->getName());
      return erg;
    } else {
      return readInt();
    }
  }
  jlong readLongElement() 
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::Long::getTYPE()->getName());
      jlong erg = readLong();
      readTagEnd(::acdk::lang::Long::getTYPE()->getName());
      return erg;
    } else {
      return readLong();
    }
  }
  float readFloatElement() 
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::Float::getTYPE()->getName());
      float erg = readFloat();
      readTagEnd(::acdk::lang::Float::getTYPE()->getName());
      return erg;
    } else {
      return readFloat();
    }
  }
  double readDoubleElement() 
  {
    if (isTagged() == true) {
      readTagStart(::acdk::lang::Double::getTYPE()->getName());
      double erg = readDouble();
      readTagEnd(::acdk::lang::Double::getTYPE()->getName());
      return erg;
    } else {
      return readDouble();
    }
  }
protected:
  void resetLRefs()
  {
    //_lrefs.resize(0);
      _lrefs = new (allocator()) ObjectArray(0);
    //_stringCache->resize(0);
    _stringCache = new (allocator()) StringArray(0);
    /*
    if (_stringCache != Nil)
    {
      _stringCache = new ::acdk::util::TreeMap();
      _maxStringId = 0;
    }*/
  }
  RObject _readBasicArray(IN(RClass) cls, int length);
  RObject _readObject(IN(::acdk::lang::RClass) cls);
  ::acdk::lang::dmi::ScriptVar _readScriptVar(int type, int flags);
};




} // io
} // acdk

#endif //acdk_io_AbstractObjectReader_h

