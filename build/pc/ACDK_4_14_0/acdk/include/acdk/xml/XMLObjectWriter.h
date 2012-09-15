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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/XMLObjectWriter.h,v 1.20 2005/04/10 12:52:41 kommer Exp $

#ifndef acdk_xml_XMLObjectWriter_h
#define acdk_xml_XMLObjectWriter_h

#include "Config.h"
#include <acdk.h>

#include <acdk/io/AbstractObjectWriter.h>
#include <acdk/util/HashMap.h>
#include "XMLObjectReader.h"

namespace acdk {
namespace xml {

using namespace acdk::lang;

USING_CLASS(::acdk::io::, Writer);
USING_CLASS(::acdk::io::, DataWriter);
USING_CLASS(::acdk::io::, ObjectWriter);
USING_CLASS(::acdk::io::, AbstractFilterWriter);
USING_CLASS(::acdk::util::, HashMap);

ACDK_DECL_CLASS(XMLObjectWriter);

/**
  API: ACDK<br/>
  Writes all ACDK Objects with meta-info to XML.
  This mechanism can also be used to store objects
  in a file.
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.20 $
  @date $Date: 2005/04/10 12:52:41 $
  @bug none known, but not very intensiv tested
  @seealso please refer to %ref[acdk_xml_XMLObjectReaderWriter_man]
*/
class ACDK_XML_PUBLIC  XMLObjectWriter
: extends ::acdk::io::AbstractObjectWriter
  
{
  ACDK_WITH_METAINFO(XMLObjectWriter)
private:
  //RHashMap _ptr2id; // RObject, Integer
  //int _maxLocalRef; 
  /** writing formated XML for easier to read */
  bool _doIndent;
  /** writing with member name tag, for documentation */
  //bool _withFieldInfo;
  /** helper for render as text */
  int _indent;

public:
  XMLObjectWriter(IN(RWriter) out, int flags = XMLSerializeDefaultFlags);
  /** writing formated XML for easier to read */
  void doIdent(bool indent) { _doIndent = indent; }
  
  /** writing XML with members element names */
  //use AbstractWriter void withFieldInfo(bool withinfo) { _withFieldInfo = withinfo; }
  
  //virtual void writeObject(RObject obj);
  //virtual void writeObject(RClass cls, RObject obj);

  foreign virtual void writeBoolean(bool b);
  foreign virtual void writeChar(char b);
  foreign virtual void writeUcChar(uc2char b);
  foreign virtual void writeShort(short b);
  foreign virtual void writeInt(int b);
  foreign virtual void writeLong(jlong b);
  foreign virtual void writeFloat(float b);
  foreign virtual void writeDouble(double b);
  foreign virtual void write(IN(RbyteArray) array, int offset = 0, int len = -1);
  foreign virtual void writeStringImpl(IN(RString) str);
  foreign virtual void writeOpaque(IN(RbyteArray) array);

  //foreign virtual void write(::acdk::lang::dmi::ScriptVar& arg);
  
  /**
    Writes start of Tag. 
    This function will be written, to 
    For example '<int>' for a int type in a XML style writer.
    This function will be called by writeObject 
    if the object should be written with tags
  */
  foreign virtual void writeTagStart(IN(RString) key, IN(RString) value = Nil);

  /**
    Writes end of Tag. 
    This function will be written, to 
    For example '</int>' for a int type.
    This function will be called by writeObject 
    if the object should be written with tags
  */
  foreign virtual void writeTagEnd(IN(RString) key, IN(RString) value = Nil);
  
  /**
    Write boot-straping ID for the given, so the corresponding
    AbstractObjectReader::readObjectId() can create an
    Object instance.
  */
  foreign virtual void writeClassId(IN(::acdk::lang::RClass) obj);
  
  /**
    In case ObjectWriter should handle writing Objects which 
    may have cyclic references, for each Object a local reference
    id. Recursive and duplicated elements will be written only
    one in the stream.
    If the implementing ObjectReader does not support
    local reference resolutions, it should throw NotSupportedException
  */

  static RString xmlencode(IN(RString) txt);
  static RString xmldecode(IN(RString) txt);
  
  //void writeObject2(RObject obj, RString label = Nil);
protected:
  void _dumpindend();
};


ACDK_DECL_CLASS(TestClass);

class ACDK_XML_PUBLIC TestClass 
: extends ::acdk::lang::Object
{
  // need meta information for serialisation
  ACDK_WITH_METAINFO(TestClass)
public:
  /// just a few dummy data elements
  RString _astring;
  /// ACDK-containes can be serialized
  RHashMap _map;
  int _ival;
public:
  
  static RObject create_instance() { return new TestClass(); }
  TestClass()
  : Object(),
    _astring(new String("Hallo")),
    _map(new HashMap()),
    _ival(42)
  {
    _map->put(new String("Kommer"), new String("Roger Rene"));
    _map->put(new String("Reinhard"), new String("Kai"));
  }
  virtual bool equals(IN(RObject) o)
  {
    if (instanceof(o, TestClass) == false)
      return false;
    RTestClass other = (RTestClass)o;
    if (_astring->equals(other->_astring) == false)
      return false;
    if (_map->equals((RObject)other->_map) == false)
      return false;
    if (_ival != other->_ival)
      return false;
    return true;
  }
};


} // xml
} // acdk

#endif //acdk_xml_XMLObjectWriter_h

