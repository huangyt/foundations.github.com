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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/XMLObjectReader.h,v 1.16 2005/04/10 12:52:41 kommer Exp $

#ifndef acdk_xml_XMLObjectReader_h
#define acdk_xml_XMLObjectReader_h

#include "Config.h"
#include <acdk.h>

#include <acdk/io/AbstractObjectReader.h>

#include <acdk/io/StreamTokenizer.h>

#include <acdk/util/HashMap.h>

#include "XMLTokenizer.h"



namespace acdk {
namespace xml {

using namespace acdk::lang;

USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, DataReader);
USING_CLASS(::acdk::io::, ObjectReader);
USING_CLASS(::acdk::io::, AbstractFilterReader);
USING_CLASS(::acdk::io::, StreamTokenizer);
USING_CLASS(::acdk::util::, HashMap);


const int XMLSerializeDefaultFlags  = acdk::io::SerializeNamed 
                                    | acdk::io::SerializeTagged 
                                    | acdk::io::SerializeLabeled 
                                    | acdk::io::SerializeReduced;

ACDK_DECL_CLASS(RemoteInterface);
/**
  An holder for a remote interface
  API: ACDK<br/>

  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/10 12:52:41 $
*/
class ACDK_XML_PUBLIC  RemoteInterface
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(RemoteInterface)
private:
  RString _objectId;
public:
  RemoteInterface(IN(RString) obid)
  : Object(),
    _objectId(obid)
  {
  }
  RString objectId() { return _objectId; }
};

ACDK_DECL_CLASS(XMLObjectReader);



/**
  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.16 $
  @date $Date: 2005/04/10 12:52:41 $
  @bug _withFieldInfo == true will probably not work
*/
class ACDK_XML_PUBLIC  XMLObjectReader
: extends ::acdk::io::AbstractObjectReader
{
  ACDK_WITH_METAINFO(XMLObjectReader)
private:
  //RHashMap _ptr2id; // RObject, Integer
  //RObjectArray _readedObjects;
  //int _maxLocalRef; 
  RXMLTokenizer _tin;
  //bool _withFieldInfo;
public:
  XMLObjectReader(IN(RReader) in, int flags = XMLSerializeDefaultFlags);
  XMLObjectReader(IN(RXMLTokenizer) tin, int flags = XMLSerializeDefaultFlags);
  
  foreign virtual bool readBoolean();
  foreign virtual char readChar();
  foreign virtual uc2char readUcChar();
  foreign virtual double readDouble();
  foreign virtual float readFloat();
  foreign virtual int readInt();
  foreign virtual jlong readLong();
  foreign virtual short readShort();
  foreign virtual RString readStringImpl();
  foreign virtual RbyteArray readOpaque();

  /**
    simply ignores the additionall method
  */
  //virtual RObject readObject(::acdk::lang::RClass cls) { return readObject(false); }
  
  /**
    Reads start of Tag. 
    For example '<int>' for a int type in a XML style Reader.
    This function will be called by writeObject 
    if the object should be written with tags
    if the expected tag does not exists, throw 
    ObjectStreamException
  */
  
  foreign virtual RString readTagStart(IN(RString) key, IN(RString) expected = Nil);
  
  /**
    Writes end of Tag. 
    For example '</int>' for a int type.
    This function will be called by writeObject 
    if the object should be written with tags
  */
  foreign virtual RString readTagEnd(IN(RString) key, IN(RString) expected = Nil);
  foreign virtual RClass readClassId();

  //acdk::lang::dmi::ScriptVar readScriptVar();
  

protected:
  
  /** reads the &lt;field&gt; tag. if existant returns the name prop */
  RString readFieldInfo();
  //void putlRef(RObject obj, int lref);
  //RObject getlRef(int lref);
  RObject readBasicArray(IN(RClass) cls, int length);
  void readSimpleStartToken(IN(RString) str);
  void readSimpleEndToken(IN(RString) str);
  RString _readXMLText();
};



} // xml
} // acdk

#endif //acdk_xml_XMLObjectReader_h

