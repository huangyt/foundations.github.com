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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/XMLObjectReader.cpp,v 1.15 2005/03/08 18:50:42 kommer Exp $


#include "Config.h"
#include <acdk.h>
#include <acdk/lang/Boolean.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>
#include <acdk/lang/dmi/SysFields.h>
#include <acdk/lang/reflect/Modifier.h>

#include <acdk/text/Base64.h>
#include <acdk/net/URLDecoder.h>

#include "XMLObjectReader.h"
#include "XMLObjectWriter.h"

namespace acdk {
namespace xml {

using namespace acdk::lang;
using namespace acdk::lang::sys;
using ::acdk::lang::reflect::Modifier;

XMLObjectReader::XMLObjectReader(IN(RReader) in, int flags)
: AbstractObjectReader(in, flags),
  _tin(new XMLTokenizer(in))
  //_withFieldInfo(readWithFieldInfo)
{
}
XMLObjectReader::XMLObjectReader(IN(RXMLTokenizer) tin, int flags)
: AbstractObjectReader(tin->getIn(), flags), 
  _tin(tin)
  //_readedObjects(new ObjectArray(0)),
  //_withFieldInfo(readWithFieldInfo)
{
}

void
XMLObjectReader::readSimpleStartToken(IN(RString) str)
{
  int tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_LT) 
    THROW1(Exception, "expect '<'");
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_SYMBOL) {
    THROW1(Exception, "expect Symbol");
  }
  RString clsname = _tin->element();
  /*
  if (clsname->compareTo("field") == 0) {
    while (_tin->nextToken() != XMLTokenizer::TOK_LT)
      ;
    tk = _tin->nextToken();
    clsname = _tin->element();
  }*/
  if (clsname->compareTo(str) != 0)
    THROW1(Exception, RString("expect \"") + str + "\"");
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_GT) 
    THROW1(Exception, "expect '>'");
}

void
XMLObjectReader::readSimpleEndToken(IN(RString) str)
{
  int tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_LT) 
    THROW1(Exception, "expect '<'");
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_SLASH) 
    THROW1(Exception, "expect '/'");
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_SYMBOL) {
    THROW1(Exception, "expect Symbol");
  }
  RString clsname = _tin->element();
  if (clsname->compareTo(str) != 0)
    THROW1(Exception, RString("expect \"") + str + "\"");
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_GT) 
    THROW1(Exception, "expect '>'");
}


RString
XMLObjectReader::_readXMLText() // ## not used?
{
  if (_tin->nextToken() != XMLTokenizer::TOK_TEXT)
    THROW1(Exception, "expect Text");
  return _tin->element();
}

//virtual 
bool 
XMLObjectReader::readBoolean()
{
  int tk = _tin->nextToken();
  RString val = ::acdk::net::URLDecoder().decode(_tin->element());
  bool erg = Boolean::valueOf(val);
  return erg;
}
 
//virtual 
char 
XMLObjectReader::readChar()
{
  int tk = _tin->nextToken();
  RString val = ::acdk::net::URLDecoder().decode(_tin->element());
  char erg = (char)val->charAt(0);
  return erg;
}

//virtual 
uc2char 
XMLObjectReader::readUcChar()
{
  int tk = _tin->nextToken();
  RString val = ::acdk::net::URLDecoder().decode(_tin->element());
  return val->charAt(0);
}


//virtual 
double 
XMLObjectReader::readDouble()
{
  int tk = _tin->nextToken();
  RString val = ::acdk::net::URLDecoder().decode(_tin->element());
  double erg = Float::parseFloat(val);
  return erg;
}

//virtual 
float 
XMLObjectReader::readFloat()
{
  int tk = _tin->nextToken();
  RString val = ::acdk::net::URLDecoder().decode(_tin->element());
  float erg = Float::parseFloat(val);
  return erg;
}

//virtual 
int 
XMLObjectReader::readInt()
{
  int tk = _tin->nextToken();
  RString val = ::acdk::net::URLDecoder().decode(_tin->element());
  int erg = Integer::parseInt(val);
  return erg;
}

//virtual 
jlong 
XMLObjectReader::readLong()
{
  int tk = _tin->nextToken();
  RString val = ::acdk::net::URLDecoder().decode(_tin->element());
  jlong erg = Long::parseLong(val);
  return erg;
}
 
//virtual 
short 
XMLObjectReader::readShort()
{
  int tk = _tin->nextToken();
  RString val = ::acdk::net::URLDecoder().decode(_tin->element());
  short erg = Short::parseShort(val);
  return erg;
}



//virtual 
RString 
XMLObjectReader::readStringImpl()
{
  int tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_TEXT)
    THROW1(Exception, "expect Text");
  return ::acdk::net::URLDecoder().decode(_tin->element());
}

#define READBASARRAY(ClassName,baseType,ShortReader) \
if (elclass == ClassName ::getTYPE()) { \
    R##baseType##Array ba = new baseType##Array(length); \
    for (int i = 0; i < length; i++) { \
      RString fname = readFieldInfo(); \
      ba[i] = read##ShortReader(); \
      if (fname != Nil) \
        readSimpleEndToken("field"); \
    } \
    return (RObject)ba; \
} \


RObject 
XMLObjectReader::readBasicArray(IN(RClass) cls, int length)
{
  RClass elclass = Class::getSingeltonClass((dmi::ClazzInfo*)cls->objectClazzInfo()->userInfo);
  READBASARRAY(Boolean, bool, Boolean);
  READBASARRAY(Character, char, Char);
  READBASARRAY(Short, short, Short);
  READBASARRAY(Integer, int, Int);
  READBASARRAY(Long, long, Long);
  READBASARRAY(Float, float, Float);
  READBASARRAY(Double, double, Double);
  return Nil;
}

/*
void
XMLObjectReader::putlRef(RObject obj, int lref)
{
  _readedObjects->ensureCapacity(lref + 1);
  _readedObjects[lref] = obj;
}

RObject 
XMLObjectReader::getlRef(int lref)
{
  if (lref >= _readedObjects->length())
    return Nil;
  return _readedObjects[lref];
}
*/

RString
XMLObjectReader::readFieldInfo()
{
  //if (_withFieldInfo == false)
  //  return Nil;
  int tk = _tin->nextToken();
  if (tk == XMLTokenizer::TOK_LT) {
    tk = _tin->nextToken();
    if (_tin->element()->compareTo("field") == 0) {
      while (_tin->nextToken() !=  XMLTokenizer::TOK_GT)
        ;
      return "thefield";
    } else {
      _tin->unread();
      _tin->unread();
    } 
  } else {
    _tin->unread();
  }
  return Nil;
}



//virtual 
RString 
XMLObjectReader::readTagStart(IN(RString) key, IN(RString) expected/* = Nil */)
{
  int tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_LT)
    THROW1(Exception, "expect '<'"); //## better execption
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_SYMBOL) 
    THROW1(Exception, "expect Symbol");
  RString rkey = _tin->element();
  if (rkey->equals(key) == false)
    THROW1(Exception, "expect " + key + ", readed " + rkey);
  tk = _tin->nextToken();
  if (expected == Nil) {
    if (tk != XMLTokenizer::TOK_GT)
      THROW1(Exception, "expect '>'"); //## better execption
    return rkey;
  }
  // not implemented yet
  return key;
}

  
//virtual 
RString 
XMLObjectReader::readTagEnd(IN(RString) key, IN(RString) expected/* = Nil*/)
{
  int tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_LT)
    THROW1(Exception, "expect '<'"); //## better execption
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_SLASH)
    THROW1(Exception, "expect '/'"); //## better execption

  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_SYMBOL) 
    THROW1(Exception, "expect Symbol");
  RString rkey = _tin->element();
  if (rkey->equals(key) == false)
    THROW1(Exception, "expect " + key + ", readed " + rkey);
  tk = _tin->nextToken();
  if (expected == Nil) {
    if (tk != XMLTokenizer::TOK_GT)
      THROW1(Exception, "expect '>'"); //## better execption
    return rkey;
  }
  // not implemented yet
  return key;
}

//virtual 
RClass 
XMLObjectReader::readClassId()
{
  int tk = _tin->nextToken();
  jlong version = 0;
  if (tk != XMLTokenizer::TOK_LT)
    THROW1(Exception, "expect '<'"); //## better execption
  
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_SYMBOL) 
    THROW1(Exception, "expect Symbol");
  if (_tin->element()->equals("class") == false)
    THROW1(Exception, "expect class token");

  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_SYMBOL || _tin->element()->equals("value") == false)
    THROW1(Exception, "expect value");
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_EQ)
    THROW1(Exception, "expect =");
  
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_STRING) 
    THROW1(Exception, "expect String");
  
  RString classname = _tin->element();
  tk = _tin->nextToken();
  if (withSerialVersionUID() == true && classname->equals("Nil") == false)
  {
    tk = _tin->nextToken();
    if (tk != XMLTokenizer::TOK_SYMBOL || _tin->element()->equals("SVUID") == false)
      THROW1(Exception, "expect SVUID");
    tk = _tin->nextToken();
    if (tk != XMLTokenizer::TOK_EQ)
      THROW1(Exception, "expect =");
    tk = _tin->nextToken();
    if (tk != XMLTokenizer::TOK_STRING) 
      THROW1(Exception, "expect String");
    try {
      version = Long::parseLong(_tin->element());
    } catch (RNumberFormatException ex) {
      THROW1(Exception, "expect long number for version id");
    }
  }
  if (tk != XMLTokenizer::TOK_SLASH)
    THROW1(Exception, "expect '/'"); //## better execption
  
  tk = _tin->nextToken();
  if (tk != XMLTokenizer::TOK_GT)
    THROW1(Exception, "expect '>'"); //## better execption
  
  if (classname->equals("Nil") == true)
    return Nil;
  RClass cls = Class::forName(::acdk::net::URLDecoder().decode(classname));
  if (version != 0)
  {
     jlong oid = cls->objectClazzInfo()->getSerialVersionUID();
     if (oid != version)
       THROW0(InvalidClassException);
    
  }
  return cls;
}

//virtual 
RbyteArray 
XMLObjectReader::readOpaque()
{

  int len = readInt();
  if (len == 0)
    return new byteArray(0);
  RbyteArray erg = new byteArray(0);
  read(erg);
  return ::acdk::text::Base64::decode(erg);
}


} // xml
} // acdk



