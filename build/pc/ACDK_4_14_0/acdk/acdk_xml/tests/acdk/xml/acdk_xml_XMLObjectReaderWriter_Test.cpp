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
// $Header: /cvsroot/acdk/acdk/acdk_xml/tests/acdk/xml/acdk_xml_XMLObjectReaderWriter_Test.cpp,v 1.13 2005/03/07 20:21:42 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Integer.h>

#include <acdk/io/FileWriter.h>
#include <acdk/io/FileReader.h>
#include <acdk/io/Reader.h>
#include <acdk/util/Date.h>
#include <acdk/util/HashMap.h>
#include "../../../src/acdk/xml/XMLObjectWriter.h"
#include "../../../src/acdk/xml/XMLObjectReader.h"


namespace tests {
namespace acdk {
namespace xml {

using namespace ::acdk::lang;
USING_CLASS(::acdk::io::, FileWriter);
USING_CLASS(::acdk::io::, FileReader);
USING_CLASS(::acdk::util::, HashMap);

USING_CLASS(::acdk::xml::, XMLObjectWriter);
USING_CLASS(::acdk::xml::, XMLObjectReader);
USING_CLASS(::acdk::xml::, TestClass);

class XMLReaderWriter_Test
: public Object
{
public:
  static void readWrite(RObject obj);
  static int acdkmain(RStringArray args);

};

//static 
void 
XMLReaderWriter_Test::readWrite(RObject object)
{
  RString fname = "./TestClass.xml";
  {
    FileWriter fout(fname);
    // the XMLObjectWriter 
    XMLObjectWriter xmlout(&fout, ::acdk::xml::XMLSerializeDefaultFlags | ::acdk::io::SerializeAll);
    // dump out with field information
    //##xmlout->withFieldInfo(true); 
    xmlout.writeObject(object);
  }
  FileReader fin(fname);
  XMLObjectReader xmlin(&fin, ::acdk::xml::XMLSerializeDefaultFlags | ::acdk::io::SerializeAll);
  RObject readedObject = xmlin.readObject();
  if (object == readedObject && object == Nil) {
     System::out->println("Test OK with NIL");
  } else if (readedObject->equals(object) == false) {
    System::out->println("Test failed  with obj: " + object->getClass()->getName());
    System::out->println(object->toString() + " | " + readedObject->toString());
  } else
    System::out->println("Test OK with obj: " + object->getClass()->getName());
}

//static
int
XMLReaderWriter_Test::acdkmain(RStringArray args)
{
  // create an instance to serialize
  
  // test for Nil
  readWrite(Nil);
  readWrite(new String("Hello"));
  readWrite(new StringBuffer("Hello"));
  RintArray ia = new intArray(2);
  ia[0] = 12;
  ia[1] = 31;
  readWrite((RObject)ia);
  /* does't work at the moment
  RObjectArray ra = new ObjectArray(1);
  ra[0] = new Object();
  readWrite(&ra);
  ra[0] = &ra;
  readWrite(&ra);
  
  
  RHashMap hm = new HashMap();
  //hm->put(&RString("Roger"), &RString("Kommer"));
  readWrite(&hm);

  {
    RTestClass object = new TestClass();
    readWrite((RObject)object);
  }
  */
  /*
  RString fname = new String("./XMLReaderWriter_Test.xml");
  RObject theobject = new ::acdk::util::Date();
  RHashMap hm = new HashMap();
  hm->put(RString("Roger"), RString("Kommer"));
  hm->put(RString("Marco"), RString("Ernst"));
  theobject = hm;

  RIntegerArray sa = new IntegerArray(2);
  RInteger integer = new Integer(42);
  sa[0] = integer;
  sa[1] = integer;
  //theobject = sa;

  RdoubleArray da = new doubleArray(2);
  da[0] = 14.5;
  da[1] = 17.5;
  //theobject = da;
  //hm->put(sa, da);
  //hm->put(RString("The Props"), System::getProperties());
  
  theobject = System::getProperties();
  theobject = new TestClass();
  System::out->println("Write object: " + theobject->toString());
  RFileWriter fout = new FileWriter(fname);
  RXMLObjectWriter xmlout = new XMLObjectWriter(fout);
  xmlout->withFieldInfo(true); //not supported yet
  xmlout->writeObject(theobject);
  RFileReader fin = new FileReader(fname);
  RXMLObjectReader xmlin = new XMLObjectReader(fin);
  RObject obj = xmlin->readObject();
  if (theobject->equals(obj) == false) {
    System::out->println("Test failed");
  } else {
    System::out->println("Test OK");
  }
  System::out->println("read  object: " + obj->toString());
*/
  return 0;
}

} // namespace xml
} //namespace acdk 
} //namespace tests

int 
main(int argc, char* argv[], char** envptr)
{
  return acdk::lang::System::main(tests::acdk::xml::XMLReaderWriter_Test::acdkmain, argc, argv, envptr);
}
