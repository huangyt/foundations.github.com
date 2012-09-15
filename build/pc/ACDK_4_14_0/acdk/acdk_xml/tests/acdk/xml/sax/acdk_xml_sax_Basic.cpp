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
// $Header: /cvsroot/acdk/acdk/acdk_xml/tests/acdk/xml/sax/acdk_xml_sax_Basic.cpp,v 1.14 2005/02/05 10:45:38 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/CharToByteWriter.h>
#include <acdk/io/MemWriter.h>
#include <acdk/locale/Encoding.h>
#include <acdk/tools/aunit/TestRunner.h>
#include <acdk/io/MemReader.h>
#include <org/xml/sax/HandlerBase.h>
#include <org/xml/sax/InputSource.h>

#include <acdk/xml/sax/XMLParser.h>
//#include <acdk/xml/libxmlsax/LibXMLReader.h>

namespace tests {
namespace acdk {
namespace xml {
namespace sax {

BEGIN_DECLARE_TEST( SaxBasic_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( inputSource )
  DECLARE_TEST( error )
  DECLARE_TEST( latinText )
END_DECLARE_TEST( SaxBasic_Test  )

BEGIN_DEFINE_TEST( SaxBasic_Test )
  ADD_TEST( SaxBasic_Test, standard ) 
  ADD_TEST( SaxBasic_Test, inputSource ) 
  ADD_TEST( SaxBasic_Test, error ) 
  ADD_TEST( SaxBasic_Test, latinText ) 
  
END_DEFINE_TEST( SaxBasic_Test )


using namespace ::org::xml::sax;
using namespace ::acdk::xml::sax;
using namespace ::acdk::lang;

class TestHandler 
: extends HandlerBase
{
public:
  StringArray character;
  StringArray whiteSpaces;
  StringArray startElements;
  StringArray endElementsElements;
  SAXParseExceptionArray errors;
  TestHandler() 
  : HandlerBase()
  , character(0)
  , whiteSpaces(0)
  , startElements(0)
  , endElementsElements(0)
  , errors(0)
  { }
  virtual void characters(IN(RString) text, int start, int length) THROWS1(RSAXException)
  {
    System::out->print("C: [");
    System::out->print(text->substr(start, length));
    System::out->println("]");
    character.append(text->substr(start, length));
  }
  virtual void ignorableWhitespace(IN(RString) text, int start, int length) THROWS1(RSAXException) 
  { 
    System::out->print("WS: [");
    System::out->print(text->substr(start, length));
    System::out->println("]");
    whiteSpaces.append(text->substr(start, length));
  }

  virtual void startElement(IN(RString) s, IN(RAttributeList) attributelist) THROWS1(RSAXException)
  {
    System::out->println ("SE: [" + s + "]");
    startElements.append(s);
    
  }
  virtual void endElement(IN(RString) s) THROWS1(RSAXException)
  {
    System::out->println ("EE: [" + s + "]");
    endElementsElements.append(s);
  }
  virtual void error(IN(RSAXParseException) saxparseexception) THROWS1(RSAXException)
  {
    System::out->println ("error: [" + saxparseexception->toString() + "]");
    errors.append(saxparseexception);
  }
};


void 
SaxBasic_Test::standard()
{
  TestHandler th;
  //XMLParser p;
  ::acdk::xml::sax::XMLParser p;
  p.setEntityResolver(&th);
  p.setDocumentHandler(&th);
  const byte* xmlstring = (const byte*)"<?xml version=\"1.0\"?> <hello>World</hello>";
  p.parse(xmlstring);
}

void 
SaxBasic_Test::inputSource()
{
  TestHandler th;
  XMLParser p;
  p.setEntityResolver(&th);
  p.setDocumentHandler(&th);
  const byte* xmlstring = (const byte*)"<?xml version=\"1.0\"?>\n"
                                       "<hello>\n"
                                       "World\n"
                                       "</hello>";
  ::acdk::io::MemReader car(new byteArray(xmlstring, strlen((const char*)xmlstring)));
  InputSource is(&car);
  p.parse(&is);
}

void 
SaxBasic_Test::error()
{
  TestHandler th;
  XMLParser p;
  p.setEntityResolver(&th);
  p.setDocumentHandler(&th);
  p.setErrorHandler(&th);
  const byte* xmlstring = (const byte*)"<?xml version=\"1.0\"?>\n"
                                        "<hello= =>World<\\nohello>";
  ::acdk::io::MemReader car(new byteArray(xmlstring, strlen((const char*)xmlstring)));
  InputSource is(&car);
  p.parse(&is);
}

void dumpString(IN(RString) s)
{
  for (int i = 0; i < s->length(); ++i)
    System::out->print(RString("[") + (int)s->charAt(i) + "=" + s->charAt(i) + "]");
  System::out->println("");
}

void
SaxBasic_Test::latinText()
{
  TestHandler th;
  XMLParser p;
  p.setEntityResolver(&th);
  p.setDocumentHandler(&th);
  RString ts = _US("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
                                        "<hello>\\u00c4hnliche Stra\\u00dfenbahnen auf dem D\\u00f6rnberg</hello>");

  ::acdk::io::MemWriter mout;
  ::acdk::io::CharToByteWriter cbout(&mout, ::acdk::locale::Encoding::getEncoding("latin-1")->getEncoder());
  cbout.writeString(ts);
  cbout.writeChar('\0');
  cbout.close();
  const byte* xmlstring = mout.getBuffer()->begin();
  
                                        ;
  p.parse(xmlstring);
  testAssert(th.character.length() == 1);
  RString t = th.character[0];
  RString ot = _US("\\u00c4hnliche Stra\\u00dfenbahnen auf dem D\\u00f6rnberg");
  System::out->println(t);
  dumpString(t);
  System::out->println(ot);
  dumpString(ot);
  testAssert(t->equals(ot) == true);
}

} // namespace sax
} // namespace xml
} // namespace acdk
} // namespace tests

