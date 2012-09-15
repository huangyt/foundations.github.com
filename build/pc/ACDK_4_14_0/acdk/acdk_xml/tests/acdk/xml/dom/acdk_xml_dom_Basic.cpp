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
// $Header: /cvsroot/acdk/acdk/acdk_xml/tests/acdk/xml/dom/acdk_xml_dom_Basic.cpp,v 1.14 2005/02/05 10:45:38 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/CharArrayReader.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/io/MemWriter.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/xml/dom/DOMParser.h>
#include <org/w3c/dom/NodeList.h>

namespace tests {
namespace acdk {
namespace xml {
namespace sax {

BEGIN_DECLARE_TEST( DomBasic_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( files )
  DECLARE_TEST( invalideXml )
  DECLARE_TEST( singleFile )
END_DECLARE_TEST( DomBasic_Test  )

BEGIN_DEFINE_TEST( DomBasic_Test )
  ADD_TEST( DomBasic_Test, standard ) 
  ADD_TEST( DomBasic_Test, files ) 
  ADD_TEST( DomBasic_Test, invalideXml ) 
  
  ADD_TEST( DomBasic_Test, singleFile ) 
  
END_DEFINE_TEST( DomBasic_Test )



using namespace ::org::w3c::dom;
using namespace ::acdk::xml::dom;

void 
DomBasic_Test::standard()
{
  ::acdk::xml::dom::DOMParser parser;
  const char* text = 
  "<?xml version=\"1.0\"?>"
  "<book>"
  "<title value=\"asdf\"/>"
  "<content>"
    "Text of Book"
  "</content>"
  "</book>"
  
  ""
  ;
  parser.parse(text);
}

static ::acdk::xml::dom::RXMLDocument parseDocument(::acdk::io::File& f)
{
  try {
    ::acdk::io::RReader in = f.getReader();
    ::acdk::io::MemWriter bout;
    in->trans(&bout);
    //RString text = f.getReader()->getCharReader()->readString();
    ::acdk::xml::dom::DOMParser parser;
    ::acdk::xml::dom::RXMLDocument doc = parser.parse(bout.getBuffer());
    System::out->println("Parsed: " + doc->toString() + "\n" + doc->toXML());  
    return doc;
  } catch (RThrowable ex) {
    System::out->println("Parsing file failed: " + f.getCanonicalPath() + ": " + ex->getMessage());
    return Nil;
  }
  return Nil;
}

static ::acdk::xml::dom::RXMLDocument parseDocument(IN(RString) fname)
{
  RString s = System::getAcdkHome() + "/acdk_xml/cfg/tests/acdk/xml/" + fname;
  ::acdk::io::File f(s);
  return parseDocument(f);
}

void
DomBasic_Test::files()
{
   RString s = System::getAcdkHome() + "/acdk_xml/cfg/tests/acdk/xml";
  ::acdk::io::File dir(s);
  RStringArray files = dir.list(new ::acdk::io::GlobFilenameFilter("*.xml"), ::acdk::io::FileListFiles);
  bool testFailed = false;
  for (int i = 0; i < files->length(); ++i)
  {
    ::acdk::io::File f(&dir, files[i]);
    if (parseDocument(f) == Nil)
    {
      testFailed = true;
      System::out->println("Test failed: " + f.getCanonicalPath());
    }
  }
  testAssertComment(testFailed == false, "One or more tests are failed");
}

void
DomBasic_Test::invalideXml()
{
   RString s = System::getAcdkHome() + "/acdk_xml/cfg/tests/acdk/xml/invalid";
  ::acdk::io::File dir(s);
  RStringArray files = dir.list(new ::acdk::io::GlobFilenameFilter("*.xml"), ::acdk::io::FileListRecursive | ::acdk::io::FileListFiles);
  bool testFailed = false;
  for (int i = 0; i < files->length(); ++i)
  {
    ::acdk::io::File f(&dir, files[i]);
    if (parseDocument(f) != Nil)
    {
      testFailed = true;
      System::out->println("Test failed: " + f.getCanonicalPath());
    }
  }
  testAssertComment(testFailed == false, "One or more tests are failed");
}


void
DomBasic_Test::singleFile()
{
  //::acdk::xml::dom::RXMLDocument doc = parseDocument("02_PcData.xml");
  //::acdk::xml::dom::RXMLDocument docp = new ::acdk::xml::dom::XMLDocument();
  //docp->setDocumentRoot(new XMLElement("sadf"));
  /*
  ::acdk::xml::dom::RXMLDocument doc = parseDocument("40_XPathExpr.xml");
  RNodeList nl = NodeUtil::selectNodes((RDocument)&doc, "/book/title");
  RNode node = NodeUtil::selectNode(NodeUtil::selectNode((RDocument)&doc, "/book"), "title");
  {
    ::acdk::xml::dom::RXMLDocument doc = parseDocument("invalid/10_InvalidPcText.xml");
  }
  */
  {
    //::acdk::xml::dom::RXMLDocument doc = parseDocument("80_NsTest.xml");
  }
  {
    ::acdk::xml::dom::RXMLDocument doc = parseDocument("60_Latin1Encoding.xml");
    RString v = doc->selectNode("/book/body/text()")->getNodeValue();
    {
      RString v2 = doc->selectNode("/book")->selectNode("body/text()")->getNodeValue();
      testAssert(v->equals(v2) == true);
    }
    System::out->println(v);
    doc = parseDocument("60_Utf8Encoding.xml");
    RString v2 = doc->selectNode("/book/body/text()")->getNodeValue();
    RString v1i = v->convert(CCUtf8);
    RString v2i = v2->convert(CCUtf8);
    System::out->println(v);
    testAssert(v->equals(v2) == true);

  }
}


} // namespace sax
} // namespace xml
} // namespace acdk
} // namespace tests

