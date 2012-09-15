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
// $Header: /cvsroot/acdk/acdk/acdk_xml/tests/acdk/xml/dom/acdk_xml_dom_LibXML_Test.cpp,v 1.17 2005/04/25 13:20:47 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/io/CharArrayReader.h>
#include <acdk/io/File.h>
#include <acdk/io/GlobFilenameFilter.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/MemReader.h>
#include <acdk/tools/aunit/TestRunner.h>

#include <acdk/xml/sax/XMLReader.h>
#include <acdk/xml/libxmldom/LibXMLDocumentBuilder.h>
#include <acdk/xml/libxmldom/LibXMLDocument.h>
#include <org/w3c/dom/NodeList.h>
#include <org/w3c/dom/DOMWriter.h>
#include <acdk/net/URL.h>

namespace tests {
namespace acdk {
namespace xml {
namespace sax {

BEGIN_DECLARE_TEST( LibXML_Test )
  DECLARE_TEST( standard )
  DECLARE_TEST( files )
  DECLARE_TEST( invalideXml )
  DECLARE_TEST( invalideValidatedXml )
  DECLARE_TEST( singleFile )
  DECLARE_TEST( selectNodes )
  DECLARE_TEST( selectNodesTyped )
  DECLARE_TEST( xpath )
  DECLARE_TEST( xmlWriter )
  DECLARE_TEST( buildDocument )  
  DECLARE_TEST( manipulateDocument )  
  DECLARE_TEST( createDocument )
  DECLARE_TEST( createDocument2 )
  DECLARE_TEST( copyBetweenDocuments )
  DECLARE_TEST( parsePrintHtml )
  DECLARE_TEST( memConsumingTest )
END_DECLARE_TEST( LibXML_Test  )

BEGIN_DEFINE_TEST( LibXML_Test )
  ADD_TEST( LibXML_Test, standard ) 
  ADD_TEST( LibXML_Test, files ) 
  ADD_TEST( LibXML_Test, invalideXml ) 
  ADD_TEST( LibXML_Test, invalideValidatedXml )
  ADD_TEST( LibXML_Test, singleFile ) 
  ADD_TEST( LibXML_Test, selectNodes ) 
  ADD_TEST( LibXML_Test, selectNodesTyped ) 
  
  ADD_TEST( LibXML_Test, xpath ) 
  
  ADD_TEST( LibXML_Test, xmlWriter ) 
  ADD_TEST( LibXML_Test, buildDocument ) 
  ADD_TEST( LibXML_Test, manipulateDocument ) 
  ADD_TEST( LibXML_Test, createDocument ) 
  ADD_TEST( LibXML_Test, createDocument2 ) 
  ADD_TEST( LibXML_Test, copyBetweenDocuments ) 
  ADD_TEST( LibXML_Test, parsePrintHtml ) 
  ADD_TEST( LibXML_Test, memConsumingTest ) 
  
END_DEFINE_TEST( LibXML_Test )



using namespace ::org::w3c::dom;
using namespace ::org::xml::sax;
using namespace ::acdk::xml::libxmldom;
using namespace ::acdk::xml::sax;


void printDomNodes(IN(::acdk::io::RPrintWriter) out, IN(::org::w3c::dom::RNode) node, IN(RString) ident)
{
  //RString textCont = node->getTextContent();
  out->println(ident + RObject(node)->toString());
  if (node->hasChildNodes() == false)
    return;
  ::org::w3c::dom::RNodeList nl = node->getChildNodes();
  int count = nl->getLength();
  for (int i = 0; i < count; ++i)
  {
    printDomNodes(out, nl->item(i), ident + " ");
  }
}

void 
LibXML_Test::standard()
{
  //System::in->readLine();
  LibXMLDocumentBuilder parser;
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
  RbyteArray ba = new byteArray((const byte*)text, strlen(text));
  ::acdk::io::MemReader memreader(ba);
  ::org::w3c::dom::RDocument doc = parser.parse((::acdk::io::RReader)&memreader);
  //printDomNodes(System::out, &doc, "");
  printDomNodes(System::out, &doc, "");
  //RLibXMLDocument(doc)->releaseNode();

}


static RLibXMLDocument parseDocument(::acdk::io::File& f, int extendedParseFlags, IN(RErrorHandler) errorHandler = Nil)
{
  //try {
    LibXMLDocumentBuilder parser;
    parser.setExtendedFlags(extendedParseFlags);
    if (errorHandler != Nil)
      parser.setErrorHandler(errorHandler);
    ::acdk::io::RReader in = f.getReader();
    //::acdk::io::MemWriter bout;
    //in->trans(&bout);
    //RString text = f.getReader()->getCharReader()->readString();
    RLibXMLDocument doc = (RLibXMLDocument)parser.parse(in, ::acdk::net::URL::fileAsUrlName(&f));
    //System::out->println("Parsed: " + doc->toString() + "\n" + doc->toXml());  
    return doc;
  //} catch (RThrowable ex) {
  //  System::out->println("Parsing file failed: " + f.getCanonicalPath() + ": " + ex->getMessage());
    return Nil;
  //}
  return Nil;
}

static RLibXMLDocument parseDocument(IN(RString) fname, int extendedParseFlags = 0, bool printDom = false)
{
  RString s = System::getAcdkHome() + "/acdk_xml/cfg/tests/acdk/xml/" + fname;
  ::acdk::io::File f(s);
  RLibXMLDocument doc = parseDocument(f, extendedParseFlags);
  if (printDom == true)
    printDomNodes(System::out, &doc, "");
  return doc;
}


void
LibXML_Test::files()
{
   RString s = System::getAcdkHome() + "/acdk_xml/cfg/tests/acdk/xml";
  ::acdk::io::File dir(s);
  RStringArray files = dir.list(new ::acdk::io::GlobFilenameFilter("*.xml"), ::acdk::io::FileListFiles);
  bool testFailed = false;
  for (int i = 0; i < files->length(); ++i)
  {
    ::acdk::io::File f(&dir, files[i]);
    try {
      parseDocument(f, 0);
    
    } catch (RSAXParseException ex) {
      System::out->println("Test failed: " + f.getCanonicalPath() + ex->getMessage());
      testFailed = true;
    }
  }
  testAssertComment(testFailed == false, "One or more tests are failed");
}

void
LibXML_Test::invalideXml()
{
   RString s = System::getAcdkHome() + "/acdk_xml/cfg/tests/acdk/xml/invalid";
  ::acdk::io::File dir(s);
  RStringArray files = dir.list(new ::acdk::io::GlobFilenameFilter("*.xml"), ::acdk::io::FileListRecursive | ::acdk::io::FileListFiles);
  bool testFailed = false;
  for (int i = 0; i < files->length(); ++i)
  {
    ::acdk::io::File f(&dir, files[i]);
    try {
      parseDocument(f, 0);
      testFailed = true;
      System::out->println("Test failed: " + f.getCanonicalPath());
    } catch (RSAXParseException ex) {
      System::out->println("Expected Exception: " + f.getCanonicalPath() + ex->getMessage());
    }
  }
  testAssertComment(testFailed == false, "One or more tests are failed");
}

void
LibXML_Test::invalideValidatedXml()
{
   RString s = System::getAcdkHome() + "/acdk_xml/cfg/tests/acdk/xml/validate/invalid";
  ::acdk::io::File dir(s);
  RStringArray files = dir.list(new ::acdk::io::GlobFilenameFilter("*.xml"), ::acdk::io::FileListRecursive | ::acdk::io::FileListFiles);
  bool testFailed = false;
  for (int i = 0; i < files->length(); ++i)
  {
    ::acdk::io::File f(&dir, files[i]);
    try {
      parseDocument(f, XMLRF_PARSE_DTDVALID | XMLRF_PARSE_NOENT);
      testFailed = true;
      System::out->println("Test failed: " + f.getCanonicalPath());
    } catch (RSAXParseException ex) {
      System::out->println("Expected Exception: " + f.getCanonicalPath() + ex->getMessage());
    }
  }
  testAssertComment(testFailed == false, "One or more tests are failed");
}


void
LibXML_Test::singleFile()
{
  System::out->println("This test is for development only");

  //::acdk::xml::dom::RXMLDocument doc = parseDocument("02_PcData.xml");
  //::acdk::xml::dom::RXMLDocument docp = new ::acdk::xml::dom::XMLDocument();
  //docp->setDocumentRoot(new XMLElement("sadf"));
  
  {
    //::acdk::xml::dom::RXMLDocument doc = parseDocument("80_NsTest.xml");
  }
  {
    /* 
    //RLibXMLDocument doc = parseDocument("validate/invalid/01_web_local.xml");
    ::acdk::io::File f("C:\\d\\artefaktur\\acdk_doc\\docs\\wxWidgets\\html\\wx\\wx_wxcolour.html");
    RLibXMLDocument doc = parseDocument(f, ::acdk::xml::sax::XMLRF_PARSE_HTML_FLAGS | HTML_PARSE_NOERROR, Nil);
    //printDomNodes(System::out, &doc, "");
    System::out->println("AS HTML:\n");
    doc->write(System::out->getWriter());
    System::out->println("\n\n\nAS XML:\n");
    doc->write(System::out->getWriter(), NWFWithIndent | NWFWHtmlAsXml);
    */
    /*


    doc->selectNode("/book/body/text()");
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
    */
  }
}

void
LibXML_Test::selectNodes()
{
  RLibXMLDocument doc = parseDocument("validate/01_web_local.xml");
  RNode n = doc->selectNode("/web-app/display-name");
  testAssert(n != Nil);
  n = n->selectNode("text()");
  testAssert(n->getNodeValue()->equals("sampleapp.war") == true);
  
  n = doc->selectNode("/web-app/servlet");
  doc = Nil;
  RNodeList nl = n->selectNodes("init-param");
  testAssert(nl != Nil && nl->getLength() > 0);
  RNode ne;
  ::acdk::util::RIterator it = nl->iterator();
  while (it->hasNext() == true)
  {
    ne = (RNode)it->next();
    System::out->println("Node: " + ne->getNodeName());
  }
}

void
LibXML_Test::selectNodesTyped()
{
  RLibXMLDocument doc = parseDocument("10_SelectTyped.xml");
  RString docAsXML = doc->toXML();
  RObject o;
  RString s;
  RNumber n;
  RBoolean b;
  o = doc->selectObject("/entries/entry");
  testAssert(instanceof(o, NodeList) == true);
  o = doc->selectObject("/entries/entry[1]");
  testAssert(instanceof(o, Node) == true);
  
  // xpath casting functions string(), number(), and boolean(). 
  
  // select path and cast it to string
  o = doc->selectObject("string(/entries/entry[@type=\"stringval\"]/text())");
  testAssert(instanceof(o, String) == true);
  s = (RString)o;
  testAssert(s->equals("asdf") == true);

  // select path and cast it to number
  o = doc->selectObject("number(/entries/entry[@type=\"intval\"]/text())");
  testAssert(instanceof(o, Number) == true);
  n = (RNumber)o;
  testAssert(n->intValue() == 12345);

  // select path and cast it to boolean
  o = doc->selectObject("boolean(/entries/entry[@type=\"boolval\"]/text())");
  testAssert(instanceof(o, Boolean) == true);
  b = (RBoolean)o;
  testAssert(b->booleanValue() == true);
}

void
LibXML_Test::xpath()
{
  RLibXMLDocument doc = parseDocument("xpath/40_XPathExpr.xml");
  RNode node = doc->selectNode("/book/title");
  testAssert(node != Nil);

  node = doc->selectNode("/book/title[@value=\"This is another Title\"]");
  testAssert(node != Nil);

  node = doc->selectNode("/book/body/chapter[@num=\"2\"]");
  /* does not work testAssert(node != Nil);
  node = node->selectNode("par[text()=\"Second Text 2\"]");
  */
  /* todo
    /element/@* selects all attributes
    /element/text() selects the text inside element

  */
}

void
LibXML_Test::xmlWriter()
{
  RLibXMLDocument doc = parseDocument("validate/01_web_local.xml");
  ::acdk::io::MemWriter mout;
  DOMWriter writer(&mout, DOMWriterFormat::createPretty());
  writer.writeDocument(&doc);
  System::out->getWriter()->write(mout.getBuffer());
  System::out->println();
}

void
LibXML_Test::buildDocument()
{
  LibXMLDocumentBuilder docBuilder;
  RDocument doc = docBuilder.newDocument();
  RElement el = doc->addElement("rootNode")->addElement("childNode");
  for (int i = 0; i < 4; ++i)
  {
    el->addComment("This is comment");
    el->addElement("listel");
    el->addText("BlaBla");
    //el->
  }

  ::acdk::io::MemWriter mout;
  DOMWriter writer(&mout, DOMWriterFormat::createPretty());
  writer.writeDocument(&doc);
  System::out->getWriter()->write(mout.getBuffer());
  System::out->println();
}

void
LibXML_Test::manipulateDocument()
{
  RLibXMLDocument doc = parseDocument("validate/01_web_local.xml");
  RNode el = doc->selectNode("/web-app/servlet[1]");
  RNode removedEl = el->detach();
  
  removedEl = Nil;
}

void
LibXML_Test::createDocument()
{
  LibXMLDocumentBuilder docBuilder;
  RDocument document = docBuilder.newDocument();
  //RDocument document = new LibXMLDocument("Root");
  org::w3c::dom::RElement parent = document->createElement("Parent");
  org::w3c::dom::RElement first = document->createElement("FirstChild");
  org::w3c::dom::RElement second = document->createElement("SecondChild");
  org::w3c::dom::RElement third = document->createElement("ThirdChild");
  
  document->appendChild(&parent);
  parent->appendChild(&first);
  parent->appendChild(&second);
  parent->appendChild(&third);
  
  org::w3c::dom::RElement newFirst = document->createElement("NewFirst");
  org::w3c::dom::RElement oldFirst = (org::w3c::dom::RElement) parent->replaceChild(&newFirst, &first);
  
  // check the return value of replaceChild 
  testAssert(oldFirst->isEqualNode(&first) == true);
  
  /// make sure the old node has been replaced 
  RNodeList children = parent->getChildNodes();
  RNode firstChild = children->item(0);
  testAssert(ELEMENT_NODE == firstChild->getNodeType());
  testAssert(newFirst->isEqualNode(&firstChild) == true);
  RString xmlstr = document->toXML();
  System::out->println("Created Document: " + xmlstr);
  // try to replace a node that doesn't exist 
  org::w3c::dom::RElement badNode = document->createElement("NoChild");
  try {
    parent->replaceChild(&newFirst, &badNode);
    testAssertComment(false, "DOMException should be throwed when trying to replace non existing child");
  } catch (RDOMException e) {
    testAssert(NOT_FOUND_ERR == e->getCode());
  }
}

void
LibXML_Test::createDocument2()
{
  // sample from dom4j
  RDocument document = LibXMLDocumentBuilder().newDocument();
  RElement root = document->addElement("root");
  RElement author1 = root->addElement( "author" )
                         ->addAttribute( "name", "James" )
                          ->addAttribute( "location", "UK" )
                          ->addText( "James Strachan" );
        
  RElement author2 = root->addElement( "author" )
                         ->addAttribute( "name", "Bob" )
                         ->addAttribute( "location", "US" )
                         ->addText( "Bob McWhirter" );

  RString xmlstr = document->toXML();
  System::out->println("Created Document: " + xmlstr);
  
}

void 
LibXML_Test::copyBetweenDocuments()
{
  RLibXMLDocument book1 = parseDocument("03_Book1.xml");
  RLibXMLDocument book2 = parseDocument("03_Book2.xml");
  RNodeList nodes = book1->selectNodes("/book/chapter");
  RNode inspos = book2->selectNode("/book");
  for (int i = 0; i < nodes->getLength(); ++i)
  {
    RLibXMLNode cloned = (RLibXMLNode)nodes->item(i)->cloneNode(true);
    RElement(inspos)->appendChild(&cloned);
  }
  nodes = Nil;
  book1 = Nil;
  book2->write(System::out->getWriter(), NWFWithIndent);
}

void
LibXML_Test::parsePrintHtml()
{
  LibXMLDocumentBuilder parser;
  parser.setExtendedFlags(::acdk::xml::sax::XMLRF_PARSE_HTML_FLAGS);

  // read from file
  {
    RString extTempFile = System::getAcdkHome() + "/index.html";
    ::acdk::io::File f(extTempFile);
    if (f.exists() == false)
    {
      // skip test
      return;
    }
    RLibXMLDocument doc = (RLibXMLDocument)parser.parse(f.getReader(), f.getCanonicalPath());
    doc->write(System::out->getWriter(), NWFWithIndent, 0, "HTML");
  }
  // read from memory
  {
  char* htmlFragment =
    "<html><body><h2>&nbsp;</h2></body></html>";
   
  RbyteArray ba = new byteArray((const byte*)htmlFragment, strlen(htmlFragment));
  ::acdk::io::MemReader memreader(ba);
  RLibXMLDocument doc = (RLibXMLDocument)parser.parse(&memreader, "");
  doc->write(System::out->getWriter(), NWFWithIndent, 0, "HTML");
  System::out->println("");
  }
}

void parseXmlFile(IN(::acdk::io::RFile) f)
{
  System::out->println("parse file: " + f->getName());
  LibXMLDocumentBuilder parser;
  RLibXMLDocument doc = (RLibXMLDocument)parser.parse(f->getReader(), f->getCanonicalPath());

}

void
LibXML_Test::memConsumingTest()
{
  RString inputPath = "C:\\d\\artefaktur\\extsrc\\unicode_locals";
  ::acdk::io::RFile dir = new ::acdk::io::File(inputPath);
  if (dir->exists() == false)
    return;
  RStringArray files = dir->list();
  for (int i = 0; i < files->length(); ++i)
  {
    RString fn = files[i];
    if (fn->endsWith(".xml") == false)
      continue;
    if (fn->startsWith("ar") == false)
      continue;
    ::acdk::io::RFile xmlFile = new ::acdk::io::File(dir, fn);
    parseXmlFile(xmlFile);

  }
}


} // namespace sax
} // namespace xml
} // namespace acdk
} // namespace tests

