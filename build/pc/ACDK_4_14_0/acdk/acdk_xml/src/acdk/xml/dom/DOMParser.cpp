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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/dom/DOMParser.cpp,v 1.9 2005/02/05 10:45:36 kommer Exp $


#include "DOMParser.h"
#include <org/xml/sax/SAXParseException.h>
#include <acdk/xml/sax/XMLParser.h>
#include <org/xml/sax/SAXException.h>


namespace acdk {
namespace xml {
namespace dom {


RXMLDocument 
DOMParser::parse(IN(RString) text)
{
  acdk::xml::sax::XMLParser parser;
  parser.setDocumentHandler(this);
  parser.setErrorHandler(this);
  parser.parse(text);
  return _document;
}

RXMLDocument 
DOMParser::parse(IN(RbyteArray) text)
{
  acdk::xml::sax::XMLParser parser;
  parser.setDocumentHandler(this);
  parser.setErrorHandler(this);
  parser.parse(text);
  return _document;
}

void 
DOMParser::setDocumentLocator(IN(RLocator) locator)
{
  _locator = locator;
}

//virtual 
void 
DOMParser::startDocument() THROWS1(org::xml::sax::RSAXException)
{
  _document = new XMLDocument();
}

void 
DOMParser::endDocument() THROWS1(org::xml::sax::RSAXException)
{
}

void 
DOMParser::startElement(IN(::acdk::lang::RString) s, IN(RAttributeList) attlist) THROWS1(org::xml::sax::RSAXException)
{
  RElement e = _document->createElement(s);

  for (int i = 0; i < attlist->getLength(); ++i)
  {
    e->setAttribute(attlist->getName(i), attlist->getValue(i));
  }
  if (_curNode != Nil)
    _curNode->appendChild(&e);
  else
    _document->setDocumentRoot((RXMLElement)e);
  _curNode = &e;
  _curText = Nil;
}

void 
DOMParser::endElement(IN(::acdk::lang::RString) s) THROWS1(org::xml::sax::RSAXException)
{
  /*
  if (_curNode->hasChildNodes() == false && _curText != Nil)
    _curNode->appendChild(&_curText);
  */
  _curNode = _curNode->getParentNode();
  _curText = Nil;
}

void 
DOMParser::characters(IN(RString) str, int start, int length) THROWS1(org::xml::sax::RSAXException)
{
  if (_curNode == Nil)
    return;// ### error handling
  if (_curText != Nil)
    _curText->appendData(str->substr(start, length));
  else
  {
    _curText = _document->createTextNode(str->substr(start, length));
    _curNode->appendChild(&_curText);
  }
}
  /*
void 
DOMParser::characters(IN(RcharArray) chars, int start, int length) THROWS1(org::xml::sax::RSAXException)
{

}*/
  
void 
DOMParser::ignorableWhitespace(IN(RString) str, int start, int length) THROWS1(org::xml::sax::RSAXException)
{ 
  //ignorableWhitespace(RcharArray(ptr), start, length);
}
/*
void 
DOMParser::ignorableWhitespace(IN(RcharArray) chars, int start, int length) THROWS1(org::xml::sax::RSAXException)
{
}*/

void 
DOMParser::processingInstruction(IN(::acdk::lang::RString) s, IN(::acdk::lang::RString) s1) THROWS1(org::xml::sax::RSAXException)
{
}

void 
DOMParser::warning(IN(org::xml::sax::RSAXParseException) saxparseexception) THROWS1(org::xml::sax::RSAXException)
{
}

void 
DOMParser::error(IN(org::xml::sax::RSAXParseException) saxparseexception) THROWS1(org::xml::sax::RSAXException)
{
  org::xml::sax::RSAXParseException ex = saxparseexception;
  throw ex;
}

void 
DOMParser::fatalError(IN(org::xml::sax::RSAXParseException) saxparseexception) THROWS1(org::xml::sax::RSAXException)
{
  throw saxparseexception;
}

} // namespace dom
} // namespace xml
} // namespace acdk


