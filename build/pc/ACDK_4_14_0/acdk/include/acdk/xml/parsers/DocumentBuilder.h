// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright(C) 2000-2003 by Roger Rene Kommer / artefaktur, Kassel, Germany.
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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/parsers/DocumentBuilder.h,v 1.2 2005/02/05 10:45:37 kommer Exp $

#ifndef acdk_xml_parsers_DocumentBuilder_h
#define acdk_xml_parsers_DocumentBuilder_h
#include "parsers.h"
#include <acdk/io/File.h>
#include <org/w3c/dom/Document.h>
#include <org/xml/sax/EntityResolver.h>
#include <org/xml/sax/ErrorHandler.h>
#include <org/xml/sax/InputSource.h>

namespace acdk {
namespace xml {
namespace parsers {


ACDK_DECL_CLASS(DocumentBuilder);

class ACDK_XML_PUBLIC DocumentBuilder
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(DocumentBuilder)
public:
  DocumentBuilder()
  {
  }
  virtual org::w3c::dom::RDOMImplementation getDOMImplementation() = 0;
	virtual bool isNamespaceAware() = 0;
	virtual bool isValidating() = 0;
  virtual org::w3c::dom::RDocument newDocument() = 0;
  static RString fileToURL(IN(acdk::io::RFile) f) THROWS1(acdk::io::RIOException);
	virtual org::w3c::dom::RDocument parse(IN(acdk::io::RFile) file)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException);
  virtual org::w3c::dom::RDocument parse(IN(org::xml::sax::RInputSource) source)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException) = 0;
	virtual org::w3c::dom::RDocument parse(IN(acdk::io::RReader) reader)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException);
  virtual org::w3c::dom::RDocument parse(IN(acdk::io::RReader) reader, IN(RString) systemID)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException);
  virtual org::w3c::dom::RDocument parse(IN(RString) uri)  THROWS2(org::xml::sax::RSAXException, acdk::io::RIOException);
  virtual void setEntityResolver(IN(org::xml::sax::REntityResolver) resolver) = 0;
	virtual void setErrorHandler(IN(org::xml::sax::RErrorHandler) handler) = 0;
};

} // namespace parsers
} // namespace xml
} // namespace acdk

#endif //acdk_xml_parsers_DocumentBuilder_h
