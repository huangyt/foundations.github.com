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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/sax/XMLParser.h,v 1.12 2005/02/05 10:45:37 kommer Exp $

#ifndef acdk_xml_sax_XMLParser_h
#define acdk_xml_sax_XMLParser_h

#include <acdk.h>
#include "../Config.h"
#include <org/xml/sax/Parser.h>


namespace acdk {
namespace xml {
namespace sax {



ACDK_DECL_CLASS(XMLParser);

/** 
  API: org.xml.sax<br>
  @author Roger Rene Kommer
  @version $Revision: 1.12 $
  @date $Date: 2005/02/05 10:45:37 $
*/
class ACDK_XML_PUBLIC XMLParser
: extends ::acdk::lang::Object
, implements ::org::xml::sax::Parser
{
  ACDK_WITH_METAINFO(XMLParser)
private:
  ::acdk::util::RLocale _locale;
  ::org::xml::sax::REntityResolver _entityResolver;
  ::org::xml::sax::RDTDHandler _dtdHandler;
  ::org::xml::sax::RDocumentHandler _documentHandler;
  ::org::xml::sax::RErrorHandler _errorHandler;
  foreign void *_xmlParser;
public: 
  XMLParser();
  ~XMLParser();
 // from ::org::xml::sax::Parser
  void setLocale(IN(acdk::util::RLocale) locale) 
      THROWS1(::org::xml::sax::RSAXException);
  void setEntityResolver(IN(::org::xml::sax::REntityResolver) entityresolver);
  void setDTDHandler(IN(::org::xml::sax::RDTDHandler) dtdhandler);
  void setDocumentHandler(IN(::org::xml::sax::RDocumentHandler) documenthandler);
  void setErrorHandler(IN(::org::xml::sax::RErrorHandler) errorhandler);
  void parse(IN(::org::xml::sax::RInputSource) inputsource) 
      THROWS2(::org::xml::sax::RSAXException, acdk::io::RIOException);
  /**
    Parse a US-ASCII String.
    @param s String will be converted to US-ASCII string.
             Use parse(IN(RbyteArray) to encode bytes
  */
  void parse(IN(RString) s) THROWS2(::org::xml::sax::RSAXException, acdk::io::RIOException);
  /**
    parse a byte stream
    @param length number of bytes to parse. -1 means complete byteArray
  */
  void parse(IN(RbyteArray) s, int length = -1) THROWS2(::org::xml::sax::RSAXException, acdk::io::RIOException);
  /**
    parse a byte stream
    @param length number of bytes to parse. If -1 byte stream has to be termuinated with '\0'
  */
  foreign void parse(const byte* s, int length = -1) THROWS2(::org::xml::sax::RSAXException, acdk::io::RIOException);

  ::org::xml::sax::REntityResolver getEntityResolver() { return _entityResolver; }
  ::org::xml::sax::RDTDHandler getDTDHandler() { return _dtdHandler; }
  ::org::xml::sax::RDocumentHandler getDocumentHandler() { return _documentHandler; }
  ::org::xml::sax::RErrorHandler getErrorHandler() { return _errorHandler; }
};

} // namespace sax
} // namespace xml
} // namespace acdk

#endif //acdk_xml_sax_XMLParser_h
