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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/sax/XMLParser.cpp,v 1.13 2005/02/05 10:45:37 kommer Exp $


#include "XMLParser.h"
#include "AttributeListImpl.h"
#include <acdk/io/LineNumberReader.h>
#define COMPILED_FROM_DSP
#define XML_STATIC
#include <expat/expat.h>
#include <org/xml/sax/DocumentHandler.h>
#include <org/xml/sax/HandlerBase.h>
#include <org/xml/sax/SAXParseException.h>

namespace acdk {
namespace xml {
namespace sax {
  
namespace {

#if 0

// doesn't compile with gcc
typedef struct XML_Encoding ExpatEncoding;

ExpatEncoding* getExpatEncoding(IN(RString) enc)
{
  /*
  REncoding encoding = acdk::locale::Encoding::getEncoding(enc);
  if (encoding == Nil)
    return 0;
    */
  /* TODO 
    For ISO
      fill XML_Encoding.map < 128 with map[i] = i.
      fill XML_Encoding.map > 128 with map[i] = IsoCharacter.
    For other encodings
      fill XML_Encoding.map < 128 with map[i] = i.
      fill escape chars.

    or (as to test) fill all with with -3
  */
  return 0;
}
#endif //0

#define UF8STR(text) new String(text, CCUtf8 | NormalSST)

void characterHandler(void* userData, const char* txt, int len)
{
  XMLParser* This = reinterpret_cast<XMLParser*>(userData);
  ::org::xml::sax::RDocumentHandler dh = This->getDocumentHandler();
  if (dh != Nil)
  {
    RString s = new String(txt, len, CCUtf8 | NormalSST);
    dh->characters(s, 0, s->length());
  }
} 

void startElementHandler(void* userData, const char* name, const char** atts)
{
  XMLParser* This = reinterpret_cast<XMLParser*>(userData);
  RAttributeListImpl ali = new AttributeListImpl();
  if(atts && *atts != 0) {
    
    while(*atts != 0) 
    {
      RString name = UF8STR(*atts++);
      RString value = UF8STR(*atts++);
      ali->addAttribute(name, "", value);
    } 
  } 
  This->getDocumentHandler()->startElement(UF8STR(name), &ali);
}

void endElementHandler(void* userData, const char* name)
{
  XMLParser* This = reinterpret_cast<XMLParser*>(userData);
  This->getDocumentHandler()->endElement(UF8STR(name));
  
} 

void processingInstruction(void* userData, const char* target, const char* data)
{
  XMLParser* This = reinterpret_cast<XMLParser*>(userData);
  ::org::xml::sax::RDocumentHandler dh = This->getDocumentHandler();
  if (dh != Nil)
    dh->processingInstruction(UF8STR(target), UF8STR(data));
} 

void commentHandler(void *userData, const XML_Char *data) // ### @todo unmapped
{
  XMLParser* This = reinterpret_cast<XMLParser*>(userData);
  ::org::xml::sax::RDocumentHandler dh = This->getDocumentHandler();
  /*if (dh != Nil)
    dh->processingInstruction(UF8STR(target), UF8STR(data));
    */
}

void startCdataSectionHandler(void *userData)
{
  XMLParser* This = reinterpret_cast<XMLParser*>(userData);
  ::org::xml::sax::RDocumentHandler dh = This->getDocumentHandler();
}

void endCdataSectionHandler(void *userData)
{
  XMLParser* This = reinterpret_cast<XMLParser*>(userData);
  ::org::xml::sax::RDocumentHandler dh = This->getDocumentHandler();
}

void defaultHandler(void *userData, const XML_Char *s, int len)
{
   XMLParser* This = reinterpret_cast<XMLParser*>(userData);
  ::org::xml::sax::RDocumentHandler dh = This->getDocumentHandler();
}

int externalEntityRefHandler(XML_Parser p,
                                        const XML_Char *context,
                                        const XML_Char *base,
                                        const XML_Char *systemId,
                                        const XML_Char *publicId)
{

  return XML_STATUS_OK ;
}

} // anon namespace



XMLParser::XMLParser() 
: Object()
, _xmlParser(0)
{
  ::org::xml::sax::RHandlerBase hb = new ::org::xml::sax::HandlerBase();
  _entityResolver = &hb;
  _dtdHandler = &hb;
  _documentHandler = &hb;
  _errorHandler = &hb;
  _xmlParser = XML_ParserCreate(0);
  XML_SetUserData((XML_Parser)_xmlParser, reinterpret_cast<void*>(this));
  //XML_SetStartDoctypeDeclHandler((XML_Parser)_xmlParser, characterHandler);

  XML_SetCharacterDataHandler((XML_Parser)_xmlParser, characterHandler);
  XML_SetElementHandler((XML_Parser)_xmlParser, startElementHandler, endElementHandler);
  XML_SetCommentHandler((XML_Parser)_xmlParser, commentHandler);
  XML_SetCdataSectionHandler((XML_Parser)_xmlParser, startCdataSectionHandler, endCdataSectionHandler);
  XML_SetDefaultHandler((XML_Parser)_xmlParser, defaultHandler);
}

XMLParser::~XMLParser()
{
  if (_xmlParser != 0) {
    XML_ParserFree((XML_Parser)_xmlParser);
    _xmlParser = 0;
  }
}


//virtual 
void 
XMLParser::setLocale(IN(acdk::util::RLocale) locale) THROWS1(::org::xml::sax::RSAXException)
{
  _locale = locale;
}

//virtual 
void 
XMLParser::setEntityResolver(IN(::org::xml::sax::REntityResolver)  entityresolver)
{
   _entityResolver = entityresolver;
}

//virtual 
void 
XMLParser::setDTDHandler(IN(::org::xml::sax::RDTDHandler)  dtdhandler)
{
  _dtdHandler = dtdhandler;
}

//virtual 
void 
XMLParser::setDocumentHandler(IN(::org::xml::sax::RDocumentHandler)  documenthandler)
{
  _documentHandler = documenthandler;
}

//virtual 
void 
XMLParser::setErrorHandler(IN(::org::xml::sax::RErrorHandler) errorhandler)
{
  _errorHandler = errorhandler;
}

USING_CLASS(::acdk::io::, Reader);
USING_CLASS(::acdk::io::, AbstractFilterReader);

USING_CLASS(::acdk::io::, LineNumberReader);


class ByteLineNumberReader
: extends AbstractFilterReader
{
  int _lineNo;
  bool _eof;
public:
  ByteLineNumberReader(IN(RReader) in)
  : AbstractFilterReader(in)
  , _lineNo(0)
  , _eof(false)
  {
  }
  int getLineNumber() { return _lineNo; }
  virtual int read()
  {
    int c = _in->read();
    if (c == -1)
    {
      _eof = true;
      return -1;
    }
    if (c == '\n')
      ++_lineNo;
    return c;
  }
  RbyteArray readLine()
  {
    if (_eof == true)
      return Nil;
    RbyteArray ba = new byteArray(0);
    int i;
    while ((i = read()) != -1)
    {
      ba->append((byte)i);
      if (i == '\n')
        return ba;
    }
    _eof = true;
    return ba;
  }
};

//virtual 
void 
XMLParser::parse(IN(::org::xml::sax::RInputSource) inputsource) 
                                THROWS2(::org::xml::sax::RSAXException, acdk::io::RIOException)
{
  RString encstr = inputsource->getEncoding();
  if (encstr != Nil)
    XML_SetEncoding((XML_Parser)_xmlParser, encstr->c_str());
  RReader rin = inputsource->getByteStream();
  ByteLineNumberReader lin(&rin);
  RbyteArray str = lin.readLine();
  while (str != Nil)
  {
    RbyteArray tstr = lin.readLine();
    int ret = XML_Parse((XML_Parser)_xmlParser, (const char*)str->data(), str->length(), tstr == Nil);  
    
    if (ret == 0) 
    {
      enum XML_Error xmerr = XML_GetErrorCode((XML_Parser)_xmlParser);
      int colno = XML_GetCurrentColumnNumber((XML_Parser)_xmlParser);
      RString msg = new String(XML_ErrorString(xmerr));
        ::org::xml::sax::RSAXParseException spe 
          = new ::org::xml::sax::SAXParseException(msg, inputsource->getPublicId()
                                                   , inputsource->getPublicId()
                                                   , lin.getLineNumber(), colno);
      _errorHandler->error(spe);
      break;
    }
    str = tstr;
  }
}



//virtual 
void 
XMLParser::parse(IN(RString) s) 
  THROWS2(::org::xml::sax::RSAXException, acdk::io::RIOException)
{
  _documentHandler->startDocument();
  RString ts = s->convert(CCUtf8);
  const byte* bb = ts->byte_begin();
  const byte* be = ts->byte_end();
  parse(bb, be - bb);
}

void 
XMLParser::parse(IN(RbyteArray) s, int length) THROWS2(::org::xml::sax::RSAXException, acdk::io::RIOException)
{
  _documentHandler->startDocument();
  if (length == -1)
    length = s->length();
  parse(s->data(), length);
}

void 
XMLParser::parse(const byte* s, int length) THROWS2(::org::xml::sax::RSAXException, acdk::io::RIOException)
{
  if (length == -1)
    length = strlen((const char*)s);
  int ret = XML_Parse((XML_Parser)_xmlParser, (const char*)s, length, 1);
  if (ret == 0) 
  {
    enum XML_Error xmerr = XML_GetErrorCode((XML_Parser)_xmlParser);
    int lineno = XML_GetCurrentLineNumber((XML_Parser)_xmlParser);
    int colno = XML_GetCurrentColumnNumber((XML_Parser)_xmlParser);
    RString msg = new String(XML_ErrorString(xmerr));
    ::org::xml::sax::RSAXParseException spe 
        = new ::org::xml::sax::SAXParseException(msg, Nil, Nil, lineno, colno);
    _errorHandler->error(spe);

  }
  _documentHandler->endDocument();
}


} // namespace sax
} // namespace xml
} // namespace acdk


