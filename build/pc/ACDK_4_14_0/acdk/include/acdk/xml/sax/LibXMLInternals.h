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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/sax/LibXMLInternals.h,v 1.7 2005/02/05 10:45:37 kommer Exp $

#ifndef acdk_xml_sax_LibXMLInterals_h
#define acdk_xml_sax_LibXMLInterals_h

#include <acdk.h>
#include "sax.h"
#include <org/w3c/dom/Node.h>
#include <acdk/io/FileReader.h>

#include <libxml/SAX.h>
#include <libxml/parser.h>


namespace acdk {
namespace xml {
namespace sax {

ACDK_DECL_CLASS(XMLReader);

} // namespace sax
} // namespace xml
} // namespace acdk

#define PROPERTIES_PREFIX "http://xml.org/sax/properties/"
#define FEATURES_PREFIX "http://xml.org/sax/features/"

#define XML2STR(text) (text == 0 ? RString(Nil) : RString(new String((const char*)text, CCUtf8 | NormalSST)))
#define XML2STR_LEN(text, length) (text == 0 ? RString(Nil) : RString(new String((const char*)text, length, CCUtf8 | NormalSST)))

#define STR2XML(str) ((str) == Nil ? (const xmlChar*)0 : (const xmlChar*)(str)->convert(CCUtf8)->c_str())
#define STR2XMLDUP(str) xmlCharStrdup((const char*)STR2XML(str))
#define STR2XMLSTR(str) ((str)->convert(CCUtf8))


struct SaxParseContext
{
  acdk::io::RReader _in;
  acdk::xml::sax::XMLReader* _xmlReader;
  xmlParserCtxtPtr ctx;
  xmlSAXLocatorPtr loc;
  xmlSAXHandlerPtr sax;
  RString publicId;
  RString systemId;
  
  static SaxParseContext* sctxFromCtx(void* sp) 
  { return (SaxParseContext*)((xmlParserCtxtPtr)sp)->_private; }
  void* getXmlCtx() { return (void*)ctx; }

  SaxParseContext(acdk::xml::sax::XMLReader* reader, IN(RString) pubId, IN(RString) sysId, IN(acdk::io::RReader) in)
  : _in(in)
  , _xmlReader(reader)
  , ctx(0)
  , loc(0)
  , sax(0)
  , publicId(pubId)
  , systemId(sysId)
  {
  }
  void setCtx(xmlParserCtxtPtr ctx_)
  {
    ctx = ctx_;
    if (ctx != 0)
      sax = ctx_->sax;
  }
  /*
  ReaderWrapper(const char* filename)
    : _in(new acdk::io::FileReader(filename))
  {
  }
  */
  int read(char* buffer, int len)
  {
    try {
      return _in->read((byte*)buffer, 0, len);
    } catch (acdk::io::RIOException ex) {
      return -1;
    }

  }
  int close()
  {
    try {
      _in->close();
    } catch (acdk::io::RIOException ex) {
      return -1;
    }
    return 0;
  }
  static int	readCB(void* context, char* buffer, int len)
  {
    //return sctxFromCtx(context)->read(buffer, len);
    return ((SaxParseContext*)context)->read(buffer, len);
  }
  /*
  static void* openCB(char const* filename)
  {
    return (void*)new ReaderWrapper(filename);
  }
  */
  static int closeCB(void* context)
  {
    //return sctxFromCtx(context)->close();
    return ((SaxParseContext*)context)->close();
  }
};

/*
struct SaxParseContext
{
  
  SaxParseContext(acdk::xml::sax::XMLReader* reader, xmlParserCtxtPtr ctx_, IN(RString) pubId, IN(RString) sysId, ReaderWrapper* rw)
  : _xmlReader(reader)
  , ctx(ctx_)
  , loc(0)
  , sax(ctx_->sax)
  , publicId(pubId)
  , systemId(sysId)
  , readerWrapper(rw)
  {
  }
};
*/

struct StackSavedStr
{
  xmlChar* _str;
  StackSavedStr(xmlChar* str) : _str(str) {}
  ~StackSavedStr() { xmlFree(_str); }
};


#endif //acdk_xml_sax_LibXMLInterals_h
