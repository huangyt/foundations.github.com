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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/DOMWriter.h,v 1.6 2005/02/05 10:45:37 kommer Exp $


#ifndef org_w3c_dom_DOMWriter_h
#define org_w3c_dom_DOMWriter_h

#include "Node.h"
#include "Document.h"
#include "Element.h"
#include "Attr.h"
#include "Entity.h"
#include "Text.h"

#include <acdk/io/AbstractCharFilterWriter.h>
#include <acdk/io/CharToByteWriter.h>
#include <acdk/locale/Encoding.h>

namespace org {
namespace w3c {
namespace dom {

enum DOMWriterFormatFlags
{
  /** print new lines */
  DWFFNewLines              = 0x0001,
  /** print xml encoding in declaration */
  DWFFSuppressEncodingDecl      = 0x0002,
  /** print a new line after each declaration */
  DWFFWithNewLinesAfterDecl = 0x0004,
  /** write empty tags with open and close tag */
  DWFFExpandEmptyElements   = 0x0008,
  /** Trim the text elements */
  DWFFTrimText              = 0x0010,
  /** use this if to add a single white space after text, but before (child) start tag */
  DWFFPadText               = 0x0020,
  /** don't print XML decl at start of document */
  DWFFSuppressXmlDecl       = 0x0040,

  DWFFDefaultXmlFlags          =  0,
  DWFFCompactFlags             = 0,
  DWFFPrettyFlags             = DWFFNewLines | DWFFWithNewLinesAfterDecl | DWFFNewLines /* | DWFFSuppressEncodingDecl */
};
ACDK_DEF_LIB_ENUM(ACDK_ORG_XML_PUBLIC, DOMWriterFormatFlags);

ACDK_DECL_CLASS(DOMWriterFormat);

/**
  defines a format for writing a dom
*/
class ACDK_ORG_XML_PUBLIC DOMWriterFormat
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(DOMWriterFormat)
protected:
  /** combination of DOMWriterFormatFlags */
  int _formatFlags;
  acdk::locale::REncoding _encoding;
  RString _lineSep;
  bool _doResolveEntityRefs;
  RString _indentString;
public:
  DOMWriterFormat(int fflags = DWFFDefaultXmlFlags, IN(acdk::lang::RString) encoding = "utf-8");
  //int identSize() { return _identSize; }
  //void setIdentSize(int ident) { _identSize = ident; }
  acdk::locale::REncoding getEncoding() { return _encoding; }
  acdk::lang::RString getEncodingAsString() { return _encoding->getName(); }
  void setEncoding(IN(acdk::locale::REncoding) encoding) {  _encoding = encoding; }

  bool getNewLines() { return _formatFlags & DWFFNewLines; }
  void setNewLines(bool flag) { if (flag == true) _formatFlags |= DWFFNewLines; else _formatFlags &= ~DWFFNewLines; }
  
  bool getSuppressXmlDecl() { return _formatFlags & DWFFSuppressXmlDecl; }
  void setSuppressXmlDecl(bool flag) { if (flag == true) _formatFlags |= DWFFSuppressXmlDecl; else _formatFlags &= ~DWFFSuppressXmlDecl; }
  bool getSuppressEncodingDecl() { return _formatFlags & DWFFSuppressEncodingDecl; }
  void setSuppressEncodingDecl(bool flag) { if (flag == true) _formatFlags |= DWFFSuppressEncodingDecl; else _formatFlags &= ~DWFFSuppressEncodingDecl; }
  bool getNewLineAfterDecl() { return _formatFlags & DWFFWithNewLinesAfterDecl; }
  bool getExpandEmptyElements() { return _formatFlags & DWFFExpandEmptyElements; }
  void setExpandEmptyElements(bool flag) { if (flag == true) _formatFlags |= DWFFExpandEmptyElements; else _formatFlags &= ~DWFFExpandEmptyElements; }
  bool getTrimText() { return _formatFlags & DWFFTrimText; }
  void setTrimText(bool flag) { if (flag == true) _formatFlags |= DWFFTrimText; else _formatFlags &= ~DWFFTrimText; }
  bool getPadText() { return _formatFlags & DWFFPadText; }
  void setPadText(bool flag) { if (flag == true) _formatFlags |= DWFFPadText; else _formatFlags &= ~DWFFPadText; }
  
  RString getLineSeperator() { return _lineSep; }
  static RDOMWriterFormat createCompact()
  {
    RDOMWriterFormat dwf = new DOMWriterFormat(DWFFCompactFlags);
    dwf->setIndent("");
    return dwf;
  }
  static RDOMWriterFormat createPretty()
  {
    return new DOMWriterFormat(DWFFPrettyFlags);
  }
  int getMaxUnescapedCharacter();
  RString _escapeAttrValue(IN(RString) value);
  foreign void  encodeCharacter(uc2char ch, StringBuffer& sb);
  RString getIndent() { return _indentString; }
  void setIndent(IN(RString) s) { _indentString = s; }
};

USING_CLASS(acdk::io::, AbstractCharFilterWriter);


ACDK_DECL_CLASS(DOMWriter);

/**
 The DOMWriter class is an ACDK extension, not specified by w3c org
*/
class ACDK_ORG_XML_PUBLIC DOMWriter
: extends acdk::io::AbstractCharFilterWriter
{
  ACDK_WITH_METAINFO(DOMWriter)
protected:
  RDOMWriterFormat _format;
  int _indentLevel;
  bool _preserve;
  bool _doResolveEntityRefs;
  int _lastWrittenNodeType;
  bool _escapeText;
public:
  DOMWriter(IN(acdk::io::RWriter) out, IN(RDOMWriterFormat) format) 
    : AbstractCharFilterWriter(new acdk::io::CharToByteWriter(out, format->getEncoding()->getEncoder()))
  , _format(format)
  , _indentLevel(0)
  , _preserve(false)
  , _doResolveEntityRefs(true)
  , _lastWrittenNodeType(0)
  , _escapeText(true)
  {
  }
  virtual void writeNode(IN(RNode) node);
  virtual void writeDocument(IN(RDocument) doc);
  virtual void writeElement(IN(RElement) node);
  virtual void writeAttribute(IN(RAttr) node);
  virtual void writeCDATA(IN(RString) text);
  virtual void writeText(IN(RText) node);
  virtual void writeEntity(IN(REntity) node);
  virtual void writeProcessingInstruction(IN(RProcessingInstruction) node);
  virtual void writeComment(IN(RString) text);
  virtual void writeDocType(IN(RDocumentType) doctype);

  void writeln();
  void writeIndent();
  bool getResolveEntityRefs() { return _doResolveEntityRefs; }
  void setResolveEntityRefs(bool resolve) { _doResolveEntityRefs = resolve; }
protected:
  void _writeDeclaration();
  void _closeEmptyTag(IN(RString) tagname);
  void  _writeAttributes(IN(RElement) element);
  void _writeElementContent(IN(RElement) element);
  void _writeEntityRef(IN(RString) name);
  void _writeNodeValue(IN(RString) text);
  RString _escapeElementEntities(IN(RString) text);


};

} // namespace dom
} // namespace w3c
} // namespace org

#endif //org_w3c_dom_DOMWriter_h
