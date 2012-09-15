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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/org/w3c/dom/DOMWriter.cpp,v 1.3 2005/02/05 10:45:37 kommer Exp $


#include "DOMWriter.h"
#include <acdk/util/StringTokenizer.h>
#include <org/xml/sax/helpers/MiscXmlUtils.h>

namespace org {
namespace w3c {
namespace dom {


DOMWriterFormat::DOMWriterFormat(int fflags, IN(acdk::lang::RString) encoding)
: _formatFlags(fflags)
, _encoding(acdk::locale::Encoding::getEncoding(org::xml::sax::helpers::MiscXmlUtils::xmlEncodingNameToAcdkEncodingName(encoding)))
, _lineSep("\n")
, _indentString("  ")
{
}

void 
DOMWriter::writeNode(IN(RNode) node)
{
  int nt = node->getNodeType();
  switch (nt) 
  {
  case ELEMENT_NODE:
    writeElement((RElement)node);
    break;
  case ATTRIBUTE_NODE:
    writeAttribute((RAttr)node);
    break;
  case TEXT_NODE:
    writeText((RText)node);
    break;
  case CDATA_SECTION_NODE:
    writeCDATA(node->getNodeValue());
    break;
  case ENTITY_REFERENCE_NODE:
    writeEntity((REntity)node);
    break;
  case PROCESSING_INSTRUCTION_NODE:
    writeProcessingInstruction((RProcessingInstruction)node);
    break;
  case COMMENT_NODE:
    writeComment(node->getNodeValue());
    break;
  case DOCUMENT_NODE:
    writeDocument((RDocument)node);
    break;
  case DOCUMENT_TYPE_NODE:
    writeDocType((RDocumentType)node);
    break;
  default:
    THROW2(DOMException, INVALID_STATE_ERR, "Invalid node type: " + nt);
  }
}

void 
DOMWriter::writeDocument(IN(RDocument) doc)
{
  _writeDeclaration();
  if (doc->getDoctype() != Nil) 
  {
    writeIndent();
    writeDocType(doc->getDoctype());
  }
  RNode fc = doc->getFirstChild();
  while (fc != Nil)
  {
    writeNode(fc);
    fc = fc->getNextSibling();
  }
  writeln();
}

void 
DOMWriter::writeDocType(IN(RDocumentType) doctype)
{
  if (doctype == Nil)
    return;
  _out->writeString(doctype->toXML());
}

void 
DOMWriter::_writeDeclaration()
{
  //RString encoding = _format->getEncoding();
  if (_format->getSuppressXmlDecl() == true)
    return;
  _out->writeString("<?xml version = \"1.0\"");
  if (_format->getSuppressEncodingDecl() == false)
  {
    _out->writeString(" encoding=\"" + _format->getEncodingAsString() + "\"");
  }
  _out->writeString("?>");
  if (_format->getNewLineAfterDecl() == true)
    writeln();
}

void 
DOMWriter::writeElement(IN(RElement) node)
{
  int size = node->getChildCount();
  
  RString qualifiedName = node->getNodeName(); 
  
  writeln();
  writeIndent();
  
  _out->writeString("<");
  _out->writeString(qualifiedName);
  // ### @todo handle namespace
  
  
  bool textOnly = true;
  int childCount = node->getChildCount();

  for (int i = 0; i < childCount; i++) 
  {
    RNode childNode = node->getChild(i);
    if (instanceof(childNode, Element) == true ||
        instanceof(childNode, Comment) == true)
    {
      textOnly = false;
      break;
    }
  }

  _writeAttributes(node);
  
  _lastWrittenNodeType = ELEMENT_NODE;
  
  if (size <= 0) 
  {
    _closeEmptyTag(qualifiedName);
  }
  else 
  {
    _out->writeString(">");
    if (textOnly == true) 
    {
      _writeElementContent(node);
    }
    else 
    {
      ++_indentLevel;
      _writeElementContent(node);
      --_indentLevel;
      writeln();
      writeIndent();
    }
    _out->writeString("</");
    _out->writeString(qualifiedName);
    _out->writeString(">");
  }
  // ## @todo namespace scope
  
}


void 
DOMWriter::writeAttribute(IN(RAttr) node)
{
  RString name = node->getName();
  RString value = node->getValue();
  _out->writeString(" ");
  _out->writeString(name);
  _out->writeString("=\"");
  _out->writeString(_format->_escapeAttrValue(value));
  _out->writeString("\"");
}

void 
DOMWriter::writeCDATA(IN(RString) text)
{
  _out->writeString("<![CDATA[");
  _out->writeString(text); // ## @todo split section, if needed
  _out->writeString("]]>");

  _lastWrittenNodeType = CDATA_SECTION_NODE;
}


void 
DOMWriter::writeText(IN(RText) node)
{
  _writeNodeValue(node->getNodeValue());
}

void 
DOMWriter::writeEntity(IN(REntity) node)
{
  if (getResolveEntityRefs() == false) 
    _writeEntityRef(node->getNodeName());
  else
    _out->writeString(node->getNodeValue()); // check if this is correct
  
}

void 
DOMWriter::_writeEntityRef(IN(RString) name)
{
  _out->writeString("&");
  _out->writeString(name);
  _out->writeString(";");

  _lastWrittenNodeType = ENTITY_REFERENCE_NODE;
}

void 
DOMWriter::writeProcessingInstruction(IN(RProcessingInstruction) node)
{
  _out->writeString("<?" );
  _out->writeString(node->getNodeName());
  _out->writeString(" ");
  _out->writeString(node->getNodeValue());
  _out->writeString("?>");
  writeln();
  _lastWrittenNodeType = PROCESSING_INSTRUCTION_NODE;
}

void 
DOMWriter::writeComment(IN(RString) text)
{
  if (_format->getNewLines()) 
  {
    writeln();
    writeIndent();
  }
  _out->writeString("<!--");
  _out->writeString(text);
  _out->writeString("-->");

  _lastWrittenNodeType = COMMENT_NODE;
}



void 
DOMWriter::writeln()
{
  _out->writeString(_format->getLineSeperator());
}

void
DOMWriter::writeIndent()
{
  RString indent = _format->getIndent();
  if (indent == Nil || indent->length() == 0) 
    return;

  for (int i = 0; i < _indentLevel; i++) 
    _out->writeString(indent);
}

void 
DOMWriter::_closeEmptyTag(IN(RString) tagname)
{
  if (_format->getExpandEmptyElements() == true)
  {
    _out->writeString("></");
    _out->writeString(tagname);
    _out->writeString(">");
  }
  else
  {
    _out->writeString("/>");
  }
}

void 
DOMWriter::_writeAttributes(IN(RElement) element) 
{
  for ( int i = 0, size = element->attributeCount(); i < size; i++ ) 
  {
    
    RAttr attribute = element->attribute(i);
    // ### @todo handle namespaces, like xmlns here
    writeAttribute(attribute);
    
  }
}

bool _getPreservedAttr(IN(RElement) element, bool defaultValue) 
{
  RAttr attr = (RAttr)element->getAttributeNode("xml::space");
  if (attr == Nil)
    return defaultValue;
  if (attr->getValue()->equals("preserve") == true)
    return true;
  return false;
}


void 
DOMWriter::_writeElementContent(IN(RElement) element)
{
  bool trim = _format->getTrimText();
  bool oldPreserve = _preserve;
  if (trim == true) 
  { 
    _preserve = _getPreservedAttr(element, _preserve);
    trim = _preserve == false;
  }
  if (trim == true) 
  {
    RText lastTextNode = Nil;
    StringBuffer theBuffer;
    RStringBuffer buffer = Nil; //&theBuffer;
    bool textOnly = true;
    for ( int i = 0, size = element->getChildCount(); i < size; i++ ) 
    {
      RNode node = element->getChild(i);
      if (instanceof(node, Text) == true) 
      {
        if (lastTextNode == Nil) 
        {
          lastTextNode = (RText)node;
        }
        else 
        {
          if (buffer == Nil)
            buffer = &theBuffer;
          buffer << node->getNodeValue();
        }
      }
      else 
      {
        if (textOnly == false && _format->getPadText() == true) 
        {
          _out->writeString(" ");
        }
        
        textOnly = false;
        
        if (lastTextNode != Nil ) 
        {
          if (buffer != Nil) 
          {
            _out->writeString(buffer->toString());
            buffer = Nil;
            theBuffer.reset();
          }
          else {
            _out->writeString(lastTextNode->getNodeValue());
          }
          lastTextNode = Nil;
          
          if (_format->getPadText() == true) 
          {
            _out->writeString(" ");
          }
        }
        writeNode(node);
      }
    }
    if (lastTextNode != Nil) 
    {
      if (textOnly == false && _format->getPadText() == true) 
      {
        _out->writeString(" ");
      }
      if (buffer != Nil) 
      {
        _out->writeString(buffer->toString());
        buffer = Nil;
        theBuffer.reset();
      }
      else {
        _out->writeString(lastTextNode->getNodeValue());
      }
      lastTextNode = Nil;
    }
  }
  else 
  {
    RNode lastTextNode = Nil;
    for ( int i = 0, size = element->getChildCount(); i < size; i++ ) 
    {
      RNode node = element->getChild(i);
      if (instanceof(node, Text) == true) 
      {
        writeNode(node);
        lastTextNode = node;
      } 
      else 
      {
        if (lastTextNode != Nil && _format->getPadText() == true) 
          _out->writeString(" ");
        
        writeNode(node);
        
        if (lastTextNode != Nil && _format->getPadText() == true) 
          _out->writeString(" ");
        
        lastTextNode = Nil;
      }
    }
  }
  _preserve = oldPreserve;
}


void 
DOMWriter::_writeNodeValue(IN(RString) text_)
{
  RString text = text_;
  if (text != Nil && text->length() > 0) 
  {
    if (_escapeText == true) 
      text = _escapeElementEntities(text);
    
    if (_format->getTrimText() == true) 
    {
      bool first = true;
      acdk::util::StringTokenizer tokenizer(text, " ");
      while (tokenizer.hasMoreTokens() == true) 
      {
        RString token = tokenizer.nextToken();
        if ( first == true) 
        {
          first = false;
          if ( _lastWrittenNodeType == TEXT_NODE )
            _out->writeString(" ");
          
        }
        else 
        {
          _out->writeString(" ");
        }
        _out->writeString(token);
        _lastWrittenNodeType = TEXT_NODE;
      }
    }
    else 
    {
      _lastWrittenNodeType = TEXT_NODE;
      _out->writeString(text);
    }
  }
}

RString 
DOMWriter::_escapeElementEntities(IN(RString) text) 
{
  String::iterator it = text->begin();
  String::iterator end = text->end();
  StringBuffer sb(text->length() * 1.2);
  for (; it < end; ++it)
  {
    ucchar ch = *it;
    switch(ch)
    {
    case '<':
      sb << "&lt;";
      break;
    case '>':
      sb << "&gt;";
      break;
    case '&':
      sb << "&amp;";
      break;
    case '\t': 
    case '\n': 
    case '\r':
      // don't encode standard whitespace characters
      if (_preserve == true) 
        sb << ch;
      break;
    default:
      if (ch < 32 || _format->getMaxUnescapedCharacter() < ch) 
        sb << "&#" << (int) ch << ";";
      else
        sb << ch;
      break;
    }
  }
   return sb.toString();
}

int 
DOMWriterFormat::getMaxUnescapedCharacter()
{
  return 127; // for ISO codes may also up to 255
}

void 
DOMWriterFormat::encodeCharacter(uc2char ch, StringBuffer& sb)
{
  switch(ch)
  {
  case '<':
    sb << "&lt;";
    break;
  case '>':
    sb << "&gt;";
    break;
  case '\'':
    sb <<  "&apos;";
    break;
  case '\"':
    sb << "&quot;";
    break;
  case '&':
    sb << "&amp;";
    break;
  case '\t': 
  case '\n': 
  case '\r':
    sb << ch;
    break;
  default:
    if (ch < 32 || ch > getMaxUnescapedCharacter()) 
      sb << "&#" << (int) ch << ";";
    else
      sb << ch;
    break;
  }
}

RString 
DOMWriterFormat::_escapeAttrValue(IN(RString) value)
{
  StringBuffer sb(value->length() * 1.2);
  String::iterator it = value->begin();
  String::iterator end = value->end();
  for (; it < end; ++it)
  {
    encodeCharacter(*it, sb);
  }
  return sb.toString();
}


} // namespace dom
} // namespace w3c
} // namespace org

