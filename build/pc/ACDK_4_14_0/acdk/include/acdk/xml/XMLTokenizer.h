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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/XMLTokenizer.h,v 1.8 2005/02/05 10:45:36 kommer Exp $

#ifndef acdk_xml_XMLTokenizer_h
#define acdk_xml_XMLTokenizer_h

#include <acdk.h>
#include <acdk/io/Reader.h>

#include "Config.h"

namespace acdk {
namespace xml {

using namespace ::acdk::lang;
using namespace ::acdk::io;


ACDK_DECL_CLASS(XMLTokenizer);

/**
  parses XML
  should be reworked, because doesn't fit into io concept
 */
class ACDK_XML_PUBLIC XMLTokenizer
: extends ::acdk::lang::Object
{
  ACDK_WITH_METAINFO(XMLTokenizer)
public:
  /**
    The expected tokens for a XML stream.
   */
  static const short TOK_INVALID;
  static const short TOK_EOF;
  static const short TOK_SYMBOL;
  static const short TOK_STRING;
  static const short TOK_TEXT;
  static const short TOK_COMMENT;
  static const short TOK_LT;        // <
  static const short TOK_GT;        // >
  static const short TOK_QSIGN;     // ?
  static const short TOK_EQ;        // =
  static const short TOK_SLASH;     // /
  static const short TOK_EXCLAM;    // !
  static const short TOK_BAR;       // |
  static const short TOK_LPAREN;    // (
  static const short TOK_RPAREN;    // )
  static const short TOK_LBRACKET;  // [
  static const short TOK_RBRACKET;  // ]
  static const short TOK_PLUS;      // +
  static const short TOK_ASTERISK;  // *
  static const short TOK_COMMA;     // ,
  static const short TOK_SEMICOLON; // ;
  static const short TOK_NSIGN;     // #
  static const short TOK_APOSTR;    // '
  static const short TOK_PERCENT;   // %

  XMLTokenizer(IN(RReader) in);
  virtual short nextToken();
  virtual RString element();
  virtual void unread();
  RReader getIn() { return _in; }
protected:
  virtual void _skipWhitespace();
  virtual short _readSymbol();
  virtual short _readString();
  virtual short _readComment();
  virtual short _readText();
private:
  inline int _read()
  {
    if (_pushedBack == -1) return _in->read();
    int c = _pushedBack;
    _pushedBack = -1;
    return c;
  }
  RReader _in;
  RStringBuffer _elem;
  short _lastTok;
  bool _useLast;
  bool _isOpen;
  int _pushedBack;
};

} // namespace xml
} // namespace acdk

#endif //acdk_xml_XMLTokenizer_h
