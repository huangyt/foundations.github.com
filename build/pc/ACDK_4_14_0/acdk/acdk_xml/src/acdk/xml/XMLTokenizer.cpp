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
// $Header: /cvsroot/acdk/acdk/acdk_xml/src/acdk/xml/XMLTokenizer.cpp,v 1.7 2005/03/08 18:50:42 kommer Exp $

#include <acdk.h>
#include <acdk/lang/StringBuffer.h>
#include <acdk/lang/Character.h>
#include <acdk/io/Reader.h>
#include <acdk/io/IOException.h>

#include "XMLTokenizer.h"


namespace acdk {
namespace xml {

using namespace ::acdk::lang;
using namespace ::acdk::io;

// static
const short XMLTokenizer::TOK_INVALID = 1;
// static
const short XMLTokenizer::TOK_EOF = 2;
// static
const short XMLTokenizer::TOK_SYMBOL = 4;
// static
const short XMLTokenizer::TOK_STRING = 5;
// static
const short XMLTokenizer::TOK_TEXT = 6;
// static
const short XMLTokenizer::TOK_COMMENT = 7;
// static
const short XMLTokenizer::TOK_LT = 8;         // <
// static
const short XMLTokenizer::TOK_GT = 9;         // >
// static
const short XMLTokenizer::TOK_QSIGN = 10;     // ?
// static
const short XMLTokenizer::TOK_EQ = 11;        // =
// static
const short XMLTokenizer::TOK_SLASH = 12;     // /
// static
const short XMLTokenizer::TOK_EXCLAM = 13;    // !
// static
const short XMLTokenizer::TOK_BAR = 14;       // |
// static
const short XMLTokenizer::TOK_LPAREN = 15;    // (
// static
const short XMLTokenizer::TOK_RPAREN = 16;    // )
// static
const short XMLTokenizer::TOK_LBRACKET = 17;  // [
// static
const short XMLTokenizer::TOK_RBRACKET = 18;  // ]
// static
const short XMLTokenizer::TOK_PLUS = 19;      // +
// static
const short XMLTokenizer::TOK_ASTERISK = 20;  // *
// static
const short XMLTokenizer::TOK_COMMA = 21;     // ,
// static
const short XMLTokenizer::TOK_SEMICOLON = 22; // ;
// static
const short XMLTokenizer::TOK_NSIGN = 23;     // #
// static
const short XMLTokenizer::TOK_APOSTR = 24;    // '
// static
const short XMLTokenizer::TOK_PERCENT = 25;   // %

XMLTokenizer::XMLTokenizer(IN(RReader) in)
{
  _in = in;
  _useLast = false;
  _isOpen = false;
  _pushedBack = -1;
  _elem = new StringBuffer(1024);
}
  
// virtual
RString
XMLTokenizer::element() {
  return _elem->toString();
}

//  virtual 
short
XMLTokenizer::nextToken()
{
  char c;
  int i = 0;
  if (_useLast == true) {
    _useLast = false;
    return _lastTok;
  }
  
  _skipWhitespace ();
  i = _read();
  if (i == -1) // if EOF
    return _lastTok = TOK_EOF;
  
  c = (char)i;
  
  if (_isOpen != true) {
    if (c != '<') {
      _pushedBack=i;
      return _lastTok = _readText();
    }
  }
  
  switch (c) {
  case '<':
    _isOpen = true;
    return _lastTok = TOK_LT;
  case '>':
    _isOpen = false;
    return _lastTok = TOK_GT;
  case '?':
    return _lastTok = TOK_QSIGN;
  case '/':
    return _lastTok = TOK_SLASH;
  case '=':
    return _lastTok = TOK_EQ;
  case '(':
    return _lastTok = TOK_LPAREN;
  case ')':
    return _lastTok = TOK_RPAREN;
  case '[':
    return _lastTok = TOK_LBRACKET;
  case ']':
    return _lastTok = TOK_RBRACKET;
  case '|':
    return _lastTok = TOK_BAR;
  case '*':
    return _lastTok = TOK_ASTERISK;
  case '+':
    return _lastTok = TOK_PLUS;
  case ',':
    return _lastTok = TOK_COMMA;
  case ';':
    return _lastTok = TOK_SEMICOLON;
  case '%':
    return _lastTok = TOK_PERCENT;
  case '#':
    return _lastTok = TOK_NSIGN;
  case '\'':
    return _lastTok = TOK_APOSTR;
  case '"':
    // String einlesen
    return _lastTok = _readString();
  default:
    if (_isOpen == true) {
      if (Character::isLetterOrDigit(c) == true || 
          c == '[') // allow to read arrays
      {
        // Symbol (Element oder Attributbezeichner)       
        _pushedBack = i;
        //          _in->unread(i);
        return _lastTok = _readSymbol();
      }
      else if (c == '!') {
        int b;
        b = _read();
        c = (char)b;
        _pushedBack = b;
        // _in->unread(b);
        if (c == '-')
          return _lastTok = _readComment();
        else
          return _lastTok = TOK_EXCLAM;
      }
      else {
        return _lastTok = TOK_INVALID;
      }
    }
    else {
      _pushedBack = i;
      return _lastTok = _readText();
    }
  } //switch
}

// virtual
void
XMLTokenizer::unread ()
{
  _useLast = true;
}

// protected:
//  virtual 
void
XMLTokenizer::_skipWhitespace()
{
  char c;
  int i = 0;
  do {
    i = _read();
    // i = _in->read();
    c = (char)i;
    if (Character::isSpace(c) != true) {
      _pushedBack = i;
      // _in->unread(i);
      return;
    }
    else if (i == -1)
      return;
  } while (true);
}

// virtual
short
XMLTokenizer::_readSymbol ()
{
  char c;
  int ivalue = 0;
  _elem->reset();
  while (true) {
    ivalue = _read();
    // ivalue = _in->read();
    c = (char) ivalue;
    if (ivalue == -1 || Character::isSpace(c) == true) {
      // Symbol ist abgeschlossen
      break;
    } else if (c == '=' || c == '/' || c == '>' || c == '?' || c == '|' ||
             c == ')' || c == '\'' || c == ',' || c == ';') {
      // Symbol ist abgeschlossen, das gelesene Zeichen wird
      // aber noch benoetigt
      _pushedBack = ivalue;
      // _in->unread(ivalue);
      break;
    } else if (Character::isLetterOrDigit(c) == true ||
             c == '-' || (c == '_' && _elem->length() > 0))
      // korrektes Zeichen -> anhaengen
      _elem->append(c);
    else {
      // Zeichen nicht erlaubt ?
      return TOK_INVALID;
    }
  }
  // alle Grossbuchstaben in Kleinbuchstaben aendern !!!!
  return TOK_SYMBOL;
}

//  virtual 
short
XMLTokenizer::_readString()
{
  char c;
  int ivalue = 0;
  _elem->reset();
  
  while (true) {
    ivalue = _read();
    // ivalue = _in->read();
    c = (char)ivalue;
    if (ivalue == -1)
      // String ist noch nicht abgeschlossen
      return TOK_INVALID;
    else if (c == '\\') {
      // naechstes Zeichen quoten
    }
    else if (c == '"') {
      // String ist abgeschlossen
      return TOK_STRING;
    }
    else
      _elem->append(c);
  }
}

//  virtual 
short
XMLTokenizer::_readComment()
{
  char c1, c2;
  int ivalue = 0;
  _elem->reset();
  ivalue = _read();
  // ivalue = _in->read();
  c1 = (char)ivalue;
  ivalue = _read();
  // ivalue = _in->read();
  c2 = (char)ivalue;
  if (c1 != '-' || c2 != '-' || ivalue == -1)
    return TOK_INVALID;
  
  while (true) {
    ivalue = _read();
    c1 = (char)ivalue;
    if (ivalue == -1)
      return TOK_INVALID;
    if (c1 == '!') {
      for (int i = 0; i < 2; i++) {
        ivalue = _read();
        if (ivalue == -1)
          return TOK_INVALID;
        c1 = (char)ivalue;
        if (c1 != '-') {
          _pushedBack = ivalue;
          if (i == 0)
            _elem->append('!');
          else
            _elem->append("!-");
          continue;
        }
      }
      ivalue = _read();
      if (ivalue == -1)
        return TOK_INVALID;
      c1 = (char)ivalue;
      if (c1 != '>') {
        _pushedBack = ivalue;
        _elem->append("!--");
        continue;
      }
      return TOK_COMMENT;
    }
    else
      _elem->append(c1);
  }
}

//  virtual 
short
XMLTokenizer::_readText()
{
  char c;
  int ivalue = 0;
  StringBuffer buf(10);
  _elem->reset();
  while (true) {
    ivalue = _read();
    // ivalue = _in->read();
    c = (char)ivalue;
    if (ivalue == -1)
      return TOK_EOF;
    else if (c == '<') {
      _pushedBack = ivalue;
      // _in->unread(ivalue);
      return TOK_TEXT;
    }
    else if (c == '&') {
      buf.reset();
      while (c != ';') {
        buf.append(c);
        ivalue = _read();
        // ivalue = _in->read();
        c = (char)ivalue;
        if (ivalue == -1)
          return TOK_EOF;
      }
      RString str = buf.toString();
      if (str->compareTo("&lt") == 0)
        _elem->append('<');
      else if (str->compareTo("&gt") == 0)
        _elem->append('>');
      else if (str->compareTo("&amp") == 0)
        _elem->append('&');
      else if (str->compareTo("&apos") == 0)
        _elem->append('\'');
      else if (str->compareTo("&quot") == 0)
        _elem->append('"');
    }
    else
      _elem->append(c);
  }
}


} // namespace xml
} // namespace acdk

