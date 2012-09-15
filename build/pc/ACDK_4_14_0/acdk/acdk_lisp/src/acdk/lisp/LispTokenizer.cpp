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
// $Header: /cvsroot/acdk/acdk/acdk_lisp/src/acdk/lisp/LispTokenizer.cpp,v 1.14 2005/03/08 18:54:12 kommer Exp $


#include <acdk.h>
#include <acdk/lang/System.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Integer.h>
#include <acdk/io/File.h>

#include "LispTokenizer.h"

USING_CLASS(::acdk::io::, StreamTokenizer);

namespace acdk {
namespace lisp {

using namespace acdk::lang;

LispTokenizer::LispTokenizer(IN(::acdk::io::RCharReader) in)
: StreamTokenizer(in),
  _currentLine(new StringBuffer()),
  _wss("")
{
}

bool
LispTokenizer::isLispIdentifier(char c)
{
  if (Character::isWhitespace(c) == false &&
      c != ';' &&
      c != '(' &&
      c != ')' 
      )
    return true;
  return false;
}

bool
LispTokenizer::_readIdentifier()
{
  //bool isnegativ = false;
  RStringBuffer sb = new StringBuffer();
  int c = 0;
  do {
    c = read();
    if (c == -1) {
      sval = sb->toString();
      _eof = true;
      return sb->length() != 0;
    }
    if (isLispIdentifier(c) == true) 
      sb->append(char(c));
    else {
      unread(c);
      break;
    }
  } while (c != -1);
  sval = sb->toString();
  return true;
}

RString
LispTokenizer::_readLispComment()
{
  StringBuffer sb(10);
  int c = 0;
  int nc = 0;
  try {
    while ((c = read()) != -1) {
      switch (c) {
      case '\"' :
        sb.append(_readUnescapedCString());
        break;
      case '|' :
        nc = c;
        do {
          sb.append(char(nc));
          nc = read();
        } while (nc == '|');
        sb.append(char(nc));
        if (nc == '#')
          return sb.toString();
        break;
      default:
        sb.append(char(c));
        break;
      }
    }
    if (c == -1)
      _eof = true;
  } catch (::acdk::io::RIOException) {
  }
  return sb.toString();
}



//virtual 
int 
LispTokenizer::nextToken()
{
  int tok = _nextToken();
  if (tok == TT_EOF) // ##### is this okay here????
    return tok;
  int idx = 0;
  if (_wantWS == false) {
    idx = _wss->indexOf('\n');
    if (idx != -1)
      _currentLine = new StringBuffer(_wss->substr(idx)); // StringBuffer::asign(RString), oder aehnliches Fehlt!
  }
  idx = sval->indexOf('\n');
  if (idx != -1)
    _currentLine = new StringBuffer(sval->substr(idx));
  else
    _currentLine->append(sval);
  if (tok == StreamTokenizer::TT_EOF)  // ##### this is meanless!!!!
    _eof = true;
  //System::err->print(sval)->flush();
  return tok;
}


int 
LispTokenizer::_nextToken()
{
  sval = "";
  ttype = 0;
  if (_eof == true) {
    //_eof = false; // next nextToken, the underlying stream will throw EOFException
    return ttype = TT_EOF;
  }
  _wss = _readWhiteSpaces();
  if (_wantWS == true && _wss != Nil && _wss->length() > 0) {
    sval = _wss;
    return ttype = TT_WS;
  } else if (_eof == true) {
    _eof = false; 
    return ttype = TT_EOF;
  }
  if (ttype == TT_EOF || _eof == true)
    return TT_EOF;
  int c = read();
  if (c == -1)
    return c;
  int sc = 0;
  if (c == ';') {
    //sval = RString("//") + _readCxxComment();
    sval = RString(";") + _readCxxComment();
    if (_wantComments == true)
      return ttype = TT_CXXCOMMENT;
    return nextToken();
  } 
  if (c == '#') {
    sc = read();
    if (sc == '|') {
      sval = RString("#|") + _readLispComment();
      if (_wantComments == true)
        return ttype = TT_CCOMMENT;
      return nextToken();
    } else
      unread(sc);
    return ttype = c;
  }
  if (c == '\'' || c == ')' || c == '(' || c == '`')
    return ttype = c;
  if (c == ',') 
  {
    sc = read();
    if (sc == '@')
      return '@';
    unread(sc);
    return ',';

  }
  if (c == '-' || c == '+' || Character::isDigit((uc2char)c) == true) {
    unread(c);
    if (_readNumber() == true)
      return ttype = TT_NUMBER;
    c = read();
  }
  if (c == '\n' || c == '\r') {
    char cbuf[3]; cbuf[0] = char(c); cbuf[1] = 0; cbuf[2] = 0;
    if (c == '\r') {
      c = read();
      if (c == '\n')
        cbuf[1] = c;
      else
        unread(c);
    }
    if (_wantNL == true)  {
      sval = cbuf;
      return ttype = TT_EOL;
    }
  }
  if (c == '"') {
    sval = _readCString();
    return ttype = TT_STRING;
  }
  unread(c);
  if (_readIdentifier() == true) {
      return ttype = TT_WORD;
    return ttype = read();
  }
  char cbuf[2]; cbuf[0] = char(c); cbuf[1] = 0;
  sval = cbuf;
  return ttype = c;

}

RString 
LispTokenizer::currentLineReference()
{
  return getDeviceName() + "(" + Integer::toString(lineno()) + ")";
}

/// returns the the current parsing Line
RString 
LispTokenizer::currentLine()
{
  return _currentLine->toString();
}

} // namespace lisp
} // namespace acdk


