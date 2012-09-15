// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 1999-2000 by Roger Rene Kommer, artefaktur
// Projekt: ACDK
// 
// $Header: /cvsroot/acdk/acdk/acdk_doc_urlify/src/acdk/doc/urlify/HTMLStreamTokenizer.cpp,v 1.5 2005/03/09 20:04:24 kommer Exp $
//
// $Log: HTMLStreamTokenizer.cpp,v $
// Revision 1.5  2005/03/09 20:04:24  kommer
// typo
//
// Revision 1.4  2003/06/19 14:37:18  kommer
// source comment header ajusted
//
// Revision 1.3  2003/06/19 13:17:24  kommer
// merged acdk-3-unicode into MAIN
//
// Revision 1.2.2.1  2003/05/13 13:47:23  kommer
// panta rei
//
// Revision 1.2  2001/09/02 10:06:06  kommer
// panta rei
//
// Revision 1.1  2001/03/03 20:11:32  kommer
// panta rei
//
// Revision 1.1.1.1  2000/11/23 09:53:26  roger
// initial release
//
// Revision 1.1.1.1  2000/11/22 13:35:20  roger
// initial acdk sources
//
// Revision 1.2  2000/06/03 18:48:32  roger
// panta rei
//
// Revision 1.1  2000/04/25 11:57:29  roger
// initial revision
//
// Revision 1.4  2000/02/08 16:29:38  roger
// RefHolder and Arrays changed
//
// Revision 1.3  1999/10/24 11:57:02  roger
// panta rei
//
// Revision 1.2  1999/10/23 15:28:16  roger
// panta rei
//
// Revision 1.1  1999/10/22 19:04:07  roger
// initial revision
//


#include <acdk.h>
#include <acdk/lang/Character.h>

#include "HTMLStreamTokenizer.h"

namespace acdk {
namespace io {

using namespace acdk::lang;

//static 
const int HTMLStreamTokenizer::TT_HTMLCOMMENT = -100;
const int HTMLStreamTokenizer::TT_TAG_UNKNOWN = -101;
const int HTMLStreamTokenizer::TT_TAG_UNKNOWN_END = -102;
const int HTMLStreamTokenizer::TT_TAG_COMMENT = -103;
const int HTMLStreamTokenizer::TT_TAG_COMMENT_END = -104;
const int HTMLStreamTokenizer::TT_TAG_A       = -105;
const int HTMLStreamTokenizer::TT_TAG_A_END       = -106;
const int HTMLStreamTokenizer::TT_TAG_H       = -107;
const int HTMLStreamTokenizer::TT_TAG_H_END       = -108;

//virtual 
int 
HTMLStreamTokenizer::_readTag()
{
  int c;
  int tk = StreamTokenizer::nextToken();
  bool isEnd = false;
  int tagttype = isEnd ? TT_TAG_UNKNOWN_END : TT_TAG_UNKNOWN;  
  if (tk == '!') {
    tagttype = TT_TAG_COMMENT;
  } else {
    if (tk == '/') {
      isEnd = true;
      tk = StreamTokenizer::nextToken();
    }
    if (tk == '>') {
      sval = (isEnd ? "</" : "<") + sval;
      return ttype = tagttype;
    }
  }
  if (sval->indexOf("a") != -1) 
    tagttype = isEnd ? TT_TAG_A_END : TT_TAG_A;
  if (sval->indexOf("h") == 0) 
    tagttype = isEnd ? TT_TAG_H_END : TT_TAG_H;

  RStringBuffer sb = new StringBuffer((isEnd ? "</" : "<")  + sval);
  while ((c = StreamTokenizer::nextToken()) != TT_EOF) {
    if (c == TT_STRING) {
      sb->append("\"" + sval+ "\"");
    } else
      sb->append(sval);
    if (c == '>')  {
      sval = sb->toString();
      break;
    }
  }
  return ttype = tagttype;
}

//virtual 
int 
HTMLStreamTokenizer::nextToken()
{
  if (_eof == true) {
    _eof = false; // next nextToken, the underlying stream will throw EOFException
    return ttype = TT_EOF;
  }
  RString wss = _readWhiteSpaces();
  if (_wantWS == true && wss != Nil && wss->length() > 0) {
    sval = wss;
    return ttype = TT_WS;
  } else if (_eof == true) {
    _eof = false; 
    return ttype = TT_EOF;
  }
  if (ttype == TT_EOF)
    return TT_EOF;
  int c = _in->readChar();
  if (c == -1)
    return c;
  if (c == '"') {
    sval = _readCString();
    return ttype = TT_STRING;
  }
  if (Character::isUnicodeIdentifierStart((uc2char)c) == true) {
    unread(c);
    if (_readIdentifier() == true)
      return ttype = TT_WORD;
    return ttype = c;
  }
  if (c == '<') 
    return _readTag();
  char cbuf[2]; cbuf[0] = char(c); cbuf[1] = 0;
  sval = SCS(cbuf);
  return ttype = c;
}

} //namespace io 
} //namespace acdk 
