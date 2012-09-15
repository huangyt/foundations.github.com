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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/StreamTokenizer.h,v 1.26 2005/04/09 19:26:45 kommer Exp $

#ifndef acdk_io_StreamTokenizer_h
#define acdk_io_StreamTokenizer_h

#include "PushbackCharReader.h"
#include "Storage.h"
#include "LineNumberCharReader.h"
#include <acdk/lang/dmi/DmiObject.h>
#include <acdk/lang/Number.h>

namespace acdk {
namespace io {

using namespace acdk::lang;

/**
  internal  structure to hold text position
*/
struct ACDK_CORE_PUBLIC CharStreamPos
{
  int charPos;
  int linePos;
  int columnPos;
  CharStreamPos() : charPos(0), linePos(0),  columnPos(0) {}
  CharStreamPos(int chp, int lp, int clp) : charPos(chp), linePos(lp),  columnPos(clp) {}
};


ACDK_DECL_CLASS(StreamTokenizer);

/**
  Parses a character stream to token.

  API: ACDK<br/>
  @author Roger Rene Kommer (mailto:kommer@artefaktur.com)
  @version $Revision: 1.26 $
  @date $Date: 2005/04/09 19:26:45 $
*/
class ACDK_CORE_PUBLIC StreamTokenizer
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(StreamTokenizer)
public:
  static const int TT_EOF;
  static const int TT_EOL;
  static const int TT_NUMBER;
  static const int TT_WORD;

  /** a quoted String, following the c/java notation */
  static const int TT_STRING;
  /** a quoted character in the c/java notation */
  static const int TT_QCHAR;
  /** a comment, following the C-Notation. 
      By default this token will not be returned. Use wantComments() to tongle this value
  */
  static const int TT_CCOMMENT;
  /** a comment, beginning with 2 slashes until the end of line.
    By default this token will not be returned. Use wantComments() to tongle this value
  */
  static const int TT_CXXCOMMENT;
  /** a sequense of whitespaces. 
      Readed whitespace string can be found in sval.
      Only returns this token, if set with 
      wantWhiteSpace(bool b);
  */
  static const int TT_WS;
  static const int TT_OPERATOR;
//% ACDK_BEGIN_FIELDS
public:
  RNumber nval;
  RString sval;
  uc2char cval;
  int ttype;
protected:
  RPushbackCharReader _in;
  RLineNumberCharReader _lineReader;
  /**
    Tokenizer should return white spaces
  */
  bool _wantWS;
  /**
    return parsed comments (C/C++-Comments) or just throw it away
    sval contains the comment text without the comment chars
  */
  bool _wantComments;
  /** if true, a new line character will return seperated from WS, otherwise, 
      it will return embedded in whitespaces. */
  bool _wantNL;
  
  /**
    if true read ::acdk::lang::String 
    as identifier
  */
  bool _readCxxIdentifier;
  /**
    parse standard operator and return TT_OPERATOR
    if not set, just return the standard character
  */
  bool _parseOperator;
  /** true if unlying stream is return -1 */
  
  bool _eof;
  /**
    read numbers as strings, not as number
  */
  bool _readNumberAsString;
  /**
    parse C/C++ comments
  */
  bool _parseCCComments;
public:
  StreamTokenizer(IN(RCharReader) reader);
  virtual ~StreamTokenizer();
  virtual void commentChar(int ch);
  virtual void eolIsSignificant(bool flag);
  virtual int lineno();
  virtual void lowerCaseMode(bool fl);
  virtual int nextToken();
  virtual void ordinaryChar(int ch);
  virtual void ordinaryChars(int low, int hi);
  virtual void parseNumbers();
  virtual void pushBack();
  virtual void quoteChar(int ch);
  virtual void resetSyntax();
  
  /** returns current token for logging */
  virtual RString toString();
  /** returns current token as original code */
  RString toCode();
  virtual void whitespaceChars(int low, int hi);
  virtual void wordChars(int low, int hi);
  virtual RString getDeviceName();

  /** reading the rest of current line, whithout parsing it. 
    API: extended
  */
  virtual void skipLine();

  bool wantWhiteSpace(bool b)
  {
    bool oldwws = _wantWS;
    _wantWS = b;
    return oldwws;
  }
  bool wantNewline(bool b)
  {
    bool oldwws = _wantNL;
    _wantNL = b;
    return oldwws;
  }
  /**
    want parsed C/C++ comments return as token or discarge
    has no effect if parseCCComments() is false
  */
  bool wantComments(bool b)
  {
    bool oldwc = _wantComments;
    _wantComments = b;
    return oldwc;
  }
  /**
    should C/C++ comments parse
  */
  void parseCCComments(bool b) { _parseCCComments = b; }
  bool parseCCComments() { return _parseCCComments; }
  void readCxxIdentifier(bool b) { _readCxxIdentifier = b; }
  bool readCxxIdentifier() { return _readCxxIdentifier; }
  void readOperator(bool b) { _parseOperator = b; }
  bool readOperator() { return _parseOperator; }
  bool readNumberAsString() { return _readNumberAsString; }
  void readNumberAsString(bool b) { _readNumberAsString = b; }
  /** 
    API: Extended */
  int read();
  void unread(int c);
  void unread(IN(RString) str);
  void pushBack(int typ, IN(RString) stringval);
  bool eof() { return _eof || ttype == TT_EOF; }
  
  /**
    For Debugging output returns the current positition of 
    the stream.
    <FileNameWithPath>:<lineNumber>,<xPos>
  */
  RString getStreamPos(bool withXPos = true);
  foreign CharStreamPos getCharStreamPos();
  void getCharStreamPos(OUT(int) charPos, OUT(int) linePos, OUT(int) columnPos)
  {
    CharStreamPos csp = getCharStreamPos();
    charPos = csp.charPos;
    linePos = csp.linePos;
    columnPos = csp.columnPos;
  }
  RString lastReaded();
  /**
    return the current token as ScriptVar
    In case of number values return as smallest possible type
  */
  acdk::lang::dmi::RDmiObject getCurrentToken() { return new acdk::lang::dmi::DmiObject(getCurrentSvToken()); }
  foreign acdk::lang::dmi::ScriptVar getCurrentSvToken();
  static foreign RString toCode(int tk, IN(acdk::lang::dmi::ScriptVar) sv);
  /**
    maps a escaped character to the corresponding decoded character following C enconding
    '\n' becomes new line
    '\t' a tab space
    '\x' x
  */
  static int mapCEscapedChar(int nc);

protected:
  
  RString _readCxxComment();
  RString _readCComment();
  RString _readCString();
  /** read a quoted character */
  bool _readCChar();

  RString _readUnescapedCString();
  RString _readWhiteSpaces();
  bool _isWhiteSpace(int c);
  bool  _readNumber();
  bool  _readIdentifier();
  
};  


} // io
} // acdk


#endif //acdk_io_StreamTokenizer_h

