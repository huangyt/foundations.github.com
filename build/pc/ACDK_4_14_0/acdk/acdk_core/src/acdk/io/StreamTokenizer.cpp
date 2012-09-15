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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/io/StreamTokenizer.cpp,v 1.42 2005/04/15 12:39:35 kommer Exp $




#include <acdk.h>
#include "StreamTokenizer.h"
//#include "LineNumberReader.h"
#include "EOFException.h"

#include <acdk/lang/System.h>
#include <acdk/lang/StringBuffer.h>
#include <acdk/lang/Character.h>
#include <acdk/lang/Byte.h>
#include <acdk/lang/Short.h>
#include <acdk/lang/Integer.h>
#include <acdk/lang/Long.h>
#include <acdk/lang/Float.h>
#include <acdk/lang/Double.h>

#include <acdk/lang/UnsupportedOperationException.h>
#include <acdk/lang/NumberFormatException.h>
#include <acdk/locale/Encoding.h>

namespace acdk {
namespace io {

//#define LOCAL_DEBUG

#if defined(LOCAL_DEBUG)
#define DOUT(strexpr) \
do { \
  StringBuffer sb; \
  sb << strexpr; \
  System::out->println(sb.toString()); \
} while (false)
#else
#define DOUT(strexpr) do { } while(false)
#endif


const int StreamTokenizer::TT_EOF =                     -1;
const int StreamTokenizer::TT_EOL =                     '\n';
const int StreamTokenizer::TT_NUMBER =                  -2;
const int StreamTokenizer::TT_WORD =                    -3;
const int StreamTokenizer::TT_STRING =                  -4;
const int StreamTokenizer::TT_CCOMMENT =                -5;
const int StreamTokenizer::TT_CXXCOMMENT =              -6;
const int StreamTokenizer::TT_WS =                      -7;
const int StreamTokenizer::TT_QCHAR =                   -8;
const int StreamTokenizer::TT_OPERATOR  =               -9;

#if defined(__BORLANDC__)
enum TToken
{
  tt_eof = -1,
#define TT_EOF tt_eof
  tt_eol = '\n',
#define TT_EOL tt_eol
  tt_number = -2,
#define TT_NUMBER tt_number
  tt_word = -3,
#define TT_WORD tt_word
  tt_string = -4,
#define TT_STRING tt_string
  tt_ccomment = -5,
#define TT_CCOMMENT tt_ccomment
  tt_cxxcomment = -6,
#define TT_CXXCOMMENT tt_cxxcomment
  tt_ws = -7,
#define TT_WS tt_ws
  tt_qchar = -8,
#define TT_QCHAR tt_qchar
  tt_operator = -9
#define TT_OPERATOR tt_operator
};
#endif // __BORLANDC__

StreamTokenizer::StreamTokenizer(IN(RCharReader) reader)
: Object(),
  nval(),
  sval(),
  ttype(0),
  _in(),
  _lineReader(Nil),
  _wantWS(false),
  _wantComments(false),
  _wantNL(false),
  _readCxxIdentifier(true),
  _parseOperator(false),
  _eof(false)
, _readNumberAsString(false)
, _parseCCComments(true)
{
  if (instanceof(reader, LineNumberCharReader) == false)
    _lineReader = new LineNumberCharReaderImpl(reader);
  else
    _lineReader = RLineNumberCharReader(reader);
  if (instanceof(_lineReader, PushbackCharReader) == false)
    _in = new PushbackCharReaderImpl((RCharReader) _lineReader, 4096);
  else
    _in = RPushbackCharReader(_lineReader);
}



//virtual
StreamTokenizer::~StreamTokenizer()
{

}

int
StreamTokenizer::read()
{
  int c = _in->readChar();
  DOUT("StreamTokenizer::read: [" << (ucchar)c << "]");
  return c;
}
void
StreamTokenizer::unread(int c)
{
  DOUT("StreamTokenizer::unread: [" << (ucchar)c << "]");
  _in->unread(c);
}

void
StreamTokenizer::unread(IN(RString) str)
{
  DOUT("StreamTokenizer::unread: [" << str << "]");
  _in->unread(str);
}

//virtual
void
StreamTokenizer::commentChar(int ch)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual
void
StreamTokenizer::eolIsSignificant(bool flag)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual
int
StreamTokenizer::lineno()
{
  return _lineReader->getLineNumber();
}

//virtual
void
StreamTokenizer::lowerCaseMode(bool fl)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

void
StreamTokenizer::skipLine()
{
  (void)_lineReader->readLine();
}

RString
StreamTokenizer::_readCxxComment()
{
  
  StringBuffer str(10);
  int c = 0;
  try {
    while ((c = read()) != -1) {
      str.append(ucchar(c));
      if (c == '\n')  {
        unread(c);
        break;
      }
    }
  } catch (RIOException ) {
  }
  if (c == -1)
    _eof = true;
  return str.toString();
}

//static
int 
StreamTokenizer::mapCEscapedChar(int nc)
{
  switch (nc) 
  {
  case 't': return '\t';
  case 'v': return '\v';
  case 'r': return '\r';
  case 'n': return '\n';
  case 'b': return '\b';
  case 'a': return '\a';
  case 'f': return '\f';
  case '0': return '\0';
  default: return nc;
  }
}

#ifndef DOXYGENONLY

/// @internal
uc2char
StreamTokenizer_readEscapedUnicodeChar(StreamTokenizer& This)
{
  char tbuf[7];
  tbuf[0] = '\\';
  tbuf[1] = 'u';
  int nc = 0;
  for (int i = 2; i < 6; ++i)
    tbuf[i] = This.read();
  tbuf[6] = 0;
  uc2char uc = String::decodeAscUnicodeCharacter(tbuf);
  return uc;
}
#endif //#ifndef DOXYGENONLY

RString
StreamTokenizer::_readCString()
{
  StringBuffer sb;
  int c = 0;
  int nc = 0;
  while (true)
  {
    c = read();
    switch (c) {
    case -1 :
      //ttype = TT_EOF;
       THROW1(Exception, "Syntax errro: Unescaped C-String: " + sb.toString());
      return sb.toString();
    case '\\':
      nc = read();
      if (nc == 'u')
      {
        uc2char uc = StreamTokenizer_readEscapedUnicodeChar(*this);
        sb.append(uc);
      }
      else
        sb.append((ucchar)mapCEscapedChar(nc));
      break;
    case '\"':
      return sb.toString();
    default:
      sb.append(ucchar(c));
      break;
    }
  }
  return sb.toString();
}

RString
StreamTokenizer::_readUnescapedCString()
{
  StringBuffer sb;
  int c = 0;
  while ((c = read()) != -1) {
    switch(c) {
    case '\\' :
      sb.append(ucchar(c))->append((ucchar)read());
      break;
    case '\"' :
      sb.append(ucchar(c));
      return sb.toString();
    }
  }
  if (c == -1)
     ttype = TT_EOF;
  return sb.toString();
}

RString
StreamTokenizer::_readCComment()
{
  StringBuffer sb(10);
  int c = 0;
  int nc = 0;
  while (true)
  {
     c = read();
     switch (c) {
     case -1:
        //_eof = true;
        THROW1(Exception, "Unterminated C-Comment" + sb.toString());
        //return sb.toString();
     case '*' :
        nc = c;
        do {
           sb.append(ucchar(nc));
           nc = read();
        } while (nc == '*');
        sb.append(ucchar(nc));
        if (nc == '/')
           return sb.toString();
        break;
     default:
        sb.append(ucchar(c));
        break;
     }
  }
  return sb.toString();
}


bool
StreamTokenizer::_isWhiteSpace(int c)
{
  return UnicodeCharacter::isWhitespace(c) || UnicodeCharacter::isControl(c);
}

RString
StreamTokenizer::_readWhiteSpaces()
{
  int c;
  StringBuffer sb;

  do
  {
    c = read();
    if (c == -1) {
      if (sb.length() == 0) {
        ttype = TT_EOF;
        return "";
      } else {
        _eof = true;
        return sb.toString();
      }
    }
    if (_isWhiteSpace(c) == false ||
        ((c == '\r' || c == '\n') && _wantNL == true))
    {
      unread(c);
      return sb.toString();
    }
    sb.append(ucchar(c));
  } while(true);

  return sb.toString();
}

#if !defined(DOXYGENONLY)
/// @internal
bool isFloatSuffix(int c)
{
  return c == 'f' || 
         c == 'F' ||
         c == 'd' ||
         c == 'D';
}

/// @internal
bool isIntSuffix(int c)
{
  return c == 'b' ||
         c == 'B' ||
         c == 's' ||
         c == 'S' ||
         c == 'i' ||
         c == 'I' ||
         c == 'l' ||
         c == 'L';
}

/// @internal
bool isTypeSuffix(int c)
{
  return isFloatSuffix(c) || isIntSuffix(c);
}

/// @internal
bool isStartOfNum(StringBuffer& sb, bool prefixReaded)
{
  return (sb.length() == 0) || (prefixReaded == true && sb.length() == 1);
}

/// @internal
char getNumberCharAt(StringBuffer& sb, bool prefixReaded, int idx)
{
  if (sb.length() == 0)
    return 0;
  if (prefixReaded == true)
    idx += 1;
  if (sb.length() <= idx)
    return 0;
  return sb.charAt(idx);
}

#endif //!defined(DOXYGENONLY)

bool
StreamTokenizer::_readNumber()
{
  //bool isnegativ = false;
  StringBuffer sb;
  bool isfloating = false;
  bool prefixReaded = false;
  bool nullPrefix = false;
  bool hexPrefix = false;
  bool hasexponent = false;
  bool hasTypeSuffix = false;
  bool numberReaded = false;
  int c = 0;
  do {
    c = read();
    if (c == -1) {
      if (sb.length() > 0) {
        _eof = true;
        break;
      } else {
        ttype = TT_EOF;
        return false;
      }
    }
    if ((c == '-' || c == '+') && _parseOperator == true)
      break;

    if ((c == 'x' || c == 'X') && (getNumberCharAt(sb, prefixReaded, 0) == '0'))
    {
      hexPrefix = true;
      sb.append((char)c);
      continue;
    }
    if (hexPrefix == false && c == 'e' && hasexponent == false)
    {
      hasexponent = true;
      isfloating = true;
      sb.append((char)c);
      continue;
    }
    if (c == '-' || c == '+' || (hexPrefix == true ?  Character::isHexDigit((ucchar)c) == true : Character::isDigit((ucchar)c) == true))
    {
      if (c == '0' && isStartOfNum(sb, prefixReaded))
        nullPrefix = true;
      
      if (c == '-' || c == '+')
      {
        if (prefixReaded == true)
        {
          _in->unread(c);
          break;
        }
        else
          prefixReaded = true;
      }
      else
        numberReaded = true;
      sb.append(char(c));
    }
    else if (c == '.' && isfloating == false) 
    {
      isfloating = true;
      sb.append(char(c));
    } 
    else if ((isfloating == true && isFloatSuffix(c) == true) ||
             (isfloating == false && isIntSuffix(c) == true))
    {
      if (numberReaded == false)
      {
        // sample: "-i"
        _in->unread(c);
        unread(sb.toString());
        return false;
      }
      sb.append((char)c);
      continue;
    } 
    else 
    {
      unread(c);
      break;
    }
  } while (true);

  if (numberReaded == false)
  {
     unread(sb.toString());
     return false;
  }
  if ((sb.length() == 1) && ((sb.charAt(0) == '-') || (sb.charAt(0) == '+'))) 
  {
    unread(sb.charAt(0));
    return false;
  }
  if (isfloating == true && sb.charAt(sb.length() - 1) == '.')
  {
    unread(sb.charAt(sb.length() - 1));
    sb.set(sb.toString()->substr(0, sb.length() - 1));
    isfloating = false;
  }
  
  try 
  {
    nval = Number::decodeToNumber(sb.toString(), true);
    if (nval == Nil)
    {
      unread(sb.toString());
      return false;
    }
    sval = sb.toString();
    return true;
  } catch (RNumberFormatException ) {
    unread(sb.toString());
  }
  return false;
}

bool
StreamTokenizer::_readIdentifier()
{
  //bool isnegativ = false;
  StringBuffer sb;
  int c = 0;
  do {
    c = read();
    if (c == -1) {
      if (sb.length() > 0) {
        _eof = true;
        break;
      } else {
        ttype = TT_EOF;
        return false;
      }
    }
    if (Character::isUnicodeIdentifierPart((ucchar)c) == true ||
        (readCxxIdentifier() && c == ':'))
      sb.append(ucchar(c));
    else {
      unread(c);
      break;
    }
  } while (true);
  RString tstr = sb.toString();
  if (tstr->length() == 1 && tstr->charAt(0) == ':') {
    unread(tstr);
    return false;
  }
  sval = tstr;
  return true;
}

bool
StreamTokenizer::_readCChar()
{
  int c = read();
  if (c == -1) {
    ttype = TT_EOF;
    return false;
  }
  StringBuffer sb("");
  
  int retc = c;
  if (c == '\\') 
  {
    c = read();  
    if (c == 'u')
      retc = StreamTokenizer_readEscapedUnicodeChar(*this);
    else
      retc = mapCEscapedChar(c);
    sb.append((ucchar)retc);
  }
  else
    sb.append((ucchar)c);

  int nlc = read();
  if (nlc != '\'') {
    unread(sb.toString()->substr(1));
    return false;
  }
  //sb.append((ucchar)nlc);
  sval = sb.toString();
  cval = retc;
  return true;
}


#if defined(LOCAL_DEBUG)
# define DUMP_READED() do { System::out->print(wss + sval); System::out->flush(); } while (false)
#else
# define DUMP_READED() do { } while (false)
#endif

namespace {
char st_operator[] =
{
   '+', '-', '*', '/', '.',
   '!', '~', '&', '|', '=',
   '<', '>', '[',
   0
};
bool isOperator(uc2char ch)
{
  for (int i = 0; st_operator[i] != 0; ++i)
    if (st_operator[i] == ch)
      return true;
  return false;
}

} // anon namespace

//virtual
int
StreamTokenizer::nextToken()
{
read_nextToken:
  if (_eof == true) {
    _eof = false; // next nextToken, the underlying stream will throw EOFException
    return ttype = TT_EOF;
  }
  try {
    RString wss = _readWhiteSpaces();
    if (_wantWS == true && wss != Nil && wss->length() > 0) 
    {
      DUMP_READED();
      sval = wss;
      return ttype = TT_WS;
    } 
    else if (_eof == true) 
    {
      _eof = false;
      return ttype = TT_EOF;
    }
    if (ttype == TT_EOF)
      return TT_EOF;
    int c = _in->readChar();
    if (c == -1)
      return ttype = TT_EOF;
    int sc = 0;
    if (c == '/' && _parseCCComments == true)
    {
      sc = _in->readChar();
      if (sc == -1) 
      {
        _eof = true;
      } 
      else if (sc == '/') 
      {
        sval = RString("//") + _readCxxComment();
        if (_wantComments == true) 
        {
          DUMP_READED();
          if (ttype == TT_EOF)
             _eof = true;
          return ttype = TT_CXXCOMMENT;
        } 
        else if (ttype == TT_EOF)
           return ttype;
	goto read_nextToken;
        //return nextToken();
      } 
      else if (sc == '*') 
      {
        sval = RString("/*") + _readCComment();
        if (_wantComments == true) {
          DUMP_READED();
          if (ttype == TT_EOF)
             _eof = true;
          return ttype = TT_CCOMMENT;
        }
	goto read_nextToken;
        //return nextToken();
      } else
        unread(sc);
      if (_parseOperator == false)
      {
        DUMP_READED();
        return ttype = c;
      }
    }
    if (_readNumberAsString == false)
    {
      if (((c == '-' || c == '+') && _parseOperator == false) || Character::isDigit((ucchar)c) == true)
      {
        unread(c);
        if (_readNumber() == true) {
          DUMP_READED();
          return ttype = TT_NUMBER;
        }
        if (_parseOperator == false)
        {
          DUMP_READED();
          return ttype = read(); // we've just unread this char.
        }
        else
        {
          c = read();
        }
      }
    }
    if (c == '\n' || c == '\r')
    {
      if (_wantNL == true)
      {
        char cbuf[3]; cbuf[0] = ucchar(c); cbuf[1] = 0; cbuf[2] = 0;
        if (c == '\r') {
          c = read();
          if (c == '\n')
            cbuf[1] = c;
          else if (c == -1)
            _eof = true;
          else
            unread(c);
        }
        sval = new String(cbuf, NormalSST | CCAscii);
        DUMP_READED();
        return ttype = TT_EOL;
      }
    }
    if (c == '\'')
    {
      if (_readCChar() == true) {
        DUMP_READED();
        if (ttype == TT_EOF)
         THROW1(Exception, "Char [" + sval + "] not terminated with quote");
        return (ttype = TT_QCHAR);
      }
      DUMP_READED();
      return c;
    }
    if (c == '"')
    {
      sval = _readCString();
      if (ttype == TT_EOF) // not
        THROW1(Exception, "String [" + sval + "] not terminated with quote");
      DUMP_READED();
      return ttype = TT_STRING;
    }
    if (Character::isUnicodeIdentifierStart((ucchar)c) == true
        || (readCxxIdentifier() && c == ':'))
    {
      unread(c);
      if (_readIdentifier() == true) {
        DUMP_READED();
        return ttype = TT_WORD;
      }
      DUMP_READED();
      return ttype = read();
    }
    if (_parseOperator == true)
    {
      if (isOperator(c) == true)
      {
        StringBuffer sb;
        sb.append((uc2char)c);
        do {
          c = read();
          if (c == -1)
          {
            _eof = true;
            break;
          }
          if (isOperator(c) == false)
          {
            unread(c);
            break;
          }
          sb.append((uc2char)c);
        }while (true);
        sval = sb.toString();
        return ttype = TT_OPERATOR;
      }
    }
    if (c < 0x80)
    {
      char cbuf[2]; cbuf[0] = char(c); cbuf[1] = 0;
      sval = new String(cbuf, NormalSST | CCAscii);
    }
    else
    {
      ucchar cbuf[2]; cbuf[0] = ucchar(c); cbuf[1] = 0;
      sval = new String(cbuf, NormalSST | CCUcs2);
    }
    DUMP_READED();
    return ttype = c;
  } catch (REOFException) {
    return TT_EOF;
  }
}

//virtual
void
StreamTokenizer::ordinaryChar(int ch)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual
void
StreamTokenizer::ordinaryChars(int low, int hi)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual
void
StreamTokenizer::parseNumbers()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual
void
StreamTokenizer::pushBack()
{
  pushBack(ttype, sval);
}

void
StreamTokenizer::pushBack(int type, IN(RString) stringval)
{
  if (type == TT_STRING)
  {
    unread("\"" + acdk::locale::Encoding::getUnicodeCEscapeEncoding()->getEncoder()->encode(stringval) + "\"");  
  } else if (type == TT_CXXCOMMENT) {
    unread("// " + stringval + "\n");
  } else if (type == TT_CCOMMENT) {
    unread("/** " + stringval + "*/");
  } else if (type == TT_WORD ||
            type == TT_NUMBER ||
            type == TT_WS ||
            type == TT_OPERATOR ||
            type == TT_QCHAR)
    unread(stringval);
  else
    unread(type);
}

//virtual
void
StreamTokenizer::quoteChar(int ch)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual
void
StreamTokenizer::resetSyntax()
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}


//virtual
RString
StreamTokenizer::toString()
{
  
  if (ttype == TT_EOF)
    return "EOF";
  else if (ttype == TT_EOL)
    return "EOL";
  else if (ttype == TT_NUMBER)
    return "Number: [" + nval->toString() + "]";
  else if (ttype == TT_WORD)
    return RString("Word: [") + sval + "]";
  else if (ttype == TT_STRING)
    return RString("String: [") + sval + "]";
  else if (ttype == TT_CCOMMENT)
    return RString("CComment: [") + sval + "]";
  else if (ttype == TT_CXXCOMMENT)
    return RString("CXXComment: [") + sval + "]";
  else if (ttype == TT_WS)
    return RString("WhiteSpace: [") + sval + "]";
  else if (ttype == TT_OPERATOR)
    return RString("Operator: [") + sval + "]";
  else {
    ucchar cbuf[2]; cbuf[0] = ucchar(ttype); cbuf[1] = 0;
    return RString("Unknown: [") + cbuf + "]";
  }
}


RString
StreamTokenizer::toCode()
{
  RString stringval = sval;
  if (ttype == TT_STRING)
  {
    return "\"" + acdk::locale::Encoding::getCEscapeEncoding()->getEncoder()->encode(stringval) + "\"";
    //return "\"" + stringval + "\"";
  } else if (ttype == TT_QCHAR) {
    return SBSTR("'" << stringval << "'");
  } else if (ttype == TT_CXXCOMMENT) {
    return "// " + stringval + "\n";
  } else if (ttype == TT_CCOMMENT) {
    return "/** " + stringval + "*/";
  } else if (ttype == TT_WORD ||
            ttype == TT_NUMBER ||
            ttype == TT_WS ||
            ttype == TT_OPERATOR)
    return stringval;
  else
  {
    if (ttype < 0x80)
    {
      char ch[2];
      ch[0] = (char)ttype; ch[1] = 0;
      return new String(ch, NormalSST | CCAscii);
    }
    else
    {
      ucchar ch[2];
      ch[0] = (ucchar)ttype; ch[1] = 0;
      return new String(ch, NormalSST | CCUcs2);
    }
  }
  return ""; // never reached
}

//static foreign 
RString 
StreamTokenizer::toCode(int tk, IN(acdk::lang::dmi::ScriptVar) sv)
{
  int ttype = tk;
  if (tk == TT_STRING)
    return "\"" + acdk::locale::Encoding::getCEscapeEncoding()->getEncoder()->encode(sv.toString()) + "\"";

  if (ttype == TT_CXXCOMMENT) 
    return "// " + sv.toString() + "\n";
  
  if (ttype == TT_CCOMMENT) 
    return "/** " + sv.toString() + "*/";
  
  if (ttype == TT_WORD ||
            ttype == TT_NUMBER ||
            ttype == TT_WS ||
            ttype == TT_OPERATOR)
    return sv.toString();
  
  if (ttype == TT_QCHAR) 
    return SBSTR("'" << acdk::locale::Encoding::getCEscapeEncoding()->getEncoder()->encode(String::valueOf(sv.getUcCharVar()))  << "'");

  if (ttype < 0x80)
  {
    char ch[2];
    ch[0] = (char)ttype; ch[1] = 0;
    return new String(ch, NormalSST | CCAscii);
  }
  else
  {
    ucchar ch[2];
    ch[0] = (ucchar)ttype; ch[1] = 0;
    return new String(ch, NormalSST | CCUcs2);
  }
}

//virtual
void
StreamTokenizer::whitespaceChars(int low, int hi)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}

//virtual
void
StreamTokenizer::wordChars(int low, int hi)
{
  THROW1(UnsupportedOperationException, "Not implemented yet");
}


//virtual
RString
StreamTokenizer::getDeviceName()
{
  RReader r = _in->getReader();
  if (r == Nil)
    return "";
  return r->getReaderStorage()->getDeviceName();
}

RString
StreamTokenizer::getStreamPos(bool withXPos/* = true*/)
{
  StringBuffer sb(256);
  sb.append(getDeviceName());
  sb.append(":");
  sb.append(_lineReader->getLineNumber());
  if (withXPos == false)
    return sb.toString();
  sb.append(",");
  sb.append(_lineReader->getColumnNumber());
  return sb.toString();
}

CharStreamPos 
StreamTokenizer::getCharStreamPos()
{
  return CharStreamPos(_lineReader->getCharPos(), _lineReader->getLineNumber(), _lineReader->getColumnNumber());
}

RString
StreamTokenizer::lastReaded()
{
  return toString();
}

acdk::lang::dmi::ScriptVar
StreamTokenizer::getCurrentSvToken()
{
  switch(ttype)
  {
  case TT_EOF: return acdk::lang::dmi::ScriptVar();
  case TT_EOL: return inOf(sval);
  case TT_NUMBER:
    return nval->toScriptVar();
  case TT_WORD:
  case TT_STRING:
  case TT_CCOMMENT:
  case TT_CXXCOMMENT:
  case TT_OPERATOR:
  case TT_WS:
    return inOf(sval);
  case TT_QCHAR:
    return inOf(cval);
  default:
    ucchar uc = (ucchar)ttype;
    return inOf(uc);
  }
  return inOf(sval);
}


} // namespace io
} // namespace acdk




