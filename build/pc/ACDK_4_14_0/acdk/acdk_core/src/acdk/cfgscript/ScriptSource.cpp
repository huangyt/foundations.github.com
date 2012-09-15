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


#include "ScriptSource.h"

namespace acdk {
namespace cfgscript {


ScriptSource::ScriptSource(IN(RString) name, IN(RString) sourcetext, IN(acdk::io::CharStreamPos) sp)
: _sourceName(name)
, _in(Nil)
, _buffer(new StringBuffer(sourcetext))
, _charPos(sp.charPos)
, _linePos(sp.linePos)
, _columnPos(sp.columnPos)
, _eof(false)
{
  //_buffer->append(sourcetext->substr(0, _charPos));
  _in = new acdk::io::StringReader("");
  _sourcePositions.push_back(0);
  for (int i = 0; i < sourcetext->length(); ++i)
  {
    if (sourcetext->charAt(i) == '\n' && sourcetext->length() > i)
    {
      _sourcePositions.push_back(i);
    }
  }
}

int
ScriptSource::readChar()
{
  int tk;
  bool readFromBuffer = false;
  if (_charPos < _buffer->length())
  {
    tk = _buffer->charAt(_charPos);
    readFromBuffer = true;
  }
  else
  {
    if (_eof == true)
      return -1;
    tk = _in->readChar();
  }
  if (tk == -1)
  {
    _eof = true;
    return -1;
  }
  if (tk == '\n')
  {
    ++_linePos;
    _columnPos = 0;
    if (readFromBuffer == false)
      _sourcePositions.push_back(_charPos);
  }
  ++_charPos;
  if (readFromBuffer == false)
    _buffer->append((uc2char)tk);
  return tk;
}

bool
ScriptSource::isEof()
{
  if (_charPos < _buffer->length())
    return false;
  return _eof;
}

RString
ScriptSource::readString()
{
  RString buferg;
  if (_charPos < _buffer->length())
  {
    buferg = _buffer->toString()->substr(_charPos);
    _incrementReaded(buferg);
  }
  RString readerg = _in->readString();
  _incrementReaded(readerg);
  _buffer->append(readerg);
  if (buferg != Nil)
    return buferg + readerg;
  return readerg;
}

void
ScriptSource::_incrementReaded(IN(RString) str)
{
  int newlines = str->elementCount('\n');
  if (newlines > 0)
  {
    _linePos += newlines;
    int idx = str->lastIndexOf('\n');
    _columnPos = str->length() - idx;
  }
  else
  {
    _columnPos += str->length();
  }
}

void
ScriptSource::setSourcePos(IN(acdk::io::CharStreamPos) sp)
{
  _charPos = sp.charPos; _linePos = sp.linePos; _columnPos = sp.columnPos;
}

void
ScriptSource::_setCharPos(int newCharPos)
{

  if (_charPos - newCharPos < _columnPos)
  {
    _charPos = newCharPos;
    _columnPos -= (_charPos - newCharPos);
    return;
  }
  _charPos = newCharPos;
  if (_charPos == 0)
  {
    _linePos = 0;
    _columnPos = 0;
    return;
  }
  int slp;
  for (slp = _linePos; slp >= 0 && _sourcePositions[slp] > _charPos; --slp)
  {
  }
  //++slp;
  _linePos = slp;
  _columnPos = _charPos - _sourcePositions[slp];

    // ### TODO check if unreaded char is really ch
  // ### TODO check if _charPos == 0

}

void
ScriptSource::unread(ucchar ch)
{
  _setCharPos(_charPos - 1);
}


void
ScriptSource::unread(IN(RString) str)
{
  ACDK_FQ_SUPER_QUALIFIER(::acdk::io::, PushbackCharReader)::unread(str);
  //already done by PushbackCharReader::unread: _setCharPos(_charPos - str->length());
}

void
ScriptSource::resetPushbackBuffer()
{
  // nothing
}

RString
ScriptSource::readLine()
{
  StringBuffer sb;
  int sicLinePos = _linePos;
  _charPos = _buffer->length();
  while (true)
  {
    int ch = readChar();
    if (ch == -1)
    {
      RString s = sb.toString();
      return s->length() == 0 ? RString(Nil) : s;
    }
    if (_linePos != sicLinePos)
      return sb.toString();
  }
  return Nil;
}


RString
ScriptSource::getLine(int line)
{
  acdk::io::CharStreamPos begin, end;
  if (_getLine(_linePos + line, begin, end) == false)
    return Nil;
  return _buffer->toString()->substr(begin.charPos, end.charPos);
}

int
ScriptSource::seekCharBack()
{
  if (_charPos == 0)
    return -1;
  int ch = _buffer->toString()->charAt(_charPos - 1);
  if (ch == '\n')
  {
    _linePos -= 1;
    _columnPos = _charPos - _sourcePositions[_linePos];

  }
  else
  {
    --_columnPos;
  }
  ++_charPos;
  return _charPos;
}

bool
ScriptSource::_getLine(int lineNo, acdk::io::CharStreamPos& begin, acdk::io::CharStreamPos& end)
{/*
  if (lineNo == _linePos)
  {
    begin = getSourcePos();
    begin.charPos = begin.charPos - begin.columnPos;
    begin.columnPos = 0;
    SourcePos sicpos = getSourcePos();
    end = _seekToEndLine();
    setSourcePos(sicpos);
    return true;
  }*/


  if (_sourcePositions.size() <= lineNo + 1)
  {
    if (_fetchNextLine(lineNo + 1) == false)
      return false;
  }
  if (lineNo < 0)
    return false;
  int prevPos = _sourcePositions[lineNo];
  int nextPos = _buffer->length();
  if (_sourcePositions.size() > lineNo + 1)
    nextPos = _sourcePositions[lineNo + 1];
  begin.charPos = prevPos;
  begin.columnPos = 0;
  begin.linePos = lineNo;
  end.charPos = nextPos;
  end.linePos = lineNo;
  end.columnPos = nextPos - prevPos;
    return true;
}

bool
ScriptSource::_fetchNextLine(int lineNo)
{
  if (isEof() == true)
    return false;
  acdk::io::CharStreamPos sicpos = getSourcePos();
  int sicLinePos = _linePos;
  _charPos = _buffer->length();

  int readed = 0;

  while (true)
  {
    if (isEof() == true)
      return false;
    int ch = readChar();
    if (ch == -1 && readed == 0)
      return false;
    ++readed;
    if (ch == -1)
    {
      setSourcePos(sicpos);
      return true;
    }

    if (_linePos == lineNo)
    {
      setSourcePos(sicpos);
      return true;
    }
  }
  setSourcePos(sicpos);
  return false;
}


} // namespace cfgscript
} // namespace acdk


