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

#ifndef acdk_cfgscript_ScriptSource_h
#define acdk_cfgscript_ScriptSource_h


#include "Props.h"
#include "SourceTokenizer.h"

#include <acdk/io/PushbackCharReader.h>
#include <acdk/io/LineNumberCharReader.h>
#include <acdk/io/StringReader.h>

namespace acdk {
namespace cfgscript {

ACDK_DECL_CLASS(ScriptSource);

/**
  Source for scripts
*/
ACDK_CLASSATTRIBUTE(acdk.tools.mc.ClazzFlagAttribute(acdk.lang.dmi.MiNoDmiProxy))
class ACDK_CFGSCRIPT_LIB_PUBLIC ScriptSource
: extends acdk::lang::Object
, implements acdk::io::PushbackCharReader
, implements acdk::io::LineNumberCharReader
{
  ACDK_WITH_METAINFO(ScriptSource)
private:
  RString _sourceName;
  acdk::io::RCharReader _in;
  RStringBuffer _buffer;
  /**
    each entry is the character position of 
    begining of the line
  */
  acdk::lang::sys::core_vector<int> _sourcePositions;
  int _charPos;
  int _linePos;
  int _columnPos;
  bool _eof;
public:
  ScriptSource(IN(RString) name, IN(acdk::io::RCharReader) in)
  : _sourceName(name)
  , _in(in)
  , _buffer(new StringBuffer())
  , _charPos(0)
  , _linePos(0)
  , _columnPos(0)
  , _eof(false)
  {
    _sourcePositions.push_back(0);
  }
  ScriptSource(IN(RString) name, IN(RString) sourcetext)
  : _sourceName(name)
  , _in(new acdk::io::StringReader(sourcetext))
  , _buffer(new StringBuffer())
  , _charPos(0)
  , _linePos(0)
  , _columnPos(0)
  , _eof(false)
  {
    _sourcePositions.push_back(0);
  }
  foreign 
    ScriptSource(IN(RString) name, IN(RString) sourcetext, IN(acdk::io::CharStreamPos) sp);
  int charPos() { return _charPos; }
  

  // LineNumberCharReader
  foreign virtual int getCharPos() { return _charPos; }
  foreign virtual void setCharPos(int ch) { _charPos = ch; }
  
  foreign int getLineNumber() { return _linePos; }
  foreign void setLineNumber(int lineno) { _linePos = lineno; }
  foreign int getColumnNumber() { return _columnPos; }
  foreign void setColumnNumber(int columnNo) { _columnPos = columnNo; }
  foreign RString readLine();

  //PushbackCharReader
  foreign void unread(ucchar ch);
  foreign void unread(IN(RString) str);
  foreign void resetPushbackBuffer();

  // ScriptSource
  foreign acdk::io::CharStreamPos getSourcePos() const { return acdk::io::CharStreamPos(_charPos, _linePos, _columnPos); }
  foreign void setSourcePos(const acdk::io::CharStreamPos& sp);
  
  
  virtual int readChar();
  /**
    read until EOF and return as string
  */
  virtual RString readString();

  virtual void close() { _in->close(); }
  virtual acdk::io::RReader getReader(IN(acdk::locale::REncoder) enc = Nil) { return _in->getReader(enc); }
  foreign RScriptSource getSourceFragment(IN(acdk::io::CharStreamPos) start, IN(acdk::io::CharStreamPos) end)
  {
    return new ScriptSource(_sourceName, _buffer->toString()->substr(0, end.charPos), start);
  }
  RObject clone() 
  {
    return new ScriptSource(_sourceName, _buffer->toString(), acdk::io::CharStreamPos(_charPos, _linePos, _columnPos));
  }
  /**
    get a line of code, relative to current line
    0 return the current line
    < -1 return lines before current line
    If line not exists return Nil
  */
  RString getLine(int line);
  int seekCharBack();
  bool isEof();
protected:

  void _incrementReaded(IN(RString) str);
  bool _getLine(int lineNo, OUT(acdk::io::CharStreamPos) begin, OUT(acdk::io::CharStreamPos) end);
  bool _fetchNextLine(int lineNo);
  void _setCharPos(int newCharPos);
};

} // namespace cfgscript
} // namespace acdk 
  
#endif //acdk_cfgscript_ScriptSource_h
