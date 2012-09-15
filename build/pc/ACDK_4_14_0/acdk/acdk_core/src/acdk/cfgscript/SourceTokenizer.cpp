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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/cfgscript/SourceTokenizer.cpp,v 1.15 2005/05/02 23:07:26 kommer Exp $

#include "SourceTokenizer.h"
#include "ScriptEval.h" // vor DOUT() macro

#include <acdk/io/ByteToCharReader.h>
#include <acdk/io/MemWriter.h>
#include <acdk/io/StringReader.h>

namespace acdk {
namespace cfgscript {




TokenizedSource::TokenizedSource(INP(RString) fileName, int flags)
: _tokenIdx(0)
, _endTokenIdx(-1)
, _fileName(fileName)
, _sourceTokenized(false)
, _flags(flags)
{
  //_flagStack.push_back(flags);
}

TokenizedSource::TokenizedSource(INP(RString) fileName, INP(RString) sourceText, int flags)
: _tokenIdx(0)
, _endTokenIdx(-1)
, _fileName(fileName)
, _sourceText(sourceText)
, _sourceTokenized(false)
, _flags(flags)
{
  //_flagStack.push_back(flags);
}

TokenizedSource::TokenizedSource(INP(RString) fileName, INP(acdk::io::RReader) sourceReader, int flags)
: _tokenIdx(0)
, _endTokenIdx(-1)
, _fileName(fileName)
, _sourceText(Nil)
, _reader(sourceReader)
, _sourceTokenized(false)
, _flags(flags)

{
}

int 
TokenizedSource::_parseTemplateText()
{
  acdk::io::CharStreamPos lastStreamPos = _streamTokenizer->getCharStreamPos();
  acdk::io::MemWriter mout;
  int tk;
  while ((tk = _streamTokenizer->read()) != -1)
  {
    if (tk == '<')
    {
      tk = _streamTokenizer->read();
      if (tk == '@')
      {
        tk = _streamTokenizer->read();
        
        _tokenStack.push_back(SourceToken(STkTemplateText, inOf(mout.getBuffer()), lastStreamPos));
        if (tk == '=')
        {
          _tokenStack.push_back(SourceToken(STkTemplateExpression, inOf(0), _streamTokenizer->getCharStreamPos()));
        } 
        else if (tk == '!')
        {
          _tokenStack.push_back(SourceToken(STkTemplateComment, inOf(0), _streamTokenizer->getCharStreamPos()));
        }
        else
          _streamTokenizer->unread(tk);
        return 0;
      }
      else
      {
        mout.write((byte)'<');
        _streamTokenizer->unread(tk);
      }
    }
    else
      mout.write((byte)tk);
  }
  _tokenStack.push_back(SourceToken(STkTemplateText, inOf(mout.getBuffer()), lastStreamPos));
  return -1;
}

void 
TokenizedSource::_parseTemplate()
{
  
  while (true)
  {
    int ret = _parseTemplateText();
    if (ret == -1)
      return;
    ret = _parseSource();
    if (ret == -1)
      return;
  }
}

int 
TokenizedSource::_parseSource()
{
  int tk;
  acdk::io::CharStreamPos lastStreamPos;
  lastStreamPos.linePos = 1;
  acdk::io::CharStreamPos curStreamPos;
  
  while ((tk = _streamTokenizer->nextToken()) != acdk::io::StreamTokenizer::TT_EOF)
  {
    acdk::lang::dmi::ScriptVar sv = _streamTokenizer->getCurrentSvToken();
    curStreamPos = _streamTokenizer->getCharStreamPos();
    DOUT("TK: " << sv.toCode());
    if (_flags & STParseTemplate)
    {
      if (tk == '@')
      {
        tk = _streamTokenizer->read();
        if (tk == '>')
        {
          return 0;
        }
        else
        {
          _streamTokenizer->unread('>');
          _streamTokenizer->unread('@');
        }
      }
    }
    if (tk == acdk::io::StreamTokenizer::TT_OPERATOR)
    {
      RString sop = sv.getStringVar();
      static StaticAsciiLiteral lit_arrow("->");
      if (sop->equals(lit_arrow) == true)
        sv = inOf(".");
    }
    if (tk == ':')
    {
      tk = _streamTokenizer->nextToken();
      if (tk == ':')
      {
        _tokenStack.push_back(SourceToken(acdk::io::StreamTokenizer::TT_OPERATOR, inOf("."), lastStreamPos));
        continue;
      }
      else
      {
        _streamTokenizer->pushBack();
        tk = ':';
      }
    }
    _tokenStack.push_back(SourceToken(tk, sv, lastStreamPos));
    lastStreamPos = _streamTokenizer->getCharStreamPos();
    lastStreamPos.linePos += 1;
  }
  
  return -1;
}

void 
TokenizedSource::parseAll()
{
  _loadSource();
  while (true)
  {
    if (_flags & STParseTemplate)
      if (_parseTemplateText() == -1)
        break;
    if (_parseSource() == -1)
      break;
  }
  _tokenStack.push_back(SourceTokenizer::_getEofToken());
  _sourceTokenized = true;

  /*
  int tk;
  acdk::io::CharStreamPos lastStreamPos;
  lastStreamPos.linePos = 1;
  acdk::io::CharStreamPos curStreamPos;
  
  while ((tk = _streamTokenizer->nextToken()) != acdk::io::StreamTokenizer::TT_EOF)
  {
    acdk::lang::dmi::ScriptVar sv = _streamTokenizer->getCurrentSvToken();
    curStreamPos = _streamTokenizer->getCharStreamPos();
    //DOUT("TK: " << sv.toCode());
    if (tk == acdk::io::StreamTokenizer::TT_OPERATOR)
    {
      if (sv.getStringVar()->equals("->") == true)
        sv = inOf(".");
    }
    if (tk == ':')
    {
      tk = _streamTokenizer->nextToken();
      if (tk == ':')
      {
        _tokenStack.push_back(SourceToken(acdk::io::StreamTokenizer::TT_OPERATOR, inOf("."), lastStreamPos));
        continue;
      }
      else
      {
        _streamTokenizer->pushBack();
        tk = ':';
      }
    }
    _tokenStack.push_back(SourceToken(tk, sv, lastStreamPos));
    lastStreamPos = _streamTokenizer->getCharStreamPos();
    lastStreamPos.linePos += 1;
  }
  _tokenStack.push_back(SourceTokenizer::_getEofToken());
  _sourceTokenized = true;
  */
}

void 
TokenizedSource::_loadSource()
{
  if (_reader != Nil)
  {
    acdk::io::ByteToCharReader cin(_reader);
    _sourceText = cin.readString();
  }
  else if (_sourceText == Nil)
  {
    acdk::io::FileReader fin(_fileName);
    acdk::io::ByteToCharReader cin(&fin);
    _sourceText = cin.readString();
  }
  if (_streamTokenizer == Nil)
  {
    _streamTokenizer = new acdk::io::StreamTokenizer(new acdk::io::StringReader(_sourceText));
    _streamTokenizer->wantNewline(true);
    _streamTokenizer->readCxxIdentifier(false);
    _streamTokenizer->readOperator(true);
  }
}





RString 
TokenizedSource::getCodeOfLine(int lineNo)
{
  return _sourceText->peekLine(lineNo);
  /*
  StringBuffer sb;
  int beginCodePos = -1;
  int endCodePos = -1;
  for (int i = 0; i < _tokenStack.size(); ++i)
  {
    if (_tokenStack[i].sourcePos.linePos == lineNo)
    {
      if (beginCodePos == -1)
        beginCodePos = _tokenStack[i].sourcePos.charPos;
      
    }
    else if (_tokenStack[i].sourcePos.linePos > lineNo && i > 0)
    {
      endCodePos = _tokenStack[i - 1].sourcePos.charPos;
      break;
    }
  }
  if (endCodePos == -1)
    endCodePos = _tokenStack[_tokenStack.size() - 2].sourcePos.charPos;
  if (beginCodePos != -1 && endCodePos != -1)
  {
    RString l = _sourceText->substr(beginCodePos); //, endCodePos);
    l = l->trim(TrimLeft | TrimNewLines);
    int endlidx = getFirstIndexOf(l, "\n\r");
    if (endlidx == -1)
      return l;
    return l->substr(0,endlidx);
  }
  return Nil;
  */
}


int 
SourceTokenizer::nextToken()
{
  if (_tokenIdx >= _endTokenIdx)
    return -1;
  
  while (_skipToken() && _tokenized->_tokenStack[_tokenIdx].tk != acdk::io::StreamTokenizer::TT_EOF)
    ++_tokenIdx;
  int ret =  _tokenized->_tokenStack[_tokenIdx++].tk;
  return ret;
}

RString
TokenizedSource::_dumpTokens()
{
  StringBuffer tsb;
  for (int i = 0; i < _tokenStack.size(); ++i)
  {
    SourceToken& st = _tokenStack[i];
    tsb << "[" << i << "|(" << st.sourcePos.linePos << "," << st.sourcePos.columnPos << ")=[" << st.value.toCode() << "]]\n";
  }
  return tsb.toString();
}

//foreign 
SourceToken& 
SourceTokenizer::nextSourceToken()
{
  int tk = nextToken();
  if (tk == -1)
    return _getEofToken();
  return curSourceToken();
}

void 
SourceTokenizer::pushBack()
{
  --_tokenIdx;
  while (_skipToken()  && _tokenIdx > 0)
  {
    --_tokenIdx;
  }
}

bool
SourceTokenizer::_skipToken()
{
  int flags = getFlags();
  int tk = _tokenized->_tokenStack[_tokenIdx].tk;
  if ((flags & STWantWhiteSpace) == 0)
  {
    if (tk == acdk::io::StreamTokenizer::TT_WS)
      return true;
  }
  if ((flags & STWantNewLine) == 0)
  {
    if (tk == acdk::io::StreamTokenizer::TT_EOL)
      return true;
  }
  if ((flags & STWantComments) == 0)
  {
    if (tk == acdk::io::StreamTokenizer::TT_CCOMMENT || tk == acdk::io::StreamTokenizer::TT_CXXCOMMENT)
      return true;
  }
  return false;
}


acdk::lang::dmi::ScriptVar& 
SourceTokenizer::curValue()
{
  return _tokenized->_tokenStack[_tokenIdx - 1].value;
}

int 
SourceTokenizer::curToken()
{
  if (_tokenIdx >= _endTokenIdx)
    return -1;
  return _tokenized->_tokenStack[_tokenIdx - 1].tk;
}

SourceToken& 
SourceTokenizer::curSourceToken()
{
  if (_tokenIdx >= _endTokenIdx)
    return _getEofToken();

  if (_tokenIdx == 0)
  {
    if (_tokenized->_tokenStack.size() < 1)
      return _getEofToken();
    return _tokenized->_tokenStack[_tokenIdx];
  }
  return _tokenized->_tokenStack[_tokenIdx  - 1];
}





//static 
SourceToken& 
SourceTokenizer::_getEofToken()
{
  static SourceToken endOfFileToken(-1, Nil, acdk::io::CharStreamPos(-1, -1, -1));
  return endOfFileToken;
}

//virtual 
void 
TokenizedSource::getCollectableFields(FieldReferences& fields)
{
  fields.push_back((RObject*)_fileName._ref_this());
  fields.push_back((RObject*)_sourceText._ref_this());
  fields.push_back((RObject*)_reader._ref_this());
  fields.push_back((RObject*)_streamTokenizer._ref_this());
  TokenContainer::iterator it = _tokenStack.begin();
  TokenContainer::iterator end = _tokenStack.end();
  for (; it != end; ++it)
  {
    if ((*it).value.isObjectType() == true)
      fields.push_back((RObject*)(*it).value.getObjectRef()._ref_this());
  }
}

} // acdk
} // cfgscript


