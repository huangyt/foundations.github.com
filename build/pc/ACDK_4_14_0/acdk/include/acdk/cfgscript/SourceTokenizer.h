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
// $Header: /cvsroot/acdk/acdk/acdk_core/src/acdk/cfgscript/SourceTokenizer.h,v 1.16 2005/05/02 23:07:26 kommer Exp $

#ifndef acdk_cfgscript_SourceTokenizer_h
#define acdk_cfgscript_SourceTokenizer_h

#include "Config.h"
#include <acdk/io/File.h>
#include <acdk/io/StreamTokenizer.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace cfgscript {

/**
  represents an parsed CfgScript token.
  This class is only used inside the interpreter
*/
struct ACDK_CFGSCRIPT_LIB_PUBLIC SourceToken
{
  int tk;
  acdk::lang::dmi::ScriptVar value;
  acdk::io::CharStreamPos sourcePos;
  SourceToken(int token, IN(acdk::lang::dmi::ScriptVar) val, IN(acdk::io::CharStreamPos) spos)
  : tk(token)
  , value(val)
  , sourcePos(spos)
  {
  }
};

enum SourceTk
{
  STkTemplateText = -100,
  STkTemplateExpression = -101,
  STkTemplateComment = -102
};

enum STFlags
{
  STWantWhiteSpace = 0x00001,
  STWantNewLine    = 0x00002,
  STWantComments   = 0x00004,
  STParseTemplate  = 0x00008
};
ACDK_DEF_LIB_ENUM(ACDK_CFGSCRIPT_LIB_PUBLIC, STFlags);

ACDK_DECL_CLASS(TokenizedSource);

/**
  before executing a CfgScript the source will be parsed
  to token.
  This class is only used inside the interpreter.
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC TokenizedSource
: extends acdk::lang::Object
{
public:
  typedef acdk::lang::sys::core_vector<SourceToken> TokenContainer;
  TokenContainer _tokenStack;
  int _tokenIdx;
  int _endTokenIdx;
  RString _fileName;
  RString _sourceText;
  acdk::io::RReader _reader;
  acdk::io::RStreamTokenizer _streamTokenizer;
  bool _sourceTokenized;
  /**
    STFlags currently only 0 or STParseTemplate
  */
  int _flags;

  TokenizedSource(INP(RString) fileName, int flags = 0);
  TokenizedSource(INP(RString) fileName, INP(RString) sourceText, int flags = 0);
  TokenizedSource(INP(RString) fileName, INP(acdk::io::RReader) sourceReader, int flags = 0);
  void parseAll();
  
  /**
    return Nil if line cannot be found
  */
  RString getCodeOfLine(int lineNo);
  SourceToken& getSourceToken(int tkidx) 
  { 
    if (tkidx > _tokenStack.size() - 1)
      tkidx = _tokenStack.size() - 1;
    return _tokenStack[tkidx]; 
  }
  int getEndTokenIdx() 
  {
    if (_endTokenIdx == -1)
      return _tokenStack.size();
    return _endTokenIdx;
  }
  foreign RString _dumpTokens();
  virtual void getCollectableFields(FieldReferences& fields);
protected:
  void _loadSource();
  void _parseTemplate();
  int _parseTemplateText();
  int _parseSource();


};

ACDK_DECL_CLASS(SourceTokenizer);

/**
  before executing a CfgScript the source will be parsed
  to token.
  This class is only used inside the interpreter.
*/
class ACDK_CFGSCRIPT_LIB_PUBLIC SourceTokenizer
//: extends acdk::io::StreamTokenizer
: extends acdk::lang::Object
{
public:
  RTokenizedSource _tokenized;
  int _tokenIdx;
  int _endTokenIdx;
protected:
  acdk::lang::sys::core_vector<int> _flagStack;
  
public:
  //SourceTokenizer(INP(RString) fileName, int flags = 0);
  //SourceTokenizer(INP(RString) fileName, INP(RString) sourceText, int flags = 0);
  SourceTokenizer(INP(RTokenizedSource) source, int flags = 0)
    : _tokenized(source)
    , _tokenIdx(source->_tokenIdx)
    , _endTokenIdx(source->getEndTokenIdx())
  {
    _flagStack.push_back(flags);
    
  }
  int nextToken();
  foreign SourceToken& nextSourceToken();
  foreign SourceToken& curSourceToken();
  void pushBack();
  foreign acdk::lang::dmi::ScriptVar& curValue();
  RString getCurTokenAsCode() { return acdk::io::StreamTokenizer::toCode(curSourceToken().tk, curSourceToken().value); }
  //RString getCurTokenAsString() { return acdk::io::StreamTokenizer::toCode(curSourceToken().tk, curSourceToken().value); }
  int curToken();
  
  inline int getFlags() { return _flagStack[_flagStack.size() - 1]; }
  inline void pushFlags(int flags) { _flagStack.push_back(flags); }
  inline void popFlags() { _flagStack.pop_back(); }
  int getCurrentTokenIndex() { return _tokenIdx; }
  void setCurrentTokenIndex(int tokenIndex) { _tokenIdx = tokenIndex; }
  /**
    return token represents EOF
  */
  foreign static SourceToken& _getEofToken();
  foreign RString _dumpTokens();
protected:
  bool _skipToken();
 
  
};

#if !defined(DOXYGENONLY)

struct SourceTokenizerPosGuard
{
  RSourceTokenizer _st;
  int _sp;
  SourceTokenizerPosGuard(INP(RSourceTokenizer) st) 
  : _st(st) 
  {
    _sp = _st->getCurrentTokenIndex();
  }
  ~SourceTokenizerPosGuard()
  {
    _st->setCurrentTokenIndex(_sp);
  }
};

#endif //!defined(DOXYGENONLY)

} // cfgscript
} // acdk

#endif //acdk_cfgscript_SourceTokenizer_h

