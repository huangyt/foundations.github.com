// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_Scanner_h
#define acdk_aci_Scanner_h

#include <acdk.h>

#include "../Config.h"
#include <acdk/lang/sys/core_vector.h>
#include <acdk/text/RegExp.h>
#include <acdk/io/StreamTokenizer.h>

#include "../util/TStackedSet.h"
#include "../util/CodeLocation.h"
#include <acdk/util/THashSet.h>
#include "ParseEnv.h"

namespace acdk {
namespace aci {

namespace ast {
ACDK_DECL_CLASS(AstNode);
ACDK_DECL_CLASS(Terminal);

} // ast

namespace parser {



#ifdef DOUT
# undef DOUT
#endif

#define DBG_LEVEL 1

extern int dout_indent;

#define DBG_OUT_IMPL(msg) \
do { \
  for (int i_ = 0; i_ < dout_indent; ++i_) \
    std::cout << " "; \
  std::cout << msg << std::endl; \
} while (false)


# define DBG_OUT_NOIMPL(msg) do { } while(false)

#if DBG_LEVEL >= 1
#define EOUT(msg) DBG_OUT_IMPL(msg)
#else
# define EOUT(msg) DBG_OUT_NOIMPL(msg)
#endif

#if DBG_LEVEL >= 2
#define DOUT(msg) DBG_OUT_IMPL(msg)
#else
#define DOUT(msg) DBG_OUT_NOIMPL(msg)
#endif

#ifdef TOUT
# undef TOUT
#endif

#if DBG_LEVEL >= 3
#define TOUT(msg) DBG_OUT_IMPL(msg)
#else
# define TOUT(msg) DBG_OUT_NOIMPL(msg)
#endif

inline 
RString getDoutIndent()
{
  StringBuffer sb;
  for (int i = 0; i < dout_indent; ++i) 
    sb.append(" ");
  return sb.toString();
}

ACDK_DECL_CLASS(TerminalParseNode);

using acdk::io::CharStreamPos;

ACDK_DECL_CLASS(Scanner);

/*
struct ACDK_ACI_PUBLIC CodePosition
: public CharStreamPos
{
  RString sourceName;
  CodePosition()
    : CharStreamPos()
  {
  }
  CodePosition(IN(RString) fname, int cp, int hp, int vp)
  : CharStreamPos(cp, hp, vp)
  , sourceName(fname)
  {
  }
  CodePosition(const CodePosition& other)
  : CharStreamPos(other)
  , sourceName(other.sourceName)
  {
  }
  void reset(IN(RString) fname)
  {
    sourceName = fname;
    columnPos = charPos = linePos = 0;
  }
};
*/

#if 0

ACDK_DECL_CLASS(ScannerTerminal);

/// ScannerTerminal is a terminal, like identifier, keyword, operator
class ACDK_ACI_PUBLIC ScannerTerminal
: extends acdk::lang::Object
{
public:
  acdk::text::RRegExp regexp;
  RString orgExpr;
  int tokenIndex;
  int scanPos;
  /**
    0 lowest prio
    10 highest prio
  */
  byte matchPrio;
  int bindToSubEx;
  /**
    only used for container
  */
  ScannerTerminal()
  {
  }
  ScannerTerminal(IN(RString) str, byte prio, int bind = 0)
  : regexp(new acdk::text::RegExp("^" + str)) 
  , orgExpr(str)
  , tokenIndex(-1)
  , scanPos(-1)
  , matchPrio(prio)
  , bindToSubEx(bind)
  {
  }
  ScannerTerminal(const ScannerTerminal& other)
  : regexp(other.regexp) 
  , orgExpr(other.orgExpr)
  , tokenIndex(other.tokenIndex)
  , scanPos(other.scanPos)
  , matchPrio(other.matchPrio)
  , bindToSubEx(other.bindToSubEx)
  {
  }
  RString toString() { return orgExpr; }
  virtual RString decode(IN(RString) str) { return str; }
  virtual int matchNext(String::iterator it, String::iterator end);
  virtual bool isWs();
  virtual int hashCode() { return orgExpr->hashCode(); }
};

struct ACDK_ACI_PUBLIC ScannerToken
{
  int token;
  RString sval;
  CodePosition _codePosition;
  RScannerTerminal _terminal;
  ScannerToken()
  : token(-1)
  {
  }
  ScannerToken(int tk, IN(RString) s, IN(RScannerTerminal) terminal, const CodePosition& cpos)
  : token(tk)
  , sval(s)
  , _codePosition(cpos)
  , _terminal(terminal)
  {
  }
  ScannerToken(const ScannerToken& other)
    : token(other.token)
    , sval(other.sval)
    , _codePosition(other._codePosition)
    , _terminal(other._terminal)
  {
  }
  inline bool isWs();
  const CodePosition& getCodePosition() { return _codePosition; }
};


ACDK_DECL_CLASS(StringTerminal);

struct ACDK_ACI_PUBLIC StringTerminal
: extends ScannerTerminal
{
public:
  StringTerminal()
  : ScannerTerminal("", 8)
  {
  }
  RString toString() { return "<C-Literal>"; }
  virtual RString decode(IN(RString) str);
  virtual int matchNext(String::iterator it, String::iterator end);
};

ACDK_DECL_CLASS(CCommentTerminal);

struct ACDK_ACI_PUBLIC CCommentTerminal
: extends ScannerTerminal
{
public:
  CCommentTerminal()
  : ScannerTerminal("", 8)
  {
  }
  RString toString() { return "<C-Comment>"; }
  virtual int matchNext(String::iterator it, String::iterator end);
  virtual bool isWs() { return true; }
};

ACDK_DECL_CLASS(CxxCommentTerminal);

struct ACDK_ACI_PUBLIC CxxCommentTerminal
: extends ScannerTerminal
{
public:
  CxxCommentTerminal()
  : ScannerTerminal("", 8)
  {
  }
  RString toString() { return "<C++-Comment>"; }
  virtual int matchNext(String::iterator it, String::iterator end);
  virtual bool isWs() { return true; }
};

ACDK_DECL_CLASS(OperatorTerminal);

struct ACDK_ACI_PUBLIC OperatorTerminal
: extends ScannerTerminal
{
public:
  OperatorTerminal()
  : ScannerTerminal("", 2)
  {
  }
  RString toString() { return "<operator>"; }
  //virtual RString decode(IN(RString) str);
  virtual int matchNext(String::iterator it, String::iterator end);
};


ACDK_DECL_CLASS(RegExpTerminal);

struct ACDK_ACI_PUBLIC RegExpTerminal
: extends ScannerTerminal
{
public:
  RegExpTerminal(IN(RString) regExpText)
  : ScannerTerminal(regExpText, 2)
  {
  }
  RString toString() { return "<regexp>"; }
};

struct ScannerTokenStack;

#endif //0

/**
  The Scanner reads from a text source
*/
class ACDK_ACI_PUBLIC Scanner
: extends acdk::lang::Object
{
  //typedef acdk::lang::sys::core_vector<ScannerToken> TokenStack;
  typedef acdk::aci::ast::TerminalArray TokenStack;
  //typedef acdk::aci::util::TStackedSet<acdk::util::THashSet<RScannerTerminal> > TerminalSetType;
  typedef acdk::aci::util::TStackedSet<acdk::util::THashSet<RTerminalParseNode> > TerminalSetType;
  typedef TerminalSetType::RefType RTerminalSetType;
  
  /**
    contains the preparsed token
  */
  TokenStack _stack;
  int _flags;
  /** 
    _stack[_top] points to the last recent read
  */
  int _top;
  /**
    points to the next character to read
  */
  int _bufferTop;
  /**
    contains the text to scann
  */
  RString _inBuffer;
  /**
    last code position for next scanning token
  */
  acdk::aci::util::CodeLocation _codeLocation;
  //acdk::lang::sys::core_vector<RScannerTerminal> _terminals;
  /**
    a set of terminals
  */
  RParseEnv _parseEnv;
public:
  enum Flags
  {
    ScanWs   = 0x00000001,
    WantWs   = 0x00000002,
    WantEol  = 0x00000004
  };
  
  static const int TT_EOF;
  static const int TT_EOL;
  static const int TT_WS;
  static const int TT_MINRESERVED;
  static const int TT_MAXRESERVED;
  

  Scanner(IN(acdk::io::RReader) in, int flags = 0);
  Scanner(IN(RString) text = Nil, int flags = 0);
  
  RParseEnv getParseEnv() { return _parseEnv; }
  /**
    creates a scanner with all 
    terminal of this scanner
  */
  RScanner createSubTextScanner(IN(RString) text);
  
  int registerTerminal(IN(RTerminalParseNode) st);
  /**
    @param expr regular expression
    @param prio if multiple terminals matches, select ScannerTerminal with higher prio
    @param index Select TokenIndex
                 -1 means that Scanner uses next free negativ index
    @param bindToSubEx if expr has subexpressions "(.*)[^A-Z]) bind result to 
           sub expression number. If bindToSubEx == 0 uses whole matching string
  */
  //int registerTerminal(IN(RString) expr, byte prio, int index = -1, int bindToSubEx = 0);

  void setInBuffer(IN(RString) buffer)
  {
    _inBuffer = buffer;
    _bufferTop = 0;
    //_stack.erase(_stack.begin(), _stack.end());
    _stack.resize(0);
    _top = -1;
    //_codeLocation.reset();
  }

  void wantWs(bool b) { if (b == true) _flags |= WantWs; else _flags &= ~WantWs; }
  acdk::aci::util::RCodeLocation getClonedCodeLocation();
  acdk::aci::util::RCodeLocation getCodeLocation();
  //foreign const acdk::aci::util::CodeLocation& getCodeLocation() const { return _codeLocation; }
  //foreign void setCodeLocation(acdk::aci::util::CodeLocation& cl) { _codeLocation = cl; }
  acdk::aci::ast::RTerminal getNext();
  acdk::aci::ast::RTerminal peek(int pos = 1);

  /*
  /// next token from stream
  ScannerToken* fetch();
  ScannerToken* tryFetchRegExp(IN(RString) regExp);
  /// look one token ahead
  ScannerToken* peek(int pos = 1);
  */
  void dumpStack()
  {
    std::cout << "TKStack ____________" << std::endl;
    for (int i = 0; i < _stack.length(); ++i)
    {
      //### @todo fixme std::cout << "[" << i << "] (" << _stack[i].token << ") \"" << _stack[i].sval->c_str() << "\"";
      if (i == _top)
        std::cout << " <-- Top";
      std::cout << std::endl;
    }
  }
  /**
    resets all prepared ScannerToken behind _bufferTop
  */
  void resetPreparsedTerminals();
  /**
    set to last token 
  */
  void resetTokenIdx(int tkidx);
protected:
  /**
    returns the top token index
  */
  int _readNext();
private:
  void _init();
   int _scanNext();
  //int _safeToken(int tokenIndex, IN(RString) parsed, IN(RScannerTerminal) st);
  
  friend class ScannerTokenStack;
 
};

/*
inline
bool ScannerToken::isWs() 
{ 
  return token == Scanner::ScanWs || (_terminal != Nil && _terminal->isWs() == true); 
}
*/

/**
  Implement a scoped scanner. 
  If commit() was not called the destructor 
  seeks back in the source stream
*/
struct ACDK_ACI_PUBLIC ScannerTokenStack
{
private:
  Scanner* _scanner;
  /** token index */
  int _savedTop;
  /** scanner commited */
  int _commited;
  int _ignoreTk;
  
public:
  ScannerTokenStack(Scanner* scanner)
  : _scanner(scanner)
  , _savedTop(scanner->_top)
  , _commited(false)
  , _ignoreTk(-1)
  //, _savedPos(scanner->getCodeLocation())
  {
  }
  ~ScannerTokenStack()
  {
    if (_commited == false)
      reject();
  }
  void setIgnoreTk(int tk) { _ignoreTk = tk; }
  void commit()
  {
    _commited = true;
  }
  void reject()
  {
    TOUT("RJ: " << _savedTop);
    _scanner->resetTokenIdx(_savedTop);// = _savedTop;
    //_scanner->setCodeLocation(_savedPos);
  }
  void wantWs(bool want) { _scanner->wantWs(want); }
  
  bool ignoreTk(int tk)
  {
    return tk != Scanner::TT_EOF && tk == _ignoreTk; 
  }

  acdk::aci::ast::RTerminal getNext() { return _scanner->getNext(); }
  acdk::aci::ast::RTerminal peek(int pos = 1);
  acdk::aci::ast::RTerminal top();

  
};

} // parser
} // aci
} // acdk

#endif //acdk_aci_Scanner_h
