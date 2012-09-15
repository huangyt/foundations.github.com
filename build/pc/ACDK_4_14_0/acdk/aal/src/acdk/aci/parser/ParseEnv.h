// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_parser_ParseEnv_h
#define acdk_aci_parser_ParseEnv_h

#include <acdk.h>
#include "../Config.h"
#include "../util/TStackedSet.h"
#include <acdk/util/THashMap.h>
#include <acdk/util/THashSet.h>
#include <acdk/cfgscript/Props.h>

#include "TerminalParseNode.h"


namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(ParseEnv);

typedef acdk::aci::util::TStackedMap<acdk::util::THashMap<RString, parser::RParseNodeArray> > CodeParserTable;
typedef ::RefHolder<CodeParserTable> RAstNodeParserTable;

typedef acdk::aci::util::TStackedSet<acdk::util::THashSet<RString> > StackedStringSet;
typedef ::RefHolder<StackedStringSet> RStackedStringSet;

typedef acdk::aci::util::TStackedMap<acdk::util::THashMap<RString, parser::RTerminalParseNode> > TerminalTable;
typedef ::RefHolder<TerminalTable> RTerminalTable;

typedef acdk::aci::util::TStackedSet<acdk::util::THashSet<parser::RTerminalParseNode> > StackedTerminalSet;
typedef ::RefHolder<StackedTerminalSet> RStackedTerminalSet;

ACDK_DECL_CLASS(ParseFrame);

class ACDK_ACI_PUBLIC ParseFrame
: extends acdk::lang::Object
{
public:
  RString _name;
  
  
  /**
    String -> ParseNodeArray
  */
  foreign RAstNodeParserTable _codeParserTable;
  /**
    String of ParseNode names which should not be reduced
  */
  foreign RStackedStringSet _notReduceRules;
  
  foreign RStackedStringSet _keyWords;
  
  /// String -> RParseNode which represents terminals
  foreign RTerminalTable _terminalTable;
  foreign RStackedTerminalSet _terminalSet;
  acdk::cfgscript::RProps _syntaxScannerProps;
public:
  ParseFrame(IN(RString) name = Nil)
  : _name(name)
  , _codeParserTable(new CodeParserTable())
  , _notReduceRules(new StackedStringSet())
  , _keyWords(new StackedStringSet())
  , _terminalTable(new TerminalTable())
  , _terminalSet(new StackedTerminalSet())
  , _syntaxScannerProps(new acdk::cfgscript::Props("ScannerProps", acdk::cfgscript::PropsNoFlags))
  {
  }
  void pushScope()
  {
    _codeParserTable = _codeParserTable->pushScope();
    _notReduceRules = _notReduceRules->pushScope();
    _keyWords = _keyWords->pushScope();
    _terminalTable = _terminalTable->pushScope();
    _terminalSet = _terminalSet->pushScope();
    _syntaxScannerProps = new acdk::cfgscript::Props("ScannerProps", acdk::cfgscript::PropsNoFlags, _syntaxScannerProps);
  }
  void popScope()
  {
    _codeParserTable = _codeParserTable->popScope();
    _notReduceRules = _notReduceRules->popScope();
    _keyWords = _keyWords->popScope();
    _terminalTable = _terminalTable->popScope();
    _terminalSet = _terminalSet->popScope();
    _syntaxScannerProps = _syntaxScannerProps->getParentProps();
  }
};

ACDK_DECL_CLASS(ParseEnv);

class ACDK_ACI_PUBLIC ParseEnv
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(ParseEnv)
 
public:
  RParseFrame _currentParseFrame;
  
  ParseEnv()
  : _currentParseFrame(new ParseFrame())
  {
  }
  void pushScope() 
  { 
    _currentParseFrame->pushScope(); 
  }
  void popScope() 
  { 
     // ### @todo unregister terminals from scanner
    _currentParseFrame->popScope(); 
  }
  /// ### @todo _keyWords is superflous
  bool isKeyword(IN(RString) kw) { return _currentParseFrame->_keyWords->contains(kw); }
  /**
    see also Scanner::registerTerminal
  */
  /*
  int addKeyword(IN(RString) kw, IN(RString) qkw, byte prio, int index = -1, int bindToSubEx = 0) 
  { 
    //_currentParseFrame->_keyWords->add(kw); 
    return _scanner->registerTerminal(new KeywordParseNode("Keyword", kw));
    //return _scanner->registerTerminal(qkw, prio, index, bindToSubEx);
  }
  */
  /*int addTerminal(IN(RString) kw, IN(RString) qkw, byte prio, int index = -1, int bindToSubEx = 0) 
  {
  }*/
  bool notRecudeRole(IN(RString) rn) 
  { 
    return _currentParseFrame->_notReduceRules->contains(rn); 
  }
  RParseNodeArray getParseNodes(IN(RString) rn);
  
  void addParseNode(IN(RString) rn, IN(RParseNode) node);

  void addParseNodes(IN(RString) rn, IN(RParseNodeArray) pna) 
  { 
    _currentParseFrame->_codeParserTable->put(rn, pna); 
  }
  void addNotReducedRule(IN(RString) rn);
  RTerminalParseNode getTerminal(IN(RString) rn) 
  { 
    return _currentParseFrame->_terminalTable->get(rn); 
  }
  void addTerminal(IN(RString) rn, IN(RTerminalParseNode) node);
  
  acdk::cfgscript::RProps getSyntaxScannerProps()
  {
    return _currentParseFrame->_syntaxScannerProps;
  }
  RintArray getIgnoreTokens()
  {
    RObject obj = _currentParseFrame->_syntaxScannerProps->getObjectVal("_ignore_token");
    if (obj == Nil)
    {
      obj = new intArray(0);
      _currentParseFrame->_syntaxScannerProps->setObjectVal("_ignore_token", obj);
    }
    return RintArray(obj);
  }
  bool isIgnoreToken(int tk)
  {
    RintArray tks = getIgnoreTokens();
    for (int i = 0; i < tks->length(); ++i)
      if (tks[i] == tk)
        return true;
    return false;
  }
  void addIgnoreToken(int tk)
  {
    RintArray tks = getIgnoreTokens();
    tks->append(tk);
  }
  void checkRules(IN(RCompiler) comp);
};


} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_ParseEnv_h

