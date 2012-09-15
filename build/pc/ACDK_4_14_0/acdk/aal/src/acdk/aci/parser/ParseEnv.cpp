// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#include "ParseEnv.h"
#include "SyntaxParseNode.h"
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace aci {
namespace parser {

RParseNodeArray 
ParseEnv::getParseNodes(IN(RString) rn) 
{ 
  typedef ::RObjectArrayImpl<RParseNodeArray> RParseNodeArrayArray;
  RParseNodeArrayArray t = _currentParseFrame->_codeParserTable->getValues(rn); 
  RParseNodeArray ret = new ParseNodeArray(0);
  for (int i = 0; i < t->length(); ++i)
    ret->concat(t[i]);
  return ret;
}

void 
ParseEnv::addParseNode(IN(RString) rn, IN(RParseNode) node)
{ 
  ACDK_NLOGP("acdk.aci.Parser", Debug300, "ParseEnv::addParseNode", LOG_NPV("RuleName", rn) << LOG_NPV("Syntax", node->getSyntax()));
  RParseNodeArray ca = _currentParseFrame->_codeParserTable->CodeParserTable::MapType::get(rn);
  if (ca == Nil)
  {
    ca = new ParseNodeArray(1); 
    ca[0] = node;
    _currentParseFrame->_codeParserTable->put(rn, ca);
  } 
  else
  {
    ca->append(&node); 
  }
}
void 
ParseEnv::addNotReducedRule(IN(RString) rn) 
{ 
  _currentParseFrame->_notReduceRules->add(rn); 
}

void 
ParseEnv::addTerminal(IN(RString) rn, IN(RTerminalParseNode) node) 
{ 
  ACDK_NLOGP("acdk.aci.Parser", Debug300, "ParseEnv::addTerminal", LOG_NPV("RuleName", rn) << LOG_NPV("Syntax", node->getSyntax()));
  if (node->getScannerTokenId() == -1)
  {
    int size = _currentParseFrame->_terminalSet->size();
    node->setScannerTokenId(size + 1);
  }
  _currentParseFrame->_terminalSet->add(node);
  _currentParseFrame->_terminalTable->put(rn, node); 
  addParseNode(rn, &node);
}

void 
ParseEnv::checkRules(IN(RCompiler) comp)
{
  TerminalTable::RKeyArrayType ks = _currentParseFrame->_codeParserTable->getKeys();
  for (int i = 0; i < ks->length(); ++i)
  {
    RParseNodeArray na =  _currentParseFrame->_codeParserTable->get(ks[i]);
    for (int j = 0; j < na->length(); ++j)
    {
      RParseNode n = na[j];
      if (instanceof(n, SyntaxParseNode) == true)
      {
        RSyntaxParseNode(n)->checkRules(comp);
      }
    }
  }
}


} // parser
} // aci
} // acdk



