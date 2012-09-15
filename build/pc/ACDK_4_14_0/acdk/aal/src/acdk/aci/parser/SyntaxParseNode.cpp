// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#include "SyntaxParseNode.h"
#include "../Compiler.h"
#include "../ast/AstNode.h"
#include "../ast/Keyword.h"

#include "KeywordParseNode.h"

#include <acdk/lang/Character.h>
#include <acdk/lang/System.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace aci {
namespace parser {

using namespace acdk::aci::ast;



SyntaxParseNode::SyntaxParseNode(IN(RString) nodeName, IN(RString) syntax, IN(RString) helpText, bool saveRule)
: ParseNode(nodeName, helpText)
, _saveRule(saveRule)
, _syntax(syntax)
, _throwOnFail(false)
{
  _syntaxTree = _parseToTree();
  
}

SyntaxParseNode::SyntaxParseNode(IN(RString) nodeName, IN(RSyntaxNode) sn, IN(RString) syntax, IN(RString) helpText, bool saveRule)
: ParseNode(nodeName, helpText)
, _syntax(syntax)
, _saveRule(saveRule)
, _throwOnFail(false)
, _syntaxTree(sn)
{

}

void _registerKeywords(IN(RCompiler) comp, IN(RSyntaxNode) n)
{
  if (instanceof(n, SyntaxKeyword) == true)
  {
    RSyntaxKeyword sk(n);
    sk->_tk = comp->registerKeyword(sk->keyWord);
  }
  else
  {
    for (int i = 0; i < n->childs->length(); ++i)
      _registerKeywords(comp, n->childs[i]);
  }
}

void 
SyntaxParseNode::onRegister(IN(RCompiler) comp)
{
  _registerKeywords(comp, _syntaxTree);
}

RAstNode 
SyntaxParseNode::createStandardAstNode()
{
  return new AstNodeWithChilds(Nil, this, getNodeName());
}

RAstNode 
SyntaxParseNode::parse(IN(RCompiler) compiler)
{
  return parseSyntax(compiler);
}



RAstNode 
SyntaxParseNode::parseSyntax(IN(RCompiler) compiler)
{
  String::iterator it =  _syntax->begin();
  RAstNode parent = createStandardAstNode();
  RAstNodeArray sibs = new AstNodeArray(0);
  RAstNodeArray erg = _syntaxTree->scan(compiler, parent, sibs, this);
  if (erg == Nil)
    return Nil;
  for (int i = 0; i < erg->length(); ++i)
    parent->addChild(erg[i], true);
  return parent;
  //return parseSyntax(compiler,&it, _syntax->end(), Execute);
}



//virtual 
void 
SyntaxParseNode::printSyntax(IN(acdk::io::RPrintWriter) out)
{
  if (_syntax != Nil)
  {
    out->println(getNodeName() + "\n: " + _syntax + "\n;\n");
    return;
  }
}

void 
SyntaxParseNode::checkRules(IN(RCompiler) comp)
{
  if (_syntaxTree != Nil)
    _syntaxTree->checkRules(comp);
}


} // namespace parser
} // aci
} // acdk

