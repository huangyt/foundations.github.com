// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_parser_SyntaxParseNode_h
#define acdk_aci_parser_SyntaxParseNode_h

#include <acdk.h>
#include "../Config.h"
#include "ParseNode.h"
#include "../ast/AstNode.h"
#include "SyntaxNode.h"

namespace acdk {
namespace aci {
namespace parser {


ACDK_DECL_CLASS(SyntaxParseNode);

/**
  The SyntaxParseNode parses the input by a BNF syntax
*/
class ACDK_ACI_PUBLIC SyntaxParseNode
: extends ParseNode
{
  ACDK_WITH_METAINFO(SyntaxParseNode)
public:
  
protected:
  bool _saveRule;
  bool _throwOnFail;
  RString _syntax;
  RSyntaxNode _syntaxTree;
public:
  SyntaxParseNode(IN(RString) nodeName, IN(RString) syntax, IN(RString) helpText = "", bool saveRule = false);
  SyntaxParseNode(IN(RString) nodeName, IN(RSyntaxNode) sn, IN(RString) syntax, IN(RString) helpText = "", bool saveRule = false);
  virtual void onRegister(IN(RCompiler) comp);
  /**
    Parses this node.
    @return the parsed RAstNode or Nil if input doesn't match
  */
  virtual acdk::aci::ast::RAstNode parse(IN(RCompiler) compiler);
  virtual acdk::aci::ast::RAstNode createStandardAstNode();
  RString getNodeName() { return _nodeName; }
  RString getSyntax() { return _syntax; }
  void setSyntax(IN(RString) syntax) { _syntax = syntax; }
  virtual void printSyntax(IN(acdk::io::RPrintWriter) out);
  RString toString() { return _nodeName + ": " + _syntax; }
  /** 
    parses a syntax definition 
    @param inputFn file name of the input
  */
  static void parseSyntaxText(IN(RCompiler) comp, IN(RString) input, IN(RString) inputFn = "");
  /**
    checks if all rules are known
  */
  void checkRules(IN(RCompiler) comp);
  RSyntaxNode getSyntaxTree() { return _syntaxTree; }
protected:
  RSyntaxNode _parseToTree();
  acdk::aci::ast::RAstNode parseSyntax(IN(RCompiler) compiler);
};


} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_SyntaxParseNode_h

