// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_parser_SyntaxNode_h
#define acdk_aci_parser_SyntaxNode_h

#include <acdk.h>
#include "../Config.h"


namespace acdk {
namespace aci {
namespace parser {

ACDK_DECL_CLASS(ParseNode);




enum SyntaxType
{
  ST_Or,
  ST_Follow,
  ST_Optional,
  ST_Rule,
  ST_Keyword,
  ST_ZeroOrMore,
  ST_OneOrMore,
  ST_Eval,
  ST_SaveRule,
  ST_SaveIfOptRule,
  ST_HideSubRule,
  ST_Commit,
  ST_Error
};
ACDK_DEF_LIB_ENUM(ACDK_ACI_PUBLIC, SyntaxType);

ACDK_DECL_CLASS(SyntaxNode);
/**
  A SyntaxNode is used by SyntaxParseNode
  to represent nodes of the BNF syntax tree
*/
class ACDK_ACI_PUBLIC SyntaxNode
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(SyntaxNode)
public:
  SyntaxType syntaxType;
  int flags;
  RSyntaxNodeArray childs;
  SyntaxNode(SyntaxType tp)
  : syntaxType(tp)
  , flags(0)
  , childs(new SyntaxNodeArray(0))
  {
  }
  RSyntaxNode back() { return childs->get(childs->length() - 1); }
  RSyntaxNode pop_back() 
  { 
    RSyntaxNode t = childs->get(childs->length() - 1); 
    childs->remove(childs->length() - 1); 
    return t;
  }
  void push_back(IN(RSyntaxNode) n)
  {
    childs->append(n);
  }
  
  static RString toName(SyntaxType tp);
  virtual RString toString() { return toName(syntaxType); }
  void renderToTree(StringBuffer& sb, int ident);
  void checkRules(IN(RCompiler) comp);
  /**
    @return Nil if scan was not sucessfully
            return empty AstNodeArray if scan is successfully, but not produced
            any nodes
  */
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn) = 0;
};

ACDK_DECL_CLASS(SyntaxFollow);

class ACDK_ACI_PUBLIC SyntaxFollow
: extends SyntaxNode
{
public:
  SyntaxFollow() : SyntaxNode(ST_Follow) {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};

ACDK_DECL_CLASS(SyntaxRule);

class ACDK_ACI_PUBLIC SyntaxRule
: extends SyntaxNode
{
public:
  RString ruleName;
  SyntaxRule(IN(RString) rn) 
  : SyntaxNode(ST_Rule) 
  , ruleName(rn)
  {}
  RString toString() { return SyntaxNode::toString() + ": " + ruleName; }
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};

ACDK_DECL_CLASS(SyntaxKeyword);
class ACDK_ACI_PUBLIC SyntaxKeyword
: extends SyntaxNode
{
public:
  RString keyWord;
  int _tk;
  SyntaxKeyword(IN(RString) kw) 
  : SyntaxNode(ST_Keyword) 
  , keyWord(kw)
  , _tk(-1)
  {}
  RString toString() { return SyntaxNode::toString() + ": " + keyWord; }
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};

ACDK_DECL_CLASS(SyntaxOr);

class ACDK_ACI_PUBLIC SyntaxOr
: extends SyntaxNode
{
public:
  SyntaxOr() 
  : SyntaxNode(ST_Or) 
  {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};

ACDK_DECL_CLASS(SyntaxOptional);

class ACDK_ACI_PUBLIC SyntaxOptional
: extends SyntaxNode
{
public:
  SyntaxOptional() 
  : SyntaxNode(ST_Optional) 
  {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};


ACDK_DECL_CLASS(SyntaxZeroOrMore);

class ACDK_ACI_PUBLIC SyntaxZeroOrMore
: extends SyntaxNode
{
public:
  SyntaxZeroOrMore() 
  : SyntaxNode(ST_ZeroOrMore) 
  {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};

ACDK_DECL_CLASS(SyntaxOneOrMore);

class ACDK_ACI_PUBLIC SyntaxOneOrMore
: extends SyntaxNode
{
public:
  SyntaxOneOrMore() 
  : SyntaxNode(ST_OneOrMore) 
  {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};


ACDK_DECL_CLASS(SyntaxCommit);

class ACDK_ACI_PUBLIC SyntaxCommit
: extends SyntaxNode
{
public:
  SyntaxCommit() : SyntaxNode(ST_Commit) {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};


ACDK_DECL_CLASS(SyntaxEval);

class ACDK_ACI_PUBLIC SyntaxEval
: extends SyntaxNode
{
public:
  RString evalSource;
  SyntaxEval(IN(RString) text) 
  : SyntaxNode(ST_Eval) 
  , evalSource(text)
  {}
  RString toString() { return SyntaxNode::toString() + ": " + evalSource; }
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};


ACDK_DECL_CLASS(SyntaxSave);

class ACDK_ACI_PUBLIC SyntaxSave
: extends SyntaxNode
{
public:
  SyntaxSave() 
  : SyntaxNode(ST_SaveRule) 
  {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};


ACDK_DECL_CLASS(SyntaxSaveIfSub);

class ACDK_ACI_PUBLIC SyntaxSaveIfSub
: extends SyntaxNode
{
public:
  SyntaxSaveIfSub() 
  : SyntaxNode(ST_SaveIfOptRule) 
  {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};

ACDK_DECL_CLASS(SyntaxHideSubRule);

class ACDK_ACI_PUBLIC SyntaxHideSubRule
: extends SyntaxNode
{
public:
  SyntaxHideSubRule() 
  : SyntaxNode(ST_HideSubRule) 
  {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};



ACDK_DECL_CLASS(SyntaxError);

class ACDK_ACI_PUBLIC SyntaxError
: extends SyntaxNode
{
  RString _message;
public:
  
  SyntaxError(IN(RString) msg) 
  : SyntaxNode(ST_Error) 
  , _message(msg)
  {}
  virtual acdk::aci::ast::RAstNodeArray scan(IN(RCompiler) comp, IN(acdk::aci::ast::RAstNode) parent, IN(acdk::aci::ast::RAstNodeArray) siblings, IN(RParseNode) pn);
};


} // parser
} // aci
} // acdk


#endif //acdk_aci_parser_SyntaxNode_h

