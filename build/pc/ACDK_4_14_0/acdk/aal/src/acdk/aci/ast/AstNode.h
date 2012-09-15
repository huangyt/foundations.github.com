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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/ast/AstNode.h,v 1.5 2005/02/05 10:44:51 kommer Exp $


#ifndef acdk_aci_ast_AstNode_h
#define acdk_aci_ast_AstNode_h

#include <acdk.h>
#include <acdk/cfgscript/Props.h>
#include <acdk/io/StreamTokenizer.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/lang/UnsupportedOperationException.h>

#include "../parser/parser.h"
#include "../parser/ParseNode.h"
#include "../SymbolTable.h"
#include "AstNodeVisitor.h"
#include "../util/CodeLocation.h"

namespace acdk {
namespace aci {

ACDK_DECL_CLASS(Compiler);
namespace vm {
ACDK_DECL_CLASS(EvalEnv);
}


namespace ast {


ACDK_DECL_CLASS(AstNode);

/**
  Alternativelly to enumerations
  the queries on AST may be strings
*/
foreign enum TransformState
{
  DetectSource,
  BuildSyntax,
  InvalidSyntax,
  BuildDeclSemantic,
  InvalidDeclSemantic,
  BuildDefSemantic,
  InvalidDefSemantic,
  BuildOpCode,
  InvalidOpCode,
  CollectOpCode
};


foreign enum CodeType
{
  CTSyntax    = 0x00002,
  CTSemantic  = 0x00004,
  CTOpCode    = 0x00008
  
};


foreign enum AstFlags
{
  /**
    This AstNode was transformed to another AST
  */
  AFActive      = 0x0001,
  AFDeactivated = 0x0002,
  /**
    AstNode was generated in a transform process
  */
  AFSynthetic   = 0x0004,
  AFNeedSyntaxTrans = 0x0010,
  AFNeedSemTrans    = 0x0020,
  /**
    used in parsing when discarge unneded parse nodes
  */
  AFSaveNode        = 0x0100,
  AFInitialFlags = AFActive | AFNeedSyntaxTrans | AFNeedSemTrans
};



class ACDK_ACI_PUBLIC AstNode
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(AstNode)
protected:
  foreign AstNode* _parent;
  /**
    One of enum CodeType
  */
  int _codeType;
  /**
    combination of AstFlags
  */
  int _astFlags;
  /**
    Properties assigned with this AstNode used while parsing
    by default it is nil
  */
  acdk::cfgscript::RProps _astProps;
public:
  AstNode(IN(RAstNode) parent = Nil, int astFlags = AFInitialFlags)
    : _parent(0)
    , _astFlags(astFlags)
  {
    if (parent != Nil)
      _parent = &parent;
  }
  RString toString() 
  { 
    return Object::toString();
  }
  virtual RString getNodeName() { return ""; }
  /**
    return the location in the code
    if it has no own Code location, try to asc parent AstNode
  */
  virtual acdk::aci::util::RCodeLocation getCodeLocation()
  {
    if (_parent == 0)
      return Nil;
    return _parent->getCodeLocation();
  }
  /**
    return the underlying source text
  */
  RString getSourceTextFragment();
  /**
    return the ParseNode which created this AstNode
    May be Nil if this AstNode was created synthetical
    default implementation return the ParseNode of the parent
    AstNode
  */
  virtual acdk::aci::parser::RParseNode getParseNode()
  {
    if (_parent == 0)
      return Nil;
    return _parent->getParseNode();
  }
  /**
    return true if this AstNode should be saved after successfull build.
    Many syntax elements are only used to solve ambiguity and doesn't
    contain usefull information. These nodes may discarged after 
    Nodes are built.
    @return returns by default false
  */
  bool getSaveNodeAfterBuild() { return _astFlags & AFSaveNode; }
  void setSaveNodeAfterBuild(bool save) { if (save == true) _astFlags |= AFSaveNode; else _astFlags &= ~AFSaveNode; }

  /////////////////////////////////////////////////////////////////////
  // Neighbor Node operations
  /////////////////////////////////////////////////////////////////////
  RAstNode getParent() { return _parent; }
  void setParent(IN(RAstNode) parent) { _parent = &parent; }

  virtual int getChildCount() { return 0; }
  virtual RAstNode getChild(int idx)
  {
    THROW1(UnsupportedOperationException, "AstNode::getChild(int idx)");
    return Nil;
  }
  virtual void addChild(IN(RAstNode) childNode, bool setParent)
  {
    THROW1(UnsupportedOperationException, "AstNode::addChild()");
  }
  virtual RAstNode removeLastChild()
  {
    THROW1(UnsupportedOperationException, "AstNode::removeLastChild()");
    return Nil;
  }
  
  /**
    in case this AstNode is synthetical (generated in a transform process)
    return the orinal AstNode, which also can be found in Source
    Transformed/replaced/deleted AstNodes are marked as Deactived
  */
  virtual RAstNode getOrgAstNode() 
  { 
    if (_astFlags & AFDeactivated)
    {
      if (_parent == 0)
        return Nil;
      return _parent->getOrgAstNode();
    }
    return this; 
  }
  virtual void replaceWithTranformed(IN(RAstNode) newNode)
  {
    _astFlags |= AFDeactivated;
    _parent->addChild(newNode, false);
    newNode->setOrgNode(this);
  }
  virtual void setOrgNode(IN(RAstNode) orgNode)
  {
    THROW1(UnsupportedOperationException, "AstNode::setOrgNode()");
  }
  /**
    get the nth version of child with is type of type
    @param nth number of child 
    @param deep search also n childs of child
  */
  virtual RAstNode queryChild(IN(RClass) type, int nth = 0, int deep = 0) { return Nil; }
  /**
    using the node name to query direct child
  */
  virtual RAstNode queryChild(IN(RString) nodeName, int nth = 0, int deep = 0) { return Nil; }

  //don't use this: RAstNode getNthChild(int i) { THROW1(Exception, "has no child"); }

  /////////////////////////////////////////////////////////////////////
  // transformation operations
  /////////////////////////////////////////////////////////////////////
  /**
    build the semantic of the AST
    The default implementation does nothing
  */
  virtual void buildSemantic(IN(RCompiler) comp) {}
  /**
    generate Executable/OpCode
    The default implementation does nothing
  */
  virtual void genOpCode(IN(RCompiler) comp) {}

  /*
    Traversing
    @param listener callback
    @param parseState one of TransformListenerState
    @param flags combination of AstFlags used as filter
    @return how to continue traversing, see TraverseResult
  */
  virtual TraverseResult traverse(IN(RAstNodeVisitor) listener, IN(RCompiler) comp, ParseState parseState, int flags = AFActive);

  /////////////////////////////////////////////////////////////////////
  // Semantic operations
  /////////////////////////////////////////////////////////////////////
  virtual RSymbolTable getSymbolTable()
  {
    if (_parent == 0)
      return Nil;
    return _parent->getSymbolTable();
  }
  /**
    return the semantic element of this AstNode or Nil
    Sem may be types or incomplete types (like methods or fields)
  */
  virtual RSemanticElem getSem() { return Nil; }
  /**
    return the semantic of the current Node.
    A method returns the type of the return value
  */
  virtual RSemanticElem getExpressionSem() { return Nil; }
  
  virtual RDClazzInfo findType(IN(RString) tn) 
  { 
    if (_parent == 0)
      return Nil;
    return _parent->findType(tn);
  }
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op)
  {
    if (_parent == 0)
      return Nil;
    return _parent->findSubSem(str, op);
  }
  /////////////////////////////////////////////////////////////////////
  // Property operations
  /////////////////////////////////////////////////////////////////////
  /**
    return the Properties assigned with this sub-AST
    if current AstNode _astProps is Nil return the parent one
    if _parent == 0 return the compiler getSyntaxParserEnv()
  */
  acdk::cfgscript::RProps getAstProps(IN(RCompiler) compiler);
  /**
    creates Props in this AstNode if not already exists
  */
  acdk::cfgscript::RProps createAstProps(IN(RCompiler) compiler);
protected:
  bool shouldTraverse(int flags)
  {
    if (flags & AFActive && ((_astFlags & AFActive) != AFActive))
      return false;
    /* @todo AFNeedSyntaxTrans = 0x0010,
      AFNeedSemTrans    = 0x0020,
      */
    return true;
  }
};


ACDK_DECL_CLASS(AstNodeFromParseNode);

/**
  This AstNode was generated by a ParseNode
  ### @todo nodeName is probably superflous, because it can be queried by _parseNode->getNodeName()
*/
class ACDK_ACI_PUBLIC AstNodeFromParseNode
: extends AstNode
{
  ACDK_WITH_METAINFO(AstNodeFromParseNode)
protected:
  acdk::aci::parser::RParseNode _parseNode;
  RString _nodeName;
public:
  AstNodeFromParseNode(IN(RAstNode) parent, IN(acdk::aci::parser::RParseNode) pn, IN(RString) nodeName)
  : AstNode(parent)
  , _parseNode(pn)
  , _nodeName(nodeName)
  {
  }
  RString toString() { return getNodeName(); }
  virtual RString getNodeName() 
  { 
    return _nodeName; 
  }
  virtual acdk::aci::parser::RParseNode getParseNode() 
  { 
    return _parseNode; 
  }
};

ACDK_DECL_CLASS(AstNodeWithChilds);
/**
  ### @todo AstNodeWithChilds doesn't neccessary derived from AstNodeFromParseNode
*/
class ACDK_ACI_PUBLIC AstNodeWithChilds 
: extends AstNodeFromParseNode
{
  ACDK_WITH_METAINFO(AstNodeWithChilds)
protected:
  RAstNodeArray _childs;
public:
  AstNodeWithChilds(IN(RAstNode) parent, IN(acdk::aci::parser::RParseNode) pn, IN(RString) nodeName)
  : AstNodeFromParseNode(parent, pn, nodeName)
  , _childs(new AstNodeArray(0))
  {
  }
  virtual int getChildCount() { return _childs->length(); }
  virtual RAstNode getChild(int idx) { return _childs[idx]; }
  
  virtual RAstNode removeLastChild()
  {
    RAstNode code = _childs[_childs->length() - 1];
    _childs->remove(_childs->length() - 1);
    return code;
  }
  virtual void addChild(IN(RAstNode) childNode, bool setParent)
  {
    _childs->append(childNode);
    if (setParent == true)
      childNode->setParent(this);
  }
  virtual acdk::aci::util::RCodeLocation getCodeLocation();
  virtual void buildSemantic(IN(RCompiler) comp);
  virtual void genOpCode(IN(RCompiler) comp);
  virtual TraverseResult traverse(IN(RAstNodeVisitor) listener, IN(RCompiler) comp, ParseState parseState, int flags = AFActive);
  virtual RAstNode queryChild(IN(RClass) type, int nth = 0, int deep = 0);
  virtual RAstNode queryChild(IN(RString) nodeName, int nth = 0, int deep = 0);
  
  
};


ACDK_DECL_CLASS(AstNodeWithSymbols);

class ACDK_ACI_PUBLIC AstNodeWithSymbols
: extends AstNodeWithChilds
{
  RSymbolTable _symbolTable;
public:
  AstNodeWithSymbols(IN(RAstNode) parent, IN(acdk::aci::parser::RParseNode) pn, IN(RString) nodeName)
  : AstNodeWithChilds(parent, pn, nodeName)
  {
  }
};


} // ast
} // aci
} // acdk


#endif //acdk_aci_ast_AstNode_h
