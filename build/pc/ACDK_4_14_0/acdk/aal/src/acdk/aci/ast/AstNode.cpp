// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 


#include <acdk.h>
#include "../aci.h"
#include "AstNode.h"
#include "../Compiler.h"
#include "../parser/TerminalParseNode.h"
#include "Terminal.h"

namespace acdk {
namespace aci {
namespace ast {


acdk::cfgscript::RProps 
AstNode::getAstProps(IN(RCompiler) compiler)
{
  if (_astProps != Nil)
    return _astProps;
  if (_parent != 0)
    return _parent->getAstProps(compiler);
  return compiler->getSyntaxParserEnv();
}

acdk::cfgscript::RProps 
AstNode::createAstProps(IN(RCompiler) compiler)
{
  if (_astProps != Nil)
    return _astProps;
  acdk::cfgscript::RProps parent = getAstProps(compiler);
  return _astProps = new acdk::cfgscript::Props(acdk::cfgscript::PropsEvalDefault, parent);
}

RString 
AstNode::getSourceTextFragment()
{
// ### @todo implement using CodeLocation
  return "";
}

TraverseResult 
AstNode::traverse(IN(RAstNodeVisitor) listener, IN(RCompiler) comp, ParseState parseState, int flags) 
{
  TraverseResult result = listener->onTransform(parseState, this);
  if (result == TraverseSkipChilds)
    return TraverseContinue;
  return result;
}

acdk::aci::util::RCodeLocation 
AstNodeWithChilds::getCodeLocation()
{
  //### @todo implement use the topmost left and right of the child
  return Nil;
}

void 
AstNodeWithChilds::buildSemantic(IN(RCompiler) comp)
{
  for (int i = 0; i < _childs->length(); ++i)
    _childs[i]->buildSemantic(comp);
}

void 
AstNodeWithChilds::genOpCode(IN(RCompiler) comp)
{
  for (int i = 0; i < _childs->length(); ++i)
    _childs[i]->genOpCode(comp);
}

TraverseResult
AstNodeWithChilds::traverse(IN(RAstNodeVisitor) listener, IN(RCompiler) comp, ParseState parseState, int flags)
{
  if (shouldTraverse(flags) == false)
    return TraverseContinue;
  TraverseResult result = listener->onTransform(parseState, this);
  if (result == TraverseSkipChilds)
    return TraverseContinue;

  for (int i = 0; i < _childs->length(); ++i)
  {
    TraverseResult tr = _childs[i]->traverse(listener, comp, parseState, flags);
    if (tr == TraverseSkipSiblings)
      break;
  }
  return TraverseContinue;
}

//virtual 
RAstNode 
AstNodeWithChilds::queryChild(IN(RClass) type, int nth, int deep)
{
  int i;
  for (i = 0; i < _childs->length(); ++i)
  {
    if (type->isAssignableFrom(_childs[i]->getClass()) == true)
    {
      if (nth > 0)
        --nth;
      else
        return _childs[i];
    }
  }
  if (deep == 0)
    return Nil;
  for (i = 0; i < _childs->length(); ++i)
  {
    RAstNode ret = _childs[i]->queryChild(type, nth, deep - 1);
    if (ret != Nil)
      return ret;
  }
  return Nil;
}

RAstNode 
AstNodeWithChilds::queryChild(IN(RString) nodeName, int nth, int deep)
{
  int i;
  for (i = 0; i < _childs->length(); ++i)
  {
    if (nodeName->equals(_childs[i]->getNodeName()) == true)
    {
      if (nth > 0)
        --nth;
      else
        return _childs[i];
    }
  }
  if (deep == 0)
    return Nil;
  for (i = 0; i < _childs->length(); ++i)
  {
    RAstNode ret = _childs[i]->queryChild(nodeName, nth, deep - 1);
    if (ret != Nil)
      return ret;
  }
  return Nil;
}

} // ast
} // aci
} // acdk

