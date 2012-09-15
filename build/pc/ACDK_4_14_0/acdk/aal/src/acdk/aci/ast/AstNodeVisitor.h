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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/ast/AstNodeVisitor.h,v 1.6 2005/02/05 10:44:51 kommer Exp $


#ifndef acdk_aci_ast_AstNodeVisitor_h
#define acdk_aci_ast_AstNodeVisitor_h

namespace acdk {
namespace aci {
namespace ast {

enum ParseState
{
  /**
    AST has been build, but no type information is available
  */
  PSSyntax    = 0x00010,
  /*
    AST is build and has type information
  */
  PSSemantic  = 0x00020,
  /**
    AST is build and has generated OpCodes
  */
  PSOpCode    = 0x00040
};
ACDK_DEF_LIB_ENUM(ACDK_ACI_PUBLIC, ParseState);


enum TraverseResult
{
  TraverseContinue,
  TraverseSkipSiblings,
  TraverseSkipChilds
};
ACDK_DEF_LIB_ENUM(ACDK_ACI_PUBLIC, TraverseResult);


ACDK_DECL_CLASS(AstNode);

ACDK_DECL_INTERFACE(AstNodeVisitor);

/**
  Listener/Transformer of AstNode
*/
class ACDK_ACI_PUBLIC AstNodeVisitor
      ACDK_INTERFACEBASE
{
public:
  /*
    Traversing
    @return how to continue traversing see TraverseResult
  */
  virtual TraverseResult onTransform(ParseState parseState, IN(RAstNode) code) = 0;
};

} // namespace ast
} // namespace aci
} // namespace acdk

#endif //acdk_aci_ast_AstNodeVisitor_h
