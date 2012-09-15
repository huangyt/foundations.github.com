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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/ast/AciCodeAttributeData.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_ast_AciCodeAttributeData_h
#define acdk_aci_ast_AciCodeAttributeData_h

#include <acdk.h>
#include "AstNode.h"


namespace acdk {
namespace aci {
namespace ast {


ACDK_DECL_CLASS(AciCodeAttributeData);

/**
  Attribute attached to ClazzMethodInfo
  which holds the OpCodes of the method
*/
class ACDK_ACI_PUBLIC AciCodeAttributeData
: extends acdk::lang::Object
{
public:
  RAstNode _astNode;
  acdk::aci::vm::RExecutableArray _code;
  AciCodeAttributeData(IN(RAstNode) astNode, IN(acdk::aci::vm::RExecutableArray) code)
  : _astNode(astNode)
  , _code(code)
  {}

};

} // ast
} // aci
} // acdk



#endif //acdk_aci_ast_AciCodeAttributeData_h
