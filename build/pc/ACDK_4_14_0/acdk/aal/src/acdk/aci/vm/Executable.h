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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/vm/Executable.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_vm_Executable_h
#define acdk_aci_vm_Executable_h

#include <acdk.h>
#include "../Config.h"
#include "../ast/AstNodeVisitor.h"


namespace acdk {
namespace aci {
namespace vm {

ACDK_DECL_CLASS(EvalEnv);

ACDK_DECL_INTERFACE(Executable);

/**
  Execute something inside the VM
*/
class ACDK_ACI_PUBLIC Executable
      ACDK_INTERFACEBASE
{
  ACDK_WITH_METAINFO(Executable)
public:
  virtual void execute(IN(REvalEnv) env);

  virtual void execute_inner(IN(REvalEnv) env) = 0;
  //virtual OpCodeOp getOpCode() = 0;
  
  virtual void printOpCode(IN(acdk::io::RPrintWriter) out) = 0;
  /**
    return branch label, should return Nil if has no label
    @returns Nil by default
  */
  virtual RString getLabel() { return Nil; }
  
};


ACDK_DECL_CLASS(ExecutableCollector);
class  ACDK_ACI_PUBLIC ExecutableCollector
: extends acdk::lang::Object
, implements acdk::aci::ast::AstNodeVisitor
{
public:
  RExecutableArray _exes;
  ExecutableCollector()
    : _exes(new ExecutableArray(0))
  {
  }
  virtual acdk::aci::ast::TraverseResult onTransform(acdk::aci::ast::ParseState parseState, IN(acdk::aci::ast::RAstNode) code);
  RExecutableArray getExecutables() { return _exes; }
};

} // vm
} // aci
} // acdk



#endif //acdk_aci_vm_Executable_h
