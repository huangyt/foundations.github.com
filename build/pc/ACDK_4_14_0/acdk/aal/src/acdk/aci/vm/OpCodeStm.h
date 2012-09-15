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
// $Header: /cvsroot/acdk/acdk/aal/src/acdk/aci/vm/OpCodeStm.h,v 1.4 2005/02/05 10:44:51 kommer Exp $

#ifndef acdk_aci_vm_OpCodeStm_h
#define acdk_aci_vm_OpCodeStm_h

#include <acdk.h>
#include "../ast/AstNode.h"
#include "OpCodeOp.h"
#include "Executable.h"

namespace acdk {
namespace aci {
namespace vm {


enum OpCodeOp;




ACDK_DECL_CLASS(OpCodeStm);

class  ACDK_ACI_PUBLIC OpCodeStm
: extends acdk::aci::ast::AstNode
, implements Executable
{
  ACDK_WITH_METAINFO(OpCodeStm)
public:
  OpCodeOp _op;
  RString _label;
  RString _comment;
  
  RString toString();
  virtual OpCodeOp getOpCode() { return _op; }
  
  OpCodeStm(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, IN(RString) label = Nil, IN(RString) comment = Nil);
  
  static RString opToString(OpCodeOp op);

  foreign static OpCodeOpDescription* getOpCodeDescriptionTable();
  foreign static ROpCodeStm createOpCode(IN(RString) ident, const acdk::lang::dmi::ScriptVar& sv = acdk::lang::dmi::ScriptVar(), IN(RString) label = Nil);
  foreign static ROpCodeStm createOpCodeFnc(OpCodeOp op, IN(RString) name, IN(RString) label, const acdk::lang::dmi::ScriptVar& val)
  {
    return new OpCodeStm(Nil, op, label, Nil);
  }
  virtual void execute_inner(IN(REvalEnv) env);
  virtual RString getCodeName() { return opToString(_op); }

  virtual void printOpCode(IN(acdk::io::RPrintWriter) out);
  
  virtual RString getLabel() { return _label; }
  
  RString getLabelToPrint()
  {
    if (_label == Nil)
      return "";
    return _label + ": ";
  }
  RString getCommentToPrint() 
  {
    if (_comment == Nil)
      return "";
    return " // " + _comment;
  }
  void  printThisNode(IN(acdk::io::RPrintWriter) out, IN(RString) indent)
  {
    printOpCode(out);
  }
};



ACDK_DECL_CLASS(AfOp);
class  ACDK_ACI_PUBLIC AfOp
: extends OpCodeStm
{
  ACDK_WITH_METAINFO(AfOp)
public:
  int _index;
  AfOp(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, int index, IN(RString) label = Nil, IN(RString) comment = Nil) 
  : OpCodeStm(parent, op, label, comment) 
  , _index(index)
  {}
  RString toString();
  virtual void execute_inner(IN(REvalEnv) env);
  virtual void printOpCode(IN(acdk::io::RPrintWriter) out);
  foreign static ROpCodeStm createOpCodeFnc(OpCodeOp op, IN(RString) name, IN(RString) label, const acdk::lang::dmi::ScriptVar& val)
  {
    return new AfOp(Nil, op, val.getIntVar(), label);
  }
};




ACDK_DECL_CLASS(GlobVarOp);

class  ACDK_ACI_PUBLIC GlobVarOp
: extends OpCodeStm
{
  ACDK_WITH_METAINFO(GlobVarOp)
public:
  RString _varName;
  /// used for OCO_CREATEGLOB
  RString _typeName; 
  /// used for OCO_CREATEGLOB
  const acdk::lang::dmi::ClazzInfo* _type;

  GlobVarOp(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, IN(RString) varName, IN(RString) typeName = Nil, IN(RString) label = Nil, IN(RString) comment = Nil) 
  : OpCodeStm(parent, op, label, comment) 
  , _varName(varName)
  , _typeName(typeName)
  , _type(0)
  {}
  GlobVarOp(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, IN(RString) varName, IN(RString) typeName, const acdk::lang::dmi::ClazzInfo* ci, IN(RString) label = Nil, IN(RString) comment = Nil) 
  : OpCodeStm(parent, op, label, comment) 
  , _varName(varName)
  , _typeName(varName)
  , _type(ci)
  {}
  RString toString();
  virtual void execute_inner(IN(REvalEnv) env);
  virtual void printOpCode(IN(acdk::io::RPrintWriter) out);
  foreign static ROpCodeStm createOpCodeFnc(OpCodeOp op, IN(RString) name, IN(RString) label, const acdk::lang::dmi::ScriptVar& val)
  {
    if (val.isUndef() == true)
      return new GlobVarOp(Nil, op, Nil, label);
    else
      return new GlobVarOp(Nil, op, val.getStringVar(), label);
  }
};

ACDK_DECL_CLASS(BranchOp);

class  ACDK_ACI_PUBLIC BranchOp
: extends OpCodeStm
{
  ACDK_WITH_METAINFO(BranchOp)
public:
  int _pc;
  RString _label;
  BranchOp(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, IN(RString) target, IN(RString) label = Nil, IN(RString) comment = Nil) 
  : OpCodeStm(parent, op, label, comment) 
  , _pc(-1)
  , _label(target)
  {}
  RString toString();
  virtual void execute_inner(IN(REvalEnv) env);
  virtual void printOpCode(IN(acdk::io::RPrintWriter) out);
  foreign static ROpCodeStm createOpCodeFnc(OpCodeOp op, IN(RString) name, IN(RString) label, const acdk::lang::dmi::ScriptVar& val)
  {
    return new BranchOp(Nil, op, val.getStringVar(), label);
  }
};

ACDK_DECL_CLASS(OpCodeStm1);

class  ACDK_ACI_PUBLIC OpCodeStm1
: extends OpCodeStm
{
  ACDK_WITH_METAINFO(OpCodeStm1)

  acdk::lang::dmi::ScriptVar _val;
public:
  OpCodeStm1(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, const acdk::lang::dmi::ScriptVar& val, IN(RString) label = Nil, IN(RString) comment = Nil)
  : OpCodeStm(parent, op, label, comment)
  , _val(val)
  {
  }
  OpCodeStm1(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, IN(acdk::lang::dmi::RDmiObject) val, IN(RString) label = Nil, IN(RString) comment = Nil)
  : OpCodeStm(parent, op, label, comment)
  , _val(*val)
  {
  }
  RString toString();
  virtual void execute_inner(IN(REvalEnv) env);
  virtual void printOpCode(IN(acdk::io::RPrintWriter) out);
  foreign static ROpCodeStm createOpCodeFnc(OpCodeOp op, IN(RString) name, IN(RString) label, const acdk::lang::dmi::ScriptVar& val)
  {
    return new OpCodeStm1(Nil, op, val, label);
  }
};


ACDK_DECL_CLASS(ObjectOp);

class  ACDK_ACI_PUBLIC ObjectOp
: extends OpCodeStm
{
  ACDK_WITH_METAINFO(ObjectOp)
protected:
  RString _objectName;
public:
  enum InvocationFlags
  {
    NumberOfArgsMask = 0x000000FF
  };
  
  ObjectOp(IN(acdk::aci::ast::RAstNode) parent, OpCodeOp op, IN(RString) objName, IN(RString) label = Nil, IN(RString) comment = Nil) 
  : OpCodeStm(parent, op, label, comment) 
  , _objectName(objName)
  {}
  virtual void execute_inner(IN(REvalEnv) env);
  virtual void printOpCode(IN(acdk::io::RPrintWriter) out);
  foreign static ROpCodeStm createOpCodeFnc(OpCodeOp op, IN(RString) name, IN(RString) label, const acdk::lang::dmi::ScriptVar& val)
  {
    return new ObjectOp(Nil, op, val.getStringVar(), label);
  }
};


} // vm
} // aci
} // acdk



#endif //acdk_aci_vm_OpCodeStm_h
