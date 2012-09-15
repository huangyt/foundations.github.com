// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#include "AalCompiler.h"
#include "AalObject.h"
#include "ClassDecl.h"
#include <acdk/lang/System.h>
#include <acdk/lang/reflect/Modifier.h>



namespace acdk {
namespace aal {

using namespace acdk::aci;
using namespace acdk::lang::dmi;



void 
ClosureExpr::postParse(IN(RCompiler) comp)
{
  RString closurename = RString("Closure") + comp->getCounter();
  RCode classdecldef = comp->createCode("ClassDeclDef");
  RTypeName closuretn = (RTypeName)comp->createCode("TypeName");
  closuretn->setTypeName(closurename);
  
  classdecldef->addSubNode(&closuretn);
  classdecldef->addSubNode(findCode("DerivedDef", 3));
  /*
  ClassDeclMember:
      ClassMemberAttr:
      FqTypeName:
        TypeName: int
      VarName: i
  */

  RCode arguments = findCode("ClosureArguments", 2);
  RVarDefinitionArray closureArgs = new VarDefinitionArray(0);
  int i;
  for (i = 0; i < arguments->_subNodes->length(); ++i)
  {
    RVarDefinition vd = (RVarDefinition)_parent->findSubSem(RVarName(arguments->_subNodes[i])->getVarName(), "");
    closureArgs->append(vd);
  }
  for (i = 0; i < closureArgs->length(); ++i)
  {
    RCode member = comp->createCode("ClassDeclMember");
    member->addSubNode(comp->createCode("ClassMemberAttr"));
    RCode tp = FqTypeName::createCode(comp, closureArgs[i]->getType()->getName());
    member->addSubNode(tp);
    member->addSubNode(VarName::createCode(comp, closureArgs[i]->getName()));
    classdecldef->addSubNode(member);

  }
  
  /*
  ClassDeclConstructor:
      ClassMemberAttr:
        'public'
      MethodName: AClass
      FunctionParamsDecl:
        Parameter:
          FqTypeName:
            TypeName: int
          VarName: i_
      ClassDeclConstructorInitializerList:
        ClassDeclConstructorInitializer:
          TypeName: i
          Arguments:
            Argument:
              VarName: i_
      FunctionBlock:
        Block:
          Statements:
    */
  RCode constr = comp->createCode("ClassDeclConstructor");
  RCode constrattr = comp->createCode("ClassMemberAttr");
  constrattr->addSubNode(Keyword::createCode(comp, "public"));
  constr->addSubNode(constrattr);
  RCode methodname = VarName::createCode(comp, closurename, "MethodName");
  constr->addSubNode(methodname);
  RCode params = comp->createCode("FunctionParamsDecl");
  for (i = 0; i < closureArgs->length(); ++i)
  {
    RCode param = comp->createCode("Parameter");
    param->addSubNode(FqTypeName::createCode(comp, closureArgs[i]->getType()->getName()));
    param->addSubNode(VarName::createCode(comp, closureArgs[i]->getName() + "_"));
    params->addSubNode(param);
  }
  constr->addSubNode(params);
  RCode cil = comp->createCode("ClassDeclConstructorInitializerList");
  for (i = 0; i < closureArgs->length(); ++i)
  {
    RCode ci = comp->createCode("ClassDeclConstructorInitializer");
    ci->addSubNode(FqTypeName::createCode(comp, closureArgs[i]->getName()));
    RCode args = comp->createCode("Arguments");
    RCode arg = comp->createCode("Argument");
    arg->addSubNode(VarName::createCode(comp, closureArgs[i]->getName() + "_"));
    args->addSubNode(arg);
    ci->addSubNode(args);
    cil->addSubNode(ci);
  }
  constr->addSubNode(cil);


  RCode funcblock = comp->createCode("FunctionBlock");
  RCode block = comp->createCode("Block");
  block->addSubNode(comp->createCode("Statements"));
  funcblock->addSubNode(block);
  constr->addSubNode(funcblock);
  classdecldef->addSubNode(constr);
  RCode parentstatement = findParentCode("Statement");
  RCode statements = comp->createCode("Statements");
  parentstatement->replaceThisWith(statements);
  statements->addSubNode(classdecldef);
  statements->addSubNode(parentstatement);

  // insert the method definitions of the closure
  for (i = 2; i < _subNodes->length(); ++i)
    classdecldef->addSubNode(_subNodes[i]);

  //parentstatement->_parent->insertBefore(classdecldef, parentstatement);
  classdecldef->postParse(comp);
  /*
  NewExpr:
        FqTypeName:
          TypeName: AClass
        Arguments:
          Argument:
            VarName: i
  */
  RCode newex = comp->createCode("NewExpr");
  newex->addSubNode(FqTypeName::createCode(comp, closurename));
  RCode newargs = comp->createCode("Arguments");
  for (i = 0; i < closureArgs->length(); ++i)
  {
    RCode arg = comp->createCode("Argument");
    arg->addSubNode(VarName::createCode(comp, closureArgs[i]->getName()));
    newargs->addSubNode(arg);
  }
  newex->addSubNode(newargs);
  RCode thissic = this;
  replaceThisWith(newex);
  newex->postParse(comp);
  //RTypeName 

  /*
  Statement:
  LVarDecl:
    FqTypeName:
      TypeName: IFace
    VarName: iface
    VarInitializer:
      ClosureExpr:
        ClosureArguments:
          VarName: i
        DerivedDef:
          DerivedInterfaceDef:
            FqTypeName:
              TypeName: IFace
        ClassDeclMethod:
          ClassMemberAttr:
            'public'
          ReturnType:
            FqTypeName:
              TypeName: int
          MethodName: foo
          FunctionParamsDecl:
          FunctionBlock:
            Block:
              Statement:
                ReturnStatement:
                  VarName: i
    ---------------------------------------------------
    Statement:
  ClassDeclDef:
    TypeName: AClass
    DerivedDef:
      DerivedInterfaceDef:
        FqTypeName:
          TypeName: IFace
    ClassDeclMember:
      ClassMemberAttr:
      FqTypeName:
        TypeName: int
      VarName: i
    ClassDeclConstructor:
      ClassMemberAttr:
        'public'
      MethodName: AClass
      FunctionParamsDecl:
        Parameter:
          FqTypeName:
            TypeName: int
          VarName: i_
      ClassDeclConstructorInitializerList:
        ClassDeclConstructorInitializer:
          TypeName: i
          Arguments:
            Argument:
              VarName: i_
      FunctionBlock:
        Block:
          Statements:
    ClassDeclMethod:
      ClassMemberAttr:
        'public'
      ReturnType:
        FqTypeName:
          TypeName: int
      MethodName: foo
      FunctionParamsDecl:
      FunctionBlock:
        Block:
          Statement:
            ReturnStatement:
              VarName: i
Statement:
  LVarDecl:
    FqTypeName:
      TypeName: IFace
    VarName: acls
    VarInitializer:
      NewExpr:
        FqTypeName:
          TypeName: AClass
        Arguments:
          Argument:
            VarName: i
  */
  
}



} // aal
} // acdk


