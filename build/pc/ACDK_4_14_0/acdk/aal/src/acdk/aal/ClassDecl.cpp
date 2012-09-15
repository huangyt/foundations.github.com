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



namespace acdk {
namespace aal {

using namespace acdk::aci;
using namespace acdk::lang::dmi;


  //static 
int 
ClassMemberAttr::parseFlags(IN(RString) attrname)
{
  if (attrname->equals("public") == true)
    return MiPublic;
  if (attrname->equals("private") == true)
    return MiPrivate;
  if (attrname->equals("protected") == true)
    return MiProtected;
  if (attrname->equals("static") == true)
    return MiStatic;
  THROW1(Exception, "Unknown class member attribute: " + attrname);
}

void 
ClassMemberAttr::postParse(IN(RCompiler) comp)
{
  if (_subNodes == Nil)
    return;
  for (int i = 0; i < _subNodes->length(); ++i)
  {
    _memberFlags |= parseFlags(_subNodes[i]->getCodeString());
  }
}

void 
ClassDeclMember::postParse(IN(RCompiler) comp)
{
  if (findFlagInParents(ParseImplOnly) != Nil)
    return;
  RString tn = RFqTypeName(findCode("FqTypeName", 2))->getTypeName();
  RClassMemberAttr cmattr = (RClassMemberAttr)findCode("ClassMemberAttr");
  int flags = 0;
  if (cmattr != Nil)
  {
    cmattr->postParse(comp);
    flags = cmattr->getMemberFlags();
  }
  RVarName vn = (RVarName)findCode("VarName");
  RDClazzInfo ci = (RDClazzInfo)_parent->getSemanticElement();
  if (ci->getFlags() & MiCiInterface)
    THROW2(CodeException, "interface " + ci->getName() + " cannot have fields", this);
  RDClazzInfo td = comp->findType(tn, this);

  RDClazzFieldInfo dfi = ci->addField(flags, vn->getVarName(), td);
  dfi->_fieldInfo->accessor = AalObject::FieldAccessor;
  setSemanticElement(&dfi);
  
  RCode initializer = findCode("VarInitializer", 2);
  if (initializer != Nil)
  {
    if ((flags & MiStatic) == 0)
      THROW2(CodeException, "static fields cannot have an initializer: " + dfi->getName(), this);
    initializer->postParse(comp);
  }
}

int
Code_findLabelPos(IN(RExecutableArray) oca, IN(RString) label)
{
  for (int i = 0; i < oca->length(); ++i)
  {
    RString l = oca[i]->getLabel();
    if (l != Nil && l->equals(label) == true)
      return i;
  }
  return -1;
}

void 
ClassDeclMember::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RCode initializer = findCode("VarInitializer", 2);
  
  if (initializer != Nil)
  {
    RDClazzFieldInfo dfi = (RDClazzFieldInfo)getSemanticElement();  
    RDClazzInfo owner = dfi->getOwnerClass();
    RDClazzMethodInfo initmethod = owner->getClassInitializerMethod();
    RExecutableArray methodinitCode = initmethod->getCode();
    RExecutableArray initCode = new ExecutableArray(0);
    
    initializer->emitOpCode(comp, initCode);
    comp->addOpCode(initCode, new OpCodeStm1(OCO_PUSH, this, inOf(owner->getName()), "classname"));
    comp->addOpCode(initCode, new OpCodeStm1(OCO_PUSH, this, inOf(dfi->getName()), "fieldname"));
    comp->addOpCode(initCode, new OpCodeStm1(OCO_PUSH, this, createInvokeFlags(0), "dmi flags"));
    comp->addOpCode(initCode, new OpCodeStm(OCO_POKE_STATIC, this, ""));
    comp->addOpCode(initCode, new OpCodeStm(OCO_POP, this, ""));
    
    int pos = Code_findLabelPos(methodinitCode, "__method_start");
    if (pos == -1)
      pos = 0;
    for (int i = 0; i < initCode->length(); ++i)
      methodinitCode->insert(pos + i, initCode[i]);
  }
}

void 
Parameter::postParse(IN(RCompiler) comp)
{
  RCode mdef = findParentCode(makeStringArray("ClassDeclMethod", "ClassDeclConstructor", "ClassDeclOperator", 
                                              "DefunDecl", "ExtendStatement"), 3);
  
  RDClazzMethodInfo mi = (RDClazzMethodInfo)mdef->getSemanticElement();
  int flags = 0;
  // ### parse flags
  RFqTypeName tn = (RFqTypeName)findCode("FqTypeName");
  RVarName vn = (RVarName)findCode("VarName");
  RString tpname = tn->getTypeName();
  RDClazzInfo td = comp->findType(tpname, this);
  if (td == Nil)
    THROW2(CodeException, "Cannot find type: " + tpname, this); 
  mi->addParameter(vn->getVarName(), td);
  RSymbolTable st = getSymbolTable();
  RVarDefinition vd = new VarDefinition(flags, vn->getVarName(), td);
  st->newVarDecl(vd);
  setSemanticElement(&vd);
}

void 
Parameter::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  // nothing?
}


void 
FunctionParamsDecl::postParse(IN(RCompiler) comp)
{
  Code::postParse(comp);
  getSymbolTable()->printSymbolTable(System::out, "");
}




void 
ClassDeclDef::postParse(IN(RCompiler) comp)
{
  RCode root = getRootNode();
  //root->printParentCodeTree(acdk::lang::System::out, "");
  //printParentCodeTree(acdk::lang::System::out, "");
  RTypeName tn = RTypeName(findCode("TypeName", 2));
	//RFqTypeName fqtn = RFqTypeName(findCode("FqTypeName", 2));
  RDClazzInfo ci = getSymbolTable()->getMethodClazzInfo();
  ci->setName(tn->getTypeName());
  ci->getImplClazzInfo()->static_dispatch = AalObject::StandardDispatch;
  RCode parentns = findParentCode("NamespaceDecl");
  if (parentns != Nil)
  {
    ci->setNamespace(parentns->getSemanticElement()->getName());
  }
  else
  {
    ci->setNamespace("");
  }
  if (getCodeName()->equals("InterfaceDeclDef") == true)
  {
    ci->getFlags() |= MiCiInterface | MiCiAbstract;
  }
  
  ci->registerClazzInfo();

  setSemanticElement(&ci);
  _flags |= ParseDeclOnly;
  Code::postParse(comp);
  ci->setMethodAltNames();
  RDClazzInfo super = ci->getSuperClass();
  if (super != Nil && super->isThrowable() == true)
    ci->getFlags() |= MiCiThrowable;

  if (ci->hasAnyConstructor() == false) // create default constructor
  {
    /*
ClassDeclConstructor:
  ClassMemberAttr:
    'public'
  MethodName: AClass
  FunctionParamsDecl:
  FunctionBlock:
    Block:
    */

    RCode cdc = comp->createCode("ClassDeclConstructor");
    {
      RCode ma = comp->createCode("ClassMemberAttr");
      ma->addSubNode(Keyword::createCode(comp, "public"));
      cdc->addSubNode(ma);
    }
    cdc->addSubNode(VarName::createCode(comp, ci->getName(), "MethodName"));
    cdc->addSubNode(comp->createCode("FunctionParamsDecl"));
    cdc->addSubNode(comp->createCode("ClassDeclConstructorInitializerList"));
    {
      RCode fb = comp->createCode("FunctionBlock");
      fb->addSubNode(comp->createCode("Block"));
      cdc->addSubNode(fb);
    }
    addSubNode(cdc);
    cdc->postParse(comp);
  }

  _flags &= ~ParseDeclOnly;
  _flags |= ~ParseImplOnly;
  Code::postParse(comp);
  _flags &= ~ParseImplOnly;
}

void 
ClassDeclDef::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  Code::emitOpCode(comp, oca);
  RDClazzInfo ci = (RDClazzInfo)getSemanticElement();
  comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(ci->getName()), "class name to initialize"));   
  comp->addOpCode(oca, new OpCodeStm(OCO_INIT_CLAZZ, this, ""));  
  RDClazzMethodInfo initmethod = ci->getClassInitializerMethod(false);
  if (initmethod != Nil)
  {
    RExecutableArray initCode = initmethod->getCode();
    if (initCode->length() > 0)
    {
      if (initCode[initCode->length() - 1]->getOpCode() != OCO_RET)
      {
        comp->addOpCode(initCode, new OpCodeStm1(OCO_PUSH, this, ScriptVar(), "void return"));   
        comp->addOpCode(oca, new BranchOp(OCO_RET, this, "", Nil));
      }
    }
  }
}

void 
DerivedSuperDef::postParse(IN(RCompiler) comp)
{
  if (findFlagInParents(ParseImplOnly) != Nil)
    return;
  RDClazzInfo ci = (RDClazzInfo)getParentSemanticElement();
  RString str = RFqTypeName(findCode("FqTypeName", 2))->getTypeName();
  RDClazzInfo super = comp->findType(str, this);
  if (super == Nil)
    THROW2(CodeException, "Cannot find class: " + str, this);
  ci->addSuper(super);
}

void 
DerivedInterfaceDef::postParse(IN(RCompiler) comp)
{
  if (findFlagInParents(ParseImplOnly) != Nil)
    return;
  RDClazzInfo ci = (RDClazzInfo)getParentSemanticElement();
  for (int i = 0; i < _subNodes->length(); ++i)
  {
    RString str = RFqTypeName(_subNodes[i]->findCode("FqTypeName", 2))->getTypeName();
    RDClazzInfo super = comp->findType(str, this);
    if (super == Nil)
      THROW2(CodeException, "Cannot find class: " + str, this);
    ci->addSuper(super);
  }
}

bool hasConstructorInitializer(IN(RCode) initializerlist, IN(RString) name)
{
  RCodeArray sn = initializerlist->_subNodes;
  if (sn == Nil)
    return false;
  for (CodeArray::iterator it = sn->begin(); it < sn->end(); ++it)
  {
    RCode lh = (*it)->get(0);
    RSemanticElem lhsem = lh->getSemanticElement();
    if (instanceof(lhsem, DClazzMethodInfo) == true)
      lhsem = &lhsem->getOwnerClass();
    RString semname = lhsem->getName();
    if (semname->equals(name) == true)
      return true;
  }
  return false;
}

void 
ClassDeclMethod::postParse(IN(RCompiler) comp)
{
  RCode funcblock = findCode("FunctionBlock", 2);
  RCode params = findCode("FunctionParamsDecl", 2);
  RDClazzInfo ci = (RDClazzInfo)_parent->getSemanticElement();
  if (getSemanticElement() == Nil)
  {
    if (ci == Nil) // than free standing function
    {
      /*
Statement:
  FunctionDeclDef:
    ClassDeclMethod:
      ReturnType:
        FqTypeName:
          TypeName: int
      MethodName: foo
      FunctionParamsDecl:
        Parameter:
          FqTypeName:
            TypeName: int
          VarName: i
      FunctionBlock:
        Block:
          Statement:
            ReturnStatement:
              AdditiveExpr:
                VarName: i
                '+'
                Literal:
                  : 1
Statement:
  ClassDeclDef:
    TypeName: Foo
    DerivedDef:
    ClassDeclOperator:
      ClassMemberAttr:
        'public'
        'static'
      ReturnType:
        FqTypeName:
          TypeName: int
      Operator: ()
      FunctionParamsDecl:
        Parameter:
          FqTypeName:
            TypeName: int
          VarName: i
      FunctionBlock:
        Block:
          Statement:
            ReturnStatement:
              AdditiveExpr:
                VarName: i
                '+'
                Literal:
                  : 1
    */
      //System::out->println("\nBefore function:");
      //printCodeTree(System::out, "");

      RCode orgmn = findCode("MethodName");
      
      RCode clsdecldef = comp->createCode("ClassDeclDef");
      clsdecldef->addSubNode(TypeName::createCode(comp, orgmn->getName()));
      clsdecldef->addSubNode(comp->createCode("DerivedDef"));
      RCode cloperator = comp->createCode("ClassDeclOperator");
      {
        RCode memberattr = comp->createCode("ClassMemberAttr");
        memberattr->addSubNode(Keyword::createCode(comp, "public"));
        memberattr->addSubNode(Keyword::createCode(comp, "static"));
        cloperator->addSubNode(memberattr);
      }
      {
        cloperator->addSubNode(findCode("ReturnType"));
        cloperator->addSubNode(Operator::createCode(comp, "()"));
        int i = getChildIndex(findCode("FunctionParamsDecl"));
        for (; i < _subNodes->length(); ++i)
          cloperator->addSubNode(_subNodes[i]);
        /*
        cloperator->addSubNode(findCode("FunctionParamsDecl"));
        cloperator->addSubNode(findCode("FunctionBlock"));
        */
      }
      clsdecldef->addSubNode(cloperator);
      RCode thissic = this;
      replaceThisWith(clsdecldef);
      System::out->println("\nafter function:");
      clsdecldef->printCodeTree(System::out, "");
      clsdecldef->postParse(comp);
      return;
    }
    RClassMemberAttr cmattr = (RClassMemberAttr)findCode("ClassMemberAttr", 2);
    int flags = 0;
    if (cmattr != Nil)
    {
      cmattr->postParse(comp);
      flags = cmattr->getMemberFlags();
    }
    RCode rettype = findCode("ReturnType", 2);
    RString retstr;
    if (rettype != Nil)
      retstr = RFqTypeName(rettype->_subNodes[0])->getTypeName();
    else
      retstr = "void";
    
    RVarName cmethodname = (RVarName)findCode("MethodName", 2);
    RString methodname;
    if (cmethodname == Nil) // is operator
    {
      ROperator opname = ROperator(findCode("Operator", 2));
      methodname = opname->operatorToFuncName();

    }
    else
      methodname = cmethodname->getName();
  
    RSymbolTable pst = getParentSymbolTable();
    
    RDClazzMethodInfo methodInfo = ci->createMethod(methodname);
    RDClazzInfo td = comp->findType(retstr, this);
    methodInfo->setReturnType(td);

    setSemanticElement(&methodInfo);
   
    acdk::lang::dmi::ClazzMethodInfo* cmi = const_cast<acdk::lang::dmi::ClazzMethodInfo*>(methodInfo->getMethodInfo());

    cmi->flags |= flags;

    if (flags & MiStatic && cmi->flags & MiMiConstructor) // is static class initializer
    {
      cmi->flags &= ~MiMiConstructor;
      ci->setClassInitializerMethod(methodInfo);
    }
    //if (flags & Modifier::| MiPublic;
    params->postParse(comp);
  
    if (methodInfo->isStatic() == false ||  methodInfo->isConstructor())
    {
      params->getSymbolTable()->newVarDecl(0, ci, "this", 0);
      params->getSymbolTable()->printSymbolTable(System::out, "");
      
    }
    if (funcblock == Nil)
    {
      methodInfo->getFlags() |= MiCiAbstract;
      ci->getFlags() |= MiCiAbstract;
    }
  }
  if (findFlagInParents(ParseDeclOnly) == Nil)
  {
  // check if invocation of default super constructors has to be generated
     RDClazzMethodInfo methodInfo(_sem);
    if (methodInfo->isConstructor() == true)
    {
      RCode initlist = findCode("ClassDeclConstructorInitializerList", 2);   
      
      
      
      
      if (initlist != Nil)
      {
        initlist->setSymbolTable(params->getSymbolTable());
        initlist->setSemanticElement(&ci);
        initlist->postParse(comp);
      }
      RCodeArray newInitList = new CodeArray(0);
      RDClazzInfoArray supers = ci->getSuperClasses();
      int inspos = 0;
      int i;
      for (i = 0; i < supers->length(); ++i)
      {
        if (hasConstructorInitializer(initlist, supers[i]->getName()) == false)
        {
          RCode initializer = comp->createCode("ClassDeclConstructorInitializer");
          initializer->addSubNode(FqTypeName::createCode(comp, supers[i]->getName()));
          initializer->addSubNode(comp->createCode("Arguments"));
          initlist->insertSubNode(inspos++, initializer);
          newInitList->append(initializer);
        }
      }
      for (i = 0; i < newInitList->length(); ++i)
        newInitList[i]->postParse(comp);
    }
    if (funcblock != Nil)
    {
      funcblock->setSymbolTable(params->getSymbolTable());
      funcblock->postParse(comp);
    }
  }
}

void 
ClassDeclMethod::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RDClazzMethodInfo cmi(getSemanticElement());

  RExecutableArray ca = cmi->getCode();
  if (cmi->isStatic() == false ||  cmi->isConstructor())
  {
    RCode initlist = findCode("ClassDeclConstructorInitializerList", 2);
    if (initlist != Nil)
    {
      
      initlist->emitOpCode(comp, ca);
    }
  }
  RCode body = findCode("FunctionBlock", 2);
  if (body != Nil)
  {
    comp->addOpCode(ca, new OpCodeStm(OCO_NOP, this, "", "__method_start"));
    body->emitOpCode(comp, ca);
    Compiler::resolveLabels(ca);
  }
  cmi->makeCodeAttribute();

  System::out->println("Function " + cmi->getName() + "[");
  for (int i = 0; i < ca->length(); ++i)
    ca[i]->printOpCode(System::out);
  System::out->println("]");
}


void 
ClassDeclConstructorInitializer::postParse(IN(RCompiler) comp)
{
  printCodeTree(System::out, "");
  RFqTypeName lvar = RFqTypeName(_subNodes[0]);
  RCode rval = _subNodes[1];
  RSemanticElem sem = _parent->getSemanticElement();
  RString lvarname = lvar->getTypeName();
  RString lname = lvar->getLastElemName();
  RSemanticElem lsem = sem->findSubSem(lname, "");
  if (lsem == Nil)
  {
    lsem = sem->findSubSem(lvarname, "");
    if (lsem == Nil)
    {
      lsem = comp->findType(lvarname, this);
      if (lsem == Nil)
        THROW2(CodeException, "Cannot find semantic element: " + lvarname, this);
    }
  }
  if (instanceof(lsem, DClazzMethodInfo) == true) // Super-Initialization
  {
    RDClazzMethodInfo dcmi(lsem);
    if (MetaInfo::checkMemberAccess(MiProtected, dcmi->getFlags()) == false)
    {
      THROW2(CodeException, "Super Constructor needs at least have protected access rights: " + dcmi->toString(), this);
    }
    _subNodes[0]->setSemanticElement(lsem);
  }
  else if (instanceof(lsem, DClazzFieldInfo) == true) // Field-Initialization
  {
    //## check if ArgCount is really 1
    _subNodes[0]->setSemanticElement(lsem);
  }
  else if (instanceof(lsem, DClazzInfo) == true) // super access
  {
    // is Class name of super

    _subNodes[0]->setSemanticElement(lsem);
  }
  else
    THROW2(CodeException, "Unknown Semantic left hand element for constructor initialization: " + lsem->getName(), this);

  rval->postParse(comp);
}

void
ClassDeclConstructorInitializer::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{
  RFqTypeName lvar = RFqTypeName(_subNodes[0]);
  RCode rval = _subNodes[1];
  rval->emitOpCode(comp, oca);
  //RString lname = lvar->getLastElemName();
  RSemanticElem lsem = lvar->getSemanticElement();
  RString lname = lsem->getName();
  int thisidx = getParentSymbolTable()->getVarIndex("this");
  if (instanceof(lsem, DClazzMethodInfo) == true) // Super-Initialization
  {
    //comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(fdef->getName()), "function name"));
   
    RCode arguments = findCode("Arguments");
    int argcount = arguments->getSubNodesCount();

    comp->addOpCode(oca, new AfOp(OCO_LOAD, this, thisidx, "this"));
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(lname), "function name")); 
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, createInvokeFlags(MiIvTransientCall | MiProtected, argcount), 
                                        RString("flags = TRANSIENT | PROTECTED, argcount = ") + argcount));
    comp->addOpCode(oca, new OpCodeStm(OCO_INVOKE, this, Nil));
    comp->addOpCode(oca, new OpCodeStm(OCO_POP, this, "remove invoke result")); 
  }
  else if (instanceof(lsem, DClazzFieldInfo) == true) // Field-Initialization
  {
    comp->addOpCode(oca, new AfOp(OCO_LOAD, this, thisidx, "this"));
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(lname), "fieldname"));
    comp->addOpCode(oca, new OpCodeStm1(OCO_PUSH, this, inOf(0), "dmi flags"));
    comp->addOpCode(oca, new OpCodeStm(OCO_POKE, this, Nil));
    comp->addOpCode(oca, new OpCodeStm(OCO_POP, this, "remove poke result")); 
  }
  else 
    THROW2(CodeException, "unsupported left hand type for constructor initialization", this);
}

void 
NamespaceDecl::postParse(IN(RCompiler) comp)
{
  
  RString ns = RTypeName(_subNodes[0])->getTypeName();
  RCode parentns = findParentCode("NamespaceDecl");
  if (parentns != Nil)
  {
    ns = parentns->getSemanticElement()->getName() + "/" + ns;
  }
  setSemanticElement(new NamespaceDefinition(ns)); // #### UnitInfo not used?!
  
  for (int i = 1; i < _subNodes->length(); ++i)
    _subNodes[i]->postParse(comp);
}

void 
UsingDecl::postParse(IN(RCompiler) comp)
{
  RString ns = RFqTypeName(_subNodes[0])->getTypeName();
  RSymbolTable pst = getParentSymbolTable();
  RNamespaceDefinition nsd = new NamespaceDefinition(ns);
  pst->addSeeAlsoSem(&nsd);
  //Code::postParse(comp);
}

void 
DefunDecl::postParse(IN(RCompiler) comp)
{
  RString defunname = RTypeName(_subNodes[0])->getIdentifier();
  // ## TODO throwable decl

  RDClazzInfo ci = getSymbolTable()->getMethodClazzInfo();
  ci->setName(defunname);
  ci->getImplClazzInfo()->static_dispatch = AalObject::StandardDispatch;
  RCode parentns = findParentCode("NamespaceDecl");
  if (parentns != Nil)
  {
    ci->setNamespace(parentns->getSemanticElement()->getName());
  }
  else
  {
    ci->setNamespace("");
  }
  ci->registerClazzInfo();
  
  RClassMemberAttr cmattr = (RClassMemberAttr)findCode("ClassMemberAttr", 2);
  int flags = 0;
  if (cmattr != Nil)
  {
    cmattr->postParse(comp);
    flags = cmattr->getMemberFlags();
  }
  RCode rettype = findCode("ReturnType", 2);
  RString retstr;
  retstr = RFqTypeName(rettype->_subNodes[0])->getTypeName();
  RString methodname = "operator_po_pc";
  RDClazzMethodInfo methodInfo = ci->createMethod(methodname);
  RDClazzInfo td = comp->findType(retstr, this);
  methodInfo->setReturnType(td);
  setSemanticElement(&methodInfo);
  acdk::lang::dmi::ClazzMethodInfo* cmi = const_cast<acdk::lang::dmi::ClazzMethodInfo*>(methodInfo->getMethodInfo());
  //cmi->flags |= flags;
  cmi->flags |= MiCiInterface;
  RCode params = findCode("FunctionParamsDecl", 2);
  params->postParse(comp);
  
}
void 
ExtendStatement::postParse(IN(RCompiler) comp)
{
  // ClassMemberAttr ReturnType FqTypeName '.'& VarName Arguments FunctionBlock
  RClassMemberAttr mattrc = (RClassMemberAttr)findCode("ClassMemberAttr", 2);
  RCode returnc = findCode("ReturnType", 2);
  RFqTypeName clscode = (RFqTypeName)findCode("FqTypeName", 2);
    
  RCode params = findCode("FunctionParamsDecl", 2);
  RCode funcblockc = findCode("FunctionBlock", 2);
  
  
  
  int flags = 0;
  if (mattrc != Nil)
  {
    mattrc->postParse(comp);
    flags = mattrc->getMemberFlags();
  }
  RString clsname = clscode->getNotLastElemName();
  RString funcname = clscode->getLastElemName();
  RString retstr = RFqTypeName(returnc->_subNodes[0])->getTypeName();
  RDClazzInfo cls = comp->findType(clsname, this);
  RDClazzMethodInfo methodInfo = cls->createMethod(funcname);
  RDClazzInfo returntd = comp->findType(retstr, this);
  methodInfo->setReturnType(returntd);
  setSemanticElement(&methodInfo);
  acdk::lang::dmi::ClazzMethodInfo* cmi = const_cast<acdk::lang::dmi::ClazzMethodInfo*>(methodInfo->getMethodInfo());
  cmi->flags |= flags;
  
  params->postParse(comp);

  setSemanticElement(&cls);
  if (funcblockc != Nil)
  {
    funcblockc->setSymbolTable(params->getSymbolTable());
    funcblockc->postParse(comp);
  }
}

void
ExtendStatement::emitOpCode(IN(RCompiler) comp, IN(RExecutableArray) oca)
{

}



} // aal
} // acdk

