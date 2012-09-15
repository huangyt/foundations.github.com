// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_ClazzSymbolTable_h
#define acdk_aci_ClazzSymbolTable_h

#include "SymbolTable.h"
#include "DClazzInfo.h"

namespace acdk {
namespace aci {


class ACDK_ACI_PUBLIC UnitSymbolTable
: extends SymbolTable
{
  RDUnitInfo _unitInfo;
public:
  UnitSymbolTable(IN(RDUnitInfo) unitInfo) 
  : SymbolTable() 
  , _unitInfo(unitInfo)
  {
  }
  virtual RDClazzInfo getType(IN(RString) n);
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);
};


ACDK_DECL_CLASS(ClazzSymbolTable);

class ACDK_ACI_PUBLIC ClazzSymbolTable
: extends SymbolTable
{
  RDClazzInfo _clazzInfo;
public:
  ClazzSymbolTable();
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);

  virtual int newVarDecl(int flags, IN(RDClazzInfo) type, IN(RString) var, int pos = 0);
  virtual RDClazzInfo getMethodClazzInfo(IN(RString) name = Nil);
/*
  virtual RTypeDefinition getType(IN(RString) n);
  virtual RVarDefinition getVar(IN(RString) n);

  virtual void newVarDecl(IN(RString) type, IN(RString) var);
  
  virtual void newType(IN(RTypeDefinition) td);
  virtual void addSeeAlsoType(IN(RSymbolTable) st)
  {
    _seeAlsoTypes->append(new acdk::lang::ref::WeakReference(&st));
  }
  virtual void addSeeAlsoVar(IN(RSymbolTable) st)
  {
    _seeAlsoVars->append(new acdk::lang::ref::WeakReference(&st));
  }
  */
};

} // acdk
} // aci


#endif //acdk_aci_ClazzSymbolTable_h
