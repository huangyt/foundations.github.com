// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#include "ClazzSymbolTable.h"

namespace acdk {
namespace aci {


//virtual 
RSemanticElem 
UnitSymbolTable::findSubSem(IN(RString) n, IN(RString) op)
{
  /*
  Unit unit(_unitInfo->getUnitInfo());
  RUnitArray su = unit.getChilds();
  for (int i = 0; i < su->length(); ++i)
  {
    if (su[i]->getName()->equals(n) == true)
      return new TypeDefinition(); //###
  }
  acdk::lang::dmi::UnitInfo* ui = _unitInfo->getUnitInfo();
  acdk::lang::dmi::UnitInfo* p = 
   _packageNext
   */
  return Nil;
}

RDClazzInfo 
UnitSymbolTable::getType(IN(RString) n)
{
  return Nil;
}

ClazzSymbolTable::ClazzSymbolTable() 
: _clazzInfo(Nil)
{
  _clazzInfo = new DClazzInfo();
  setFlag(StTypeMask, IsClass);
}

int 
ClazzSymbolTable::newVarDecl(int flags, IN(RDClazzInfo) type, IN(RString) var, int pos)
{
  _clazzInfo->addField(flags, var, type);
  return -1;
}

RDClazzInfo 
ClazzSymbolTable::getMethodClazzInfo(IN(RString) name)
{
  return _clazzInfo;
}

RSemanticElem
ClazzSymbolTable::findSubSem(IN(RString) name, IN(RString) op)
{
  THROW1(Exception, "Not implemented Yet");
  return Nil;
}


} // acdk
} // aci

