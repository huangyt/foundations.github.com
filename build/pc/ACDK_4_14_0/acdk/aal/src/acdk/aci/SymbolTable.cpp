// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 


#include "SymbolTable.h"
//#include "OpCode.h"
#include <acdk/io/PrintWriter.h>
#include <acdk/lang/System.h>

namespace acdk {
namespace aci {

using namespace acdk::lang::ref;


RSemanticElem 
NamespaceDefinition::findSubSem(IN(RString) str, IN(RString) op)
{
  RString fqname = _name + "/" + str;
  RString fqasc = fqname->convert(CCUtf8);
  const char* ns = fqasc->c_str();
  const acdk::lang::dmi::ClazzInfo* ci =  acdk::lang::dmi::ClazzInfo::getClazzInfoRoot();
  while (ci != 0)
  {
    if (strstr(ci->ns, ns) == ci->ns)
    {
      if (strlen(ci->ns) == strlen(ns))
        return new NamespaceDefinition(fqname);
      if (*(ci->ns + strlen(ns)) == '/')
        return new NamespaceDefinition(fqname);
    }
    ci = ci->_next;
  }
  ci = acdk::lang::dmi::ClazzInfo::findClazzInfo(fqname);
  if (ci != 0)
    return (RSemanticElem)DClazzInfo::getInstance(ci);
  // no throw, because may have to search in muliple ns
  return Nil;
}

SymbolTable::SymbolTable(IN(RSymbolTable) parent)
: _flags(0)
, _blockStart(-1)
, _tvarCounter(0)
{
  setFlag(StTypeMask, IsLocal);

  if (parent != Nil)
  {
     _typeMap = parent->_typeMap;
     _variables = new VarDefinitionArray(parent->_variables->length());
     for (int i = 0; i < _variables->length(); ++i)
      _variables[i] = parent->_variables[i];
    _blockStart = _variables->length();
    _seeAlsoTypes = parent->_seeAlsoTypes;
    _seeAlsoVars = parent->_seeAlsoTypes;
    _classes = parent->_classes;

    //_seeAlsoTypes->append(new acdk::lang::ref::WeakReference((RObject)&parent));
  } 
  else
  {
    _typeMap = new acdk::util::HashMap();
    _variables = new VarDefinitionArray(0);
    _seeAlsoTypes = new acdk::lang::ref::WeakReferenceArray(0);
    _seeAlsoVars = new acdk::lang::ref::WeakReferenceArray(0);
    _classes = new DClazzInfoArray(0);
  }
}

SymbolTable::~SymbolTable()
{
  if (_blockStart != -1)
  {
    for (int i = _variables->length() - 1; i > _blockStart; --i)
      _variables->remove(i);
  }
}

RSemanticElem 
SymbolTable::findSubSem(IN(RString) n, IN(RString) op)
{
  RObject obj = _typeMap->get(&n);
  if (obj != Nil)
    return RSemanticElem(obj);
  int idx = getVarIndex(n);
  if (idx != -1)
    return (RSemanticElem)_variables[idx];
  if (_seeAlsoSemElements != Nil)
  {
    for (int i = 0; i < _seeAlsoSemElements->length(); ++i)
    {
      RSemanticElem td = _seeAlsoSemElements[i]->findSubSem(n, op);
      if (td != Nil)
        return td;
    }
  }
  for (int i = 0; i < _seeAlsoTypes->length(); ++i)
  {
    RSymbolTable t = (RSymbolTable)_seeAlsoTypes[i]->get();
    if (t != Nil)
    {
      RSemanticElem td = t->findSubSem(n, op);
      if (td != Nil)
        return td;
    }
  }
  return Nil;
}

RDClazzInfo 
SymbolTable::getType(IN(RString) n)
{
  RObject obj = _typeMap->get(&n);
  if (obj != Nil)
    return RDClazzInfo(obj);
  
  for (int i = 0; i < _seeAlsoTypes->length(); ++i)
  {
    RSymbolTable t = (RSymbolTable)_seeAlsoTypes[i]->get();
    if (t != Nil)
    {
      RDClazzInfo td = t->getType(n);
      if (td != Nil)
        return td;
    }
  }
  if (_seeAlsoSemElements != Nil)
  {
    for (int i = 0; i < _seeAlsoSemElements->length(); ++i)
    {
      RSemanticElem td = _seeAlsoSemElements[i]->findSubSem(n, "");
      if (td != Nil && instanceof(td, DClazzInfo) == true)
        return RDClazzInfo(td);
    }
  }
  return Nil;
}

int 
SymbolTable::createTempVar(IN(RDClazzInfo) type)
{
  StringBuffer sb("__tvar");
  sb.append(++_tvarCounter);
  return newVarDecl(0, type, sb.toString());
}

int 
SymbolTable::getVarIndex(IN(RString) str)
{
  for (int i = _variables->length() - 1; i >= 0; --i)
  {
    if (_variables[i]->getName()->equals(str) == true)
      return i;
  }
  return -1;
}

RVarDefinition 
SymbolTable::getVar(IN(RString) n)
{
  int idx = getVarIndex(n);
  if (idx != -1)
    return _variables[idx];

  WeakReferenceArray::array_iterator it = _seeAlsoVars->begin();
  for (; it < _seeAlsoVars->end(); ++it)
  {
    RSymbolTable t = (RSymbolTable)(*it)->get();
    RVarDefinition td = t->getVar(n);
      if (td != Nil)
        return td;
  }
  return Nil;
}


void 
SymbolTable::newType(IN(RDClazzInfo) td)
{
  RString cname = td->getName();
  if (_typeMap->containsKey(&cname) == true)
    THROW1(Exception, "Type with name already exists: " + td->getName());
  _typeMap->put(&cname, &td);
}

int 
SymbolTable::newVarDecl(int flags, IN(RString) tn, IN(RString) vn, int pos)
{
  
  RDClazzInfo td = getType(tn);
  if (td == Nil)
    THROW1(Exception, "No such type: " + tn);
  return newVarDecl(flags, td, vn, pos);
}

int 
SymbolTable::newVarDecl(int flags, IN(RDClazzInfo) td, IN(RString) vn, int pos)
{
  return newVarDecl(new VarDefinition(flags, vn, td), pos);
}

bool 
SymbolTable::isVarInLocalBlockDefined(IN(RString) name)
{
  int idx = getVarIndex(name);
  if (idx == -1)
    return false;
  if (idx < _blockStart)
    return false;
  return true;
}

int 
SymbolTable::newVarDecl(IN(RVarDefinition) vd, int pos )
{
  if (isVarInLocalBlockDefined(vd->getName()))
    THROW1(Exception, "Variable with same name already exists: " + vd->getName());
  if (pos == -1)
  {
    _variables->append(vd);
    System::out->println("Create Var " + vd->getName() + " at idx: " +  (_variables->length() - 1));
    return _variables->length() - 1;
  }
  System::out->println("Create Var " + vd->getName() + " at idx: " +  pos);
  _variables->insert(pos, vd);
  return pos;
}



RDClazzInfo 
SymbolTable::getMethodClazzInfo(IN(RString) name)
{
  
  if (name != Nil)
  {
    for (DClazzInfoArray::array_iterator it = _classes->begin(); it < _classes->end(); ++it)
    {
      if ((*it)->getName()->equals(name) == true)
        return *it;
    }
  }
  RDClazzInfo ret = new DClazzInfo(name);
  _classes->append(ret);
  return ret;
}




RDClazzMethodInfo 
SymbolTable::getCurMethod() 
{ 
  if (_curMethod != Nil)
    return _curMethod; 
  for (int i = 0; i < _seeAlsoTypes->length(); ++i)
  {
    RSymbolTable t = (RSymbolTable)_seeAlsoTypes[i]->get();
    if (t != Nil)
    {
      RDClazzMethodInfo mi = t->getCurMethod();
      if (mi != Nil)
        return mi;
    }
  }
  return Nil;
}

void 
SymbolTable::printSymbolTable(IN(acdk::io::RPrintWriter) out, IN(RString) indent)
{
  printVars(out, indent, true);
}

void 
SymbolTable::printVars(IN(acdk::io::RPrintWriter) out, IN(RString) indent, bool all)
{
  for (VarDefinitionArray::array_iterator vdit = _variables->begin(); vdit < _variables->end(); ++vdit)
  {
    (*vdit)->printVar(out, indent);
  }
  if (all == false)
    return;
  for (int i = 0; i < _seeAlsoVars->length(); ++i)
  {
    out->println(indent + "See also: [");
    RSymbolTable st = (RSymbolTable)_seeAlsoVars[i]->get();
    st->printSymbolTable(out, indent + " ");
    out->println(indent + "]");
  }
}

void 
VarDefinition::printVar(IN(acdk::io::RPrintWriter) out, IN(RString) indent)
{
  out->println(indent + _type->getName() + " " + _name);
}

AliasSymbolTable::AliasSymbolTable(IN(RSymbolTable) aliased)
: SymbolTable()
, _alias(aliased)
{
  _flags |= IsAlias;
}

RDClazzInfo 
GlobalSymbolTable::getType(IN(RString) n)
{
  RDClazzInfo  td = SymbolTable::getType(n);
  if (td != Nil)
    return td;
  
  RClass cls = Class::findClass(n); // throws Ex
  if (cls != Nil)
  {
    td = DClazzInfo::getInstance(cls);
    newType(td);
  }
  return td;
}




//virtual 
RSemanticElem 
GlobalSymbolTable::findSubSem(IN(RString) str, IN(RString) op)
{
  RSemanticElem  sem = SymbolTable::findSubSem(str, op);
  if (sem != Nil)
    return sem;
  {
    RString ascns = str->convert(CCUtf8);
    const char* ns = ascns->c_str();
    const acdk::lang::dmi::ClazzInfo* ci =  acdk::lang::dmi::ClazzInfo::getClazzInfoRoot();
    while (ci != 0)
    {
      if (strstr(ci->ns, ns) == ci->ns)
      {
        if (strlen(ci->ns) == strlen(ns))
          return new NamespaceDefinition(str);
        if (*(ci->ns + strlen(ns)) == '/')
          return new NamespaceDefinition(str);
      }
      ci = ci->_next;
    }
  }
  const acdk::lang::dmi::ClazzInfo* ci = acdk::lang::dmi::ClazzInfo::findClazzInfo(str);
  if (ci != 0)
  {
    RDClazzInfo td = DClazzInfo::getInstance(ci);
    newType(td);
    return &td;
  }
  return Nil;
}



} // acdk
} // aci


