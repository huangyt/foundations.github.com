// -*- mode:C++; tab-width:2; c-basic-offset:2; indent-tabs-mode:nil -*- 
//
// Copyright (C) 2000-2005 by Roger Rene Kommer / artefaktur, Kassel, Germany.
// ALL RIGHTS RESERVED
// 
// This file is part of ACDK.
// artefaktur provides this software "as is" without express or implied warranty.
// Any commercial use of this software requires a license.
// 

#ifndef acdk_aci_SymbolTable_h
#define acdk_aci_SymbolTable_h

#include "DClazzInfo.h"
#include <acdk/lang/ref/WeakReference.h>
#include <acdk/util/HashMap.h>


namespace acdk {
namespace aci {



ACDK_DECL_CLASS(VarDefinition);

class ACDK_ACI_PUBLIC VarDefinition
: extends acdk::lang::Object
, implements SemanticElem
{
  ACDK_WITH_METAINFO(VarDefinition)
  //DECL_ACDK_DEFAULT_METACLASS(SemanticElem)
  /// name of Type, may be empty for anonymous types
  RString _name;
  RDClazzInfo _type;
  RDClazzFieldInfo _fieldInfo;
  int _dflags;
public:
  
  VarDefinition()
  {
  }
  VarDefinition(int flags, IN(RString) n, RDClazzInfo tp = Nil)
  : _name(n)
  , _type(tp)
  {
  }
  VarDefinition(IN(RDClazzFieldInfo) fieldInfo)
  : _fieldInfo(fieldInfo)
  {
    _name = fieldInfo->getName();
    _type = fieldInfo->getType();
  }
  VarDefinition(const VarDefinition& vd)
    : _name(vd._name)
    , _type(vd._type)
    , _fieldInfo(vd._fieldInfo)
  {
  }
  int hashCode() { return _name->hashCode(); }
  virtual RString getName() { return _name; }
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op) { return _type->findSubSem(str, op); }
  virtual RDClazzInfo getType() 
  { 
    if (_type == Nil)
      return Nil;
    return _type->getType(); 
  }
  RDClazzFieldInfo getFieldInfo() { return _fieldInfo; }
  int& getFlags() { return _dflags; }
  void printVar(IN(acdk::io::RPrintWriter) out, IN(RString) indent);
};
/*
inline bool operator<(IN(RVarDefinition) f, IN(RVarDefinition) s)
{
  return f->getName()->compareTo(s->getName()) < 0;
}*/


ACDK_DECL_CLASS(NamespaceDefinition);

class ACDK_ACI_PUBLIC NamespaceDefinition
: extends acdk::lang::Object
, implements SemanticElem
{
  ACDK_WITH_METAINFO(NamespaceDefinition)

  /// name of Type, may be empty for anonymous types
  RString _name;

  /// currently not used
  int _flags;
public:
  NamespaceDefinition(IN(RString) unitname)
  : _name(unitname)
  , _flags(0)
  {
  }
  int& getFlags() { return _flags; }
  virtual RString getName() { return _name; }
  virtual RDClazzInfo getType() { return Nil; }
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);
};


ACDK_DECL_CLASS(SymbolTable);

class ACDK_ACI_PUBLIC SymbolTable
: extends acdk::lang::Object
{
  ACDK_WITH_METAINFO(SymbolTable)
public:
  int _flags;
  /// String -> DClazzInfo 
  acdk::util::RHashMap _typeMap;
  /// String -> VarDefinition
  //acdk::util::RHashMap _varMap;
  RVarDefinitionArray _variables;
  int _blockStart;
  /// contains RSymbolTable
  acdk::lang::ref::RWeakReferenceArray _seeAlsoTypes;
  acdk::lang::ref::RWeakReferenceArray _seeAlsoVars;
  RDClazzInfoArray _classes;
  RDClazzMethodInfo _curMethod;
  RSemanticElemArray _seeAlsoSemElements;
  int _tvarCounter;
public:
  enum Flags
  {
    // Type of this SymbolTable
    IsLocal     = 0x0000001,
    IsUnit      = 0x0000002,
    IsClass     = 0x0000003,
    StTypeMask  = 0x000000F,
    /**
      Just an alias for another
      SymbolTable
    */
    IsAlias     = 0x0000010,
    /**
      is child of another StackFrame
    */
    IsChild     = 0x0000020
  };
  SymbolTable(IN(RSymbolTable) parent = Nil);
  ~SymbolTable();
  virtual RDClazzInfo getType(IN(RString) n);
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);

  virtual RVarDefinition getVar(IN(RString) n);

  virtual int newVarDecl(int flags, IN(RString) type, IN(RString) varname, int pos = -1);
  virtual int newVarDecl(int flags, IN(RDClazzInfo) type, IN(RString) varname, int pos = -1);
  virtual int newVarDecl(IN(RVarDefinition) vd, int pos = -1);
  /**
    creates temporary var.
    @return Var Index
  */
  virtual int createTempVar(IN(RDClazzInfo) type);

  virtual void newType(IN(RDClazzInfo) td);
  virtual void addSeeAlsoType(IN(RSymbolTable) st)
  {
    _seeAlsoTypes->append(new acdk::lang::ref::WeakReference((RObject)&st));
  }
  virtual void addSeeAlsoVar(IN(RSymbolTable) st)
  {
    _seeAlsoVars->append(new acdk::lang::ref::WeakReference((RObject)&st));
  }
  void addSeeAlsoSem(IN(RSemanticElem) sem)
  {
    if (_seeAlsoSemElements == Nil)
      _seeAlsoSemElements = new SemanticElemArray(0);
    _seeAlsoSemElements->append(sem);
  }
  /** 
    returns either function wrapper class or Class
  */
  virtual RDClazzInfo getMethodClazzInfo(IN(RString) name = Nil);
  virtual void setCurMethod(IN(RDClazzMethodInfo) cm) { _curMethod = cm; }
  /**
    find the outer clazz definition, connected with the current method
    Searches also in the seealso types.
  */
  virtual RDClazzMethodInfo getCurMethod();
  
  
  virtual int getVarIndex(IN(RString) str);
  virtual int getVarMaxIndex() { return _seeAlsoVars == Nil ? 0 : _seeAlsoVars->length(); }
  /**
    return true if variable in current local block (idx >= _blockStart)
    defined.
  */
  bool isVarInLocalBlockDefined(IN(RString) name);

  virtual void printSymbolTable(IN(acdk::io::RPrintWriter) out, IN(RString) indent);
  virtual void printVars(IN(acdk::io::RPrintWriter) out, IN(RString) indent, bool all = false);

  void setFlag(int flag) { _flags |= flag; }
  void setFlag(int mask, int val) { _flags = (_flags & ~mask) | val; }
  int getFlag(int mask) { return (_flags & mask); }
};

ACDK_DECL_CLASS(AliasSymbolTable);

class ACDK_ACI_PUBLIC AliasSymbolTable
: extends SymbolTable
{
  RSymbolTable _alias;
public:
  
  AliasSymbolTable(IN(RSymbolTable) aliased);

  virtual RDClazzInfo getType(IN(RString) typname) { return _alias->getType(typname); }
  virtual RSemanticElem findSubSem(IN(RString) sem, IN(RString) op) { return _alias->findSubSem(sem, op); }
  virtual RVarDefinition getVar(IN(RString) n) { return _alias->getVar(n); }
  virtual int newVarDecl(IN(RVarDefinition) vd, int pos = -1) { return _alias->newVarDecl(vd, pos); }
  virtual int createTempVar(IN(RDClazzInfo) type) { return _alias->createTempVar(type); }
  virtual int getVarIndex(IN(RString) str) { return _alias->getVarIndex(str); }
  virtual int getVarMaxIndex() { return _alias->getVarMaxIndex(); }
  virtual void newType(IN(RDClazzInfo) td) { _alias->newType(td); }
  virtual RDClazzInfo getMethodClazzInfo(IN(RString) name = Nil) { return _alias->getMethodClazzInfo(name); }
  virtual void setCurMethod(IN(RDClazzMethodInfo) cm) { _alias->setCurMethod(cm); }
  virtual RDClazzMethodInfo getCurMethod() { return _alias->getCurMethod(); }
  virtual void printSymbolTable(IN(acdk::io::RPrintWriter) out, IN(RString) indent) { _alias->printSymbolTable(out, indent); }
  
};

ACDK_DECL_CLASS(GlobalSymbolTable);

class ACDK_ACI_PUBLIC GlobalSymbolTable
: extends SymbolTable
{
public:
  GlobalSymbolTable() : SymbolTable() {}
  virtual RDClazzInfo getType(IN(RString) n);
  virtual RSemanticElem findSubSem(IN(RString) str, IN(RString) op);
};

} // acdk
} // aci

inline 
bool
operator<(IN(acdk::aci::RVarDefinition) a, IN(acdk::aci::RVarDefinition) b)
{
  if (a == Nil || a->getName() == Nil)
  {
    if (b == Nil || b->getName() == Nil)
      return false;
    return true;
  }
  if (b == Nil || b->getName() == Nil)
    return false;
  return a->getName()->compareTo(b->getName()) > 0;
}

#endif //acdk_aci_SymbolTable_h
