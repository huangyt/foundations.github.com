
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

#include "DmiProxyGeneratorExt.h"
//#include "TypeScope.h"
#include <acdk/lang/System.h>
#include <acdk/lang/Void.h>
#include <acdk/io/PrintWriter.h>
#include <acdk/lang/dmi/ClazzAttributesRes.h>
#include <acdk/util/logging/Log.h>

namespace acdk {
namespace tools {
namespace mc {

using namespace acdk::lang::dmi;


typedef DmiProxyGeneratorExt::ClazzMethodInfoArray ClazzMethodInfoArray;

bool hasMethod( ClazzMethodInfoArray& methods, const ClazzMethodInfo* mi)
{
  for (ClazzMethodInfoArray::iterator it = methods.begin(); it < methods.end(); ++it)
  {
    if (mi->equals(*it, CompareName | CompareArgs) == true)
      return true;
  }
  return false;
}

void
ClazzInfo_getVirtualMethods(const ClazzInfo* ci, DmiProxyGeneratorExt::ClazzMethodInfoArray& methods)
{
  int i;
  
  for (i = 0; i < ci->getMethodsCount(); ++i)
  {
    const ClazzMethodInfo* mi = ci->methods[i];
    if (mi->flags & MiMiVirtual && mi->flags & MiMiOrgPoly && (mi->flags & MiNoDmiProxy) == 0)
    {
      if (hasMethod(methods, mi) == false)
        methods.push_back(mi);
    }
  }
  for (i = 0; i < ci->getInterfacesCount(); ++i)
  {
    if (ci->interfaces[i]->type == 0)
    {
      ACDK_LOG(Warn, SBSTR("An interface of " <<  ci->name << " is not defined"));      
    }
    else
      ClazzInfo_getVirtualMethods(ci->interfaces[i]->type, methods);
  }
}

bool
ClazzInfo_findConstructors(const ClazzInfo* ci, ClazzMethodInfoArray& methods)
{
  int i;
  bool hasPrivate = false;
  for (i = 0; i < ci->getMethodsCount(); ++i)
  {
    const ClazzMethodInfo* mi = ci->methods[i];
    if (mi->flags & MiMiConstructor)
    {
      if (mi->flags & MiPrivate)
        hasPrivate = true;
      else //if (mi->flags & MiMiOrgPoly)
        methods.push_back(mi);
    }
  }
  return methods.size() > 0 || hasPrivate == false;
}

void 
DmiProxyGeneratorExt::generateProxyMethodArgDecl(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb)
{
  for (int i = 0; i < mi->getArgumentCount(); ++i)
  {
    if (i > 0)
      sb << ", ";
    mi->methodArgs[i]->toTypeString(sb, TpFtFormatStandard);
  }
}
void 
DmiProxyGeneratorExt::generateProxyMethodThrowDecl(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb)
{
  int count = mi->getExceptionsCount();
  if (count == 0)
    return ;
  sb << " THROWS" << count << "(";
  for (int i = 0;  i < count; ++i)
  {
    if (i > 0)
      sb << ", ";
    mi->exceptions[i]->toTypeString(sb, TpFtFormatStandard);
  }
  sb << ")";
}

RString getArgEnumCaster(const ClazzMethodArgInfo* ai)
{
  ClazzAttributeResValue attr = ClazzAttributesRes::getAttribute((MetaInfo*)ai, "__enumArgInfo");
  const ClazzEnumInfo* ei = (const ClazzEnumInfo*)attr.data; 
  if (ei != 0 && ei->name != 0)
    return "(" + ei->toTypeString(TpFtAcdkType | TpFtFqName) + ")";
  return "";
}

void 
DmiProxyGeneratorExt::generateProxyMethodArgCall(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb)
{
  for (int i = 0; i < mi->getArgumentCount(); ++i)
  {
    if (i > 0)
      sb << ", ";

   if (mi->methodArgs[i]->type->isIntClazz() == true &&  
      ClazzAttributesRes::hasAttribute((MetaInfo*)mi->methodArgs[i], "__enumArgInfo"))
   {
    sb << getArgEnumCaster(mi->methodArgs[i]);
   }
    sb << (char*)mi->methodArgs[i]->name;
  }
}

void 
DmiProxyGeneratorExt::generateProxyClassDeclDefConstructor(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb)
{
  RString name = mi->name;
  sb
    << "  " << name << "_DmiProxy("
    ;
  generateProxyMethodArgDecl(mi, sb);
  sb << ")";
  generateProxyMethodThrowDecl(mi, sb);
  sb << "\n  : " << name << "(";
  generateProxyMethodArgCall(mi, sb);
  sb 
    << 
    ")\n"
    "  {\n"
    "     clazzInfo()->_resolveSupers(true, false);\n"
    "     ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_initThis(this);\n"
    "  }\n"
    ;
}

int getMethodIndex(const ClazzInfo* ci, const ClazzMethodInfo* mi)
{
  for (int i = 0; i < ci->getMethodsCount(); ++i)
    if (ci->methods[i] == mi)
      return i;
  return -1;
}

bool
hasEnumReturn(const ClazzMethodInfo* mi)
{
  return ClazzAttributesRes::hasAttribute((MetaInfo*)mi, "__enumArgInfo");

  
}
RString getReturnType(const ClazzMethodInfo* mi)
{
  if (hasEnumReturn(mi) == false)
  {
    StringBuffer sb;
    mi->returnType->toTypeString(sb, TpFtFormatStandard);
    return sb.toString();
  }
  ClazzAttributeResValue attr = ClazzAttributesRes::getAttribute((MetaInfo*)mi, "__enumArgInfo");
  
  const ClazzEnumInfo* ei = (const ClazzEnumInfo*)attr.data; 
  if (ei == 0)
  {
    ACDK_LOG(Error, SBSTR("Return enum type of method " <<  mi->name << " is not defined"));
    return "int";
  }
  return ei->toTypeString(TpFtAcdkType | TpFtFqName);
}

bool checkValidMethod(const acdk::lang::dmi::ClazzMethodInfo* mi)
{
  if (mi->returnType == 0)
  {
    ACDK_LOG(Warn, SBSTR("Return type of method " <<  mi->name << " is not defined"));
    return false;
  }
  int argcount = mi->getArgumentCount();
  for (int i = 0; i < argcount; ++i)
  {
    const ClazzMethodArgInfo* ai = mi->methodArgs[i];
    if (ai->type == 0)
    {
      ACDK_LOG(Warn, SBSTR("Argument type of argument " << ai->name  << " of method " <<  mi->name << " is not defined"));
      return false;
    }
  }
  return true;
}

void 
DmiProxyGeneratorExt::generateProxyClassMethod(const acdk::lang::dmi::ClazzInfo* super, const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb)
{
  if (checkValidMethod(mi) == false)
    return;
  const ClazzInfo* ci = (const ClazzInfo*)mi->_scopeParent;
  RString name = mi->name;
  RString cname = ci->name;
  RString supername = super->name;
  sb 
    << "  ";
  // mi->returnType->toTypeString(sb, TpFtFormatStandard);
  sb << getReturnType(mi);
  sb << " " << name << "(";
  generateProxyMethodArgDecl(mi, sb);
  sb << ")";
  generateProxyMethodThrowDecl(mi, sb);
  int argcount = mi->getArgumentCount();
  int i;
  
  sb 
    << "\n  {\n"
    ;
  if ((mi->flags & MiPrivate) == 0 && (mi->flags & MiMiAbstract) == 0)
  {
    int methodidx = getMethodIndex(ci, mi);
    sb << "    if (_dmiProxyIsOverloaded(getClazzInfo(), ";
    
    const acdk::lang::dmi::ClazzInfo* methodclazz = reinterpret_cast<const acdk::lang::dmi::ClazzInfo*>(mi->_scopeParent);
    
    RString methodSuper = SBSTR("ACDK_FQ_SUPER_QUALIFIER(::" << RString(methodclazz->ns)->replace("/", "::") << "::, " << methodclazz->name << ")");
    RString methodBase =  SBSTR("ACDK_FQ_SUPER_QUALIFIER(::" << RString(super->ns)->replace("/", "::") << "::, " << super->name << ")");
    
    //acdk::lang::System::out->println("method: " + supername + "::" + name + "; methodSuper: " + methodSuper + "; " + methodBase);
    
    sb << methodSuper << "::clazzInfo()->methods[" << methodidx << "]) == false)\n    {\n"
       << "      ";
    if ((void*)mi->returnType != (void*)Void::getTYPE()->objectClazzInfo())
      sb << "return ";
    sb << methodBase << "::" << name << "(";

    for (i = 0; i < argcount; ++i)
    {
      const ClazzMethodArgInfo* ai = mi->methodArgs[i];
      if (i > 0)
        sb << ", ";
      sb << (RString)ai->name;
    }
    sb << ");\n";
    if ((void*)mi->returnType == (void*)Void::getTYPE()->objectClazzInfo())
      sb << "      return;\n";
    sb << "    }\n";
  }
  sb 
    << "    ::acdk::lang::dmi::ScriptVar __acdk_retval;\n"
    << "    ::acdk::lang::dmi::ScriptVarArray __acdk_args(" << argcount << ");\n"
   ;
  
  for (i = 0; i < argcount; ++i)
  {
    const ClazzMethodArgInfo* ai = mi->methodArgs[i];
    RString argname = ai->name;
    sb << RString("    __acdk_args[") << i << "] = ";
    if (ai->flags & MiAiOut && ai->flags & MiAiIn)
      sb << "::acdk::lang::inoutOf(" << argname << ");\n";
    else if (ai->flags & MiAiOut)
      sb << "::acdk::lang::outOf(" << argname << ");\n";
    else
      sb << "::acdk::lang::inOf(" << argname << ");\n";
  }
  sb << "    _dmiProxyGetTarget()->standardDispatch(\"" << name
     << "\", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiProxyGetTarget()->clazzInfo(), ";
  /*
    sb << "&" << mi->_classInfo->name << "_method_" << mi->getJavaSignature(true, mi->args->size());
  else
  */
  sb << "0);\n";

  if (mi->returnType->isVoidClazz() == false) 
  {
    sb << "    return ";
    if (mi->returnType->getMetaInfo()->isEnumInfo() == true || hasEnumReturn(mi) == true)
    {
      sb << "(" << getReturnType(mi) << ")(int)";
    }      
    else if (mi->returnType->isBasicClazz() == false) 
    {
      sb << "(";
      mi->returnType->toTypeString(sb, TpFtFormatStandard);
      sb << ")(::acdk::lang::RObject)";
    }
    sb << "__acdk_retval;\n";
  }
  sb << "  }\n";
  
}

RString getScriptVarAccessor(const ClazzMethodArgInfo* ai)
{
 if (ai->type->isBoolClazz() == true)
 {
   if (ai->flags & MiAiOut)
     return "getBoolRef()"; 
   else
    return "getBoolVar()";
  }
  if (ai->type->isCharClazz() == true)
  {
   if (ai->flags & MiAiOut)
     return "getCharRef()"; 
   else
    return "getCharVar()";
  }
  if (ai->type->isUcCharClazz() == true)
  {
   if (ai->flags & MiAiOut)
     return "getUcCharRef()"; 
   else
    return "getUcCharVar()";
  }
  if (ai->type->isByteClazz() == true)
  {
   if (ai->flags & MiAiOut)
     return "getByteRef()"; 
   else
    return "getByteVar()";
  }
  if (ai->type->isShortClazz()  == true)
  {
   if (ai->flags & MiAiOut)
     return "getShortRef()"; 
   else
    return "getShortVar()";
  }
  if (ai->type->isIntClazz() == true)
  {
   if (ai->flags & MiAiOut)
     return "getIntRef()"; 
   else
    return "getIntVar()";
  }
  if (ai->type->isLongClazz()  == true)
  {
   if (ai->flags & MiAiOut)
     return "getLongRef()"; 
   else
    return "getLongVar()";
  }
  if (ai->type->isFloatClazz()  == true)
  {
   if (ai->flags & MiAiOut)
     return "getFloatRef()"; 
   else
    return "getFloatVar()";
  }
  if (ai->type->isDoubleClazz()  == true)
  {
   if (ai->flags & MiAiOut)
     return "getDoubleRef()"; 
   else
    return "getDoubleVar()";
  }
  else 
  {
    if (ai->flags & MiAiOut)
     return "getObjectRef()"; 
   else
    return "getObjectVar()";
  }
}



RString getScriptVarCaster(const ClazzMethodArgInfo* ai)
{
  if (ai->type->isBasicClazz() == true)
  {
    if (ai->type->isIntClazz() == true &&  
      ClazzAttributesRes::hasAttribute((MetaInfo*)ai, "__enumArgInfo"))
    {
      return getArgEnumCaster(ai);
    }
    return "";
  }
  StringBuffer sb;
  sb << "(";
  ai->type->toTypeString(sb, TpFtAcdkType | TpFtRHPrefix | TpFtFqName);
  if (ai->flags & MiAiOut)
    sb << "&";
  sb << ")";
  return sb.toString();
}

void 
DmiProxyGeneratorExt::generateDispatchMethod(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb)
{
  RString methodsig = mi->toTypeString(0, TpFtACDKSignature);
  RString fname = mi->name;
  sb 
    << "  static const ::acdk::lang::dmi::ClazzMethodInfo*\n"
    << "  " << methodsig << "_dispatch(::acdk::lang::Object* This_, IN(::acdk::lang::RString) fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzInfo* clazzinfo, const ::acdk::lang::dmi::ClazzMethodInfo* methinf)\n"
    << "  {\n"
    << "    "
    ;
  if (mi->returnType->isVoidClazz() == false)
  {
    sb << "ret = ";
    if (mi->flags & MiMiConstructor)
    {
      sb << "(::acdk::lang::RObject) new " << fname << "_DmiProxy";
    }
  }
  else
  {
    // ### TODO implement
  }
  sb << "(";
  int i;
  int argcount = mi->getArgumentCount();
  for (i = 0; i < argcount; ++i)
  {
    const ClazzMethodArgInfo* ai = mi->methodArgs[i];
    if (i > 0)
      sb << ", ";
    sb << getScriptVarCaster(ai) << "args[" << i << "]." << getScriptVarAccessor(ai);
  }
  sb 
    << ");\n"
    << "    return methinf;\n"
    << "  }\n";
}

bool hasProtectedDefaultConstructor(const acdk::lang::dmi::ClazzInfo* ci)
{
  for (int i = 0; i < ci->getMethodsCount(); ++i)
  {
    const acdk::lang::dmi::ClazzMethodInfo* mi = ci->methods[i];
    if ((mi->flags & MiMiConstructor) && (mi->flags & MiProtected) && mi->getArgumentCount() == 0)
      return true;
  }
  return false;
}

void 
DmiProxyGeneratorExt::generateProxyClassDecl(const acdk::lang::dmi::ClazzInfo* ci, 
                                             StringBuffer& sb,
                                             ClazzMethodInfoArray& constr, ClazzMethodInfoArray& methods)
{
  RString clsname = ci->name;
  sb 
    << "class " << clsname << "_DmiProxy\n"
    ;
  if (ci->flags & MiCiInterface)
    sb 
      << ": extends ::acdk::lang::Object\n"
      << ", implements " << clsname << "\n";
  else
    sb
      << ": extends " << clsname << "\n";
  sb
    << ", implements ::acdk::lang::dmi::DmiProxyBase\n"
    << "{\n"
    << "  ACDK_PROXY_WITH_METAINFO(" << clsname << ")\n"
    << "public:\n"
  ;
  sb << "  ::acdk::lang::Object* _cast(const ::acdk::lang::dmi::ClazzInfo* ci)\n"
        "  {\n"
        "    ::acdk::lang::Object* ret = _dmiProxyCast(ci);\n"
        "    if (ret != 0)\n"
        "      return ret;\n"
        "    ret =  " << clsname << "::_cast(ci);\n"
        "    return ret;\n"
      "  }\n"
      "  virtual void getCollectableFields(FieldReferences& fields)\n"
      "  {\n"
      "    ACDK_FQ_SUPER_QUALIFIER(" << RString(ci->ns)->replace("/", "::") << "::, " << ci->name  << ")::getCollectableFields(fields);\n"
      "    fields.push_back((::acdk::lang::RObject*)_dmiTarget._ref_this());\n"
      "  }\n"
      ;
  if ((ci->flags & MiCiInterface) == MiCiInterface || hasProtectedDefaultConstructor(ci) == true)
  {
    sb << "  static ::acdk::lang::RObject create_instance() { return new " << clsname << "_DmiProxy(); }\n";
  } 
  sb << "  virtual bool _gc_releaseRef(bool force = false) const { return ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_gc_releaseRef(this); }\n";
  sb << "  ::acdk::lang::Object* getDmiTarget(bool& forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci) { return _dmiProxygetDmiTarget(forwarded, ci); }\n";
  int i;
  if (constr.size() == 0)
  {
    ACDK_NLOG("acdk.tools.mc", Error, SBSTR("Class " <<  ci->name << " has not defined a constructor. A constructor is needed for generating a DmiProxy"));
  }
  for (i = 0; i < constr.size(); ++i)
  {
    generateProxyClassDeclDefConstructor(constr[i], sb);
  }
  for (i = 0; i < methods.size(); ++i)
  {
    generateProxyClassMethod(ci, methods[i], sb);
  }
  for (i = 0; i < constr.size(); ++i)
  {
    generateDispatchMethod(constr[i], sb);
  }
  sb << "};\n\n";
}

RString clazzInfoExpr(const ClazzInfo* ci)
{
  if (ci->isBoolClazz() == true)
    return "::acdk::lang::dmi::ClazzInfo::getBoolClazz()";
  if (ci->isCharClazz() == true)
    return "::acdk::lang::dmi::ClazzInfo::getCharClazz()";
  if (ci->isUcCharClazz() == true)
    return "::acdk::lang::dmi::ClazzInfo::getUcCharClazz()";
  if (ci->isByteClazz() == true)
    return "::acdk::lang::dmi::ClazzInfo::getByteClazz()";
  if (ci->isShortClazz()  == true)
    return "::acdk::lang::dmi::ClazzInfo::getShortClazz()";
  if (ci->isIntClazz() == true)
    return "::acdk::lang::dmi::ClazzInfo::getIntClazz()";
  if (ci->isLongClazz()  == true)
    return "::acdk::lang::dmi::ClazzInfo::getLongClazz()";
  if (ci->isFloatClazz()  == true)
    return "::acdk::lang::dmi::ClazzInfo::getFloatClazz()";
  if (ci->isDoubleClazz()  == true)
    return "::acdk::lang::dmi::ClazzInfo::getDoubleClazz()";
  if (ci->isVoidClazz() == true)
    return "::acdk::lang::dmi::ClazzInfo::getVoidClazz()";
  return ci->toTypeString(TpFtAcdkType | TpFtFqName) + "::clazzInfo()";
}

void 
DmiProxyGeneratorExt::generateProxyCiMethod(const acdk::lang::dmi::ClazzMethodInfo* mi, StringBuffer& sb)
{
  RString fname = mi->name;
  if (mi->flags & MiMiConstructor)
    fname = fname + "_DmiProxy";

  RString cname = mi->_scopeParent->name;
  int argcount = mi->getArgumentCount();
  RString methodsig = mi->toTypeString(0, TpFtACDKSignature);
  int i;
  for (i = 0; i < argcount; ++i)
  {
    const ClazzMethodArgInfo* ai = mi->methodArgs[i];
    RString argname = ai->name;
    RString argns = ai->ns;
    
    sb 
      << "::acdk::lang::dmi::ClazzMethodArgInfo " << cname << "_DmiProxy_methods_" << methodsig
      << "_arg_" << argname << " =\n{\n"
      << "  " << MetaInfo::flagsToString(ai->flags, MethodArgInfoExtFlags(0), TpFtFqName | TpFtAcdkType) << ",\n"
      << "  0, //AttributesRes\n"
      << "  \"" << argname << "\",\n"
      << "  -1, // hashCode\n"
      << "  \"" << argns << "\", // ns\n"
      << "  0, // _scopeParent\n"
      << "  0, // _nextSibling\n"
      << "  "
      ;
    
    ;
    sb << clazzInfoExpr(ai->type)  << "\n"
      << "};\n\n"
      ;
  }

  sb << "::acdk::lang::dmi::ClazzMethodArgInfo* " << cname << "_methods_" << methodsig << "_args[] = \n{\n";
  for (i = 0; i < argcount; ++i)
  {
    const ClazzMethodArgInfo* ai = mi->methodArgs[i];
    RString argname = ai->name;
    sb << "  &" << cname << "_DmiProxy_methods_" << methodsig
      << "_arg_" << argname << ",\n";
  }
  sb 
    << "  0\n};\n\n";
  
  

  sb 
    << "::acdk::lang::dmi::ClazzMethodInfo " << cname << "_DmiProxy_methods_" << methodsig << " = \n{\n"
    << "  " << MetaInfo::flagsToString(mi->flags, MethodInfoExtFlags(0), TpFtFqName | TpFtAcdkType) << ",\n"
    << "  0, //AttributesRes\n" 
    << "  \"" << fname << "\",\n"
    << "  -1, // hashCode\n"

    << "  \"\", // ns\n"
    << "  0, // _scopeParent\n" 
    << "  0, // _nextScopeSibling\n" 
    << "  " << cname << "::clazzInfo(), // returnType\n"
    << "  \"" << (RString)mi->altlabel << "_DmiProxy\", // altname\n"
    << "  -1, // altnamehashCode\n"
    << "  " << cname << "_methods_" << methodsig << "_args,\n"
    << "  0, // argumentCount\n"
    << "  0, // excpetions,\n" // ### TODO
    << "  " << cname << "_DmiProxy::" << methodsig << "_dispatch,\n" // ### TODO
    << "  ::acdk::lang::dmi::ClazzMethodInfo::DefaultDispatchThrowableFunc, \n" // ### TODO
    << "  0 // cached methodhash\n"
    << "};\n\n"
    ;

}

void 
DmiProxyGeneratorExt::generateProxyCiInterfaces(const acdk::lang::dmi::ClazzInfo* ci, StringBuffer& sb)
{
  RString cname = ci->name;

  sb 
    << "::acdk::lang::dmi::ClazzSuperInfo " << cname << "_DmiProxy_super" << " =\n{\n"
    << "  ::acdk::lang::dmi::MiPublic,\n"
    << "  0, //AttributesRes\n"
    << "  " << cname << "::clazzInfo()\n};\n\n"
    ;

  sb 
    << "::acdk::lang::dmi::ClazzSuperInfo* _" << cname << "_DmiProxy_interfaces[] =\n{\n"
    << "  &" << cname << "_DmiProxy_super,\n"
    << "  0\n};\n\n"
    ;
}

void 
DmiProxyGeneratorExt::generateProxyCiMethods(const acdk::lang::dmi::ClazzInfo* ci, ClazzMethodInfoArray& methods, StringBuffer& sb)
{
  RString cname = ci->name;
  int i;
  for (i = 0; i < methods.size(); ++i)
    generateProxyCiMethod(methods[i], sb);

  sb << "::acdk::lang::dmi::ClazzMethodInfo* " << cname << "_methods[] = \n{\n";
  for (i = 0; i < methods.size(); ++i)
  {
    const acdk::lang::dmi::ClazzMethodInfo* mi = methods[i];
    RString methodsig = mi->toTypeString(0, TpFtACDKSignature);
    sb << "  &" << cname << "_DmiProxy_methods_" << methodsig << ",\n";
  }
  sb << "  0\n};\n\n";
}

void
createStandardConstructor(const acdk::lang::dmi::ClazzInfo* ci, ClazzMethodInfoArray& constructors)
{
  static acdk::lang::dmi::ClazzMethodInfo mi;
  memset(&mi, 0, sizeof(mi));
  mi.flags = MiMiConstructor | MiMethodInfo;
  mi.name = ci->name;
  mi._scopeParent = (const NamedScopedMetaInfo*)ci;
  /*
  const char* ns;
  mutable const NamedScopedMetaInfo* _scopeParent;
  mutable const NamedScopedMetaInfo* _nextScopeSibling;
  */
  mi.returnType = ci;
  /*
  const char* altlabel;
  ClazzMethodArgInfo** methodArgs;
  ClazzInfo** exceptions;
  */
  constructors.push_back(&mi);
}

void 
DmiProxyGeneratorExt::generateProxy(const acdk::lang::dmi::ClazzInfo* ci, StringBuffer& sb)
{
  ci = ci->loadFullClazzInfo(false, true);
  if (ci->flags & MiNoDmiProxy)
    return;
  ClazzMethodInfoArray constructors;
  if (ClazzInfo_findConstructors(ci, constructors) == false)
    return;

  if (constructors.size() == 0)
  {
    createStandardConstructor(ci, constructors);
  }
  ClazzMethodInfoArray virtualmethods;
  ClazzInfo_getVirtualMethods(ci, virtualmethods);
  if (virtualmethods.size() == 0)
    return;

  generateProxyClassDecl(ci, sb, constructors, virtualmethods);
  
  generateProxyCiInterfaces(ci, sb);
  generateProxyCiMethods(ci, constructors, sb);
  

  RString clsname = ci->name;
  RString nsname = ci->ns;
  

  sb << "::acdk::lang::dmi::ClazzInfo* " << clsname << "_DmiProxy::clazzInfo()\n"
     << "{\n"
     << "static ::acdk::lang::dmi::ClazzInfo _clazzInfo =\n"
     << "  {\n"
     << "    ::acdk::lang::dmi::MiClazzInfo | ::acdk::lang::dmi::MiResolved, // clazz-flags\n"
     << "    0, //AttributesRes\n"
     << "    \"" << clsname << "_DmiProxy\", // name of class\n"
     << "  -1, // hashCode\n"
     << "    \"" << nsname << "\", // the namespace\n"
     << "     0, // _scopeParent\n"
     << "     0, // _nextSibling\n"
     << "     0, // type\n"
     << "     0, // _firstChild\n"
     << "     _" << clsname << "_DmiProxy_interfaces, // pointer to Array of ClazzInfo references\n"
     << "     0, // count of Super / Interfaces\n"
     << "     0, // pointer to Array of fields\n"
     << "     0, // count of Fields\n"
     << "     " << clsname << "_methods, // pointer to Array of Methods\n"
     << "     0, // count of Methods\n"
     ;
  if ((ci->flags & MiCiInterface) == MiCiInterface || hasProtectedDefaultConstructor(ci) == true)
    sb << "     " << clsname << "_DmiProxy::create_instance, // create-function for cloning/serializing\n";
  else
    sb << "     0, // create-function for cloning/serializing\n";

  sb
     << "     0, // create-function for cloning/serializing arrays\n"
     << "     0, // create-function for cloning/serializing arrays\n"
     << "     0, // Class* thisClass; chaching instance\n"
     << "     0, // jlong serialVersionUID; for serialization\n"
     << "     ::acdk::lang::dmi::StdDispatch::_invoke_dynamic, // dynamic_dispatch\n"
     << "     ::acdk::lang::dmi::StdDispatch::_invoke_static, // static_dispatch\n"
     << "     0, // count off all collectable members in this class\n"
     << "     0, // user defined info\n"
     << "     0 // next ClazzInfo in chain\n"
     << "  };\n"
     << "  static ::acdk::lang::dmi::RegisterClazzInfo _register_clazzInfo(&_clazzInfo);\n"
     << "  return &_clazzInfo;\n"
     << "};\n"
     << "static ::acdk::lang::dmi::RegisterClazzInfo _register_" << clsname << "_DmiProxy(" << clsname << "_DmiProxy::clazzInfo());\n"
     ;

}


void 
DmiProxyGeneratorExt::generateProxy(IN(RString) classname, IN(::acdk::io::RPrintWriter) out)
{
  if (classname->equals("acdk/lang/ObjectArrayBaseImpl") == true) // ## hack
    return;
  RClass cls = Class::forName(classname);
  StringBuffer sb;
  generateProxy(cls->objectClazzInfo(), sb);
  if (sb.length() > 0)
    out->println(sb.toString());
}

} // namespace mc
} // namespace tools
} // namespace acdk


