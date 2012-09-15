
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


#include "DmiProxyGenerator.h"
#include "MetaCompiler.h"
#include <acdk/locale/Encoding.h>
#include "FieldInfo.h"
#include "SuperInfo.h"

namespace acdk {
namespace tools {
namespace mc {

using namespace acdk::lang::dmi;

//static 
void 
DmiProxyGenerator::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("DmiProxy", "acdk.tools.mc.DmiProxyGenerator");
}

//static
void 
DmiProxyGenerator::addToClass(IN(RClassInfo) ci)
{
  //return;
  RCodeAttributeArray cas = ci->getCodeAttributes();
  for (int i = 0; i < cas->length(); ++i)
  {
    RCodeAttribute ca = cas[i];
    if (instanceof(ca, DmiProxyGenerator) == true)
      return;
  }
  ci->addCodeAttribute(new DmiProxyGenerator());
}

//foreign 
void 
DmiProxyGenerator::writeMethodProxy(IN(RMethodInfo) mi, StringBuffer& sb)
{
  if (mi->flags & MiStatic ||
      mi->flags & MiPrivate ||
      mi->isConstructor() == true ||
      mi->name->equals("getClass") == true)
    return;
  //if (Modifier::isClazzInfo(mi->flags) == false || (Modifier::isDestructor(mi->flags) == true))
  if (mi->isDestructor() == true)
    return;

  sb.append("  " + mi->returnType + " " + mi->name + "(");

  RIterator ait = mi->args->iterator();
  int ac = 0;
  while (ait->hasNext() == true) 
  {
    if (ac > 0)
      sb.append(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    sb.append(ai->toCode());
    ++ac;

  }
  sb.append(")");
  if (mi->_throws->size() > 0)
  {
    sb.append(RString(" THROWS") + mi->_throws->size() + "(");
    ac = 0;
    RIterator throws = mi->_throws->iterator();
    while (throws->hasNext() == true)
    {
      RString ct = (RString)throws->next();
      if (ac > 0)
        sb.append(", ");
      sb.append(ct);
      ++ac;
    }
    sb.append(")");
  }
  
  sb.append("\n  {\n");
  sb.append("    ::acdk::lang::dmi::ScriptVar __acdk_retval;\n");
  sb.append("    ::acdk::lang::dmi::ScriptVarArray __acdk_args(");
  sb.append(mi->args->size());
  sb.append(");\n");
  ait = mi->args->iterator();
  int i = 0;
  while (ait->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    sb.append(RString("    __acdk_args[") + i + "] = ");
    if (ai->flags & MiAiOut && ai->flags & MiAiIn)
    {
      sb.append("inoutOf(" + ai->name + ");\n");
    } else if (ai->flags & MiAiOut)
      sb.append("outOf(" + ai->name + ");\n");
    else
      sb.append("inOf(" + ai->name + ");\n");
    ++i;
    
  }
  sb << "    _dmiTarget->standardDispatch(\"" << STDDSP_STRING_CASTPLS(mi->name) 
     << "\", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiTarget->clazzInfo(), ";
  if (mi->generateMetaInfo(true) == true && mi->getNoDefaultArgCount() == mi->getArgCount())
    sb << "&" << mi->_classInfo->name << "_method_" << mi->getJavaSignature(true, mi->args->size());
  else
    sb << "0";
  sb << ");\n";

  if (mi->returnType->equals("void") == false)
  {
      sb.append("    return ");
    if (mi->hasType(mi->returnType) == TsEnum)
    { 
      sb.append("(" + mi->returnType + ")(int)");
    } 
    else if (mi->isBasicType(mi->returnType) == false) 
    {
      sb.append("(" + mi->returnType + ")(::acdk::lang::RObject)");
    }
    sb.append("__acdk_retval;\n");
  }
  sb.append("  }\n");
}

bool isObjectClass(IN(RClassInfo) ci)
{
  if (ci->name->equals("Object") == false)
    return false;
   return ci->getNamespaceAccessor()->equals("acdk/lang") == true;
}
bool 
DmiProxyGenerator::createProxyClass(IN(RClassInfo) ci, StringBuffer& sb)
{
    // check if proxy can be generated
  if (_parentOnly == true)
    return false;
  bool hasAnyConstructor = false;
  bool hasDefaultConstructor = false;
  bool hasVirtualMethods = false;
  RIterator it = ci->_orgMethods->iterator();
  while (it->hasNext() == true)
  {
    RMethodInfo mi = (RMethodInfo)it->next();
    if (mi->isConstructor() == true)
    {
      hasAnyConstructor = true;
      if (mi->isPublic() == true && mi->args->size() == 0)
        hasDefaultConstructor = true;
    }
    if (mi->isVirtual() == true)
      hasVirtualMethods = true;
  }
  if (hasVirtualMethods == false)
    return false;
  if (((ci->isInterface() == true && hasAnyConstructor == false) || hasDefaultConstructor == true) == false)
    return false;
    

  sb << "\nclass " << ci->name << "_DmiProxy\n";
  sb.append(": implements ::acdk::lang::dmi::DmiProxyBase\n");
  if (ci->isInterface() == true && isObjectClass(ci) == false)
  {
    sb << ", extends ::acdk::lang::Object\n"
       << ", implements " << ci->name;
  } 
  else
  {
    sb << ", extends " << ci->name;
  }
  sb << "\n{\n"
     << "public:\n"
     << "  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return " << ci->name << "::clazzInfo(); } \n"
     << "  virtual Object* getDmiTarget(OUT(bool) forwarded, const ::acdk::lang::dmi::ClazzInfo*& ci) { return ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::getDmiTarget(forwarded, ci); }\n"
     << "  virtual Object* _cast(const ::acdk::lang::dmi::ClazzInfo* to) { return ACDK_FQ_SUPER_QUALIFIER(::acdk::lang::dmi::, DmiProxyBase)::_cast(to); }\n"
     << "  " << ci->name << "_DmiProxy(IN(RObjectArray) proxies, IN(::acdk::lang::RObject) delegate, int flags)\n"
     << "  : DmiProxyBase(proxies, delegate, flags, " << ci->name << "::clazzInfo())\n  {}\n";

  it = ci->_orgMethods->iterator();
  while (it->hasNext() == true) 
  {

    RMethodInfo mi = RMethodInfo(it->next()); 
    if (mi->isVirtual())
      writeMethodProxy(mi, sb);
  }
  sb.append("};\n\n");

  return true;
}

//virtual 
bool 
DmiProxyGenerator::apply(IN(RCodeInfo) ci) 
{ 
  return false; // ## deactivated
  if (_generateProxy == false)
    return false;
  if (instanceof(ci, ClassInfo) == false)
    return false;
  
  RClassInfo cm(ci);
     
  
  StringBuffer sb;
  bool hasProxy = createProxyClass(cm, sb);
  
  sb << "void " << cm->name << "_create_proxy(IN(RObjectArray) proxies, IN(RObject) dmiTarget, int flags)\n{\n";
  if (hasProxy == true)
  {
    sb << "  proxies->insert(0, new " << cm->name << "_DmiProxy(proxies, dmiTarget, flags));\n";
  }
  else
  {
    RIterator it = cm->_derivides->iterator();
    while (it->hasNext() == true)
    {
      RSuperInfo si = (RSuperInfo)it->next();
      sb << "  " << si->name << "::GetClass()->getDmiProxies(proxies, dmiTarget, flags);\n";
    }
  }
  sb << "}\n\n";

  cm->addCode(sb.toString(), ModuleBeforeInit);
  
   
  

  RString identifier = ci->getMetaInfoCIdentifier();
  sb.set("");
  sb << "    ::acdk::lang::dmi::ClazzAttributesRes::attachAttribute((::acdk::lang::dmi::MetaInfo*)"; 
  //sb << "&";
  sb << identifier 
     << ", \"__dmiProxyCreator\", "
     << "::acdk::lang::dmi::ClazzAttributeResValue::makeFunctionPtrRes((void*)&" << cm->name << "_create_proxy));\n";
  ci->addCode(sb.toString(), ModuleInit);

  return true;
}

/*
bool 
DmiProxyGenerator::attachAttribute(IN(RCodeInfo) ci)
{
  RString identifier = ci->getMetaInfoCIdentifier();
  bool isPointer = false;
  if (instanceof(ci, ClassInfo) == true)
    isPointer = true;
  RString structidentifier = identifier->replace("::", "_")->replace("()", "") + "_ClazzAttributesResInitializer" + CodeAttribute::getCounter();
  StringBuffer sb("\nstruct ");
  
  sb << structidentifier
     << "\n{\n  " << structidentifier << "()\n  {\n    ";
  sb << "::acdk::lang::dmi::ClazzAttributesRes::attachAttribute((::acdk::lang::dmi::MetaInfo*)"; 
  if (isPointer == false)
    sb << "&";
  sb << identifier 
      << ", \"" << acdk::locale::Encoding::getCEscapeEncoding()->encode(key) << "\"";
  if (value != Nil)
    sb << ", ::acdk::lang::dmi::ClazzAttributeResValue::makeStringRes(\"" << acdk::locale::Encoding::getCEscapeEncoding()->encode(value) << "\")";
  sb << ");\n";
  sb << "  }\n};\n\n" << structidentifier << " " << structidentifier << "_instance;\n\n\n";
  ci->addCode(sb.toString(), ModuleBeforeDispatch);

  // not needed ci->addCode("#include <acdk/lang/dmi/ClazzAttributesRes.h>\n", ModuleInclude);
  return true;

}

*/

} // namespace mc
} // namespace tools
} // namespace acdk

