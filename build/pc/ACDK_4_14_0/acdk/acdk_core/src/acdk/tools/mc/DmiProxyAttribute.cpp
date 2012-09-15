
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


#include "DmiProxyAttribute.h"
#include "MetaCompiler.h"
#include <acdk/locale/Encoding.h>
#include "FieldInfo.h"

namespace acdk {
namespace tools {
namespace mc {

using namespace acdk::lang::dmi;

//static 
void 
DmiProxyAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("DmiProxy", "acdk.tools.mc.DmiProxyAttribute");
}



//foreign 
void 
DmiProxyAttribute::writeMethodProxy(IN(RMethodInfo) mi, StringBuffer& sb)
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
  sb.append("    _dmiserver->standardDispatch(\"" + STDDSP_STRING_CASTPLS(mi->name) + "\", __acdk_retval, __acdk_args, _dmiClient, Nil, 0, _dmiserver->clazzInfo(), &");
  sb.append(mi->_classInfo->name + "_method_" +  mi->getJavaSignature(true, mi->args->size()));
  sb.append(");\n");

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

  /*
  if (mi->returnType->equals("void") == false)
      sb.append("return ");
  if (mi->hasType(mi->returnType) == MethodInfo::Enum)
  {
    sb.append("(" + mi->returnType + ")(int)");
  } 
  else if (mi->isBasicType(mi->returnType) == false) 
  {
    sb.append("(" + mi->returnType + ")(::acdk::lang::RObject)");
  }

  sb.append("_dmiserver->invoke(\"" + mi->name + "\"");
  
  ait = mi->args->iterator();
  while (ait->hasNext() == true) 
  {
    sb.append(", ");
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    if (ai->flags & MiAiOut && ai->flags & MiAiIn)
      sb.append("inoutOf(" + ai->name + ")");
    else if (ai->flags & MiAiOut)
      sb.append("outOf(" + ai->name + ")");
    else
      sb.append("inOf(" + ai->name + ")");
    ++ac;
  }
  */
  sb.append("  }\n");
}


//virtual 
bool 
DmiProxyAttribute::apply(IN(RCodeInfo) ci) 
{ 
  if (instanceof(ci, ClassInfo) == false)
    return false;
  
  RClassInfo cm(ci);

  StringBuffer sb;
  sb << "\nclass " << cm->name << "_DmiProxy\n";
  sb.append(": extends ::acdk::lang::dmi::DmiProxy\n");
  sb.append(", implements " + cm->name + "\n{\n");
  sb.append("  virtual ::acdk::lang::dmi::ClazzInfo* getClazzInfo()  { return " + cm->name + "::clazzInfo(); } \n");
  //sb.append("  virtual ::acdk::lang::RClass getClass() { return " + cm->name + "::getClass(); } \n");
  //sb.append("  virtual const ::acdk::lang::dmi::ClazzMethodInfo* standardDispatch(const char* fname, ::acdk::lang::dmi::ScriptVar& ret, ::acdk::lang::dmi::ScriptVarArray& args, ::acdk::lang::dmi::DmiClient& dc, IN(::acdk::lang::RStringArray) namedArgs, int flags, const ::acdk::lang::dmi::ClazzMethodInfo* methinf = 0) { return  \\n");
  
  sb.append("public:\n");
  sb.append("  " + cm->name + "_DmiProxy(IN(::acdk::lang::RObject) delegate)\n");
  sb.append("  : DmiProxy(delegate)\n  {}\n");
  RIterator it = cm->_methods->iterator();
  while (it->hasNext() == true) 
  {

    RMethodInfo mi = RMethodInfo(it->next()); 
    if (mi->flags & MiMiVirtual)
      writeMethodProxy(mi, sb);
    //mi->writeDmiProxyMethod(out, this);
  }
  sb.append("};\n\n");
  sb.append("::acdk::lang::dmi::RDmiProxy\n" + cm->name + "::getDmiProxy(IN(::acdk::lang::RObject) dmiserver)\n{\n");
  sb.append("  return new " + cm->name + "_DmiProxy(dmiserver);\n}\n\n");
  cm->addCode(sb.toString(), ModuleAfterDispatch);
  return true;
}

/*
bool 
DmiProxyAttribute::attachAttribute(IN(RCodeInfo) ci)
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

