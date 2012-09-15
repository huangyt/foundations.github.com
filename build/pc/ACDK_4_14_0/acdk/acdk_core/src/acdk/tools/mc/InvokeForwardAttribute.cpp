
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


#include "InvokeForwardAttribute.h"
#include "ArgumentInfo.h"

namespace acdk {
namespace tools {
namespace mc {

using namespace acdk::lang::dmi;

bool
InvokeForwardAttribute::applyToMethod(IN(RMethodInfo) methodinfo)
{
  
  if (methodinfo->flags & MiStatic)
    return false;
  if (methodinfo->flags & MiMiConstructor)
    return false;
  RInvokeForwardAttribute ifa = (RInvokeForwardAttribute)methodinfo->getCodeAttribute("acdk/tools/mc/InvokeForwardAttribute");
  if (ifa != Nil && ifa->_generate == false)
    return false;
  if (methodinfo->name->equals("getClass") == true)
    return false;
  StringBuffer sb;
  sb << methodinfo->returnType << "\n"
     << methodinfo->_classInfo->name << "::" << methodinfo->name << "(";
  RIterator it = methodinfo->args->iterator();
  bool isFirst = true;
  int ac = 0;
  while (it->hasNext() == true)
  {
    if (isFirst == false)
      sb << ", ";
    isFirst = false;
    RArgumentInfo ai(it->next());
    sb << ai->toCode();
  }
  sb << ")";
  if (methodinfo->_throws->size() > 0)
  {
    sb << " THROWS" << methodinfo->_throws->size() << "(";
    ac = 0;
    RIterator throws = methodinfo->_throws->iterator();
    while (throws->hasNext() == true)
    {
      RString ct = (RString)throws->next();
      if (ac > 0)
        sb << ", ";
      sb << ct;
      ++ac;
    }
    sb << ")";
  }
  sb << "\n{\n";
  sb << "  ::acdk::lang::dmi::ScriptVar __acdk_retval;\n";
  sb << "  ::acdk::lang::dmi::ScriptVarArray __acdk_args(";
  sb << methodinfo->args->size();
  sb << ");\n";
  ::acdk::util::RIterator ait = methodinfo->args->iterator();
  int i = 0;
  while (ait->hasNext() == true) 
  {
    RArgumentInfo ai = (RArgumentInfo)ait->next();
    sb << "  __acdk_args["<< i << "] = ";
    if (ai->flags & MiAiOut && ai->flags & MiAiIn)
    {
      sb << "inoutOf(" << ai->name << ");\n";
    } else if (ai->flags & MiAiOut)
      sb << "outOf(" << ai->name << ");\n";
    else
      sb << "inOf(" << ai->name << ");\n";
    ++i;
  }
  sb << "  const ::acdk::lang::dmi::ClazzInfo* ci__ = getClazzInfo();\n";
  sb << "  ::acdk::lang::Object* targetObj__ = getDmiTarget(ci__);\n";
  sb << "  targetObj__->standardDispatch(\"" << STDDSP_STRING_CAST(methodinfo->name) << "\", __acdk_retval, __acdk_args, targetObj__->getDmiClient(), Nil, 0, targetObj__->getClazzInfo(), &";
  sb << methodinfo->_classInfo->name << "_method_" <<  methodinfo->getJavaSignature(true, methodinfo->args->size());
  sb << ");\n";

  if (methodinfo->returnType->equals("void") == false)
  {
      sb << "  return ";
    if (methodinfo->hasType(methodinfo->returnType) == TsEnum)
    { 
      sb << "(" << methodinfo->returnType << ")(int)";
    } 
    else if (methodinfo->isBasicType(methodinfo->returnType) == false) 
    {
      sb << "(" << methodinfo->returnType << ")(::acdk::lang::RObject)";
    }
    sb << "__acdk_retval;\n";
  }
  sb << "}\n\n";
  methodinfo->_classInfo->addCode(sb.toString(), ModuleAfterDispatch);
  return true;
}




bool 
InvokeForwardAttribute::apply(IN(RCodeInfo) codeinfo)
{
  if (instanceof(codeinfo, MethodInfo) == true)
    return applyToMethod(RMethodInfo(codeinfo));
  if (instanceof(codeinfo, ClassInfo) == false)
    return false;
  RClassInfo ci(codeinfo);
  acdk::util::RIterator it = ci->_methods->iterator();
  while (it->hasNext() == true)
  {
    applyToMethod(RMethodInfo(it->next()));
  }
  return true;
}

} // namespace mc
} // namespace tools
} // namespace acdk

