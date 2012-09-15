
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


#include "EnumArgAttribute.h"
#include "MetaCompiler.h"
#include <acdk/locale/Encoding.h>
#include "FieldInfo.h"

namespace acdk {
namespace tools {
namespace mc {


EnumArgAttribute::EnumArgAttribute(IN(RString) enumname)
: enumName(enumname)
{
}



//static 
void 
EnumArgAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("StringTag", "acdk.tools.mc.EnumArgAttribute");
}
//foreign virtual 
bool 
EnumArgAttribute::apply(IN(RCodeInfo) cm)
{
  return attachAttribute(cm);
}

//virtual 
bool EnumArgAttribute::apply(IN(RModuleInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool EnumArgAttribute::apply(IN(RClassInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool EnumArgAttribute::apply(IN(RMethodInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool EnumArgAttribute::apply(IN(RArgumentInfo) cm) { return attachAttribute((RCodeInfo)cm); }
//virtual 
bool EnumArgAttribute::apply(IN(RFieldInfo) cm) { return attachAttribute(&cm); }

bool 
EnumArgAttribute::attachAttribute(IN(RCodeInfo) ci)
{
  RString identifier = ci->getMetaInfoCIdentifier();
  bool isPointer = false;
  if (instanceof(ci, ClassInfo) == true)
    isPointer = true;

  StringBuffer sb("");

  sb << "    ::acdk::lang::dmi::ClazzAttributesRes::attachAttribute((::acdk::lang::dmi::MetaInfo*)"; 
  if (isPointer == false)
    sb << "&";
  RString enname = enumName;
  /*
  RString nsname = "";
  int idx = enumName->lastIndexOf(":");
  if (idx != -1)
  {
    enname = enumName->substr(idx + 1);
    nsname = enumName->substr(0, idx - 1);
  }
  */
  /*
  sb << identifier 
     << ", \"__enumArgInfo\", "
      << "::acdk::lang::dmi::ClazzAttributeResValue::makeFunctionPtrRes((void*)::acdk::lang::dmi::ClazzEnumInfo::findEnum(\"" 
    << enname << "\", \"" << nsname << "\")));\n";
    */
  sb << identifier 
     << ", \"__enumArgInfo\", "
      << "::acdk::lang::dmi::ClazzAttributeResValue::makeFunctionPtrRes((void*)" << enname << "MetaInf::GetEnumInfo()));\n";
  
  ci->addCode(sb.toString(), ModuleInit);

  return true;
}

} // namespace mc
} // namespace tools
} // namespace acdk


