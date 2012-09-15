
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


#include "StringTagAttribute.h"
#include "MetaCompiler.h"
#include <acdk/locale/Encoding.h>
#include "FieldInfo.h"

namespace acdk {
namespace tools {
namespace mc {


StringTagAttribute::StringTagAttribute(IN(RString) k, IN(RString) val)
: key(k)
, value(val)
{
}



//static 
void 
StringTagAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("StringTag", "acdk.tools.mc.StringTagAttribute");
}
//foreign virtual 
bool 
StringTagAttribute::apply(IN(RCodeInfo) cm)
{
  return attachAttribute(cm);
}

//virtual 
bool StringTagAttribute::apply(IN(RModuleInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool StringTagAttribute::apply(IN(RClassInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool StringTagAttribute::apply(IN(RMethodInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool StringTagAttribute::apply(IN(RArgumentInfo) cm) { return attachAttribute((RCodeInfo)cm); }
//virtual 
bool StringTagAttribute::apply(IN(RFieldInfo) cm) { return attachAttribute(&cm); }

bool 
StringTagAttribute::attachAttribute(IN(RCodeInfo) ci)
{
  RString identifier = ci->getMetaInfoCIdentifier();
  bool isPointer = false;
  if (instanceof(ci, ClassInfo) == true)
    isPointer = true;
  StringBuffer sb("");
  sb << "    ::acdk::lang::dmi::ClazzAttributesRes::attachAttribute((::acdk::lang::dmi::MetaInfo*)"; 
  if (isPointer == false)
    sb << "&";
  sb << identifier 
     << ", \"" << acdk::locale::Encoding::getCEscapeEncoding()->getEncoder()->encode(key) << "\"";
  if (value != Nil)
      sb << ", ::acdk::lang::dmi::ClazzAttributeResValue::makeStringRes(\"" 
        << acdk::locale::Encoding::getCEscapeEncoding()->getEncoder()->encode(value) << "\", false)";
  sb << ");\n";
  ci->addCode(sb.toString(), ModuleInit);

  //ci->addCode("#include <acdk/lang/dmi/ClazzAttributesRes.h>\n", ModuleInclude);

  //ci->addCode("#include <acdk/lang/dmi/ClazzAttributesRes.h>\n", ModuleInclude);

  /*
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
  */
  return true;
}

} // namespace mc
} // namespace tools
} // namespace acdk


