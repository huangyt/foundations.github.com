
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



#include "ThrowableAttribute.h"
#include "ClassInfo.h"

namespace acdk {
namespace tools {
namespace mc {

//virtual 
bool 
ThrowableAttribute::apply(IN(RCodeInfo) cm)
{
  if (instanceof(cm, ClassInfo) == false)
    return false;
  RClassInfo ci(cm);
  RString identifier = ci->getMetaInfoCIdentifier();
  RString label = ci->name;
  StringBuffer sb("");
  sb << "\nvoid ThrowException_" << label << "(IN(::acdk::lang::RThrowable) ex)\n{\n  "
     << "throw R" << ci->name << "(ex);\n}\n\n";
  ci->addCode(sb.toString(), ModuleBeforeDispatch);
  sb.reset();
  sb << "    ::acdk::lang::dmi::ClazzAttributesRes::attachAttribute((::acdk::lang::dmi::MetaInfo*)"; 
  RString key = "__throwExceptionFunc";
  // ### FIXME sb << identifier << ", \"" << acdk::locale::Encoding::getCEscapeEncoding()->encode(key) << "\"";
  sb << identifier << ", \"" << key << "\"";
  sb << ", ::acdk::lang::dmi::ClazzAttributeResValue(0xFFFF, (void*)&ThrowException_" << label << ")";
  sb << ");\n";
  ci->addCode(sb.toString(), ModuleInit);

  //not needed ci->addCode("#include <acdk/lang/dmi/ClazzAttributesRes.h>\n", ModuleInclude);
  return true;
}

} // namespace mc
} // namespace tools
} // namespace acdk


