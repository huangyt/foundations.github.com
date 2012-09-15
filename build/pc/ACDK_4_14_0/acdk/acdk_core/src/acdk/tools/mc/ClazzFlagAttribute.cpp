
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


#include "ClazzFlagAttribute.h"
#include "MetaCompiler.h"
#include <acdk/locale/Encoding.h>
#include "FieldInfo.h"

namespace acdk {
namespace tools {
namespace mc {


ClazzFlagAttribute::ClazzFlagAttribute(int flags)
: _clazzFlags(flags)
{
}

//static 
void 
ClazzFlagAttribute::initAttribute(IN(RMetaCompiler) mc)
{
  mc->registerAttribute("ClazzFlags", "acdk.tools.mc.ClazzFlagAttribute");
}
//foreign virtual 
bool 
ClazzFlagAttribute::apply(IN(RCodeInfo) cm)
{
  return attachAttribute(cm);
}

//virtual 
bool ClazzFlagAttribute::apply(IN(RModuleInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool ClazzFlagAttribute::apply(IN(RClassInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool ClazzFlagAttribute::apply(IN(RMethodInfo) cm) { return attachAttribute(&cm); }
//virtual 
bool ClazzFlagAttribute::apply(IN(RArgumentInfo) cm) { return attachAttribute((RCodeInfo)cm); }
//virtual 
bool ClazzFlagAttribute::apply(IN(RFieldInfo) cm) { return attachAttribute(&cm); }

bool 
ClazzFlagAttribute::attachAttribute(IN(RCodeInfo) ci)
{
  ci->flags |= _clazzFlags;
  return true;
}

} // namespace mc
} // namespace tools
} // namespace acdk


